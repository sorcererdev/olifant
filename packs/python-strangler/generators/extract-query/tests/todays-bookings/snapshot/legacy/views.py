"""The views. Each one does whatever it needs: SQL, rules, notifications.

Flask calls them through legacy/app.py; the reports and the nightly job call
some of them directly. Return values are dicts (rendered as JSON) or, for a
few, (dict, status) tuples.
"""

import csv
import io
from datetime import date, datetime

from legacy import db, utils
from bistro import switches
from bistro.shared.result import NotYet
from bistro.booking.queries.todays_bookings import TodaysBookings, TodaysBookingsInput

LAST_EXPORT = None


# ── bookings ────────────────────────────────────────────────────────────


def todays_bookings(day=None):
    # olifant:switch todays-bookings → booking/queries/todays_bookings
    if switches.on("todays-bookings"):
        result = getattr(NotImplemented, "olifant: wiring: the new query TodaysBookings built with its ports, as src/bistro/main.py builds it").handle(TodaysBookingsInput(getattr(NotImplemented, "olifant: args: this legacy function's parameters as the Input's fields")))
        if not isinstance(result, NotYet):
            return getattr(NotImplemented, "olifant: respond: what this legacy function returns for that result: the shape its callers expect")
    """Every booking for a day, with its table, earliest first."""
    d = utils.parse_day(day) if day else date.today()
    found = db.rows(
        "SELECT b.id, b.table_id, b.day, b.start, b.party, b.guest, b.status, t.area "
        "FROM bookings b JOIN tables t ON t.id = b.table_id "
        "WHERE b.day = ? AND b.status != 'cancelled' ORDER BY b.start, b.id",
        d.isoformat(),
    )
    for b in found:
        b["when"] = utils.format_slot(b["day"], b["start"])
    return {"day": d.isoformat(), "bookings": found, "count": len(found)}


def booking(booking_id):
    b = db.row("SELECT * FROM bookings WHERE id = ?", booking_id)
    if b is None:
        return {"error": "no such booking"}, 404
    b["when"] = utils.format_slot(b["day"], b["start"])
    return b


def make_booking(form):
    """A booking from the website form. Validates, finds a table, saves, texts."""
    errors = utils.check_booking_form(form)
    if errors:
        return {"errors": errors}, 400
    day = utils.parse_day(form.get("day"))
    party = int(form["party"])
    tables = utils.free_tables(day.isoformat(), form["start"], party)
    if form.get("table_id"):
        tables = [t for t in tables if t["id"] == form["table_id"]]
    if not tables:
        return {"errors": ["no table free then"]}, 409
    table = tables[0]
    booking_id = utils.next_booking_id()
    c = db.conn()
    c.execute(
        "INSERT INTO bookings (id, table_id, day, start, party, guest, phone, status, notes) VALUES (?, ?, ?, ?, ?, ?, ?, 'booked', ?)",
        (booking_id, table["id"], day.isoformat(), form["start"], party, form["guest"].strip(), form.get("phone"), form.get("notes")),
    )
    c.commit()
    db.audit(f"booked {booking_id} at {table['id']}")
    saved = db.row("SELECT * FROM bookings WHERE id = ?", booking_id)
    utils.confirm_booking(saved)
    deposit = utils.deposit_for(party)
    return {"id": booking_id, "table_id": table["id"], "deposit": deposit, "when": utils.format_slot(day, form["start"])}, 201


def cancel_booking(booking_id, reason=None):
    b = db.row("SELECT * FROM bookings WHERE id = ?", booking_id)
    if b is None:
        return {"error": "no such booking"}, 404
    if b["status"] == "cancelled":
        return {"error": "already cancelled"}, 409
    c = db.conn()
    c.execute("UPDATE bookings SET status = 'cancelled', notes = ? WHERE id = ?", (reason or b["notes"], booking_id))
    c.commit()
    db.audit(f"cancelled {booking_id}")
    utils.cancel_notice(b)
    return {"id": booking_id, "status": "cancelled"}


def move_booking(booking_id, start):
    b = db.row("SELECT * FROM bookings WHERE id = ?", booking_id)
    if b is None:
        return {"error": "no such booking"}, 404
    if not utils.within_hours(start):
        return {"error": "outside opening hours"}, 400
    free = utils.free_tables(b["day"], start, b["party"])
    same = [t for t in free if t["id"] == b["table_id"]]
    target = (same or free or [None])[0]
    if target is None:
        return {"error": "no table free then"}, 409
    c = db.conn()
    c.execute("UPDATE bookings SET start = ?, table_id = ? WHERE id = ?", (start, target["id"], booking_id))
    c.commit()
    db.audit(f"moved {booking_id} to {start}")
    return {"id": booking_id, "start": start, "table_id": target["id"]}


def mark_no_show(booking_id):
    b = db.row("SELECT * FROM bookings WHERE id = ?", booking_id)
    if b is None:
        return {"error": "no such booking"}, 404
    fee = utils.no_show_fee(b["party"])
    c = db.conn()
    c.execute("UPDATE bookings SET status = 'no-show', notes = ? WHERE id = ?", (f"fee {utils.money(fee)}", booking_id))
    c.commit()
    db.audit(f"no-show {booking_id}")
    return {"id": booking_id, "status": "no-show", "fee": fee}


# ── tables ──────────────────────────────────────────────────────────────


def tables_view(area=None):
    sql = "SELECT * FROM tables WHERE restaurant_id = ?"
    args = [utils.CURRENT_RESTAURANT]
    if area:
        sql += " AND area = ?"
        args.append(area)
    found = db.rows(sql + " ORDER BY id", *args)
    return {"tables": found}


def close_table(table_id):
    """Close a table for the evening: no new bookings on it."""
    t = utils.table(table_id)
    if t is None:
        return {"error": "no such table"}, 404
    c = db.conn()
    c.execute("UPDATE tables SET open = 0 WHERE id = ?", (table_id,))
    c.commit()
    utils.forget_table(table_id)
    db.audit(f"closed {table_id}")
    return {"id": table_id, "open": False}


def open_table(table_id):
    t = utils.table(table_id)
    if t is None:
        return {"error": "no such table"}, 404
    c = db.conn()
    c.execute("UPDATE tables SET open = 1 WHERE id = ?", (table_id,))
    c.commit()
    utils.forget_table(table_id)
    db.audit(f"opened {table_id}")
    return {"id": table_id, "open": True}


def seat_party(party, start=None, area=None):
    """Walk-ins: the best free table now, or why not. Several ways out."""
    error = utils.check_party(party)
    if error:
        return {"error": error}, 400
    party = int(party)
    start = start or datetime.now().strftime("%H:%M")
    if not utils.within_hours(start):
        return {"error": "kitchen closed"}, 409
    candidates = utils.free_tables(date.today().isoformat(), start, party)
    if area:
        candidates = [t for t in candidates if t["area"] == area]
        if not candidates:
            return {"error": f"nothing free in {area}"}, 409
    if not candidates:
        waiting = db.row("SELECT COUNT(*) AS n FROM waitlist WHERE restaurant_id = ?", utils.CURRENT_RESTAURANT)
        return {"error": "full", "waitlist": waiting["n"] if waiting else 0}, 409
    exact = [t for t in candidates if t["seats"] == party]
    best = (exact or candidates)[0]
    return {"table_id": best["id"], "seats": best["seats"], "start": start}


# ── exports (the accountant's CSV) ──────────────────────────────────────


def export_rows(day=None):
    """The day's bookings as CSV lines, header first: a generator."""
    global LAST_EXPORT
    d = utils.parse_day(day) if day else date.today()
    LAST_EXPORT = d.isoformat()
    yield "id,table,start,party,guest,status,deposit"
    for b in db.rows("SELECT * FROM bookings WHERE day = ? ORDER BY start, id", d.isoformat()):
        out = io.StringIO()
        csv.writer(out).writerow([b["id"], b["table_id"], b["start"], b["party"], b["guest"], b["status"], utils.deposit_for(b["party"])])
        yield out.getvalue().strip()


def export_csv(day=None):
    return "\n".join(export_rows(day)) + "\n"


# ── the dashboard ───────────────────────────────────────────────────────


def dashboard():
    today = todays_bookings()
    covers = sum(b["party"] for b in today["bookings"])
    tables = db.rows("SELECT COUNT(*) AS n, SUM(open) AS open FROM tables WHERE restaurant_id = ?", utils.CURRENT_RESTAURANT)[0]
    waiting = db.rows("SELECT guest, party FROM waitlist WHERE restaurant_id = ? ORDER BY id", utils.CURRENT_RESTAURANT)
    return {
        "bookings": today["count"],
        "covers": covers,
        "tables_open": tables["open"] or 0,
        "tables": tables["n"],
        "waitlist": waiting,
        "next": today["bookings"][0]["when"] if today["bookings"] else None,
    }


def add_to_queue(guest, party):
    """The host's walk-in queue (not the online waitlist)."""
    if utils.check_guest(guest):
        return {"error": "guest name looks wrong"}, 400
    c = db.conn()
    c.execute("INSERT INTO waitlist (restaurant_id, guest, party, added) VALUES (?, ?, ?, ?)", (utils.CURRENT_RESTAURANT, guest, int(party), datetime.now().isoformat(timespec="seconds")))
    c.commit()
    return {"queued": guest}


def form_booking(request):
    """The old HTML form posts here (a Flask request, read field by field)."""
    form = {
        "guest": request.form.get("guest", ""),
        "phone": request.form.get("phone"),
        "party": request.form.get("party", "0"),
        "start": request.form.get("start", ""),
        "day": request.form.get("day"),
        "notes": request.form.get("notes"),
    }
    return make_booking(form)
