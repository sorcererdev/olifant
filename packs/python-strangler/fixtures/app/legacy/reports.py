"""Reports for the owner: the week, no-shows, busy hours, the best tables.

Mostly SQL with a little arithmetic; format_slot (utils) shared with the
booking views.
"""

from collections import Counter, defaultdict
from datetime import date, timedelta

from legacy import db, utils


def week_summary(start=None):
    first = utils.parse_day(start) if start else date.today() - timedelta(days=date.today().weekday())
    days = [first + timedelta(days=i) for i in range(7)]
    out = []
    for d in days:
        rows = db.rows("SELECT party, status FROM bookings WHERE day = ?", d.isoformat())
        booked = [r for r in rows if r["status"] == "booked"]
        out.append(
            {
                "day": d.isoformat(),
                "bookings": len(booked),
                "covers": sum(r["party"] for r in booked),
                "cancelled": sum(1 for r in rows if r["status"] == "cancelled"),
                "no_shows": sum(1 for r in rows if r["status"] == "no-show"),
            }
        )
    totals = {k: sum(day[k] for day in out) for k in ("bookings", "covers", "cancelled", "no_shows")}
    return {"from": first.isoformat(), "days": out, "totals": totals}


def no_shows(since=None):
    since_day = utils.parse_day(since) if since else date.today() - timedelta(days=30)
    rows = db.rows(
        "SELECT id, guest, phone, day, start, party FROM bookings WHERE status = 'no-show' AND day >= ? ORDER BY day, start",
        since_day.isoformat(),
    )
    by_guest = defaultdict(list)
    for r in rows:
        r["when"] = utils.format_slot(r["day"], r["start"])
        by_guest[r["guest"]].append(r)
    repeat = sorted((g for g, rs in by_guest.items() if len(rs) > 1), key=lambda g: (-len(by_guest[g]), g))
    fees = sum(utils.no_show_fee(r["party"]) for r in rows)
    return {"since": since_day.isoformat(), "no_shows": rows, "repeat_guests": repeat, "fees": utils.money(fees)}


def busy_hours(days=28):
    since = date.today() - timedelta(days=days)
    rows = db.rows("SELECT start, party FROM bookings WHERE status = 'booked' AND day >= ?", since.isoformat())
    covers = Counter()
    for r in rows:
        covers[r["start"]] += r["party"]
    return [{"start": s, "covers": c} for s, c in sorted(covers.items())]


def best_tables(days=28):
    since = date.today() - timedelta(days=days)
    rows = db.rows(
        "SELECT table_id, COUNT(*) AS n, SUM(party) AS covers FROM bookings WHERE status = 'booked' AND day >= ? GROUP BY table_id ORDER BY covers DESC, table_id",
        since.isoformat(),
    )
    seats = {t["id"]: t["seats"] for t in db.rows("SELECT id, seats FROM tables")}
    for r in rows:
        r["use"] = round(r["covers"] / max(1, seats.get(r["table_id"], 1) * r["n"]), 2)
    return rows


def deposits_due(day=None):
    d = utils.parse_day(day) if day else date.today()
    rows = db.rows("SELECT id, guest, party FROM bookings WHERE day = ? AND status = 'booked'", d.isoformat())
    due = [{"id": r["id"], "guest": r["guest"], "deposit": utils.deposit_for(r["party"])} for r in rows if utils.deposit_for(r["party"]) > 0]
    return {"day": d.isoformat(), "due": due, "total": utils.money(sum(x["deposit"] for x in due))}


def guest_history(guest):
    rows = db.rows("SELECT day, start, party, status FROM bookings WHERE guest = ? ORDER BY day DESC, start", guest)
    for r in rows:
        r["when"] = utils.format_slot(r["day"], r["start"])
    visits = [r for r in rows if r["status"] == "booked"]
    return {
        "guest": guest,
        "visits": len(visits),
        "no_shows": sum(1 for r in rows if r["status"] == "no-show"),
        "history": rows,
        "regular": len(visits) >= 5,
    }
