"""Everything that didn't have a home. Imported by every module in legacy/.

Formatting, validation, pricing, slots, notifications and a cache, in one
place. Some of it is used; some of it was used once.
"""

import re
from datetime import date, datetime, time, timedelta

from legacy import db

CURRENT_RESTAURANT = "bistro"
_TABLE_CACHE = {}
_SETTINGS = {
    "slot_minutes": 30,
    "max_party": 8,
    "deposit_per_head": 10,
    "deposit_from_party": 6,
    "no_show_fee": 25,
    "sms_enabled": False,
}
SENT_MESSAGES = []

PHONE = re.compile(r"^\+?[0-9 ()-]{7,20}$")
NAME = re.compile(r"^[A-Za-z][A-Za-z '.-]{0,59}$")


# ── settings ────────────────────────────────────────────────────────────


def setting(name, default=None):
    return _SETTINGS.get(name, default)


def set_setting(name, value):
    _SETTINGS[name] = value


def use_restaurant(restaurant_id):
    global CURRENT_RESTAURANT
    CURRENT_RESTAURANT = restaurant_id
    _TABLE_CACHE.clear()


# ── time and slots ──────────────────────────────────────────────────────


def parse_time(text):
    if isinstance(text, time):
        return text
    hours, minutes = text.strip().split(":")
    return time(int(hours), int(minutes))


def parse_day(text):
    if text in (None, "", "today"):
        return date.today()
    if text == "tomorrow":
        return date.today() + timedelta(days=1)
    return date.fromisoformat(text)


def format_slot(day, start):
    """'Fri 9 Oct, 19:30' — used by the booking views and the reports."""
    d = parse_day(day) if isinstance(day, str) else day
    t = parse_time(start)
    return f"{d.strftime('%a')} {d.day} {d.strftime('%b')}, {t.strftime('%H:%M')}"


def opening_hours(restaurant_id=None):
    r = db.row("SELECT opens, closes FROM restaurants WHERE id = ?", restaurant_id or CURRENT_RESTAURANT)
    if r is None:
        return time(17, 0), time(23, 0)
    return parse_time(r["opens"]), parse_time(r["closes"])


def slots_for(day=None):
    opens, closes = opening_hours()
    step = timedelta(minutes=setting("slot_minutes", 30))
    current = datetime.combine(parse_day(day) if isinstance(day, str) else (day or date.today()), opens)
    end = datetime.combine(current.date(), closes) - timedelta(hours=1)
    out = []
    while current <= end:
        out.append(current.time().strftime("%H:%M"))
        current += step
    return out


def within_hours(start):
    opens, closes = opening_hours()
    t = parse_time(start)
    return opens <= t <= (datetime.combine(date.today(), closes) - timedelta(hours=1)).time()


# ── validation (returns an error string, or None) ───────────────────────


def check_guest(name):
    if not name or not NAME.match(name.strip()):
        return "guest name looks wrong"
    return None


def check_phone(phone):
    if phone and not PHONE.match(phone):
        return "phone number looks wrong"
    return None


def check_party(party):
    try:
        n = int(party)
    except (TypeError, ValueError):
        return "party must be a number"
    if n < 1:
        return "party must be at least 1"
    if n > setting("max_party", 8):
        return "party too large for online booking"
    return None


def check_booking_form(form):
    errors = []
    for e in (check_guest(form.get("guest")), check_phone(form.get("phone")), check_party(form.get("party"))):
        if e:
            errors.append(e)
    if not form.get("start") or not within_hours(form["start"]):
        errors.append("outside opening hours")
    return errors


# ── tables (cached, because the dashboard asked a lot) ──────────────────


def table(table_id):
    if table_id in _TABLE_CACHE:
        return _TABLE_CACHE[table_id]
    t = db.row("SELECT * FROM tables WHERE id = ?", table_id)
    if t is not None:
        _TABLE_CACHE[table_id] = t
    return t


def forget_table(table_id):
    _TABLE_CACHE.pop(table_id, None)


def open_tables(min_seats=1):
    return db.rows(
        "SELECT * FROM tables WHERE restaurant_id = ? AND open = 1 AND seats >= ? ORDER BY seats, id",
        CURRENT_RESTAURANT,
        min_seats,
    )


def free_tables(day, start, party):
    taken = {r["table_id"] for r in db.rows("SELECT table_id FROM bookings WHERE day = ? AND start = ? AND status = 'booked'", str(day), start)}
    return [t for t in open_tables(int(party)) if t["id"] not in taken]


# ── money ───────────────────────────────────────────────────────────────


def deposit_for(party):
    party = int(party)
    if party < setting("deposit_from_party", 6):
        return 0
    return party * setting("deposit_per_head", 10)


def no_show_fee(party):
    return setting("no_show_fee", 25) if int(party) >= 4 else 0


def money(amount):
    return f"£{amount:,.2f}"


# ── notifications ───────────────────────────────────────────────────────


def notify(phone, text):
    """Pretend to send an SMS (the real gateway was switched off in 2019)."""
    if not phone:
        return False
    SENT_MESSAGES.append((phone, text))
    if setting("sms_enabled"):
        print(f"SMS to {phone}: {text}")
    return True


def confirm_booking(booking):
    return notify(booking.get("phone"), f"Booked: {booking['guest']}, {format_slot(booking['day'], booking['start'])}, party of {booking['party']}")


def cancel_notice(booking):
    return notify(booking.get("phone"), f"Cancelled: {format_slot(booking['day'], booking['start'])}")


# ── ids ─────────────────────────────────────────────────────────────────


def next_booking_id():
    r = db.row("SELECT id FROM bookings WHERE id LIKE 'b%' ORDER BY CAST(SUBSTR(id, 2) AS INTEGER) DESC LIMIT 1")
    if r is None:
        return "b1"
    return "b" + str(int(r["id"][1:]) + 1)


def slugify(text):
    return re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")
