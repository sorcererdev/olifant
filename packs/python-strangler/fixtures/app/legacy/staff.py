"""Staff, shifts and the tip pool. Kept in module globals: the rota was a
spreadsheet first, and nobody moved it into the database.
"""

from datetime import date, datetime, timedelta

from legacy import utils

STAFF = {
    "ana": {"name": "Ana", "role": "host", "rate": 12},
    "ben": {"name": "Ben", "role": "server", "rate": 11},
    "cy": {"name": "Cy", "role": "server", "rate": 11},
    "dee": {"name": "Dee", "role": "chef", "rate": 15},
}
SHIFTS = []  # (staff_id, day, start, end)
TIPS = {}  # day -> amount


def add_shift(staff_id, day, start, end):
    if staff_id not in STAFF:
        return {"error": "no such person"}, 404
    s, e = utils.parse_time(start), utils.parse_time(end)
    if e <= s:
        return {"error": "a shift ends after it starts"}, 400
    d = utils.parse_day(day).isoformat()
    for other in SHIFTS:
        if other[0] == staff_id and other[1] == d and not (e <= utils.parse_time(other[2]) or s >= utils.parse_time(other[3])):
            return {"error": "overlaps another shift"}, 409
    SHIFTS.append((staff_id, d, start, end))
    return {"staff": staff_id, "day": d, "start": start, "end": end}


def on_shift(day=None, at=None):
    d = (utils.parse_day(day) if day else date.today()).isoformat()
    t = utils.parse_time(at) if at else datetime.now().time()
    return sorted(s[0] for s in SHIFTS if s[1] == d and utils.parse_time(s[2]) <= t < utils.parse_time(s[3]))


def hours(staff_id, week_of=None):
    first = utils.parse_day(week_of) if week_of else date.today() - timedelta(days=date.today().weekday())
    days = {(first + timedelta(days=i)).isoformat() for i in range(7)}
    total = 0.0
    for s in SHIFTS:
        if s[0] == staff_id and s[1] in days:
            start = datetime.combine(date.today(), utils.parse_time(s[2]))
            end = datetime.combine(date.today(), utils.parse_time(s[3]))
            total += (end - start).seconds / 3600
    return total


def pay(staff_id, week_of=None):
    if staff_id not in STAFF:
        return {"error": "no such person"}, 404
    h = hours(staff_id, week_of)
    return {"staff": staff_id, "hours": h, "pay": utils.money(h * STAFF[staff_id]["rate"])}


def record_tips(day, amount):
    d = utils.parse_day(day).isoformat()
    TIPS[d] = TIPS.get(d, 0) + amount
    return {"day": d, "tips": TIPS[d]}


def split_tips(day):
    """The pool, split by hours worked that day (the chef takes half a share)."""
    d = utils.parse_day(day).isoformat()
    pool = TIPS.get(d, 0)
    if pool == 0:
        return {}
    weights = {}
    for s in SHIFTS:
        if s[1] != d:
            continue
        start = datetime.combine(date.today(), utils.parse_time(s[2]))
        end = datetime.combine(date.today(), utils.parse_time(s[3]))
        share = (end - start).seconds / 3600
        if STAFF[s[0]]["role"] == "chef":
            share /= 2
        weights[s[0]] = weights.get(s[0], 0) + share
    total = sum(weights.values()) or 1
    return {who: round(pool * w / total, 2) for who, w in sorted(weights.items())}


def rota(day=None):
    d = (utils.parse_day(day) if day else date.today()).isoformat()
    rows = [s for s in SHIFTS if s[1] == d]
    return [{"name": STAFF[s[0]]["name"], "role": STAFF[s[0]]["role"], "from": s[2], "to": s[3]} for s in sorted(rows, key=lambda s: s[2])]
