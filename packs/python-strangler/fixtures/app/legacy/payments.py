"""Deposits, refunds and the till: money, with the SQL right where it's used.

The payments table is made here on first use (it came later than the rest).
"""

from datetime import date, datetime

from legacy import db, utils

PROVIDER = "fakepay"
_FAILED_CARDS = {"4000000000000002"}

PAYMENTS = """
CREATE TABLE IF NOT EXISTS payments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    booking_id TEXT NOT NULL,
    kind TEXT NOT NULL,
    amount INTEGER NOT NULL,
    card_last4 TEXT,
    at TEXT NOT NULL,
    status TEXT NOT NULL
);
"""


def _ensure():
    db.conn().executescript(PAYMENTS)


def charge(card, amount):
    """The provider (a stand-in): a card that always fails, else ok."""
    if card in _FAILED_CARDS:
        return False, "card declined"
    if amount <= 0:
        return False, "nothing to charge"
    return True, f"{PROVIDER}-{abs(hash((card, amount, datetime.now().isoformat()))) % 10**8}"


def take_deposit(booking_id, card):
    """Charge a booking's deposit, record it, and mark the booking."""
    _ensure()
    b = db.row("SELECT * FROM bookings WHERE id = ?", booking_id)
    if b is None:
        return {"error": "no such booking"}, 404
    amount = utils.deposit_for(b["party"])
    if amount == 0:
        return {"error": "no deposit for this party"}, 400
    paid = db.row("SELECT id FROM payments WHERE booking_id = ? AND kind = 'deposit' AND status = 'paid'", booking_id)
    if paid:
        return {"error": "deposit already paid"}, 409
    ok, ref = charge(card, amount)
    c = db.conn()
    c.execute(
        "INSERT INTO payments (booking_id, kind, amount, card_last4, at, status) VALUES (?, 'deposit', ?, ?, ?, ?)",
        (booking_id, amount, card[-4:], datetime.now().isoformat(timespec="seconds"), "paid" if ok else "failed"),
    )
    if ok:
        c.execute("UPDATE bookings SET notes = COALESCE(notes, '') || ' deposit paid' WHERE id = ?", (booking_id,))
    c.commit()
    db.audit(f"deposit {booking_id} {'paid' if ok else 'failed'}")
    if not ok:
        return {"error": ref}, 402
    return {"booking_id": booking_id, "amount": amount, "ref": ref}


def refund_deposit(booking_id, reason="cancelled"):
    _ensure()
    paid = db.row("SELECT * FROM payments WHERE booking_id = ? AND kind = 'deposit' AND status = 'paid'", booking_id)
    if paid is None:
        return {"error": "no deposit to refund"}, 404
    b = db.row("SELECT day FROM bookings WHERE id = ?", booking_id)
    days_before = (date.fromisoformat(b["day"]) - date.today()).days if b else 0
    if days_before < 1 and reason == "cancelled":
        return {"error": "too late to refund"}, 409
    c = db.conn()
    c.execute("UPDATE payments SET status = 'refunded' WHERE id = ?", (paid["id"],))
    c.execute(
        "INSERT INTO payments (booking_id, kind, amount, card_last4, at, status) VALUES (?, 'refund', ?, ?, ?, 'paid')",
        (booking_id, -paid["amount"], paid["card_last4"], datetime.now().isoformat(timespec="seconds")),
    )
    c.commit()
    db.audit(f"refund {booking_id}")
    return {"booking_id": booking_id, "refunded": paid["amount"]}


def till_total(day=None):
    """What the till took on a day: deposits less refunds."""
    _ensure()
    d = (utils.parse_day(day) if day else date.today()).isoformat()
    rows = db.rows("SELECT kind, amount, status FROM payments WHERE substr(at, 1, 10) = ?", d)
    taken = sum(r["amount"] for r in rows if r["status"] in ("paid", "refunded") and r["kind"] == "deposit")
    refunded = -sum(r["amount"] for r in rows if r["kind"] == "refund")
    return {"day": d, "taken": taken, "refunded": refunded, "net": taken - refunded, "shown": utils.money(taken - refunded)}


def payments_for(booking_id):
    _ensure()
    return db.rows("SELECT kind, amount, card_last4, status FROM payments WHERE booking_id = ? ORDER BY id", booking_id)
