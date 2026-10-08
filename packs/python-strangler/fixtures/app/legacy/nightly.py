"""The nightly job (cron, 03:00): reopen tables, mark no-shows, text reminders.

It calls the views directly, and the views' rules come along: a table closed
for the evening is opened here again.
"""

from datetime import date, datetime, timedelta

from legacy import db, utils, views


def reopen_all_tables():
    reopened = []
    for t in db.rows("SELECT id FROM tables WHERE open = 0"):
        views.open_table(t["id"])
        reopened.append(t["id"])
    return reopened


def mark_yesterdays_no_shows(now=None):
    now = now or datetime.now()
    yesterday = (now.date() - timedelta(days=1)).isoformat()
    marked = []
    for b in db.rows("SELECT id FROM bookings WHERE day = ? AND status = 'booked' AND notes = 'not arrived'", yesterday):
        views.mark_no_show(b["id"])
        marked.append(b["id"])
    return marked


def send_reminders(day=None):
    d = utils.parse_day(day) if day else date.today()
    sent = 0
    for b in db.rows("SELECT * FROM bookings WHERE day = ? AND status = 'booked' AND phone IS NOT NULL", d.isoformat()):
        if utils.notify(b["phone"], f"Reminder: {utils.format_slot(b['day'], b['start'])}, party of {b['party']}"):
            sent += 1
    return sent


def clean_audit(keep_days=90):
    cutoff = (datetime.now() - timedelta(days=keep_days)).isoformat(timespec="seconds")
    c = db.conn()
    deleted = c.execute("DELETE FROM audit WHERE at < ?", (cutoff,)).rowcount
    c.commit()
    return deleted


def run(now=None):
    return {
        "reopened": reopen_all_tables(),
        "no_shows": mark_yesterdays_no_shows(now),
        "reminders": send_reminders(),
        "audit_deleted": clean_audit(),
    }
