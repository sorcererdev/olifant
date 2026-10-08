"""The database. One connection for the whole process, opened on first use.

Everything in legacy/ talks to it directly: the views build SQL by hand,
utils caches rows in module globals, and the tests swap DB_PATH before the
first query. It works; nobody wants to touch it.
"""

import os
import sqlite3
from datetime import date, datetime

DB_PATH = os.environ.get("BISTRO_DB", ":memory:")
_CONN = None

SCHEMA = """
CREATE TABLE IF NOT EXISTS restaurants (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    opens TEXT NOT NULL,
    closes TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS tables (
    id TEXT PRIMARY KEY,
    restaurant_id TEXT NOT NULL,
    seats INTEGER NOT NULL,
    open INTEGER NOT NULL DEFAULT 1,
    area TEXT NOT NULL DEFAULT 'main'
);
CREATE TABLE IF NOT EXISTS bookings (
    id TEXT PRIMARY KEY,
    table_id TEXT NOT NULL,
    day TEXT NOT NULL,
    start TEXT NOT NULL,
    party INTEGER NOT NULL,
    guest TEXT NOT NULL,
    phone TEXT,
    status TEXT NOT NULL DEFAULT 'booked',
    notes TEXT
);
CREATE TABLE IF NOT EXISTS waitlist (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    restaurant_id TEXT NOT NULL,
    guest TEXT NOT NULL,
    party INTEGER NOT NULL,
    added TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS audit (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    at TEXT NOT NULL,
    what TEXT NOT NULL
);
"""


def conn():
    """The connection (opened, and the schema made, on first use)."""
    global _CONN
    if _CONN is None:
        _CONN = sqlite3.connect(DB_PATH, check_same_thread=False)
        _CONN.row_factory = sqlite3.Row
        _CONN.executescript(SCHEMA)
    return _CONN


def reset(path=":memory:"):
    """Tests: a fresh database."""
    global _CONN, DB_PATH
    if _CONN is not None:
        _CONN.close()
    _CONN = None
    DB_PATH = path
    return conn()


def seed():
    """A restaurant with five tables and a few bookings (the demo's data)."""
    c = conn()
    c.execute("INSERT OR REPLACE INTO restaurants VALUES ('bistro', 'Bistro', '17:00', '23:00')")
    for table_id, seats, area in [("t1", 2, "window"), ("t2", 2, "main"), ("t3", 4, "main"), ("t4", 4, "terrace"), ("t5", 8, "private")]:
        c.execute("INSERT OR REPLACE INTO tables (id, restaurant_id, seats, open, area) VALUES (?, 'bistro', ?, 1, ?)", (table_id, seats, area))
    today = date.today().isoformat()
    for booking_id, table_id, start, party, guest in [
        ("b1", "t1", "19:00", 2, "Ada"),
        ("b2", "t3", "19:30", 4, "Grace"),
        ("b3", "t5", "20:00", 6, "Linus"),
    ]:
        c.execute(
            "INSERT OR REPLACE INTO bookings (id, table_id, day, start, party, guest, status) VALUES (?, ?, ?, ?, ?, ?, 'booked')",
            (booking_id, table_id, today, start, party, guest),
        )
    c.commit()


def audit(what):
    """Everything that changes something says so here (sometimes)."""
    c = conn()
    c.execute("INSERT INTO audit (at, what) VALUES (?, ?)", (datetime.now().isoformat(timespec="seconds"), what))
    c.commit()


def rows(sql, *args):
    return [dict(r) for r in conn().execute(sql, args).fetchall()]


def row(sql, *args):
    r = conn().execute(sql, args).fetchone()
    return dict(r) if r else None
