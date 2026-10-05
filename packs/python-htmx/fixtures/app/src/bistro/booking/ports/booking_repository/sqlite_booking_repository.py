"""A BookingRepository in SQLite (stdlib sqlite3)."""

import sqlite3
from datetime import date, time

from bistro.booking.models.booking import Booking
from bistro.booking.models.slot import Slot
from bistro.booking.models.table import Table

SCHEMA = """
create table if not exists tables (id text primary key, seats integer not null);
create table if not exists bookings (
    id text primary key, table_id text not null, day text not null, start text not null,
    party integer not null, guest text not null
);
"""


class SqliteBookingRepository:
    def __init__(self, path: str) -> None:
        self._db = sqlite3.connect(path, check_same_thread=False)
        self._db.executescript(SCHEMA)

    def add_tables(self, tables: list[Table]) -> None:
        self._db.executemany("insert or ignore into tables (id, seats) values (?, ?)", [(t.id, t.seats) for t in tables])
        self._db.commit()

    def table(self, table_id: str) -> Table | None:
        row: tuple[str, int] | None = self._db.execute("select id, seats from tables where id = ?", (table_id,)).fetchone()
        return Table(id=row[0], seats=row[1]) if row else None

    def tables(self) -> list[Table]:
        rows: list[tuple[str, int]] = self._db.execute("select id, seats from tables order by id").fetchall()
        return [Table(id=i, seats=s) for i, s in rows]

    def booking_at(self, table_id: str, slot: Slot) -> Booking | None:
        rows = self._select("where table_id = ? and day = ? and start = ?", (table_id, slot.day.isoformat(), slot.start.isoformat()))
        return rows[0] if rows else None

    def bookings_on(self, day: date) -> list[Booking]:
        return self._select("where day = ? order by start, table_id", (day.isoformat(),))

    def save(self, booking: Booking) -> None:
        self._db.execute(
            "insert into bookings (id, table_id, day, start, party, guest) values (?, ?, ?, ?, ?, ?)",
            (booking.id, booking.table_id, booking.slot.day.isoformat(), booking.slot.start.isoformat(), booking.party, booking.guest),
        )
        self._db.commit()

    def _select(self, where: str, args: tuple[str, ...]) -> list[Booking]:
        rows: list[tuple[str, str, str, str, int, str]] = self._db.execute(
            "select id, table_id, day, start, party, guest from bookings " + where, args
        ).fetchall()
        return [
            Booking(id=i, table_id=t, slot=Slot(day=date.fromisoformat(d), start=time.fromisoformat(s)), party=p, guest=g)
            for i, t, d, s, p, g in rows
        ]
