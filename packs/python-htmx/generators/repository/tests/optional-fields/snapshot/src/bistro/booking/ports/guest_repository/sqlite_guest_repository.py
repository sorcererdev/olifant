"""GuestRepository in SQLite (stdlib sqlite3): one row per guest, a column per field."""

import sqlite3
from typing import Any

from bistro.booking.models.guest import Guest
from datetime import date

SCHEMA = """
create table if not exists "guest" (
    "id" text not null primary key,
    "name" text not null,
    "phone" text,
    "visits" integer not null,
    "regular" integer,
    "last_visit" text
)
"""
COLUMNS = '"id", "name", "phone", "visits", "regular", "last_visit"'
VALUES = ":id, :name, :phone, :visits, :regular, :last_visit"


class SqliteGuestRepository:
    def __init__(self, path: str) -> None:
        self._db = sqlite3.connect(path, check_same_thread=False)
        self._db.row_factory = sqlite3.Row
        self._db.execute(SCHEMA)

    def get(self, id: str) -> Guest | None:
        row = self._db.execute(f'select {COLUMNS} from "guest" where "id" = ?', (id,)).fetchone()
        return _decode(row) if row else None

    def save(self, guest: Guest) -> None:
        self._db.execute(f'insert or replace into "guest" ({COLUMNS}) values ({VALUES})', _encode(guest))
        self._db.commit()

    def all(self) -> list[Guest]:
        rows = self._db.execute(f'select {COLUMNS} from "guest" order by "id"')
        return [_decode(row) for row in rows]


def _encode(guest: Guest) -> dict[str, Any]:
    return {
        "id": guest.id,
        "name": guest.name,
        "phone": guest.phone,
        "visits": guest.visits,
        "regular": guest.regular,
        "last_visit": guest.last_visit.isoformat() if guest.last_visit is not None else None,
    }


def _decode(row: sqlite3.Row) -> Guest:
    return Guest(
        id=row["id"],
        name=row["name"],
        phone=row["phone"],
        visits=row["visits"],
        regular=None if row["regular"] is None else bool(row["regular"]),
        last_visit=None if row["last_visit"] is None else date.fromisoformat(row["last_visit"]),
    )
