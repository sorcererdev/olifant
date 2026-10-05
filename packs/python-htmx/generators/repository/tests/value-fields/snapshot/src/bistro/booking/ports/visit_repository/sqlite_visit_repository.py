"""VisitRepository in SQLite (stdlib sqlite3): one row per visit, a column per field."""

import sqlite3
from typing import Any

from bistro.booking.models.visit import Visit
from datetime import date, time
from bistro.booking.models.slot import Slot

SCHEMA = """
create table if not exists "visit" (
    "id" text not null primary key,
    "guest" text not null,
    "day" text not null,
    "slot" text not null,
    "party" integer not null,
    "spend" real not null,
    "regular" integer not null
)
"""
COLUMNS = '"id", "guest", "day", "slot", "party", "spend", "regular"'
VALUES = ":id, :guest, :day, :slot, :party, :spend, :regular"


class SqliteVisitRepository:
    def __init__(self, path: str) -> None:
        self._db = sqlite3.connect(path, check_same_thread=False)
        self._db.row_factory = sqlite3.Row
        self._db.execute(SCHEMA)

    def get(self, id: str) -> Visit | None:
        row = self._db.execute(f'select {COLUMNS} from "visit" where "id" = ?', (id,)).fetchone()
        return _decode(row) if row else None

    def save(self, visit: Visit) -> None:
        self._db.execute(f'insert or replace into "visit" ({COLUMNS}) values ({VALUES})', _encode(visit))
        self._db.commit()

    def all(self) -> list[Visit]:
        rows = self._db.execute(f'select {COLUMNS} from "visit" order by "id"')
        return [_decode(row) for row in rows]


def _encode(visit: Visit) -> dict[str, Any]:
    return {
        "id": visit.id,
        "guest": visit.guest,
        "day": visit.day.isoformat(),
        "slot": f"{visit.slot.day.isoformat()} {visit.slot.start.strftime('%H:%M')}",
        "party": visit.party,
        "spend": visit.spend,
        "regular": visit.regular,
    }


def _decode(row: sqlite3.Row) -> Visit:
    return Visit(
        id=row["id"],
        guest=row["guest"],
        day=date.fromisoformat(row["day"]),
        slot=Slot(
            day=date.fromisoformat(row["slot"].split()[0]),
            start=time.fromisoformat(row["slot"].split()[1]),
        ),
        party=row["party"],
        spend=row["spend"],
        regular=bool(row["regular"]),
    )
