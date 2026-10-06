"""TableRepository in SQLite (stdlib sqlite3): one row per table, a column per field."""

import sqlite3
from typing import Any

from bistro.booking.models.table import Table

SCHEMA = """
create table if not exists "table" (
    "id" text not null primary key,
    "seats" integer not null
)
"""
COLUMNS = '"id", "seats"'
VALUES = ":id, :seats"


class SqliteTableRepository:
    def __init__(self, path: str) -> None:
        self._db = sqlite3.connect(path, check_same_thread=False)
        self._db.row_factory = sqlite3.Row
        self._db.execute(SCHEMA)

    def get(self, id: str) -> Table | None:
        row = self._db.execute(f'select {COLUMNS} from "table" where "id" = ?', (id,)).fetchone()
        return _decode(row) if row else None

    def save(self, table: Table) -> None:
        self._db.execute(f'insert or replace into "table" ({COLUMNS}) values ({VALUES})', _encode(table))
        self._db.commit()

    def all(self) -> list[Table]:
        rows = self._db.execute(f'select {COLUMNS} from "table" order by "id"')
        return [_decode(row) for row in rows]


def _encode(table: Table) -> dict[str, Any]:
    return {
        "id": table.id,
        "seats": table.seats,
    }


def _decode(row: sqlite3.Row) -> Table:
    return Table(
        id=row["id"],
        seats=row["seats"],
    )
