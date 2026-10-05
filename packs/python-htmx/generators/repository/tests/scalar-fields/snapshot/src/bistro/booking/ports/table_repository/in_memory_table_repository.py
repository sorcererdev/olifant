"""TableRepository in memory: for tests, and until a real adapter exists."""

from bistro.booking.models.table import Table


class InMemoryTableRepository:
    def __init__(self) -> None:
        self._rows: dict[str, Table] = {}

    def get(self, id: str) -> Table | None:
        return self._rows.get(id)

    def save(self, table: Table) -> None:
        self._rows[table.id] = table

    def all(self) -> list[Table]:
        return [self._rows[k] for k in sorted(self._rows)]
