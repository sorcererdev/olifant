"""Where Tables are kept: by id, one each."""

from typing import Protocol

from bistro.booking.models.table import Table


class TableRepository(Protocol):
    def get(self, id: str) -> Table | None: ...

    def save(self, table: Table) -> None:
        """Adds it, or replaces the one with its id."""
        ...

    def all(self) -> list[Table]:
        """Every one, in id order."""
        ...
