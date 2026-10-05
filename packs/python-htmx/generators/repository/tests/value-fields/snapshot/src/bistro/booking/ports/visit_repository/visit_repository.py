"""Where Visits are kept: by id, one each."""

from typing import Protocol

from bistro.booking.models.visit import Visit


class VisitRepository(Protocol):
    def get(self, id: str) -> Visit | None: ...

    def save(self, visit: Visit) -> None:
        """Adds it, or replaces the one with its id."""
        ...

    def all(self) -> list[Visit]:
        """Every one, in id order."""
        ...
