"""VisitRepository in memory: for tests, and until a real adapter exists."""

from bistro.booking.models.visit import Visit


class InMemoryVisitRepository:
    def __init__(self) -> None:
        self._rows: dict[str, Visit] = {}

    def get(self, id: str) -> Visit | None:
        return self._rows.get(id)

    def save(self, visit: Visit) -> None:
        self._rows[visit.id] = visit

    def all(self) -> list[Visit]:
        return [self._rows[k] for k in sorted(self._rows)]
