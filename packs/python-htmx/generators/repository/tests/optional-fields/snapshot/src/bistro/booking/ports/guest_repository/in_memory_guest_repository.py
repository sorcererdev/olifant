"""GuestRepository in memory: for tests, and until a real adapter exists."""

from bistro.booking.models.guest import Guest


class InMemoryGuestRepository:
    def __init__(self) -> None:
        self._rows: dict[str, Guest] = {}

    def get(self, id: str) -> Guest | None:
        return self._rows.get(id)

    def save(self, guest: Guest) -> None:
        self._rows[guest.id] = guest

    def all(self) -> list[Guest]:
        return [self._rows[k] for k in sorted(self._rows)]
