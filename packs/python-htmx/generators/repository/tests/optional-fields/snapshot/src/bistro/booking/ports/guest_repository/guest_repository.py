"""Where Guests are kept: by id, one each."""

from typing import Protocol

from bistro.booking.models.guest import Guest


class GuestRepository(Protocol):
    def get(self, id: str) -> Guest | None: ...

    def save(self, guest: Guest) -> None:
        """Adds it, or replaces the one with its id."""
        ...

    def all(self) -> list[Guest]:
        """Every one, in id order."""
        ...
