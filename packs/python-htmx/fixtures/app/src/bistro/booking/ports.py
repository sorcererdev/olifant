"""What the booking service needs from the outside world."""

from datetime import date
from typing import Protocol

from bistro.booking.models import Booking, Slot, Table


class BookingRepository(Protocol):
    def table(self, table_id: str) -> Table | None: ...

    def tables(self) -> list[Table]: ...

    def booking_at(self, table_id: str, slot: Slot) -> Booking | None: ...

    def bookings_on(self, day: date) -> list[Booking]: ...

    def save(self, booking: Booking) -> None: ...


class IdGenerator(Protocol):
    def next_id(self) -> str: ...
