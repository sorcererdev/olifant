"""Where bookings and tables are kept."""

from datetime import date
from typing import Protocol

from bistro.booking.models.booking import Booking
from bistro.booking.models.slot import Slot
from bistro.booking.models.table import Table


class BookingRepository(Protocol):
    def table(self, table_id: str) -> Table | None: ...

    def tables(self) -> list[Table]: ...

    def booking_at(self, table_id: str, slot: Slot) -> Booking | None: ...

    def bookings_on(self, day: date) -> list[Booking]: ...

    def save(self, booking: Booking) -> None: ...
