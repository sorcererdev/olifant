"""A BookingRepository in memory: for tests and the first run."""

from datetime import date

from bistro.booking.models import Booking, Slot, Table


class InMemoryBookingRepository:
    def __init__(self, tables: list[Table]) -> None:
        self._tables = {t.id: t for t in tables}
        self._bookings: list[Booking] = []

    def table(self, table_id: str) -> Table | None:
        return self._tables.get(table_id)

    def tables(self) -> list[Table]:
        return list(self._tables.values())

    def booking_at(self, table_id: str, slot: Slot) -> Booking | None:
        return next((b for b in self._bookings if b.table_id == table_id and b.slot == slot), None)

    def bookings_on(self, day: date) -> list[Booking]:
        return [b for b in self._bookings if b.slot.day == day]

    def save(self, booking: Booking) -> None:
        self._bookings.append(booking)
