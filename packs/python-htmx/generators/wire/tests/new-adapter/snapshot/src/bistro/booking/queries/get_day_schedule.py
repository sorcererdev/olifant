"""What a day looks like: the tables and that day's bookings (a query)."""

from dataclasses import dataclass
from datetime import date

from bistro.booking.models.booking import Booking
from bistro.booking.models.table import Table
from bistro.booking.ports.booking_repository.booking_repository import BookingRepository
from bistro.booking.ports.table_repository.table_repository import TableRepository


@dataclass(frozen=True)
class DaySchedule:
    day: date
    tables: list[Table]
    bookings: list[Booking]


class GetDaySchedule:
    def __init__(self, bookings: BookingRepository, tables: TableRepository) -> None:
        self.bookings = bookings
        self.tables = tables

    def handle(self, day: date) -> DaySchedule:
        return DaySchedule(day=day, tables=self.bookings.tables(), bookings=self.bookings.bookings_on(day))
