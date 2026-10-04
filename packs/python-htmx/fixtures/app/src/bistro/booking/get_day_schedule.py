"""What a day looks like: the tables and that day's bookings (a query)."""

from dataclasses import dataclass
from datetime import date

from bistro.booking.models import Booking, Table
from bistro.booking.ports import BookingRepository


@dataclass(frozen=True)
class DaySchedule:
    day: date
    tables: list[Table]
    bookings: list[Booking]


class GetDaySchedule:
    def __init__(self, bookings: BookingRepository) -> None:
        self.bookings = bookings

    def handle(self, day: date) -> DaySchedule:
        return DaySchedule(day=day, tables=self.bookings.tables(), bookings=self.bookings.bookings_on(day))
