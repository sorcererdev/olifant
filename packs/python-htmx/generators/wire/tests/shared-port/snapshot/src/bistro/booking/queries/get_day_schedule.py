"""What a day looks like: the tables and that day's bookings (a query)."""

from dataclasses import dataclass
from datetime import date

from bistro.booking.models.booking import Booking
from bistro.booking.models.table import Table
from bistro.booking.ports.booking_repository.booking_repository import BookingRepository
from bistro.booking.ports.id_generator.id_generator import IdGenerator


@dataclass(frozen=True)
class DaySchedule:
    day: date
    tables: list[Table]
    bookings: list[Booking]


class GetDaySchedule:
    def __init__(self, bookings: BookingRepository, ids: IdGenerator) -> None:
        self.bookings = bookings
        self.ids = ids

    def handle(self, day: date) -> DaySchedule:
        return DaySchedule(day=day, tables=self.bookings.tables(), bookings=self.bookings.bookings_on(day))
