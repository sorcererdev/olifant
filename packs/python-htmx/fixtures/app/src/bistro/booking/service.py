"""The booking service: its commands and queries, wired to its ports.

The ports stay inside: callers (the web layer) use the commands and queries.
"""

from bistro.booking.create_booking import CreateBooking
from bistro.booking.get_day_schedule import GetDaySchedule
from bistro.booking.ports import BookingRepository, IdGenerator


class BookingService:
    def __init__(self, bookings: BookingRepository, ids: IdGenerator) -> None:
        self.create_booking = CreateBooking(bookings=bookings, ids=ids)
        self.get_day_schedule = GetDaySchedule(bookings=bookings)
