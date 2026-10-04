"""The booking service: its commands, wired to its ports."""

from bistro.booking.create_booking import CreateBooking
from bistro.booking.ports import BookingRepository, IdGenerator


class BookingService:
    def __init__(self, bookings: BookingRepository, ids: IdGenerator) -> None:
        self.bookings = bookings
        self.create_booking = CreateBooking(bookings=bookings, ids=ids)
