from bistro.booking.commands.cancel_booking import CancelBooking, CancelBookingInput
from bistro.shared.result import Ok
from bistro.booking.ports.booking_repository.in_memory_booking_repository import InMemoryBookingRepository
from bistro.booking.ports.booking_repository.booking_repository import BookingRepository


def command(
    bookings: BookingRepository | None = None,
) -> CancelBooking:
    return CancelBooking(
        bookings=bookings or InMemoryBookingRepository(),
    )


def test_cancel_booking_succeeds() -> None:
    cancel_booking = command()
    result = cancel_booking.handle(CancelBookingInput(id="1"))
    assert result == Ok(None)
