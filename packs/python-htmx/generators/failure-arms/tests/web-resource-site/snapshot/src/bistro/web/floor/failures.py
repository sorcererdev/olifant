"""Each command's failures as HTTP statuses: one exhaustive match per command (failure-arms adds a new failure's arm)."""

from bistro.booking.commands import cancel_booking


def cancel_booking_status(failure: cancel_booking.CancelBookingFailure) -> int:
    match failure:
        case cancel_booking.NotFound():
            return 404
        case cancel_booking.AlreadyCancelled():
            return 409
