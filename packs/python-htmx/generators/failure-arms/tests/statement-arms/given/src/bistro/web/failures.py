"""Each command's failures as HTTP statuses, and logged.

Failures are named through their command's module (`create_booking.SlotTaken`):
a new failure needs no new import here.
"""

from bistro.booking import create_booking


def create_booking_status(failure: create_booking.CreateBookingFailure) -> int:
    match failure:
        case create_booking.SlotTaken():
            return 409
        case create_booking.PartyTooLarge():
            return 422
        case create_booking.TableNotFound():
            return 404


def log_failure(failure: create_booking.CreateBookingFailure) -> None:
    match failure:
        case create_booking.SlotTaken():
            level = "info"
        case create_booking.PartyTooLarge():
            level = "info"
        case create_booking.TableNotFound():
            level = "warn"
    print(level, failure.reason)
