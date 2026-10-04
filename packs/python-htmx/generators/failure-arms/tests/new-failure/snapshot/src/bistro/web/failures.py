"""Each command's failures as HTTP statuses.

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
        case create_booking.Cancelled():
            return 410
