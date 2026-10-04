"""Each command's failures as HTTP statuses, and as words for a person.

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
            return 400


def create_booking_message(failure: create_booking.CreateBookingFailure) -> str:
    match failure:
        case create_booking.SlotTaken(reason=reason):
            if reason:
                return (
                    "That table is taken: "
                    + reason
                )
            return "That table is taken"
        case create_booking.PartyTooLarge():
            return "That table is too small for your party"
        case create_booking.TableNotFound():
            return "We could not find that table"
        case create_booking.Cancelled():
            return "Your booking was cancelled"
