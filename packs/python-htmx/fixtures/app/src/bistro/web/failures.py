"""Each command's failures as HTTP statuses."""

from bistro.booking.create_booking import (
    CreateBookingFailure,
    PartyTooLarge,
    SlotTaken,
    TableNotFound,
)


def create_booking_status(failure: CreateBookingFailure) -> int:
    match failure:
        case SlotTaken():
            return 409
        case PartyTooLarge():
            return 422
        case TableNotFound():
            return 404
