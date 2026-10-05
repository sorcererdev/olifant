"""Each command's failures as HTTP statuses (the module under an alias)."""

import bistro.booking.commands.create_booking as cb


def create_booking_status(failure: cb.CreateBookingFailure) -> int:
    match failure:
        case cb.SlotTaken():
            return 409
        case cb.PartyTooLarge():
            return 422
        case cb.TableNotFound():
            return 404
        case cb.Cancelled():
            return 400
