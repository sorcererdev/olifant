"""Cancel booking."""

from dataclasses import dataclass

from bistro.shared.result import Ok


@dataclass(frozen=True)
class CancelBookingInput:
    id: str


@dataclass(frozen=True)
class NotFound:
    reason: str


type CancelBookingFailure = (
    NotFound
)

type CancelBookingOutput = Ok[None] | CancelBookingFailure


class CancelBooking:
    def __init__(self, bookings: BookingRepository) -> None:
        """Takes the ports it uses; main.py passes their adapters."""

    def handle(self, input: CancelBookingInput) -> CancelBookingOutput:
        raise NotImplementedError("CancelBooking.handle: write its first case in cancel_booking__test.py, then this")
