"""Book a table for a party in a slot."""

from dataclasses import dataclass

from bistro.booking.models.booking import Booking
from bistro.booking.models.slot import Slot
from bistro.booking.ports.booking_repository.booking_repository import BookingRepository
from bistro.booking.ports.id_generator.id_generator import IdGenerator
from bistro.shared.result import Ok


@dataclass(frozen=True)
class CreateBookingInput:
    table_id: str
    slot: Slot
    party: int
    guest: str


@dataclass(frozen=True)
class SlotTaken:
    reason: str


@dataclass(frozen=True)
class PartyTooLarge:
    reason: str


@dataclass(frozen=True)
class TableNotFound:
    reason: str


type CreateBookingFailure = (
    SlotTaken
    | PartyTooLarge
    | TableNotFound
)

type CreateBookingOutput = Ok[Booking] | CreateBookingFailure


class CreateBooking:
    def __init__(self, bookings: BookingRepository, ids: IdGenerator) -> None:
        self.bookings = bookings
        self.ids = ids

    def handle(self, input: CreateBookingInput) -> CreateBookingOutput:
        raise NotImplementedError("CreateBooking.handle: write its first case in create_booking__test.py, then this")
