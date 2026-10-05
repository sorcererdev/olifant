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


@dataclass(frozen=True)
class Name:
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
        table = self.bookings.table(input.table_id)
        if table is None:
            return TableNotFound(f"there is no table {input.table_id}")
        if input.party > table.seats:
            return PartyTooLarge(f"table {table.id} seats {table.seats}, not {input.party}")
        if self.bookings.booking_at(table.id, input.slot) is not None:
            return SlotTaken(f"table {table.id} is booked at {input.slot.start:%H:%M}")
        booking = Booking(id=self.ids.next_id(), table_id=table.id, slot=input.slot, party=input.party, guest=input.guest)
        self.bookings.save(booking)
        return Ok(booking)
