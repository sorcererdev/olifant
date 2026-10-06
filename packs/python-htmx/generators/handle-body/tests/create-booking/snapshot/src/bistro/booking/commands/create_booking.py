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
        table = self.bookings.table(input.table_id)
        if table is None:
            return TableNotFound(reason=f"Table {input.table_id} not found.")
        
        if input.party > table.seats:
            return PartyTooLarge(reason=f"Party of {input.party} is larger than table {input.table_id} with {table.seats} seats.")
        
        if self.bookings.booking_at(input.table_id, input.slot) is not None:
            return SlotTaken(reason=f"Slot {input.slot} is already taken for table {input.table_id}.")
        
        booking = Booking(
            id=self.ids.next_id(),
            table_id=input.table_id,
            slot=input.slot,
            party=input.party,
            guest=input.guest,
        )
        self.bookings.save(booking)
        return Ok(booking)
