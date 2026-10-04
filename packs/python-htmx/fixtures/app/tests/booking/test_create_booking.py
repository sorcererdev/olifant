from datetime import date, time

from bistro.adapters.in_memory_booking_repository import InMemoryBookingRepository
from bistro.adapters.sequential_ids import SequentialIds
from bistro.booking.create_booking import (
    CreateBooking,
    CreateBookingInput,
    PartyTooLarge,
    SlotTaken,
    TableNotFound,
)
from bistro.booking.models import Booking, Slot, Table
from bistro.shared.result import Ok

SEVEN = Slot(day=date(2026, 10, 9), start=time(19, 0))


def command() -> tuple[CreateBooking, InMemoryBookingRepository]:
    bookings = InMemoryBookingRepository(tables=[Table(id="t1", seats=4)])
    return CreateBooking(bookings=bookings, ids=SequentialIds()), bookings


def test_books_a_free_slot() -> None:
    create, bookings = command()
    result = create.handle(CreateBookingInput(table_id="t1", slot=SEVEN, party=2, guest="Ada"))
    assert result == Ok(Booking(id="b1", table_id="t1", slot=SEVEN, party=2, guest="Ada"))
    assert isinstance(result, Ok)
    assert bookings.booking_at("t1", SEVEN) == result.value


def test_a_booked_slot_is_taken() -> None:
    create, _ = command()
    create.handle(CreateBookingInput(table_id="t1", slot=SEVEN, party=2, guest="Ada"))
    result = create.handle(CreateBookingInput(table_id="t1", slot=SEVEN, party=2, guest="Grace"))
    assert isinstance(result, SlotTaken)


def test_a_party_larger_than_the_table_is_refused() -> None:
    create, _ = command()
    result = create.handle(CreateBookingInput(table_id="t1", slot=SEVEN, party=6, guest="Ada"))
    assert isinstance(result, PartyTooLarge)


def test_an_unknown_table_is_not_found() -> None:
    create, _ = command()
    result = create.handle(CreateBookingInput(table_id="t9", slot=SEVEN, party=2, guest="Ada"))
    assert isinstance(result, TableNotFound)
