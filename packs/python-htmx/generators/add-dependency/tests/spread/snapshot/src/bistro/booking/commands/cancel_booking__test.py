from bistro.booking.commands.cancel_booking import CancelBooking, CancelBookingInput
from bistro.shared.result import Ok
from bistro.booking.ports.booking_repository.in_memory_booking_repository import InMemoryBookingRepository
from bistro.booking.ports.table_repository.in_memory_table_repository import InMemoryTableRepository
from bistro.booking.ports.id_generator.in_memory_id_generator import InMemoryIdGenerator


def command() -> CancelBooking:
    return CancelBooking(
        booking_repository=InMemoryBookingRepository(),
        table_repository=InMemoryTableRepository(),
        id_generator=InMemoryIdGenerator(),
    )


def test_cancel_booking_succeeds() -> None:
    cancel_booking = command()
    result = cancel_booking.handle(CancelBookingInput(id="1"))
    assert result == Ok(None)
