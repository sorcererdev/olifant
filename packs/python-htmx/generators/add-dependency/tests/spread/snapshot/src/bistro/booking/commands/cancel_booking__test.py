from bistro.booking.commands.cancel_booking import CancelBooking, CancelBookingInput
from bistro.shared.result import Ok
from bistro.booking.ports.booking_repository.in_memory_booking_repository import InMemoryBookingRepository
from bistro.booking.ports.booking_repository.booking_repository import BookingRepository
from bistro.booking.ports.table_repository.in_memory_table_repository import InMemoryTableRepository
from bistro.booking.ports.table_repository.table_repository import TableRepository
from bistro.booking.ports.id_generator.in_memory_id_generator import InMemoryIdGenerator
from bistro.booking.ports.id_generator.id_generator import IdGenerator


def command(
    booking_repository: BookingRepository | None = None,
    table_repository: TableRepository | None = None,
    id_generator: IdGenerator | None = None,
) -> CancelBooking:
    return CancelBooking(
        booking_repository=booking_repository or InMemoryBookingRepository(),
        table_repository=table_repository or InMemoryTableRepository(),
        id_generator=id_generator or InMemoryIdGenerator(),
    )


def test_cancel_booking_succeeds() -> None:
    cancel_booking = command()
    result = cancel_booking.handle(CancelBookingInput(id="1"))
    assert result == Ok(None)
