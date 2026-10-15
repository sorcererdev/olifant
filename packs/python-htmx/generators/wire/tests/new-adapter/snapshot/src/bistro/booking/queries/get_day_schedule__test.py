from datetime import date, time

from bistro.booking.models.booking import Booking
from bistro.booking.models.slot import Slot
from bistro.booking.models.table import Table
from bistro.booking.ports.booking_repository.in_memory_booking_repository import InMemoryBookingRepository
from bistro.booking.queries.get_day_schedule import DaySchedule, GetDaySchedule
from bistro.booking.ports.table_repository.in_memory_table_repository import InMemoryTableRepository
from bistro.booking.ports.table_repository.table_repository import TableRepository

DAY = date(2026, 10, 9)


def query(
    bookings: InMemoryBookingRepository,
    tables: TableRepository | None = None,
) -> GetDaySchedule:
    return GetDaySchedule(
        bookings=bookings,
        tables=tables or InMemoryTableRepository(),
    )


def test_lists_the_tables_and_the_days_bookings() -> None:
    bookings = InMemoryBookingRepository(tables=[Table(id="t1", seats=4), Table(id="t2", seats=2)])
    ada = Booking(id="b1", table_id="t1", slot=Slot(day=DAY, start=time(19, 0)), party=2, guest="Ada")
    other_day = Booking(id="b2", table_id="t1", slot=Slot(day=date(2026, 10, 10), start=time(19, 0)), party=2, guest="Grace")
    bookings.save(ada)
    bookings.save(other_day)
    schedule = query(bookings).handle(DAY)
    assert schedule == DaySchedule(day=DAY, tables=[Table(id="t1", seats=4), Table(id="t2", seats=2)], bookings=[ada])
