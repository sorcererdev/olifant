"""Every BookingRepository adapter keeps the same contract."""

from collections.abc import Iterator
from datetime import date, time
from pathlib import Path

import pytest

from bistro.adapters.in_memory_booking_repository import InMemoryBookingRepository
from bistro.adapters.sqlite_booking_repository import SqliteBookingRepository
from bistro.booking.models import Booking, Slot, Table
from bistro.booking.ports import BookingRepository

TABLES = [Table(id="t1", seats=4), Table(id="t2", seats=2)]
SEVEN = Slot(day=date(2026, 10, 9), start=time(19, 0))


@pytest.fixture(params=["in_memory", "sqlite"])
def repository(request: pytest.FixtureRequest, tmp_path: Path) -> Iterator[BookingRepository]:
    if request.param == "in_memory":
        yield InMemoryBookingRepository(tables=TABLES)
    else:
        repo = SqliteBookingRepository(str(tmp_path / "bistro.sqlite3"))
        repo.add_tables(TABLES)
        yield repo


def test_finds_a_table(repository: BookingRepository) -> None:
    assert repository.table("t2") == Table(id="t2", seats=2)
    assert repository.table("t9") is None


def test_saves_and_finds_a_booking(repository: BookingRepository) -> None:
    booking = Booking(id="b1", table_id="t1", slot=SEVEN, party=3, guest="Ada")
    repository.save(booking)
    assert repository.booking_at("t1", SEVEN) == booking
    assert repository.booking_at("t2", SEVEN) is None
    assert repository.bookings_on(SEVEN.day) == [booking]
