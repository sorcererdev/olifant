"""Every GuestRepository adapter keeps the same contract."""

from pathlib import Path

import pytest

from bistro.booking.models.guest import Guest
from bistro.booking.ports.guest_repository.guest_repository import GuestRepository
from bistro.booking.ports.guest_repository.in_memory_guest_repository import InMemoryGuestRepository
from bistro.booking.ports.guest_repository.sqlite_guest_repository import SqliteGuestRepository
from datetime import date


@pytest.fixture(params=["in_memory", "sqlite"])
def repository(request: pytest.FixtureRequest, tmp_path: Path) -> GuestRepository:
    if request.param == "sqlite":
        return SqliteGuestRepository(str(tmp_path / "guest.sqlite3"))
    return InMemoryGuestRepository()


def sample(id: str) -> Guest:
    return Guest(
        id=id,
        name="name",
        phone="phone",
        visits=2,
        regular=True,
        last_visit=date.fromisoformat("2023-06-15"),
    )


FIRST = "1"
SECOND = "2"


def test_gets_what_it_saved(repository: GuestRepository) -> None:
    repository.save(sample(FIRST))
    assert repository.get(FIRST) == sample(FIRST)


def test_gets_none_for_one_never_saved(repository: GuestRepository) -> None:
    assert repository.get(SECOND) is None


def test_saving_twice_keeps_one(repository: GuestRepository) -> None:
    repository.save(sample(FIRST))
    repository.save(sample(FIRST))
    assert repository.all() == [sample(FIRST)]


def test_lists_all_in_key_order(repository: GuestRepository) -> None:
    repository.save(sample(SECOND))
    repository.save(sample(FIRST))
    assert repository.all() == [sample(FIRST), sample(SECOND)]
