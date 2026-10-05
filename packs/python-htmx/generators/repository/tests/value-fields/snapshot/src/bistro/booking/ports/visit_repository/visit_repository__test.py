"""Every VisitRepository adapter keeps the same contract."""

from pathlib import Path

import pytest

from bistro.booking.models.visit import Visit
from bistro.booking.ports.visit_repository.visit_repository import VisitRepository
from bistro.booking.ports.visit_repository.in_memory_visit_repository import InMemoryVisitRepository
from bistro.booking.ports.visit_repository.sqlite_visit_repository import SqliteVisitRepository
from datetime import date, time
from bistro.booking.models.slot import Slot


@pytest.fixture(params=["in_memory", "sqlite"])
def repository(request: pytest.FixtureRequest, tmp_path: Path) -> VisitRepository:
    if request.param == "sqlite":
        return SqliteVisitRepository(str(tmp_path / "visit.sqlite3"))
    return InMemoryVisitRepository()


def sample(id: str) -> Visit:
    return Visit(
        id=id,
        guest="guest",
        day=date(2023, 10, 5),
        slot=Slot(day=date(2023, 10, 5), start=time(18, 30)),
        party=2,
        spend=2.5,
        regular=True,
    )


FIRST = "1"
SECOND = "2"


def test_gets_what_it_saved(repository: VisitRepository) -> None:
    repository.save(sample(FIRST))
    assert repository.get(FIRST) == sample(FIRST)


def test_gets_none_for_one_never_saved(repository: VisitRepository) -> None:
    assert repository.get(SECOND) is None


def test_saving_twice_keeps_one(repository: VisitRepository) -> None:
    repository.save(sample(FIRST))
    repository.save(sample(FIRST))
    assert repository.all() == [sample(FIRST)]


def test_lists_all_in_key_order(repository: VisitRepository) -> None:
    repository.save(sample(SECOND))
    repository.save(sample(FIRST))
    assert repository.all() == [sample(FIRST), sample(SECOND)]
