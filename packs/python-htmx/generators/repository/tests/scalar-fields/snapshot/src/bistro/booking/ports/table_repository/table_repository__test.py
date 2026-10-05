"""Every TableRepository adapter keeps the same contract."""

from pathlib import Path

import pytest

from bistro.booking.models.table import Table
from bistro.booking.ports.table_repository.table_repository import TableRepository
from bistro.booking.ports.table_repository.in_memory_table_repository import InMemoryTableRepository
from bistro.booking.ports.table_repository.sqlite_table_repository import SqliteTableRepository


@pytest.fixture(params=["in_memory", "sqlite"])
def repository(request: pytest.FixtureRequest, tmp_path: Path) -> TableRepository:
    if request.param == "sqlite":
        return SqliteTableRepository(str(tmp_path / "table.sqlite3"))
    return InMemoryTableRepository()


def sample(id: str) -> Table:
    return Table(
        id=id,
        seats=2,
    )


FIRST = "1"
SECOND = "2"


def test_gets_what_it_saved(repository: TableRepository) -> None:
    repository.save(sample(FIRST))
    assert repository.get(FIRST) == sample(FIRST)


def test_gets_none_for_one_never_saved(repository: TableRepository) -> None:
    assert repository.get(SECOND) is None


def test_saving_twice_keeps_one(repository: TableRepository) -> None:
    repository.save(sample(FIRST))
    repository.save(sample(FIRST))
    assert repository.all() == [sample(FIRST)]


def test_lists_all_in_key_order(repository: TableRepository) -> None:
    repository.save(sample(SECOND))
    repository.save(sample(FIRST))
    assert repository.all() == [sample(FIRST), sample(SECOND)]
