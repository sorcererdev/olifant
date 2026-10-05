"""Every Clock adapter keeps the same contract."""

import pytest

from bistro.booking.ports.clock.clock import Clock
from bistro.booking.ports.clock.in_memory_clock import InMemoryClock

ADAPTERS: list[type[Clock]] = [InMemoryClock]


@pytest.fixture(params=ADAPTERS)
def adapter(request: pytest.FixtureRequest) -> Clock:
    made: Clock = request.param()
    return made


def test_is_a_clock(adapter: Clock) -> None:
    assert isinstance(adapter, tuple(ADAPTERS))


def test_now(adapter: Clock) -> None:
    assert False, "Clock.now: write its contract"
