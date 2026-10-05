"""Clock in memory: for tests, and until a real adapter exists."""

from datetime import datetime


class InMemoryClock:
    """Keeps the Clock contract (see clock__test.py)."""

    def now(self) -> datetime:
        return datetime.now()
