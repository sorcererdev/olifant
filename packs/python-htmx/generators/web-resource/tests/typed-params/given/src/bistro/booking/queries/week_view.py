"""Week view."""

from dataclasses import dataclass
from datetime import date


@dataclass(frozen=True)
class WeekViewInput:
    day: date
    party: int
    guest: str


@dataclass(frozen=True)
class WeekViewResult:
    id: str


class WeekView:
    def __init__(self) -> None:
        """Takes the ports it uses; main.py passes their adapters."""

    def handle(self, input: WeekViewInput) -> WeekViewResult:
        raise NotImplementedError("WeekView.handle: write its first case in week_view__test.py, then this")
