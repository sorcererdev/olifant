from dataclasses import dataclass
from datetime import date, time


@dataclass(frozen=True)
class Slot:
    """A half-hour slot on a day."""

    day: date
    start: time
