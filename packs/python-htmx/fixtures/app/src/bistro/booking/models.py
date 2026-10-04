"""The booking service's domain: tables, half-hour slots, bookings."""

from dataclasses import dataclass
from datetime import date, time


@dataclass(frozen=True)
class Table:
    id: str
    seats: int


@dataclass(frozen=True)
class Slot:
    """A half-hour slot on a day."""

    day: date
    start: time


@dataclass(frozen=True)
class Booking:
    id: str
    table_id: str
    slot: Slot
    party: int
    guest: str
