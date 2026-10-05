from dataclasses import dataclass
from datetime import date

from bistro.booking.models.slot import Slot


@dataclass(frozen=True)
class Visit:
    """A guest's visit, kept for the regulars list."""

    id: str
    guest: str
    day: date
    slot: Slot
    party: int
    spend: float
    regular: bool
