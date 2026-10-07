from dataclasses import dataclass

from bistro.booking.models.slot import Slot


@dataclass(frozen=True)
class Booking:
    id: str
    table_id: str
    slot: Slot
    party: int
    guest: str
