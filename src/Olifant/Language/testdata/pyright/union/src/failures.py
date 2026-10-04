from dataclasses import dataclass


@dataclass(frozen=True)
class SlotTaken:
    reason: str


@dataclass(frozen=True)
class PartyTooLarge:
    reason: str


type CreateBookingFailure = SlotTaken | PartyTooLarge


def status(f: CreateBookingFailure) -> int:
    match f:
        case SlotTaken():
            return 409
