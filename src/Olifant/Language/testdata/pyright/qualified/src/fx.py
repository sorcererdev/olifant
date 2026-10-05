from dataclasses import dataclass


@dataclass(frozen=True)
class SlotTaken:
    reason: str


@dataclass(frozen=True)
class PartyTooLarge:
    reason: str


type Failure = SlotTaken | PartyTooLarge
