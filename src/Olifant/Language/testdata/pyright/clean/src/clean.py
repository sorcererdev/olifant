from dataclasses import dataclass


@dataclass(frozen=True)
class SlotTaken:
    reason: str


type Failure = SlotTaken


def status(f: Failure) -> int:
    match f:
        case SlotTaken():
            return 409
