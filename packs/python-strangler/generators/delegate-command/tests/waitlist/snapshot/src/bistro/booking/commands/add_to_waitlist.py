"""Add to waitlist."""

from dataclasses import dataclass

from bistro.shared.result import Ok


@dataclass(frozen=True)
class AddToWaitlistInput:
    id: str


@dataclass(frozen=True)
class NotFound:
    reason: str


type AddToWaitlistFailure = (
    NotFound
)

type AddToWaitlistOutput = Ok[None] | AddToWaitlistFailure


class AddToWaitlist:
    def __init__(self) -> None:
        """Takes the ports it uses; main.py passes their adapters."""

    def handle(self, input: AddToWaitlistInput) -> AddToWaitlistOutput:
        raise NotImplementedError("AddToWaitlist.handle: write its first case in add_to_waitlist__test.py, then this")
