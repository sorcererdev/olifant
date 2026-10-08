"""Close table."""

from dataclasses import dataclass

from bistro.shared.result import Ok


@dataclass(frozen=True)
class CloseTableInput:
    id: str


@dataclass(frozen=True)
class NotFound:
    reason: str


type CloseTableFailure = (
    NotFound
)

type CloseTableOutput = Ok[None] | CloseTableFailure


class CloseTable:
    def __init__(self) -> None:
        """Takes the ports it uses; main.py passes their adapters."""

    def handle(self, input: CloseTableInput) -> CloseTableOutput:
        raise NotImplementedError("CloseTable.handle: write its first case in close_table__test.py, then this")
