"""Find free tables."""

from dataclasses import dataclass


@dataclass(frozen=True)
class FindFreeTablesInput:
    id: str


@dataclass(frozen=True)
class FindFreeTablesResult:
    id: str


class FindFreeTables:
    def __init__(self) -> None:
        """Takes the ports it uses; main.py passes their adapters."""

    def handle(self, input: FindFreeTablesInput) -> FindFreeTablesResult:
        raise NotImplementedError("FindFreeTables.handle: write its first case in find_free_tables__test.py, then this")
