from dataclasses import dataclass


@dataclass(frozen=True)
class Table:
    id: str
    seats: int
