from dataclasses import dataclass


@dataclass(frozen=True)
class Guest:
    id: str
