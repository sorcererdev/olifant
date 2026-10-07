"""Where new ids come from."""

from typing import Protocol


class IdGenerator(Protocol):
    def next_id(self) -> str: ...
