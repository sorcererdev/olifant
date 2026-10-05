from dataclasses import dataclass
from datetime import date


@dataclass(frozen=True)
class Guest:
    id: str
    name: str
    phone: str | None
    visits: int = 0
    regular: bool | None = None
    last_visit: date | None = None
