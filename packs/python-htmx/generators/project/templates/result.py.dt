"""A command's success: its value. Failures are their own types (see each command)."""

from dataclasses import dataclass


@dataclass(frozen=True)
class Ok[T]:
    value: T
