"""A command's success: its value. Failures are their own types (see each command)."""

from dataclasses import dataclass


@dataclass(frozen=True)
class Ok[T]:
    value: T


@dataclass(frozen=True)
class NotYet:
    """The new code declines a case it does not cover yet: the legacy path runs it."""

    reason: str
