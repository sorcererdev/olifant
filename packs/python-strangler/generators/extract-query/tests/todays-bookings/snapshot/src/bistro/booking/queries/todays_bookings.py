"""Todays bookings."""

from dataclasses import dataclass


@dataclass(frozen=True)
class TodaysBookingsInput:
    id: str


@dataclass(frozen=True)
class TodaysBookingsResult:
    id: str


class TodaysBookings:
    def __init__(self) -> None:
        """Takes the ports it uses; main.py passes their adapters."""

    def handle(self, input: TodaysBookingsInput) -> TodaysBookingsResult:
        raise NotImplementedError("TodaysBookings.handle: write its first case in todays_bookings__test.py, then this")
