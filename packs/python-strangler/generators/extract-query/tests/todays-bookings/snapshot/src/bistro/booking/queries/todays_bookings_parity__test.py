"""Parity: the legacy todays_bookings and the new todays_bookings agree.

Pinned before its switch turns on: the same inputs, the same outcome. Fill in
the inputs (and what "the same" means, if the shapes differ), make it green,
then turn the switch on in the registry.
"""

import importlib

from bistro.booking.queries.todays_bookings import TodaysBookings, TodaysBookingsInput

LEGACY = importlib.import_module("legacy/views.py".removesuffix(".py").replace("/", "."))


def build() -> TodaysBookings:
    return TodaysBookings()


def test_todays_bookings_agrees_with_the_legacy_path() -> None:
    legacy_result: object = LEGACY.todays_bookings(getattr(NotImplemented, "olifant: legacy_args: arguments that exercise the legacy function"))
    new_result: object = build().handle(TodaysBookingsInput(getattr(NotImplemented, "olifant: new_args: the same inputs, as the Input's fields")))
    assert getattr(NotImplemented, "olifant: same: legacy_result and new_result agree (the legacy shape against the new result)"), (legacy_result, new_result)
