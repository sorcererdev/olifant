"""Parity: the legacy close_table and the new close_table agree.

Pinned before its switch turns on: the same inputs, the same outcome. Fill in
the inputs (and what "the same" means, if the shapes differ), make it green,
then turn the switch on in the registry.
"""

import importlib

from bistro.booking.commands.close_table import CloseTable, CloseTableInput

LEGACY = importlib.import_module("legacy/views.py".removesuffix(".py").replace("/", "."))


def build() -> CloseTable:
    return CloseTable()


def test_close_table_agrees_with_the_legacy_path() -> None:
    legacy_result: object = LEGACY.close_table(getattr(NotImplemented, "olifant: legacy_args: arguments that exercise the legacy function"))
    new_result: object = build().handle(CloseTableInput(getattr(NotImplemented, "olifant: new_args: the same inputs, as the Input's fields")))
    assert getattr(NotImplemented, "olifant: same: legacy_result and new_result agree (the legacy shape against the new result)"), (legacy_result, new_result)
