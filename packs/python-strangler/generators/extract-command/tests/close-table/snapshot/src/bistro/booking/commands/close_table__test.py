from bistro.booking.commands.close_table import CloseTable, CloseTableInput
from bistro.shared.result import Ok


def command() -> CloseTable:
    return CloseTable()


def test_close_table_succeeds() -> None:
    close_table = command()
    result = close_table.handle(CloseTableInput(id="1"))
    assert result == Ok(None)
