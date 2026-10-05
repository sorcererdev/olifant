from bistro.booking.queries.find_free_tables import FindFreeTables, FindFreeTablesInput, FindFreeTablesResult


def query() -> FindFreeTables:
    return FindFreeTables()


def test_find_free_tables_answers() -> None:
    find_free_tables = query()
    result = find_free_tables.handle(FindFreeTablesInput(id="1"))
    assert result == FindFreeTablesResult(id="1")
