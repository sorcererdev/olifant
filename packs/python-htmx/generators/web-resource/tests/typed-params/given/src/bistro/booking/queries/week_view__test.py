from datetime import date

from bistro.booking.queries.week_view import WeekView, WeekViewInput, WeekViewResult


def query() -> WeekView:
    return WeekView()


def test_week_view_answers() -> None:
    week_view = query()
    result = week_view.handle(WeekViewInput(day=date(2026, 10, 9), party=2, guest="Ada"))
    assert result == WeekViewResult(id="1")
