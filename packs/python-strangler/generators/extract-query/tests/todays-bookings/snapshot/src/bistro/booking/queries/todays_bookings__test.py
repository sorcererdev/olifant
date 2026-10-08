from bistro.booking.queries.todays_bookings import TodaysBookings, TodaysBookingsInput, TodaysBookingsResult


def query() -> TodaysBookings:
    return TodaysBookings()


def test_todays_bookings_answers() -> None:
    todays_bookings = query()
    result = todays_bookings.handle(TodaysBookingsInput(id="1"))
    assert result == TodaysBookingsResult(id="1")
