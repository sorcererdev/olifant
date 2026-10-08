"""The legacy tests: written after the fact, against a fresh in-memory database."""

from datetime import date, timedelta

import pytest

from legacy import db, nightly, reports, utils, views
from legacy.app import create_app


@pytest.fixture(autouse=True)
def fresh():
    db.reset()
    db.seed()
    utils.use_restaurant("bistro")
    utils.SENT_MESSAGES.clear()
    yield


def booking_form(**over):
    form = {"guest": "Margaret", "phone": "+44 20 7946 0000", "party": "2", "start": "19:00", "day": "today"}
    form.update(over)
    return form


# ── bookings ────────────────────────────────────────────────────────────


def test_todays_bookings_lists_the_seeded_ones_earliest_first():
    found = views.todays_bookings()
    assert [b["id"] for b in found["bookings"]] == ["b1", "b2", "b3"]
    assert found["count"] == 3


def test_a_booking_is_made_on_a_free_table_and_confirmed():
    body, status = views.make_booking(booking_form())
    assert status == 201
    assert body["table_id"] == "t2"
    assert utils.SENT_MESSAGES and utils.SENT_MESSAGES[0][0] == "+44 20 7946 0000"


def test_a_large_party_pays_a_deposit():
    body, status = views.make_booking(booking_form(party="6", start="21:00"))
    assert status == 201
    assert body["deposit"] == 60


def test_a_bad_form_is_refused_with_every_reason():
    body, status = views.make_booking(booking_form(guest="", party="0"))
    assert status == 400
    assert "guest name looks wrong" in body["errors"]
    assert "party must be at least 1" in body["errors"]


def test_no_table_free_is_a_conflict():
    views.make_booking(booking_form(party="8", start="20:00"))
    body, status = views.make_booking(booking_form(party="8", start="20:00", guest="Edsger"))
    assert status == 409


def test_cancel_twice_is_a_conflict():
    assert views.cancel_booking("b1")["status"] == "cancelled"
    assert views.cancel_booking("b1")[1] == 409


def test_moving_a_booking_keeps_its_table_when_it_can():
    moved = views.move_booking("b1", "20:30")
    assert moved == {"id": "b1", "start": "20:30", "table_id": "t1"}


def test_a_no_show_is_charged_for_four_or_more():
    assert views.mark_no_show("b2")["fee"] == 25
    assert views.mark_no_show("b1")["fee"] == 0


# ── tables ──────────────────────────────────────────────────────────────


def test_closing_a_table_takes_it_out_of_free_tables():
    assert views.close_table("t2") == {"id": "t2", "open": False}
    assert "t2" not in [t["id"] for t in utils.open_tables()]


def test_closing_an_unknown_table_is_not_found():
    assert views.close_table("t9") == ({"error": "no such table"}, 404)


def test_seat_party_finds_an_exact_fit_first():
    assert views.seat_party(4, "18:00")["table_id"] == "t3"


def test_seat_party_says_why_not():
    assert views.seat_party(0, "18:00")[1] == 400
    assert views.seat_party(2, "23:30") == ({"error": "kitchen closed"}, 409)
    assert views.seat_party(2, "18:00", "nowhere") == ({"error": "nothing free in nowhere"}, 409)


# ── exports and reports ─────────────────────────────────────────────────


def test_export_rows_starts_with_the_header():
    rows = list(views.export_rows())
    assert rows[0] == "id,table,start,party,guest,status,deposit"
    assert len(rows) == 4


def test_week_summary_totals_the_seeded_day():
    summary = reports.week_summary(date.today().isoformat())
    assert summary["totals"]["covers"] == 12


def test_no_shows_count_repeat_guests():
    for day in (date.today() - timedelta(days=2), date.today() - timedelta(days=9)):
        c = db.conn()
        c.execute("INSERT INTO bookings (id, table_id, day, start, party, guest, status) VALUES (?, 't3', ?, '19:00', 4, 'Kurt', 'no-show')", (f"x{day.day}", day.isoformat()))
        c.commit()
    assert reports.no_shows()["repeat_guests"] == ["Kurt"]


def test_nightly_reopens_closed_tables():
    views.close_table("t4")
    assert nightly.reopen_all_tables() == ["t4"]


# ── the app ─────────────────────────────────────────────────────────────


def test_the_app_serves_todays_bookings():
    client = create_app(seed=False).test_client()
    response = client.get("/bookings")
    assert response.status_code == 200
    assert response.get_json()["count"] == 3


def test_the_app_closes_a_table():
    client = create_app(seed=False).test_client()
    assert client.post("/tables/t1/close").get_json() == {"id": "t1", "open": False}
