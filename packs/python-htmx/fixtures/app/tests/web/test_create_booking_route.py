from fastapi.testclient import TestClient

from bistro.adapters.in_memory_booking_repository import InMemoryBookingRepository
from bistro.adapters.sequential_ids import SequentialIds
from bistro.booking.models import Table
from bistro.booking.service import BookingService
from bistro.web.app import create_app


def client() -> TestClient:
    service = BookingService(bookings=InMemoryBookingRepository(tables=[Table(id="t1", seats=4)]), ids=SequentialIds())
    return TestClient(create_app(service))


FORM = {"table_id": "t1", "day": "2026-10-09", "start": "19:00", "party": "2", "guest": "Ada"}


def test_the_day_page_lists_the_tables() -> None:
    page = client().get("/?day=2026-10-09")
    assert page.status_code == 200
    assert "t1" in page.text


def test_a_booking_is_created() -> None:
    response = client().post("/bookings", data=FORM)
    assert response.status_code == 201
    assert "Ada" in response.text


def test_a_taken_slot_is_a_conflict() -> None:
    c = client()
    c.post("/bookings", data=FORM)
    assert c.post("/bookings", data=FORM | {"guest": "Grace"}).status_code == 409


def test_a_party_too_large_is_unprocessable() -> None:
    assert client().post("/bookings", data=FORM | {"party": "9"}).status_code == 422


def test_an_unknown_table_is_not_found() -> None:
    assert client().post("/bookings", data=FORM | {"table_id": "t9"}).status_code == 404
