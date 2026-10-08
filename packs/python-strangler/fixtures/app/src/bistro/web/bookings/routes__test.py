from fastapi import FastAPI
from fastapi.testclient import TestClient

from bistro.booking.commands.create_booking import CreateBooking
from bistro.booking.models.table import Table
from bistro.booking.ports.booking_repository.in_memory_booking_repository import InMemoryBookingRepository
from bistro.booking.ports.id_generator.in_memory_id_generator import InMemoryIdGenerator
from bistro.booking.queries.get_day_schedule import GetDaySchedule
from bistro.web.bookings.routes import bookings_routes


def client() -> TestClient:
    bookings = InMemoryBookingRepository(tables=[Table(id="t1", seats=4)])
    app = FastAPI()
    app.include_router(
        bookings_routes(
            get_day_schedule=GetDaySchedule(
                bookings=bookings,
            ),
            create_booking=CreateBooking(
                bookings=bookings,
                ids=InMemoryIdGenerator(),
            ),
        )
    )
    return TestClient(app)


FORM = {"table_id": "t1", "day": "2026-10-09", "start": "19:00", "party": "2", "guest": "Ada"}


def test_the_day_page_lists_the_tables() -> None:
    page = client().get("/bookings?day=2026-10-09")
    assert page.status_code == 200
    assert "t1" in page.text


def test_a_booking_is_created() -> None:
    response = client().post("/bookings/create_booking", data=FORM)
    assert response.status_code == 201
    assert "Ada" in response.text


def test_a_taken_slot_is_a_conflict() -> None:
    c = client()
    c.post("/bookings/create_booking", data=FORM)
    assert c.post("/bookings/create_booking", data=FORM | {"guest": "Grace"}).status_code == 409


def test_a_party_too_large_is_unprocessable() -> None:
    assert client().post("/bookings/create_booking", data=FORM | {"party": "9"}).status_code == 422


def test_an_unknown_table_is_not_found() -> None:
    assert client().post("/bookings/create_booking", data=FORM | {"table_id": "t9"}).status_code == 404
