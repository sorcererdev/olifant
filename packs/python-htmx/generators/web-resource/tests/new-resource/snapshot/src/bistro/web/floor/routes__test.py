from fastapi import FastAPI
from fastapi.testclient import TestClient

from bistro.booking.queries.find_free_tables import FindFreeTables
from bistro.web.floor.routes import floor_routes
from bistro.booking.commands.cancel_booking import CancelBooking
from bistro.booking.ports.booking_repository.in_memory_booking_repository import InMemoryBookingRepository


def client() -> TestClient:
    app = FastAPI()
    app.include_router(
        floor_routes(
            find_free_tables=FindFreeTables(),
            cancel_booking=CancelBooking(
                bookings=InMemoryBookingRepository(),
            ),
        )
    )
    return TestClient(app)


def test_the_floor_page_shows_it() -> None:
    response = client().get("/floor", params={"id": "1"})
    assert response.status_code == 200
    assert "what the floor page shows" in response.text
