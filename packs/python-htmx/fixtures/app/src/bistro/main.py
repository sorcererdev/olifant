"""The composition root: real adapters, the service, the web app.

Nothing runs at import; uvicorn calls the factory:
    uv run uvicorn --factory --app-dir src bistro.main:create --reload
"""

from fastapi import FastAPI

from bistro.adapters.sqlite_booking_repository import SqliteBookingRepository
from bistro.adapters.uuid_ids import UuidIds
from bistro.booking.models import Table
from bistro.booking.service import BookingService
from bistro.web.app import create_app


def create(db_path: str = "bistro.sqlite3") -> FastAPI:
    bookings = SqliteBookingRepository(db_path)
    bookings.add_tables([Table(id="t1", seats=2), Table(id="t2", seats=4), Table(id="t3", seats=6)])
    return create_app(BookingService(bookings=bookings, ids=UuidIds()))
