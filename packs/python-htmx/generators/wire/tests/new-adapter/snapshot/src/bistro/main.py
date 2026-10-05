"""The composition root: adapters, then commands and queries, then routers.

Nothing runs at import; uvicorn calls the factory:
    uv run uvicorn --factory --app-dir src bistro.main:create --reload
"""

from fastapi import APIRouter, FastAPI

from bistro.booking.commands.create_booking import CreateBooking
from bistro.booking.models.table import Table
from bistro.booking.ports.booking_repository.sqlite_booking_repository import SqliteBookingRepository
from bistro.booking.ports.id_generator.uuid_id_generator import UuidIdGenerator
from bistro.booking.queries.get_day_schedule import GetDaySchedule
from bistro.web.app import create_app
from bistro.web.bookings.routes import bookings_routes
from bistro.booking.ports.table_repository.sqlite_table_repository import SqliteTableRepository


def create(db_path: str = "bistro.sqlite3") -> FastAPI:
    # adapters
    booking_repository = SqliteBookingRepository(db_path)
    booking_repository.add_tables([Table(id="t1", seats=2), Table(id="t2", seats=4), Table(id="t3", seats=6)])
    id_generator = UuidIdGenerator()
    table_repository = SqliteTableRepository(db_path)
    # commands and queries
    create_booking = CreateBooking(
        bookings=booking_repository,
        ids=id_generator,
    )
    get_day_schedule = GetDaySchedule(
        bookings=booking_repository,
        tables=table_repository,
    )
    # routers
    routers: list[APIRouter] = []
    routers.append(bookings_routes(get_day_schedule=get_day_schedule, create_booking=create_booking))
    return create_app(routers)
