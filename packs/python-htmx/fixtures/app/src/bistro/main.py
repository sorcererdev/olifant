"""The composition root: real adapters, the service, the web app.

Run: uv run uvicorn --app-dir src bistro.main:app --reload
"""

from bistro.adapters.sqlite_booking_repository import SqliteBookingRepository
from bistro.adapters.uuid_ids import UuidIds
from bistro.booking.models import Table
from bistro.booking.service import BookingService
from bistro.web.app import create_app

bookings = SqliteBookingRepository("bistro.sqlite3")
bookings.add_tables([Table(id="t1", seats=2), Table(id="t2", seats=4), Table(id="t3", seats=6)])
app = create_app(BookingService(bookings=bookings, ids=UuidIds()))
