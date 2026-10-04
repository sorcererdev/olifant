"""The web side: pages and htmx fragments over the booking service."""

from datetime import date, time
from pathlib import Path
from typing import Annotated

from fastapi import FastAPI, Form, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from bistro.booking.create_booking import CreateBookingInput
from bistro.booking.models import Slot
from bistro.booking.service import BookingService
from bistro.shared.result import Ok
from bistro.web.failures import create_booking_status

TEMPLATES = Jinja2Templates(directory=Path(__file__).parent / "templates")
SLOTS = [time(h, m) for h in range(17, 22) for m in (0, 30)]


def create_app(service: BookingService) -> FastAPI:
    app = FastAPI(title="Bistro")

    @app.get("/", response_class=HTMLResponse)
    def day_page(request: Request, day: date | None = None) -> HTMLResponse:
        schedule = service.get_day_schedule.handle(day or date.today())
        bookings = {(b.table_id, b.slot.start): b for b in schedule.bookings}
        return TEMPLATES.TemplateResponse(
            request, "day.html", {"day": schedule.day, "tables": schedule.tables, "slots": SLOTS, "bookings": bookings}
        )

    @app.post("/bookings", response_class=HTMLResponse)
    def create_booking(
        request: Request,
        table_id: Annotated[str, Form()],
        day: Annotated[date, Form()],
        start: Annotated[time, Form()],
        party: Annotated[int, Form()],
        guest: Annotated[str, Form()],
    ) -> HTMLResponse:
        result = service.create_booking.handle(
            CreateBookingInput(table_id=table_id, slot=Slot(day=day, start=start), party=party, guest=guest)
        )
        match result:
            case Ok(value=booking):
                return TEMPLATES.TemplateResponse(request, "booking.html", {"booking": booking}, status_code=201)
            case failure:
                return TEMPLATES.TemplateResponse(
                    request, "failure.html", {"reason": failure.reason}, status_code=create_booking_status(failure)
                )

    return app
