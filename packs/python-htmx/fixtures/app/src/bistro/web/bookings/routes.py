"""Bookings on the web: the day page, and booking a table (htmx)."""

from datetime import date, time
from pathlib import Path
from typing import Annotated

from fastapi import APIRouter, Form, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from bistro.booking.commands.create_booking import CreateBooking, CreateBookingInput
from bistro.booking.models.slot import Slot
from bistro.booking.queries.get_day_schedule import GetDaySchedule
from bistro.shared.result import Ok
from bistro.web.bookings.failures import create_booking_status

HERE = Path(__file__).parent
TEMPLATES = Jinja2Templates(directory=[HERE / "templates", HERE.parent / "templates"])
SLOTS = [time(h, m) for h in range(17, 22) for m in (0, 30)]


def bookings_routes(get_day_schedule: GetDaySchedule, create_booking: CreateBooking) -> APIRouter:
    router = APIRouter(prefix="/bookings")

    @router.get("", response_class=HTMLResponse)
    def page(request: Request, day: date | None = None) -> HTMLResponse:
        schedule = get_day_schedule.handle(day or date.today())
        bookings = {(b.table_id, b.slot.start): b for b in schedule.bookings}
        return TEMPLATES.TemplateResponse(
            request, "page.html", {"day": schedule.day, "tables": schedule.tables, "slots": SLOTS, "bookings": bookings}
        )

    @router.post("/create_booking", response_class=HTMLResponse)
    def create_booking_route(
        request: Request,
        table_id: Annotated[str, Form()],
        day: Annotated[date, Form()],
        start: Annotated[time, Form()],
        party: Annotated[int, Form()],
        guest: Annotated[str, Form()],
    ) -> HTMLResponse:
        result = create_booking.handle(
            CreateBookingInput(table_id=table_id, slot=Slot(day=day, start=start), party=party, guest=guest)
        )
        match result:
            case Ok(value=booking):
                return TEMPLATES.TemplateResponse(request, "booking.html", {"booking": booking}, status_code=201)
            case failure:
                return TEMPLATES.TemplateResponse(
                    request, "failure.html", {"reason": failure.reason}, status_code=create_booking_status(failure)
                )

    return router
