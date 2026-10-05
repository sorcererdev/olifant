"""Floor on the web: the page (from find_free_tables), and a route per command (htmx)."""

from pathlib import Path
from typing import Annotated

from fastapi import APIRouter, Depends, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates
from pydantic import TypeAdapter, ValidationError

from bistro.booking.queries.find_free_tables import FindFreeTables, FindFreeTablesInput
from bistro.booking.commands.cancel_booking import CancelBooking, CancelBookingInput
from bistro.web.floor.failures import cancel_booking_status
from bistro.shared.result import Ok

HERE = Path(__file__).parent
TEMPLATES = Jinja2Templates(directory=[HERE / "templates", HERE.parent / "templates"])


def floor_routes(
    find_free_tables: FindFreeTables,
    cancel_booking: CancelBooking,
) -> APIRouter:
    router = APIRouter(prefix="/floor")

    @router.get("", response_class=HTMLResponse)
    def page(request: Request, input: Annotated[FindFreeTablesInput, Depends()]) -> HTMLResponse:
        return TEMPLATES.TemplateResponse(request, "page.html", {"result": find_free_tables.handle(input)})

    @router.post("/cancel_booking", response_class=HTMLResponse)
    async def cancel_booking_route(request: Request) -> HTMLResponse:
        try:
            input = TypeAdapter(CancelBookingInput).validate_python(dict(await request.form()))
        except ValidationError as error:
            return TEMPLATES.TemplateResponse(request, "failure.html", {"reason": str(error)}, status_code=422)
        match cancel_booking.handle(input):
            case Ok(value=value):
                return TEMPLATES.TemplateResponse(request, "done.html", {"value": value}, status_code=201)
            case failure:
                return TEMPLATES.TemplateResponse(request, "failure.html", {"reason": failure.reason}, status_code=cancel_booking_status(failure))

    return router
