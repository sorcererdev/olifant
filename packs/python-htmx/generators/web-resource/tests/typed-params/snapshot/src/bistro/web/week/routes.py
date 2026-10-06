"""Week on the web: the page (from week_view), and a route per command (htmx)."""

from pathlib import Path
from typing import Annotated

from fastapi import APIRouter, Depends, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from bistro.booking.queries.week_view import WeekView, WeekViewInput

HERE = Path(__file__).parent
TEMPLATES = Jinja2Templates(directory=[HERE / "templates", HERE.parent / "templates"])


def week_routes(
    week_view: WeekView,
) -> APIRouter:
    router = APIRouter(prefix="/week")

    @router.get("", response_class=HTMLResponse)
    def page(request: Request, input: Annotated[WeekViewInput, Depends()]) -> HTMLResponse:
        return TEMPLATES.TemplateResponse(request, "page.html", {"result": week_view.handle(input)})

    return router
