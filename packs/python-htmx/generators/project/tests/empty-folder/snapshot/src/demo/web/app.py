"""The web app: the home page, and each resource's routes."""

from pathlib import Path

from fastapi import APIRouter, FastAPI, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from demo.web.resources import RESOURCES

TEMPLATES = Jinja2Templates(directory=Path(__file__).parent / "templates")


def create_app(routers: list[APIRouter]) -> FastAPI:
    app = FastAPI(title="demo")

    @app.get("/", response_class=HTMLResponse)
    def home(request: Request) -> HTMLResponse:
        return TEMPLATES.TemplateResponse(request, "home.html", {"resources": RESOURCES})

    for router in routers:
        app.include_router(router)
    return app
