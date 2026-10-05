"""The composition root: adapters, then commands and queries, then routers.

Nothing runs at import; uvicorn calls the factory:
    uv run uvicorn --factory --app-dir src demo.main:create --reload
"""

from fastapi import APIRouter, FastAPI

from demo.web.app import create_app


def create(db_path: str = "demo.sqlite3") -> FastAPI:
    # adapters
    # commands and queries
    # routers
    routers: list[APIRouter] = []
    return create_app(routers)
