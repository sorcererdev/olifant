from fastapi import FastAPI
from fastapi.testclient import TestClient

from bistro.booking.queries.week_view import WeekView
from bistro.web.week.routes import week_routes


def client() -> TestClient:
    app = FastAPI()
    app.include_router(
        week_routes(
            week_view=WeekView(),
        )
    )
    return TestClient(app)


def test_the_week_page_shows_it() -> None:
    response = client().get("/week", params={"day": "2026-10-09", "party": 1, "guest": "1"})
    assert response.status_code == 200
    assert "what the week page shows" in response.text
