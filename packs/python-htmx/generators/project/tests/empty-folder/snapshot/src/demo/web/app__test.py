from fastapi.testclient import TestClient

from demo.web.app import create_app


def test_the_home_page_names_the_app() -> None:
    page = TestClient(create_app([])).get("/")
    assert page.status_code == 200
    assert "demo" in page.text
