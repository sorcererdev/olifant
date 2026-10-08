from pathlib import Path

from fastapi import FastAPI


def test_importing_main_opens_no_database() -> None:
    import importlib

    import bistro.main

    importlib.reload(bistro.main)
    assert not Path("bistro.sqlite3").exists()


def test_create_wires_the_app_to_a_database_at_a_path(tmp_path: Path) -> None:
    from bistro.main import create

    db = tmp_path / "bistro.sqlite3"
    app = create(str(db))
    assert isinstance(app, FastAPI)
    assert db.exists()
