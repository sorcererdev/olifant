from pathlib import Path

from fastapi import FastAPI


def test_importing_main_opens_no_database() -> None:
    import importlib

    import demo.main

    importlib.reload(demo.main)
    assert not Path("demo.sqlite3").exists()


def test_create_makes_the_app(tmp_path: Path) -> None:
    from demo.main import create

    assert isinstance(create(str(tmp_path / "demo.sqlite3")), FastAPI)
