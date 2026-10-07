from legacy import db


def pytest_configure():
    db.reset()
