"""IdGenerator in memory: ids b1, b2, …, predictable, for tests and demos."""


class InMemoryIdGenerator:
    def __init__(self) -> None:
        self._n = 0

    def next_id(self) -> str:
        self._n += 1
        return f"b{self._n}"
