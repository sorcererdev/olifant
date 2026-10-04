"""Random ids for real use."""

import uuid


class UuidIds:
    def next_id(self) -> str:
        return uuid.uuid4().hex[:8]
