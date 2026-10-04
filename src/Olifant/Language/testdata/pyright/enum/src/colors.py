from enum import Enum


class Color(Enum):
    RED = 1
    BLUE = 2


def name(c: Color) -> str:
    match c:
        case Color.RED:
            return "red"
