"""Switches: which extracted paths are on. Turn one on in a PR when its new
path is trusted (its parity test passes)."""

SWITCHES: dict[str, bool] = {}
SWITCHES["seat-party"] = False


def on(name: str) -> bool:
    return SWITCHES.get(name, False)
