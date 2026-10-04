import fx


def status(f: fx.Failure) -> int:
    match f:
        case fx.SlotTaken():
            return 409
