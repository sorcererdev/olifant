from bistro.booking.commands.add_to_waitlist import AddToWaitlist, AddToWaitlistInput
from bistro.shared.result import Ok


def command() -> AddToWaitlist:
    return AddToWaitlist()


def test_add_to_waitlist_succeeds() -> None:
    add_to_waitlist = command()
    result = add_to_waitlist.handle(AddToWaitlistInput(id="1"))
    assert result == Ok(None)
