"""Conjure 2026-11 (experiment, not product code): the corpus. Copies the bistro demo at the
`fill` stage (scripts/slice-python.sh DIR fill; every test green) once per case, makes the case's
edit, commits it (so a step's diff is `git diff` in the copy) and writes cases.json beside this
file. Usage: python3 make_cases.py BISTRO_DIR OUT_DIR"""
import json, os, shutil, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
CANCEL = "src/bistro/booking/commands/cancel_booking.py"
CREATE = "src/bistro/booking/commands/create_booking.py"
SCHED = "src/bistro/booking/queries/get_day_schedule.py"
CANCEL_BODY = """        booking = self.bookings.get(input.id)
        if booking is None:
            return NotFound(f"there is no booking {input.id}")
        self.bookings.remove(input.id)
        return Ok(None)"""
CREATE_BODY = """        table = self.tables.get(input.table_id)
        if table is None:
            return TableNotFound(f"there is no table {input.table_id}")
        if input.party > table.seats:
            return PartyTooLarge(f"table {table.id} seats {table.seats}, not {input.party}")
        if any(b.table_id == table.id and b.day == input.day and b.start == input.start for b in self.bookings.all()):
            return SlotTaken(f"table {table.id} is booked at {input.start:%H:%M}")
        booking = Booking(id=self.ids.next_id(), table_id=table.id, day=input.day, start=input.start, party=input.party, guest=input.guest)
        self.bookings.save(booking)
        return Ok(booking)"""
SCHED_BODY = """        bookings = [b for b in self.bookings.all() if b.day == input.day]
        return GetDayScheduleResult(day=input.day, tables=self.tables.all(), bookings=sorted(bookings, key=lambda b: (b.start, b.table_id)))"""
COUNT = '''"""How many bookings a day has."""

from dataclasses import dataclass
from datetime import date

from bistro.booking.ports.booking_repository.booking_repository import BookingRepository


@dataclass(frozen=True)
class CountDayBookingsInput:
    day: date


class CountDayBookings:
    def __init__(self, bookings: BookingRepository) -> None:
        """Takes the ports it uses; main.py passes their adapters."""
        self.bookings = bookings

    def handle(self, input: CountDayBookingsInput) -> int:
        return sum(1 for b in self.bookings.all() if b.day == input.day)
'''

def stub(cls, test):
    return f'        raise NotImplementedError("{cls}.handle: write its first case in {test}, then this")'

# id, kind, file, unit, new body (None: leave), extra edits {path: text or None to delete}, reference next step
CASES = [
    ("nonsense-cancel", "nonsense", CANCEL, "CancelBooking.handle",
     "        bookings get the id\n        if not there say so", {},
     "Turn the two prose lines into code: get the booking by input.id, return NotFound(\"there is no booking <id>\") when missing, else remove it and return Ok(None)."),
    ("nonsense-schedule", "nonsense", SCHED, "GetDaySchedule.handle",
     "        x = self.tables.all()\n        foo = 12\n        return x", {},
     "Return a GetDayScheduleResult for input.day: all tables and that day's bookings (sorted by start, then table); drop the stray `foo = 12`."),
    ("stub-cancel", "stub", CANCEL, "CancelBooking.handle", stub("CancelBooking", "cancel_booking__test.py"), {},
     "Write the body the two tests describe: get by input.id, NotFound when missing, else remove and return Ok(None)."),
    ("stub-create", "stub", CREATE, "CreateBooking.handle", stub("CreateBooking", "create_booking__test.py"), {},
     "Start the body the four tests describe: look up the table (TableNotFound), then party size (PartyTooLarge), taken slot (SlotTaken), save a new booking with ids.next_id() and return Ok(booking); any first slice of that, in that order, is forward."),
    ("half-create", "half", CREATE, "CreateBooking.handle", "\n".join(CREATE_BODY.split("\n")[:5]), {},
     "Continue after the party check: refuse a taken slot (SlotTaken \"table t1 is booked at 19:00\"), then build the booking with ids.next_id(), save it and return Ok(booking)."),
    ("half-schedule", "half", SCHED, "GetDaySchedule.handle", SCHED_BODY.split("\n")[0], {},
     "Return GetDayScheduleResult(day=input.day, tables=self.tables.all(), bookings=the filtered list sorted by (start, table_id))."),
    ("bug-create", "bug", CREATE, "CreateBooking.handle",
     CREATE_BODY.replace('f"table {table.id} seats {table.seats}, not {input.party}"', 'f"table {table.id} has {table.seats} seats"'), {},
     "Fix PartyTooLarge's message to \"table t1 seats 4, not 6\" (the test's wording); change nothing else."),
    ("bug-cancel", "bug", CANCEL, "CancelBooking.handle", CANCEL_BODY.replace("remove(input.id)", "remove(booking.table_id)"), {},
     "Remove by the booking's id (input.id), not its table_id; change nothing else."),
    ("syntax-create", "syntax", CREATE, "CreateBooking.handle", CREATE_BODY.replace("if table is None:", "if table is None"), {},
     "Add the missing colon after `if table is None`; change nothing else."),
    ("syntax-cancel", "syntax", CANCEL, "CancelBooking.handle",
     CANCEL_BODY.replace('return NotFound(f"there is no booking {input.id}")', 'return NotFound(f"there is no booking {input.id}"'), {},
     "Close the NotFound( call's parenthesis; change nothing else."),
    ("notest-cancel", "no-test", CANCEL, "CancelBooking.handle", None, {"src/bistro/booking/commands/cancel_booking__test.py": None},
     "Write cancel_booking__test.py: one test that cancels an existing booking (Ok(None), repository empty) and one that refuses a missing id (NotFound); the body stays as it is."),
    ("notest-count", "no-test", "src/bistro/booking/queries/count_day_bookings.py", "CountDayBookings.handle", None,
     {"src/bistro/booking/queries/count_day_bookings.py": COUNT},
     "Write count_day_bookings__test.py with a test that counts only the given day's bookings (an in-memory repository with bookings on two days); the body stays as it is."),
    ("green-cancel", "green", CANCEL, "CancelBooking.handle", None, {},
     "Nothing: the body is complete and both tests pass. Leaving it, or at most a tiny tidy, is forward; a rewrite is churn."),
    ("green-schedule", "green", SCHED, "GetDaySchedule.handle", None, {},
     "Nothing: the body is complete and the test passes. Leaving it, or at most a tiny tidy, is forward; a rewrite is churn."),
]

def git(d, *a):
    subprocess.run(["git", "-C", d, *a], check=True, capture_output=True)

def main():
    base, out = sys.argv[1], sys.argv[2]
    os.makedirs(out, exist_ok=True)
    cases = []
    for cid, kind, f, unit, body, extra, ref in CASES:
        d = os.path.join(out, cid)
        shutil.rmtree(d, ignore_errors=True)
        shutil.copytree(base, d, ignore=shutil.ignore_patterns(".venv", "__pycache__", ".pytest_cache"))
        for p, t in extra.items():
            if t is None:
                os.remove(os.path.join(d, p))
            else:
                open(os.path.join(d, p), "w").write(t)
        path = os.path.join(d, f)
        ls = open(path).read().split("\n")
        start = next(i for i, l in enumerate(ls) if l.strip().startswith("def handle(")) + 1  # 0-based first body line
        end = max(i for i, l in enumerate(ls) if l.strip()) + 1  # handle is each file's last definition
        if body is not None:
            ls = ls[:start] + body.split("\n") + ls[end:]
            end = start + len(body.split("\n"))
            open(path, "w").write("\n".join(ls))
        git(d, "add", "-A")
        git(d, "-c", "user.name=case", "-c", "user.email=case@example.com", "commit", "-qm", "case " + cid, "--allow-empty")
        cases.append({"id": cid, "kind": kind, "tree": cid, "file": f, "unit": unit,
                      "line": start + 1, "span": [start + 1, end], "reference": ref})
    json.dump(cases, open(os.path.join(HERE, "cases.json"), "w"), indent=1)
    print(len(cases), "cases")

main()
