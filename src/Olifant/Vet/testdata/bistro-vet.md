# Bistro conventions

Bistro is ports and adapters: `booking/commands` and `booking/queries` hold what the app does,
`booking/models` the data, `booking/ports/<port>/` each port's Protocol with its in-memory and
SQLite adapters, `web/<area>/` the routes, and `main.py` wires it together. A **must** rule is
one we hold every change to; a **taste** rule is how we like it, and nobody needs to stop work
over it.

## Rules

### Commands and queries go through ports (must)

A command or query reaches stored data only through a port's Protocol. `sqlite3` is imported in
an adapter (`ports/<port>/sqlite_<port>.py`) and nowhere else.

### The web goes through commands and queries (must)

Code under `web/` calls commands and queries. It never imports a port, an adapter or `sqlite3`.

### Only main.py chooses adapters (must)

Commands, queries and the web depend on a port's Protocol and are handed the adapter. Only
`main.py` (and tests, for their fakes) imports an in-memory or SQLite adapter.

### Booking rules live in commands (must)

Whether something is allowed, refused or chosen is decided in a command's `handle()`. Routes
parse the form and render the result, adapters store and fetch, and models hold data and
compute facts about themselves without enforcing policy.

### Expected failures are returned (must)

Anything a guest can cause (a taken slot, an unknown booking) is a frozen dataclass with a
`reason`, a member of the command's failure union, and is returned from `handle()`. We do not
raise for these, in a command, a model or a route.

### Statuses come from failures.py (must)

Routes turn a command's failure into an HTTP status by calling its function in
`web/<area>/failures.py`, an exhaustive `match` over the union. A route does not pick a failure
status itself.

### Queries only read (must)

A query returns data and changes nothing: no saves, no clean-up on the side. If reading needs
a write, it is a command.

### Time comes from the clock port (must)

Commands, queries and models never call `date.today()`, `datetime.now()` or `time.time()`. The
current time comes from a `Clock` port (add one the first time you need it), so tests can fix
the date.

### Repository methods do one thing (must)

A repository method is one read or one write. When a change needs a read and then a write (move
a booking, cancel one), the command makes both calls; the adapter does not combine them.

### Booking rules are tested at the command (must)

Each booking rule has a test in the command's `__test.py` that runs it against the in-memory
adapter. Route tests check statuses and pages; they are not where a rule is proven.

### A module says what it is (taste)

Every command, query, port, adapter and web module opens with a one-line docstring saying
what it is for. Models and tests can do without.

### main.py wires by keyword (taste)

In `main.py`, commands and queries are built with keyword arguments, one per line, so a new
dependency is a one-line change.

### Failure unions, one member per line (taste)

A command's failure union lists one member per line in parentheses, so adding a failure is a
one-line diff.
