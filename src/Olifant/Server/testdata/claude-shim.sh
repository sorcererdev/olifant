#!/bin/sh
# Stands in for `claude` in tests. Reads the prompt on stdin; the marker in it picks the answer.
# Answers are in the errand grammar (Olifant.Server.Errand), anchored in the
# python-htmx fixture app (packs/python-htmx/fixtures/app). A STEP edits the
# fixture's table.py in the cwd (the project root), and only when it is there.
[ -n "$SHIM_ARGS" ] && printf '%s\n' "$*" >> "$SHIM_ARGS"
if [ -n "$CLAUDECODE" ]; then echo "nested" >&2; exit 3; fi
prompt=$(cat)
case "$prompt" in
  *SHIM:FAIL*) echo "no credentials" >&2; exit 2 ;;
  *SHIM:LOGIN*) echo "Please run /login"; exit 0 ;;
  *SHIM:PROSE*) echo '{"result":"Sure! Here is what I found."}'; exit 0 ;;
  *SHIM:SLEEP*) exec sleep 30 ;;
  *SHIM:SLOW*) sleep 2 ;;
esac
table=src/bistro/booking/models/table.py
# answers hold \n (backslash, n): a newline once the JSON string is decoded, or once printf %b prints it
case "$prompt" in
  *QUESTION:*) answer="- the booking table is a frozen dataclass @$table:Table\n- the day's schedule is built here @src/bistro/booking/queries/get_day_schedule.py:GetDaySchedule.handle\n- an old claim @src/nope.py:Gone" ;;
  *SECTION:*|*CLAIM:*) answer="AFTER: - a claim @$table:Table\n  - ✓ it is a dataclass @$table:Table" ;;
  *STEP:*)
    [ -f "$table" ] && printf '    def label(self) -> str: return self.id\n' >> "$table"
    answer="DONE @$table:Table.label" ;;
  *TEST:*) answer='DONE' ;;
  *) answer='ok' ;;
esac
case " $* " in
  *" --output-format json "*) printf '{"type":"result","result":"%s","duration_ms":1200,"total_cost_usd":0.0123}\n' "$answer" ;;
  *) printf '%b\n' "$answer" ;;
esac
