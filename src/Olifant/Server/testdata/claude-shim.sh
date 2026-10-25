#!/bin/sh
# Stands in for `claude` in tests. Reads the prompt on stdin; the marker in it picks the answer.
[ -n "$SHIM_ARGS" ] && printf '%s\n' "$*" >> "$SHIM_ARGS"
if [ -n "$CLAUDECODE" ]; then echo "nested" >&2; exit 3; fi
prompt=$(cat)
case "$prompt" in
  *SHIM:FAIL*) echo "no credentials" >&2; exit 2 ;;
  *SHIM:LOGIN*) echo "Please run /login"; exit 0 ;;
  *SHIM:PROSE*) echo '{"result":"Sure! Here is what I found."}'; exit 0 ;;
  *SHIM:SLEEP*) exec sleep 30 ;;
esac
case "$prompt" in
  *QUESTION:*) answer='CLAIM @src/bistro/booking/models/table.py:Table is the booking table.\\nCLAIM @src/bistro/booking/commands/close_table.py:CloseTable closes it.\\nCONCLUSION: close the table.' ;;
  *SECTION:*) answer='AFTER: @src/bistro/booking/models/table.py:Table\\nA placeholder section.' ;;
  *STEP:*) answer='DONE @src/bistro/booking/models/table.py:Table' ;;
  *TEST:*) answer='DONE' ;;
  *) answer='ok' ;;
esac
case " $* " in
  *" --output-format json "*) printf '{"type":"result","result":"%s","duration_ms":1200,"total_cost_usd":0.0123}\n' "$answer" ;;
  *) printf '%b\n' "$answer" ;;
esac
