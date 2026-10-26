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
  *SHIM:PROSE*) echo '{"result":"Sure! Here is what I found.","session_id":"shim-prose"}'; exit 0 ;;
  *SHIM:ERRORJSON*) printf '{"type":"result","is_error":true,"result":"the runner reported an error","session_id":"shim-%s"}\n' "$$"; exit 0 ;;
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
denials='[]'
case "$prompt" in
  *SHIM:DENY*) denials='[{"tool_name":"Bash","tool_use_id":"t1","tool_input":{"command":"python -m pytest t.py"}},{"tool_name":"Bash","tool_use_id":"t2","tool_input":{"command":"python -m pytest t.py"}}]' ;;
esac
case " $* " in
  *" --output-format json "*)
    printf '{"type":"result","result":"%s","duration_ms":1200,"num_turns":1,"session_id":"shim-%s","total_cost_usd":0.0123,' "$answer" "$$"
    printf '"usage":{"input_tokens":2,"output_tokens":40,"cache_creation_input_tokens":1000,"cache_read_input_tokens":5000},'
    printf '"modelUsage":{"claude-sonnet-5-5":{"inputTokens":2,"outputTokens":40,"cacheCreationInputTokens":1000,"cacheReadInputTokens":5000,"costUSD":0.0123}},'
    printf '"permission_denials":%s}\n' "$denials" ;;
  *) printf '%b\n' "$answer" ;;
esac
