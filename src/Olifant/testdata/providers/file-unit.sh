# a test-units provider: one unit on line 1, named after the request's file
# ("none" without one); with an argument, appends a line to that file per run
input=$(cat)
if [ -n "$1" ]; then echo ran >> "$1"; fi
file=$(printf '%s' "$input" | sed -n 's/.*"file":"\([^"]*\)".*/\1/p')
printf '[{"name":"%s","line":1,"end":1}]' "${file:-none}"
