# answers with its request; with an argument, appends a line to that file per run
input=$(cat)
if [ -n "$1" ]; then echo ran >> "$1"; fi
printf '%s' "$input"
