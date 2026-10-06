#!/usr/bin/env bash
# The python-htmx slice from an empty folder (north-star project 2): what a
# person does to build table bookings, as olifant calls plus the recorded
# typing in packs/python-htmx/slice/*.diff. Stages, each after the last:
#
#   slice-python.sh DIR gens       init, then generators (and the small
#                                  edits they read: fields, ports): pyright
#                                  clean, only the stubs red
#   slice-python.sh DIR behaviour  the tests and handle bodies, but
#                                  cancel_booking's: its two tests red
#   slice-python.sh DIR fill       handle-body writes cancel_booking.handle
#                                  (a recorded qwen3-coder:30b fill, verified
#                                  by its tests): all green
#   slice-python.sh DIR serve      uvicorn serves /bookings (200)
#
# OLIFANT picks the binary (default: olifant). No model: holes stay
# placeholders (--no-fill); 03-holes and the behaviour patch write what a
# model fills (in the style qwen3-coder:30b filled the pack's cases).
set -euo pipefail
DIR=$1 STAGE=$2
OLIFANT=${OLIFANT:-olifant}
PACK=$(cd "$(dirname "$0")/../packs/python-htmx" && pwd)
APP="$DIR/bistro"
g() { (cd "$APP" && "$OLIFANT" gen "python-htmx/$1" --set app=bistro --set service=booking "${@:2}" --no-fill > /dev/null); }
typed() { (cd "$APP" && git apply "$PACK/slice/$1.diff" && git add -A && git commit -qm "typed: $1"); }
step() { (cd "$APP" && git add -A && git commit -qm "$1" > /dev/null); }

case $STAGE in
gens)
  (cd "$DIR" && "$OLIFANT" init --pack "$PACK" bistro > /dev/null)
  (cd "$APP" && "$OLIFANT" gen python-htmx/service --set app=bistro --set service=booking --no-fill > /dev/null)
  g model --set model=table
  g model --set model=booking
  step "service and models"
  typed 01-fields
  g repository --set entity=table
  g repository --set entity=booking
  g port --set port=id_generator
  step "repositories and a port"
  typed 02-next-id
  g port-method-adapters --set port=id_generator --set name=next_id --set "signature=next_id(self) -> str"
  step "next_id in the adapters"
  typed 03-holes
  g command --set command=create_booking
  g command --set command=cancel_booking
  g query --set query=get_day_schedule
  step "commands and a query"
  typed 04-ports
  w() { g wire --set kind="$1" --set command="$2" --set param="$3" --set port="$4"; }
  w commands create_booking bookings BookingRepository
  w commands create_booking tables TableRepository
  w commands create_booking ids IdGenerator
  w commands cancel_booking bookings BookingRepository
  w queries get_day_schedule tables TableRepository
  w queries get_day_schedule bookings BookingRepository
  step "wired"
  g web-resource --set resource=bookings --set query=get_day_schedule --set commands=create_booking,cancel_booking
  step "the bookings resource"
  typed 05-remove
  g port-method-adapters --set port=booking_repository --set name=remove --set "signature=remove(self, id: str) -> None"
  step "remove in the adapters"
  ;;
behaviour)
  typed 06-behaviour
  ;;
fill)
  F=src/bistro/booking/commands/cancel_booking.py
  LINE=$(cd "$APP" && grep -n 'raise NotImplementedError' "$F" | cut -d: -f1)
  (cd "$APP" && "$OLIFANT" gen python-htmx/handle-body --at "$F:$LINE:9" --set app=bistro --set service=booking --set kind=commands --set command=cancel_booking --fills "$PACK/slice/fills/cancel_booking.toml" > /dev/null)
  step "handle-body: cancel_booking"
  ;;
serve)
  PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1])')
  (cd "$APP" && uv run --quiet uvicorn --factory --app-dir src bistro.main:create --port "$PORT" > /dev/null 2>&1) &
  SERVER=$!
  stop() { pkill -f "bistro.main:create --port $PORT" 2>/dev/null || true; kill "$SERVER" 2>/dev/null || true; wait "$SERVER" 2>/dev/null || true; }
  trap stop EXIT
  for _ in $(seq 1 50); do
    if code=$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$PORT/bookings?day=2026-10-09"); then
      [ "$code" = 200 ] && exit 0
      echo "GET /bookings: $code" >&2; exit 1
    fi
    sleep 0.2
  done
  echo "uvicorn did not start" >&2; exit 1
  ;;
*) echo "usage: slice-python.sh DIR gens|behaviour|fill|serve" >&2; exit 2 ;;
esac
