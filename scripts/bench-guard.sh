#!/usr/bin/env zsh
set -euo pipefail

ITERS="${ITERS:-1000}"
SLEEP_SECS="${SLEEP_SECS:-2}"
LOOP=false

if [[ "${1:-}" == "--loop" ]]; then
  LOOP=true
fi

targets=(
  "program/10k/eachm"
  "program/10k/eachm-aztecs"
  "program/10k+1/eachm"
  "program/10k+1/eachm-aztecs"
)

run_target() {
  local name="$1"

  echo "== ${name} =="
  # In some sandboxed environments Cabal returns non-zero after a successful
  # benchmark run because ~/.cabal/logs is read-only. Keep running so the
  # benchmark output remains usable.
  cabal bench pidgine-bench \
    --ghc-options=-O2 \
    --benchmark-options="-m glob ${name} --iters ${ITERS} +RTS -N -s -RTS" || true
}

run_once() {
  local name
  for name in "${targets[@]}"; do
    run_target "${name}"
  done
}

if [[ "${LOOP}" == "true" ]]; then
  while true; do
    echo "-- bench guard run @ $(date -Is)"
    run_once
    sleep "${SLEEP_SECS}"
  done
else
  run_once
fi
