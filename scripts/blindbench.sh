#!/usr/bin/env bash
set -euo pipefail

hp_mode=0
if [[ ${1:-} == "--hp2ps" ]]; then
  hp_mode=1
  shift
fi

marker=$(mktemp)
trap 'rm -f "$marker"' EXIT
: >"$marker"

common=(cabal test roboservant-test --enable-tests --enable-profiling --test-show-details=streaming)
if [[ $hp_mode -eq 1 ]]; then
  cmd=(${common[@]} --test-option=+RTS --test-option=-hc --test-option=-RTS "$@")
else
  cmd=(${common[@]} --test-option=+RTS --test-option=-p --test-option=-RTS "$@")
fi

"${cmd[@]}" >&2

latest_file() {
  local pattern=$1
  python - "$marker" "$pattern" <<'PY'
import sys
from pathlib import Path
marker = Path(sys.argv[1])
pattern = sys.argv[2]
now_threshold = marker.stat().st_mtime
candidates = []
roots = [
    (Path('.'), False),
    (Path('dist-newstyle'), True),
]
for root, recursive in roots:
    if not root.exists():
        continue
    iterator = root.rglob(pattern) if recursive else root.glob(pattern)
    for path in iterator:
        if not path.is_file():
            continue
        mtime = path.stat().st_mtime
        if mtime > now_threshold:
            candidates.append((mtime, path.resolve()))
if not candidates:
    sys.exit(1)
latest = max(candidates, key=lambda item: item[0])[1]
print(latest)
PY
}

if [[ $hp_mode -eq 1 ]]; then
  hp_file=$(latest_file "*.hp")
  ps_file="${hp_file%.hp}.ps"
  hp2ps "$hp_file" >/dev/null
  ps_file="${hp_file%.hp}.ps"
  printf '%s\n' "$ps_file"
else
  prof_file=$(latest_file "*.prof")
  printf '%s\n' "$prof_file"
fi
