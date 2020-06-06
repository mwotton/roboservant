#!/bin/env bash
#
# Remember to link me to .git/hooks/pre-commit

set -euo pipefail

files=$((git diff --cached --name-only --diff-filter=ACMR | grep -Ei "\.hs$") || true)
if [ ! -z "${files}" ]; then
    echo "$files"
    echo "$files" | xargs ormolu --mode inplace
    git add $(echo "$files" | paste -s -d " " -)
fi
