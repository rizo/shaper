#!/usr/bin/env bash

set -euo pipefail

test -L ./dune.lock && exit 0

SYSTEM=$(uname -s | tr '[:upper:]' '[:lower:]')
ln -s "${SYSTEM}-dune.lock" ./dune.lock
ln -s "${SYSTEM}-dev-tools.locks" ./dev-tools.locks
