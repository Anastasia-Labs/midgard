#!/bin/sh
# Run `docker compose` for the operator stack with this checkout's project
# name and host ports; see operator-compose.mjs. The `--` keeps node from
# reading a compose `--env-file` argument as its own option.
set -eu
exec node "$(dirname -- "$0")/operator-compose.mjs" -- "$@"
