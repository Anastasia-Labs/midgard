#!/bin/sh
# Refuses a missing or mismatched native owner binary before bootstrap starts
# the devnet or spends anything on L1. write-acceptance-env.sh repeats the same
# check (same resolver) when it pins the binary into acceptance.env.
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command node
require_run_dir
node_env="$MIDGARD_PHASE4_RUN_DIR/secrets/node.env"
[ -f "$node_env" ] || die "node env is missing"
(
  cd "$node_root"
  node --input-type=module - "$node_env" "$script_dir/native-owner.mjs" "$node_root" <<'NODE'
import { readFileSync } from "node:fs";
import { pathToFileURL } from "node:url";
import dotenv from "dotenv";

const [nodePath, resolverPath, nodeRoot] = process.argv.slice(2);
const { resolveNativeOwnerBinary } = await import(pathToFileURL(resolverPath).href);
const owner = resolveNativeOwnerBinary(dotenv.parse(readFileSync(nodePath, "utf8")), nodeRoot);
process.stdout.write(`nativeOwnerBinary=${owner.path}\n`);
NODE
)
