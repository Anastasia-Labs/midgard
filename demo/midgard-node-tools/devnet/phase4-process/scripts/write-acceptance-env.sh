#!/bin/sh
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command node
require_run_dir
wallet_env="$MIDGARD_PHASE4_RUN_DIR/secrets/wallets.env"
node_env="$MIDGARD_PHASE4_RUN_DIR/secrets/node.env"
run_env="$MIDGARD_PHASE4_RUN_DIR/run.env"
manifest="$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/contract-deployment-info.json"
repo_root=$(CDPATH= cd -- "$phase4_root/../../../.." && pwd)
blueprint="$repo_root/onchain/aiken/plutus.json"
output="$MIDGARD_PHASE4_RUN_DIR/secrets/acceptance.env"
[ -f "$wallet_env" ] || die "wallet env is missing"
[ -f "$node_env" ] || die "node env is missing"
[ -f "$run_env" ] || die "run env is missing"
[ -f "$manifest" ] || die "deployment manifest is missing"
[ -f "$blueprint" ] || die "Aiken testnet blueprint is missing"
[ ! -e "$output" ] || die "refusing to overwrite acceptance env"
(
  cd "$node_root"
  node --input-type=module - "$node_env" "$wallet_env" "$run_env" "$output" "$manifest" "$blueprint" "$node_root" "$script_dir/native-owner.mjs" <<'NODE'
import { readFileSync, writeFileSync } from "node:fs";
import { pathToFileURL } from "node:url";
import dotenv from "dotenv";

const [nodePath, walletPath, runPath, outputPath, manifestPath, blueprintPath, nodeRoot, resolverPath] = process.argv.slice(2);
const { resolveNativeOwnerBinary } = await import(pathToFileURL(resolverPath).href);
const nodeValues = dotenv.parse(readFileSync(nodePath, "utf8"));
const walletValues = dotenv.parse(readFileSync(walletPath, "utf8"));
const runValues = dotenv.parse(readFileSync(runPath, "utf8"));
for (const [source, entries] of [[nodePath, nodeValues], [walletPath, walletValues], [runPath, runValues]]) {
  const forbidden = Object.keys(entries).filter((key) => key.startsWith("MIDGARD_PHASE4_PROCESS_"));
  if (forbidden.length > 0) throw new Error(`Phase 4 acceptance authorization keys are forbidden in ${source}: ${forbidden.join(",")}`);
}
const requiredRun = (name) => {
  const value = runValues[name]?.trim();
  if (!value) throw new Error(`run.env is missing ${name}`);
  return value;
};
const runDir = requiredRun("MIDGARD_PHASE4_RUN_DIR");
const values = {
  ...nodeValues,
  ...walletValues,
  ...runValues,
  NETWORK: "Custom",
  L1_PROVIDER: "Kupmios",
  L1_OGMIOS_KEY: `http://127.0.0.1:${requiredRun("MIDGARD_PHASE4_OGMIOS_PORT")}`,
  L1_KUPO_KEY: `http://127.0.0.1:${requiredRun("MIDGARD_PHASE4_KUPO_PORT")}`,
  POSTGRES_HOST: "127.0.0.1",
  POSTGRES_PORT: requiredRun("MIDGARD_PHASE4_POSTGRES_PORT"),
  POSTGRES_USER: requiredRun("MIDGARD_PHASE4_POSTGRES_USER"),
  POSTGRES_PASSWORD: requiredRun("MIDGARD_PHASE4_POSTGRES_PASSWORD"),
  POSTGRES_DB: requiredRun("MIDGARD_PHASE4_POSTGRES_DATABASE"),
  MIN_FEE_A: "0",
  MIN_FEE_B: "0",
  RUN_GENESIS_ON_STARTUP: "false",
  MIDGARD_DOTENV_MODE: "disabled",
  MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
  MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: manifestPath,
  MIDGARD_REAL_BLUEPRINT_PATH: blueprintPath,
  MIDGARD_PHASE4_SNAPSHOT_IDENTITY_PATH: `${runDir}/snapshots/matched-v1/snapshot-identity.json`,
};
for (const label of ["A", "B"]) {
  const name = `TESTNET_GENESIS_WALLET_SEED_PHRASE_${label}`;
  if (!values[name]?.trim()) throw new Error(`Phase 4 acceptance env is missing ${name}`);
}
// Wallet C is not used by the process gate, but NodeConfig requires a complete
// genesis-wallet tuple. Keep this compatibility alias isolated and hash-bound
// inside acceptance.env; never fall through to the checkout .env.
if (!values.TESTNET_GENESIS_WALLET_SEED_PHRASE_C?.trim()) {
  values.TESTNET_GENESIS_WALLET_SEED_PHRASE_C = values.TESTNET_GENESIS_WALLET_SEED_PHRASE_A;
}
// Every node runs the Architecture G native owner and refuses to start unless
// the owner binary is pinned by path and SHA-256 (see native-owner.mjs).
const owner = resolveNativeOwnerBinary(nodeValues, nodeRoot);
values.MPF_NATIVE_OWNER_BINARY_PATH = owner.path;
values.MPF_NATIVE_OWNER_BINARY_SHA256 = owner.sha256;
// The sidecar is bound to its node's ledger store. The process gate gives each
// node its own LEDGER_MPF_DB_PATH, so each must derive its own sidecar from it
// (<LEDGER_MPF_DB_PATH>.architecture-g.sidecar) instead of sharing one path.
delete values.MPF_NATIVE_OWNER_SIDECAR_PATH;
const lines = Object.entries(values)
  .sort(([left], [right]) => left.localeCompare(right))
  .map(([key, value]) => `${key}=${JSON.stringify(value)}`);
writeFileSync(outputPath, `${lines.join("\n")}\n`, { encoding: "utf8", mode: 0o600, flag: "wx" });
NODE
)
chmod 600 "$output"
printf '%s\n' "acceptanceEnv=$output"
