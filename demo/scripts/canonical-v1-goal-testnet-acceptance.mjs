#!/usr/bin/env node

import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const requiredEnvironment = [
  "KUPO_URL",
  "OGMIOS_URL",
  "USER_SEED_PHRASE",
  "DA_L1_SUBMITTER_KEY_SOURCE",
  "POSTGRES_HOST",
  "POSTGRES_PORT",
];

if (process.env.MIDGARD_GOAL_ACCEPT_TESTNET !== "YES") {
  throw new Error(
    "state-changing acceptance requires MIDGARD_GOAL_ACCEPT_TESTNET=YES",
  );
}
if (process.env.NETWORK !== "preprod") {
  throw new Error("canonical V1 Goal acceptance targets preprod only");
}
if (process.env.L1_PROVIDER !== "Kupmios") {
  throw new Error("canonical V1 Goal acceptance requires L1_PROVIDER=Kupmios");
}
if ((process.env.L1_PROVIDER_FAILOVER ?? "").length > 0) {
  throw new Error("L1_PROVIDER_FAILOVER must be absent for exact acceptance");
}
const missing = requiredEnvironment.filter(
  (name) => (process.env[name] ?? "").length === 0,
);
if (missing.length > 0) {
  throw new Error(
    `missing required Preprod acceptance environment names: ${missing.join(", ")}`,
  );
}

const runbook = spawnSync(
  process.execPath,
  [
    resolve(
      repoRoot,
      ".agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs",
    ),
  ],
  {
    cwd: repoRoot,
    env: process.env,
    stdio: "inherit",
  },
);
if (runbook.status !== 0) {
  throw new Error("E2E acceptance runbook validation failed");
}

const implementation = resolve(
  repoRoot,
  "demo/midgard-node/scripts/canonical-v1-goal-testnet-acceptance.mjs",
);
if (!existsSync(implementation)) {
  throw new Error(
    "C80-C87/Q57/QG3/W45-W46/WG2 testnet orchestrator is not implemented; refusing to substitute a narrower live flow",
  );
}
const result = spawnSync(process.execPath, [implementation], {
  cwd: resolve(repoRoot, "demo/midgard-node"),
  env: process.env,
  stdio: "inherit",
});
if (result.status !== 0) {
  process.exit(typeof result.status === "number" ? result.status : 1);
}
