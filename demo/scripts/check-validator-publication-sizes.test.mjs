// Runs the publication size gate as CI does, on blueprints written here, with
// the limit it reads from the built midgard-core.
import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { existsSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";

const GATE = fileURLToPath(
  new URL("./check-validator-publication-sizes.mjs", import.meta.url),
);
const CORE_CONSENSUS_PROFILE = fileURLToPath(
  new URL("../midgard-core/dist/consensus-profile.js", import.meta.url),
);

assert.ok(
  existsSync(CORE_CONSENSUS_PROFILE),
  `the built midgard-core is required: ${CORE_CONSENSUS_PROFILE}; run \`pnpm --dir demo/midgard-core run build\``,
);
const { MIDGARD_CONSENSUS_LIMITS } = await import(
  pathToFileURL(CORE_CONSENSUS_PROFILE).href
);
const LIMIT = MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

// A validator whose compiled code is `bytes` long; `fill` keeps scripts of the
// same size distinct.
const validator = (title, bytes, fill = "4d") => ({
  title,
  compiledCode: fill.repeat(bytes),
});

const runGate = (validators) => {
  const directory = mkdtempSync(join(tmpdir(), "publication-size-gate-"));
  try {
    const blueprint = join(directory, "plutus.json");
    writeFileSync(blueprint, JSON.stringify({ validators }));
    return spawnSync(process.execPath, [GATE, blueprint], { encoding: "utf8" });
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

test("a validator one byte over the limit fails the gate, named with its size", () => {
  const run = runGate([
    validator("small.spend", 100),
    validator("big.withdraw", LIMIT + 1, "5a"),
    validator("big.else", LIMIT + 1, "5a"),
    validator("edge.mint", LIMIT, "6b"),
  ]);
  assert.equal(run.status, 1, run.stdout + run.stderr);
  assert.ok(
    run.stderr.includes(
      `big.withdraw, big.else: ${String(LIMIT + 1)} bytes, over the ${String(LIMIT)}-byte limit by 1`,
    ),
    run.stderr,
  );
  assert.ok(!run.stderr.includes("small.spend"), run.stderr);
  assert.ok(!run.stderr.includes("edge.mint"), run.stderr);
});

test("validators at and under the limit pass the gate", () => {
  const run = runGate([
    validator("edge.mint", LIMIT),
    validator("small.spend", 100, "6b"),
  ]);
  assert.equal(run.status, 0, run.stdout + run.stderr);
  assert.ok(
    run.stdout.includes(`edge.mint: ${String(LIMIT)} bytes, headroom 0`),
    run.stdout,
  );
});

test("a blueprint with no validators fails the gate", () => {
  const run = runGate([]);
  assert.equal(run.status, 1, run.stdout + run.stderr);
  assert.ok(run.stderr.includes("the blueprint has no validators"), run.stderr);
});
