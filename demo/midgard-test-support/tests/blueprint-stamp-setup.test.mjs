import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";

import {
  blueprintHash,
  blueprintSourceHash,
  buildRecordPath,
} from "../../scripts/lib/blueprint-stamp.mjs";
import { blueprintStampDecision } from "../blueprint-stamp-setup.js";

const pin = "aiken v1.1.23+5adf783";

// A tree with everything the stamp reads, plus a second blueprint outside
// onchain/aiken standing in for an explicitly named artifact.
const withTree = (t) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-stamp-setup-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const write = (path, contents) => {
    mkdirSync(dirname(join(root, path)), { recursive: true });
    writeFileSync(join(root, path), contents);
    return join(root, path);
  };
  for (const workflow of ["aiken-ci.yml", "midgard-node-ci.yml"])
    write(
      `.github/workflows/${workflow}`,
      `env:\n  AIKEN_FORK_VERSION: ${pin}\n`,
    );
  write("onchain/aiken/aiken.toml", 'name = "midgard/selftest"\n');
  write("onchain/aiken/aiken.lock", "# lock\n");
  write("onchain/aiken/validators/probe.ak", "validator probe { }\n");
  write("onchain/aiken/lib/midgard/probe.ak", "pub const x = 1\n");
  write("onchain/aiken/env/default.ak", "pub const network = 0\n");
  const record = (blueprintPath, overrides = {}) =>
    writeFileSync(
      buildRecordPath(blueprintPath),
      JSON.stringify({
        profile: { name: "preprod-emulator-testing" },
        blueprintHash: blueprintHash(blueprintPath),
        sourceHash: blueprintSourceHash(root),
        compiler: pin,
        ...overrides,
      }),
    );
  const named = write("build/named/plutus.json", '{"validators":[]}\n');
  return { root, write, record, named };
};

test("a fresh default blueprint, or none at all, lets the run start", (t) => {
  const { root, write, record } = withTree(t);
  assert.equal(blueprintStampDecision({ root, env: {} }).action, "run");
  record(write("onchain/aiken/plutus.json", "{}\n"));
  assert.equal(blueprintStampDecision({ root, env: {} }).action, "run");
});

test("a stale default blueprint refuses the run", (t) => {
  const { root, write, record } = withTree(t);
  record(write("onchain/aiken/plutus.json", "{}\n"));
  write("onchain/aiken/validators/probe.ak", "changed\n");
  const decision = blueprintStampDecision({ root, env: {} });
  assert.equal(decision.action, "refuse");
  assert.match(decision.message, /sources .* changed/u);
});

test("a named blueprint with a matching record runs, as the interactive emulator's does", (t) => {
  const { root, record, named } = withTree(t);
  record(named);
  const env = { MIDGARD_REAL_BLUEPRINT_PATH: named };
  assert.equal(blueprintStampDecision({ root, env }).action, "run");
});

test("a named blueprint that is mismatched refuses instead of warning", (t) => {
  for (const [why, mismatch] of [
    ["has no build record", () => {}],
    [
      "was built from other sources",
      ({ record, named }) => record(named, { sourceHash: "0".repeat(64) }),
    ],
    [
      "was built by another compiler",
      ({ record, named }) => record(named, { compiler: "aiken v1.1.22" }),
    ],
    [
      "was edited after its record",
      ({ record, named }) => {
        record(named);
        writeFileSync(named, '{"validators":[1]}\n');
      },
    ],
  ]) {
    const tree = withTree(t);
    mismatch(tree);
    const env = { MIDGARD_REAL_BLUEPRINT_PATH: tree.named };
    const decision = blueprintStampDecision({ root: tree.root, env });
    assert.equal(decision.action, "refuse", why);
    assert.match(decision.message, /MIDGARD_REAL_BLUEPRINT_PATH=/u, why);
  }
});

test("a named blueprint that does not exist refuses", (t) => {
  const { root } = withTree(t);
  const env = { MIDGARD_REAL_BLUEPRINT_PATH: join(root, "absent.json") };
  assert.equal(blueprintStampDecision({ root, env }).action, "refuse");
});

test("MIDGARD_BLUEPRINT_STAMP=warn is the one downgrade, for either blueprint", (t) => {
  const { root, write, record, named } = withTree(t);
  record(write("onchain/aiken/plutus.json", "{}\n"));
  write("onchain/aiken/validators/probe.ak", "changed\n");
  for (const env of [
    { MIDGARD_BLUEPRINT_STAMP: "warn" },
    { MIDGARD_BLUEPRINT_STAMP: "warn", MIDGARD_REAL_BLUEPRINT_PATH: named },
  ])
    assert.equal(blueprintStampDecision({ root, env }).action, "warn");
});
