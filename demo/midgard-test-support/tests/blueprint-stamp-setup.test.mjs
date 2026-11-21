import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";

import {
  blueprintHash,
  blueprintSourceHash,
  buildRecordPath,
} from "../../scripts/lib/blueprint-stamp.mjs";
import {
  blueprintStampDecision,
  rebuildBlueprint,
} from "../blueprint-stamp-setup.js";

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
        ...overrides,
      }),
    );
  const named = write("build/named/plutus.json", '{"validators":[]}\n');
  return { root, write, record, named };
};

test("a fresh default blueprint lets the run start", (t) => {
  const { root, write, record } = withTree(t);
  record(write("onchain/aiken/plutus.json", "{}\n"));
  assert.deepEqual(blueprintStampDecision({ root, env: {} }), {
    action: "run",
  });
});

test("a missing, unrecorded or stale default blueprint is rebuilt, saying why", (t) => {
  const { root, write, record } = withTree(t);
  const decide = () => blueprintStampDecision({ root, env: {} });
  assert.equal(decide().action, "rebuild");
  assert.match(decide().reason, /no blueprint at/u);
  write("onchain/aiken/plutus.json", "{}\n");
  assert.equal(decide().action, "rebuild");
  assert.match(decide().reason, /no build record/u);
  record(join(root, "onchain/aiken/plutus.json"));
  write("onchain/aiken/validators/probe.ak", "changed\n");
  assert.equal(decide().action, "rebuild");
  assert.match(decide().reason, /sources .* changed/u);
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

/** A stand-in for `deployment:build`: writes a fresh blueprint and record. */
const builder = ({ write, record }, { leaveStale = false } = {}) => {
  const calls = [];
  const deploymentBuild = async (root, profile) => {
    calls.push(profile);
    const blueprint = write("onchain/aiken/plutus.json", '{"built":true}\n');
    record(blueprint);
    if (leaveStale) write("onchain/aiken/plutus.json", '{"edited":true}\n');
    return { exitCode: 0, logPath: join(root, "build.log") };
  };
  return { calls, deploymentBuild };
};

/** An `ensure` that always builds, after `gate` (if any) resolves. */
const buildingEnsure =
  (deploymentBuild, gate = Promise.resolve()) =>
  async (root, { build }) => {
    await gate;
    const built = await build(deploymentBuild, root, "probe-profile", {});
    return { action: "built", profile: "probe-profile", ...built };
  };

const quiet = () => {};
const staleTree = (t) => {
  const tree = withTree(t);
  tree.record(tree.write("onchain/aiken/plutus.json", "{}\n"));
  tree.write("onchain/aiken/validators/probe.ak", "changed\n");
  return tree;
};

test("concurrent runs against a stale blueprint build it once; the others wait and read it", async (t) => {
  const tree = staleTree(t);
  const { calls, deploymentBuild } = builder(tree);
  let release;
  const gate = new Promise((resolve) => (release = resolve));
  const ensure = buildingEnsure(deploymentBuild, gate);
  const options = {
    root: tree.root,
    reason: "stale",
    ensure,
    compiler: () => "aiken",
    log: quiet,
  };
  const runs = [rebuildBlueprint(options), rebuildBlueprint(options)];
  release();
  await Promise.all(runs);
  assert.deepEqual(calls, ["probe-profile"]);
  assert.equal(
    blueprintStampDecision({ root: tree.root, env: {} }).action,
    "run",
  );
});

test("a build without the pinned compiler is refused with the reason, and the next run can still build", async (t) => {
  const tree = staleTree(t);
  const { calls, deploymentBuild } = builder(tree);
  const options = {
    root: tree.root,
    reason: "the sources changed",
    ensure: buildingEnsure(deploymentBuild),
    log: quiet,
  };
  await assert.rejects(
    rebuildBlueprint({
      ...options,
      compiler: () => {
        throw new Error("nix: command not found");
      },
    }),
    /the sources changed\nIt cannot be rebuilt: the pinned compiler is unavailable \(nix: command not found\)/u,
  );
  assert.deepEqual(calls, []);
  await rebuildBlueprint({ ...options, compiler: () => "aiken" });
  assert.deepEqual(calls, ["probe-profile"]);
});

test("a blueprint copied from another checkout needs no compiler", async (t) => {
  const tree = staleTree(t);
  const { deploymentBuild } = builder(tree);
  await rebuildBlueprint({
    root: tree.root,
    reason: "stale",
    ensure: async (root) => {
      await deploymentBuild(root, "copied");
      return { action: "copied", from: "/other/checkout" };
    },
    compiler: () => {
      throw new Error("no compiler");
    },
    log: quiet,
  });
  assert.equal(
    blueprintStampDecision({ root: tree.root, env: {} }).action,
    "run",
  );
});

test("a rebuild that leaves the blueprint stale fails the run", async (t) => {
  const tree = staleTree(t);
  const { deploymentBuild } = builder(tree, { leaveStale: true });
  await assert.rejects(
    rebuildBlueprint({
      root: tree.root,
      reason: "stale",
      ensure: buildingEnsure(deploymentBuild),
      compiler: () => "aiken",
      log: quiet,
    }),
    /the rebuild left a stale blueprint/u,
  );
});

test("a lock left by a run that exited is broken, and a live holder is waited on until its deadline", async (t) => {
  const tree = staleTree(t);
  const lock = join(tree.root, "onchain/aiken/build/default-blueprint.lock");
  const holdLock = (pid) => {
    rmSync(lock, { recursive: true, force: true });
    mkdirSync(lock, { recursive: true });
    writeFileSync(join(lock, "pid"), `${String(pid)}\n`);
  };
  const { calls, deploymentBuild } = builder(tree);
  const options = {
    root: tree.root,
    reason: "stale",
    ensure: buildingEnsure(deploymentBuild),
    compiler: () => "aiken",
    log: quiet,
    timeoutMs: 600,
  };
  holdLock(process.pid);
  await assert.rejects(
    rebuildBlueprint(options),
    new RegExp(`build lock .* held by PID ${String(process.pid)}`, "u"),
  );
  assert.deepEqual(calls, []);
  holdLock(spawnSync(process.execPath, ["-e", ""]).pid);
  await rebuildBlueprint(options);
  assert.deepEqual(calls, ["probe-profile"]);
});
