import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  decide,
  isTracedRefusalInput,
  tracedRefusalInputs,
} from "./traced-refusal-inputs.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const script = join(root, "scripts/ci/traced-refusal-inputs.mjs");

/** A fake git: HEAD's parents line and the diff's changed files. */
const fakeGit =
  ({ parents = "m b h", changed = [], diffStatus = 0 } = {}) =>
  (args) =>
    args[0] === "rev-list"
      ? { status: parents === null ? 128 : 0, stdout: `${parents ?? ""}\n` }
      : {
          status: diffStatus,
          stdout: changed.map((path) => `${path}\n`).join(""),
        };

test("every non-pull-request event runs the check without asking git", () => {
  for (const event of ["push", "workflow_dispatch", "merge_group", undefined]) {
    const verdict = decide({
      event,
      git: () => assert.fail("git must not be consulted"),
    });
    assert.equal(verdict.run, true, String(event));
  }
});

test("a pull request touching only non-inputs skips the check", () => {
  const verdict = decide({
    event: "pull_request",
    git: fakeGit({
      changed: [
        "demo/midgard-watcher/src/index.ts",
        "demo/midgard-node-tools/src/index.ts",
        "docs/agents/contracts.md",
        "scripts/ci/lint-workflows.mjs",
        ".github/workflows/aiken-ci.yml",
        "demo/scripts/lint-by-package.mjs",
      ],
    }),
  });
  assert.equal(verdict.run, false);
  assert.deepEqual(verdict.matched, []);
});

test("a pull request touching any input runs the check", () => {
  for (const path of [
    "onchain/aiken/validators/state_queue.ak",
    "onchain/aiken/aiken.toml",
    "onchain/aiken/aiken.lock",
    "onchain/aiken/scripts/pinned-compiler.mjs",
    "config/deployments/preprod-testing.json",
    "scripts/ci/build-aiken-fork.sh",
    "scripts/ci/traced-refusal-inputs.mjs",
    ".github/workflows/midgard-node-ci.yml",
    ".github/actions/demo-setup/action.yml",
    "demo/midgard-fault-proofs/scripts/run-traced-refusals.mjs",
    "demo/midgard-fault-proofs/scripts/traced-blueprint.mjs",
    "demo/midgard-fault-proofs/tests/support/emulator/expect-onchain-refusal.ts",
    "demo/midgard-sdk/src/index.ts",
    "demo/midgard-node/src/index.ts",
    "demo/midgard-test-support/interactive-emulator.js",
    "demo/scripts/lib/blueprint-stamp.mjs",
    "demo/pnpm-lock.yaml",
    "demo/vendor/uplc-0.2.23-midgard.1.tgz",
  ]) {
    const verdict = decide({
      event: "pull_request",
      git: fakeGit({ changed: ["docs/agents/contracts.md", path] }),
    });
    assert.equal(verdict.run, true, path);
    assert.deepEqual(verdict.matched, [path]);
  }
});

test("a file prefix is not a directory input", () => {
  assert.equal(isTracedRefusalInput("demo/midgard-node-tools/src/x.ts"), false);
  assert.equal(isTracedRefusalInput("onchainx/a"), false);
  assert.equal(isTracedRefusalInput("demo/package.json.bak"), false);
});

test("an unreadable merge commit or diff runs the check", () => {
  for (const git of [
    fakeGit({ parents: null }),
    fakeGit({ parents: "h b" }),
    fakeGit({ diffStatus: 128, changed: [] }),
  ]) {
    const verdict = decide({ event: "pull_request", git });
    assert.equal(verdict.run, true);
    assert.equal(verdict.warn, true);
  }
});

// Every file that declares a pin, test file or support module, and the
// check's own scripts, must be an input, or editing a pin could skip the
// check that reads it.
test("every refusedBy pin and the check's scripts are inputs", () => {
  const tests = join(root, "demo/midgard-fault-proofs/tests");
  const pinned = readdirSync(tests, { recursive: true })
    .map(String)
    .filter((file) => /\.(?:ts|mts|js|mjs)$/u.test(file))
    .filter((file) =>
      /\brefusedBy:\s*"/u.test(readFileSync(join(tests, file), "utf8")),
    )
    .map((file) => `demo/midgard-fault-proofs/tests/${file}`);
  assert.ok(pinned.length > 0, "found no refusedBy pin");
  assert.ok(
    pinned.some((path) => !path.endsWith(".test.ts")),
    "found no refusedBy pin in a support module",
  );
  for (const path of [
    ...pinned,
    "demo/midgard-fault-proofs/scripts/run-traced-refusals.mjs",
    "demo/midgard-fault-proofs/scripts/traced-refusal-plan.mjs",
    "demo/midgard-fault-proofs/scripts/traced-blueprint.mjs",
    "demo/midgard-fault-proofs/package.json",
  ]) {
    assert.equal(isTracedRefusalInput(path), true, path);
  }
});

// The pinned tests run on every workspace package the fault-proofs package
// depends on at run time; each must be an input.
test("the fault-proofs runtime workspace dependencies are inputs", () => {
  const manifest = JSON.parse(
    readFileSync(join(root, "demo/midgard-fault-proofs/package.json"), "utf8"),
  );
  const workspace = Object.entries(manifest.dependencies ?? {})
    .filter(([, version]) => String(version).startsWith("workspace:"))
    .map(([name]) => name.replace(/^@al-ft\//u, ""));
  assert.ok(workspace.length > 0);
  for (const directory of [...workspace, "midgard-test-support"]) {
    assert.ok(
      tracedRefusalInputs.includes(`demo/${directory}/`),
      `demo/${directory}/ is not an input`,
    );
  }
});

test("the CLI writes its decision to GITHUB_OUTPUT", () => {
  const directory = mkdtempSync(join(tmpdir(), "traced-inputs-"));
  try {
    const output = join(directory, "output");
    const result = spawnSync(process.execPath, [script], {
      cwd: root,
      encoding: "utf8",
      env: {
        PATH: process.env.PATH,
        GITHUB_EVENT_NAME: "push",
        GITHUB_OUTPUT: output,
      },
    });
    assert.equal(result.status, 0, result.stderr);
    assert.equal(readFileSync(output, "utf8"), "run=true\n");
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});

// Both consumers must follow the decision: the traced blueprint builds and the
// traced-refusal step. A skipped step leaves its job successful, so the gate
// still requires every job to succeed.
test("Node CI gates the traced builds and the traced step on the decision", () => {
  const workflow = readFileSync(
    join(root, ".github/workflows/midgard-node-ci.yml"),
    "utf8",
  );
  const invocations = workflow.match(
    /run: node scripts\/ci\/traced-refusal-inputs\.mjs/gu,
  );
  assert.equal(invocations?.length, 2, "expected two decision steps");
  assert.match(
    workflow,
    /- name: Check pinned emulator refusals against traced validators\n\s+if: \$\{\{ !cancelled\(\) && steps\.traced\.outputs\.run == 'true' \}\}/u,
  );
  assert.match(
    workflow,
    /traced-blueprints: \$\{\{ steps\.traced\.outputs\.run == 'true' \}\}/u,
  );
});
