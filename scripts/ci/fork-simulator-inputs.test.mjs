import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  decide,
  forkSimulatorInputs,
  isInput,
  repositoryInputs,
} from "./fork-simulator-inputs.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const script = join(root, "scripts/ci/fork-simulator-inputs.mjs");

const fakeGit =
  ({ parents = "m b h", changed = [], diffStatus = 0 } = {}) =>
  (args) =>
    args[0] === "rev-list"
      ? { status: parents === null ? 128 : 0, stdout: `${parents ?? ""}\n` }
      : {
          status: diffStatus,
          stdout: changed.map((path) => `${path}\n`).join(""),
        };

const real = () => repositoryInputs().inputs;

test("every non-pull-request event runs the suites without asking git", () => {
  for (const event of ["push", "workflow_dispatch", "merge_group", undefined]) {
    const verdict = decide({
      event,
      inputs: real,
      git: () => assert.fail("git must not be consulted"),
    });
    assert.equal(verdict.run, true, String(event));
  }
});

test("the follower is a runner, and its workspace dependencies are inputs", () => {
  const { runners, inputs } = repositoryInputs();
  assert.ok(runners.includes("@al-ft/midgard-l1-follower"), runners.join());
  for (const path of [
    "demo/midgard-l1-follower/src/testing/episodes.ts",
    "demo/midgard-l1-follower/tests/fork-sim/soak.test.ts",
    "demo/l1-node-transport/testing/fake-sidecar.mjs",
    "demo/midgard-test-support/vitest.js",
    "demo/pnpm-lock.yaml",
    ".github/workflows/midgard-node-ci.yml",
    "scripts/ci/fork-simulator-inputs.mjs",
  ])
    assert.equal(isInput(inputs, path), true, path);
});

test("a pull request touching only non-inputs skips the suites", () => {
  const verdict = decide({
    event: "pull_request",
    inputs: real,
    git: fakeGit({
      changed: [
        "onchain/aiken/validators/state_queue.ak",
        "demo/midgard-fault-proofs/src/index.ts",
        "demo/midgard-sdk/src/index.ts",
        "docs/agents/contracts.md",
        "scripts/ci/lint-workflows.mjs",
      ],
    }),
  });
  assert.equal(verdict.run, false, verdict.reason);
  assert.deepEqual(verdict.matched, []);
});

test("a pull request touching an input runs the suites", () => {
  const verdict = decide({
    event: "pull_request",
    inputs: real,
    git: fakeGit({
      changed: [
        "docs/agents/contracts.md",
        "demo/midgard-l1-follower/src/store/rewind.ts",
      ],
    }),
  });
  assert.equal(verdict.run, true);
  assert.deepEqual(verdict.matched, [
    "demo/midgard-l1-follower/src/store/rewind.ts",
  ]);
});

const pkg = (name, scripts = {}, workspaceDependencies = []) => ({
  directory: `demo/${name}`,
  name: `@al-ft/${name}`,
  scripts,
  workspaceDependencies: workspaceDependencies.map((dep) => `@al-ft/${dep}`),
});
const forkSim = { "test:fork-sim": "vitest run tests/fork-sim" };

// A role package that adds cases (C1, W1, N1) declares the script; it and
// everything it depends on become inputs without editing this script.
test("a package that declares test:fork-sim brings in its dependency closure", () => {
  const { runners, inputs } = forkSimulatorInputs([
    pkg("midgard-node", forkSim, ["midgard-sdk"]),
    pkg("midgard-sdk", {}, ["midgard-core"]),
    pkg("midgard-core"),
    pkg("midgard-watcher", {}, ["midgard-core"]),
    pkg("midgard-l1-follower", forkSim),
  ]);
  assert.deepEqual(runners, [
    "@al-ft/midgard-l1-follower",
    "@al-ft/midgard-node",
  ]);
  for (const path of [
    "demo/midgard-node/src/x.ts",
    "demo/midgard-sdk/src/x.ts",
    "demo/midgard-core/src/x.ts",
  ])
    assert.equal(isInput(inputs, path), true, path);
  assert.equal(isInput(inputs, "demo/midgard-watcher/src/x.ts"), false);
});

test("a dangling workspace dependency is refused", () => {
  assert.throws(
    () => forkSimulatorInputs([pkg("a", forkSim, ["b"])]),
    /not a workspace package/u,
  );
});

test("an unreadable manifest, merge commit or diff runs the suites", () => {
  const verdicts = [
    decide({
      event: "pull_request",
      inputs: () => {
        throw new Error("ENOENT");
      },
      git: fakeGit(),
    }),
    ...[
      fakeGit({ parents: null }),
      fakeGit({ parents: "h b" }),
      fakeGit({ diffStatus: 128 }),
    ].map((git) => decide({ event: "pull_request", inputs: real, git })),
  ];
  for (const verdict of verdicts) {
    assert.equal(verdict.run, true, verdict.reason);
    assert.equal(verdict.warn, true);
  }
});

test("a file prefix is not a directory input", () => {
  const inputs = real();
  assert.equal(isInput(inputs, "demo/midgard-l1-followerx/a.ts"), false);
  assert.equal(isInput(inputs, "demo/package.json.bak"), false);
});

test("the CLI writes its decision to GITHUB_OUTPUT", () => {
  const directory = mkdtempSync(join(tmpdir(), "fork-sim-inputs-"));
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

test("Node CI gates the fork simulator step on the decision", () => {
  const workflow = readFileSync(
    join(root, ".github/workflows/midgard-node-ci.yml"),
    "utf8",
  );
  assert.match(
    workflow,
    /- name: Decide whether the fork simulator runs\n\s+id: forksim\n\s+run: node scripts\/ci\/fork-simulator-inputs\.mjs/u,
  );
  assert.match(
    workflow,
    /- name: Run the fork simulator and shadow-diff suites\n\s+if: \$\{\{ steps\.forksim\.outputs\.run == 'true' \}\}\n\s+run: pnpm --dir demo -r --if-present run test:fork-sim/u,
  );
});
