// The tooling self-tests that read files outside scripts/ declare those files
// (toolingTestInputs in registry.tooling-checks.mjs), and repo-tooling-tests
// is selected from the declarations: a change to a declared file runs the
// self-tests that read it, and only those. Each declared test is also run in a
// copy of the repository holding only scripts/ and the files it declares, so
// a read it has not declared fails here instead of in Repo Tools CI after a
// push the preflight passed.

import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  readlinkSync,
  rmSync,
  symlinkSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { loadYaml } from "../ci/lint-workflows.mjs";
import { matchesAny } from "./derive.mjs";
import { buildRegistry, selectChecks } from "./registry.mjs";

// A hook running these tests exports GIT_DIR and GIT_INDEX_FILE. Inherited
// by the `git init` below, they re-initialise the outer repository instead of
// the copy.
for (const key of Object.keys(process.env))
  if (key.startsWith("GIT_")) delete process.env[key];

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const registry = buildRegistry(root);
const ID = "repo-tooling-tests";
const WHOLE_SUITE = ["node", "--test", "scripts/**/*.test.mjs"];
const tooling = registry.checks.find((check) => check.id === ID);
const inputs = tooling.testInputs ?? {};

// The working tree's files, tracked or new, as preflight would see them.
const files = spawnSync(
  "git",
  ["ls-files", "-z", "--cached", "--others", "--exclude-standard"],
  { cwd: root, encoding: "utf8", maxBuffer: 1 << 28 },
)
  .stdout.split("\0")
  .filter(
    (path) =>
      path !== "" && lstatSync(join(root, path), { throwIfNoEntry: false }),
  );

// What a change to `changed` runs of the self-tests: undefined when it does
// not select the step.
const runFor = (changed) => {
  const selection = selectChecks(registry, changed);
  const entry = selection.selected.find(({ check }) => check.id === ID);
  if (entry === undefined) return undefined;
  const steps = entry.check.plan({
    matched: entry.matched,
    full: selection.full,
  });
  assert.equal(steps.length, 1);
  return steps[0].argv;
};

test("adding a journey test file runs the wiring check that reads it, alone", () => {
  assert.deepEqual(
    runFor([
      "demo/midgard-node-tools/devnet/watcher-journeys/new-journey.test.ts",
    ]),
    ["node", "--test", "scripts/ci/node-tools-journey-tests-wired.test.mjs"],
  );
});

test("editing a workflow runs each wiring check that reads it and no other self-test", () => {
  const argv = runFor([".github/workflows/midgard-node-ci.yml"]);
  assert.ok(argv, "a Node CI workflow edit selects no self-test");
  assert.deepEqual(argv.slice(0, 2), ["node", "--test"]);
  for (const reader of [
    "scripts/ci/node-tools-journey-tests-wired.test.mjs",
    "scripts/ci/shard-counts.test.mjs",
    "scripts/ci/fork-simulator-inputs.test.mjs",
    "scripts/ci/lint-workflows.test.mjs",
  ])
    assert.ok(argv.includes(reader), reader);
  assert.ok(!argv.includes("scripts/**/*.test.mjs"));
  assert.ok(!argv.includes("scripts/contrib/campaign.test.mjs"));
});

test("a change under the step's own paths still runs every self-test", () => {
  for (const path of [
    "scripts/ci/node-tools-journey-tests-wired.test.mjs",
    "scripts/lib/aiken.mjs",
    ".githooks/pre-push",
    ".claude/settings.json",
  ])
    assert.deepEqual(runFor([path]), WHOLE_SUITE, path);
  // Alongside a declared file too: the narrower run would miss the edit.
  assert.deepEqual(
    runFor([".github/workflows/midgard-node-ci.yml", "scripts/ci/x.mjs"]),
    WHOLE_SUITE,
  );
  assert.deepEqual(
    tooling.plan({ matched: [], full: true }).map(({ argv }) => argv),
    [WHOLE_SUITE],
  );
});

test("every declared glob matches a file and selects the self-test that reads it", () => {
  assert.ok(
    Object.keys(inputs).length >= 10,
    `only ${String(Object.keys(inputs).length)} self-tests declare inputs`,
  );
  for (const [reader, globs] of Object.entries(inputs)) {
    assert.match(reader, /^scripts\/.+\.test\.mjs$/u);
    assert.ok(files.includes(reader), `${reader} does not exist`);
    for (const glob of globs) {
      const file = files.find((path) => matchesAny(path, [glob]));
      assert.ok(file, `${reader}: '${glob}' matches no file`);
      const argv = runFor([file]) ?? [];
      assert.ok(
        argv.includes(reader) || argv.includes(WHOLE_SUITE[2]),
        `${file} does not run ${reader}`,
      );
    }
  }
});

// A copy of the working tree holding scripts/ and the files `globs` match,
// with the workspace's installed dependencies linked in and its own index.
const copyWith = (globs) => {
  const copy = mkdtempSync(join(tmpdir(), "midgard-tooling-inputs-"));
  for (const path of files) {
    if (!path.startsWith("scripts/") && !matchesAny(path, globs)) continue;
    const source = join(root, path);
    const target = join(copy, path);
    mkdirSync(dirname(target), { recursive: true });
    if (lstatSync(source).isSymbolicLink())
      symlinkSync(readlinkSync(source), target);
    else copyFileSync(source, target);
  }
  const modules = join(root, "demo/node_modules");
  if (existsSync(modules)) {
    mkdirSync(join(copy, "demo"), { recursive: true });
    symlinkSync(modules, join(copy, "demo/node_modules"));
  }
  for (const args of [
    ["-c", "init.defaultBranch=main", "init", "--quiet"],
    ["add", "--all"],
  ]) {
    const git = spawnSync("git", args, { cwd: copy, encoding: "utf8" });
    assert.equal(git.status, 0, git.stderr);
  }
  return copy;
};

const runTest = (cwd, file) =>
  new Promise((done) => {
    const env = { ...process.env };
    // Set for this test's own process; inherited, it would make the nested
    // run report to this runner instead of printing its own summary.
    delete env.NODE_TEST_CONTEXT;
    const child = spawn(
      process.execPath,
      ["--test", "--test-reporter=tap", file],
      { cwd, env },
    );
    let output = "";
    child.stdout.on("data", (chunk) => (output += chunk));
    child.stderr.on("data", (chunk) => (output += chunk));
    child.on("close", (status) => done({ status, output }));
  });

const skip =
  loadYaml(root) === undefined && process.env.GITHUB_ACTIONS !== "true"
    ? "could not check: yaml absent (run `pnpm --dir demo install`)"
    : false;

test(
  "each declaring self-test passes with only scripts/ and the files it declares",
  { skip, concurrency: 4 },
  async (t) => {
    assert.ok(Object.keys(inputs).length > 0, "no self-test declares inputs");
    await Promise.all(
      Object.entries(inputs).map(([reader, globs]) =>
        t.test(reader, async () => {
          const copy = copyWith(globs);
          try {
            const { status, output } = await runTest(copy, reader);
            const failed = output.match(/^not ok .*$/gmu) ?? [];
            assert.equal(
              status,
              0,
              `${reader} fails without the files it does not declare (add them to its toolingTestInputs entry): ${failed.join("; ")}\n${output.slice(-4000)}`,
            );
            // A test that skips when a file is absent hides the read.
            assert.match(output, /^# skipped 0$/mu, `${reader} skipped a test`);
          } finally {
            rmSync(copy, { recursive: true, force: true });
          }
        }),
      ),
    );
  },
);
