// Scripts that are executed by path must be executable in the index.
//
// With core.fileMode=false (the default on WSL and some Windows checkouts) git
// neither shows nor records a lost executable bit, so a script can be committed
// as 100644 and look fine locally. A runner then fails with "Permission denied"
// (exit 126), and git silently skips a hook. This reads the modes from the
// index, which is what a clone gets, not from the working tree.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

const indexModes = () => {
  const result = spawnSync("git", ["ls-files", "-s"], {
    cwd: root,
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  assert.equal(result.status, 0, result.stderr);
  const modes = new Map();
  for (const line of result.stdout.split("\n")) {
    const match = /^(\d{6}) [0-9a-f]+ \d\t(.+)$/u.exec(line);
    if (match !== null) {
      modes.set(match[2], match[1]);
    }
  }
  return modes;
};

// A repository script at command position in a workflow `run:` line or block
// line: the start of the line, optionally after `run:` and `./`. A script
// passed to an interpreter (`bash x.sh`, `node x.mjs`) is not matched.
const commandScript =
  /^\s*(?:-\s+)?(?:run:\s*)?(?:\.\/)?((?:scripts|demo|onchain|\.githooks)\/[\w./-]+\.sh)\b/u;

export const scriptsRunByPath = (workflowText) =>
  workflowText
    .split("\n")
    .map((line) => commandScript.exec(line)?.[1])
    .filter((path) => path !== undefined);

test("the matcher finds scripts at command position only", () => {
  assert.deepEqual(
    scriptsRunByPath(
      [
        "        run: scripts/ci/build-aiken-fork.sh --prefix x",
        "          ./scripts/start-test-postgres.sh",
        "          bash scripts/generate.sh",
        "        run: node scripts/ci/lint-workflows.mjs",
      ].join("\n"),
    ),
    ["scripts/ci/build-aiken-fork.sh", "scripts/start-test-postgres.sh"],
  );
});

test("every script a workflow runs by path is executable in the index", () => {
  const modes = indexModes();
  const workflows = join(root, ".github/workflows");
  const offenders = [];
  for (const name of readdirSync(workflows)) {
    if (!/\.ya?ml$/u.test(name)) {
      continue;
    }
    const text = readFileSync(join(workflows, name), "utf8");
    for (const path of scriptsRunByPath(text)) {
      if (modes.get(path) !== "100755") {
        offenders.push(`${name}: ${path} (${modes.get(path) ?? "untracked"})`);
      }
    }
  }
  assert.deepEqual(
    offenders,
    [],
    "fix with: git update-index --chmod=+x <path>",
  );
});

test("every tracked hook and the documented helper scripts are executable", () => {
  const modes = indexModes();
  const expected = [
    ...[...modes.keys()].filter(
      (path) =>
        path.startsWith(".githooks/") &&
        !/\.(md|txt)$/u.test(path) &&
        path !== ".githooks/install",
    ),
    "scripts/start-test-postgres.sh",
  ];
  const offenders = expected.filter((path) => modes.get(path) !== "100755");
  assert.deepEqual(
    offenders,
    [],
    "fix with: git update-index --chmod=+x <path>",
  );
});
