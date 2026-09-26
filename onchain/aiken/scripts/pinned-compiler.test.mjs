import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { pinnedAikenFork, pinnedAikenVersion } from "./pinned-compiler.mjs";

const script = join(
  dirname(fileURLToPath(import.meta.url)),
  "pinned-compiler.mjs",
);

const withWorkflows = (aikenCi, nodeCi, callback) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-pinned-compiler-"));
  try {
    mkdirSync(join(root, ".github/workflows"), { recursive: true });
    writeFileSync(join(root, ".github/workflows/aiken-ci.yml"), aikenCi);
    writeFileSync(join(root, ".github/workflows/midgard-node-ci.yml"), nodeCi);
    return callback(root);
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
};

test("reads the pin the repository's workflows agree on", () => {
  assert.match(pinnedAikenVersion(), /^aiken v\d+\.\d+\.\d+\+[0-9a-f]{7}$/u);
  withWorkflows(
    "env:\n  AIKEN_FORK_VERSION: aiken v9.9.9+abcdef0\n",
    "env:\n  AIKEN_FORK_VERSION: aiken v9.9.9+abcdef0\n",
    (root) => assert.equal(pinnedAikenVersion(root), "aiken v9.9.9+abcdef0"),
  );
});

test("fails when the workflows pin different compilers", () => {
  withWorkflows(
    "env:\n  AIKEN_FORK_VERSION: aiken v9.9.9+abcdef0\n",
    "env:\n  AIKEN_FORK_VERSION: aiken v9.9.8+1234567\n",
    (root) =>
      assert.throws(
        () => pinnedAikenVersion(root),
        /pin different Aiken compilers/u,
      ),
  );
});

test("fails when a workflow declares no pin or more than one", () => {
  withWorkflows(
    "env: {}\n",
    "env:\n  AIKEN_FORK_VERSION: aiken v9.9.9+abcdef0\n",
    (root) => assert.throws(() => pinnedAikenVersion(root), /found 0/u),
  );
  withWorkflows(
    "env:\n  AIKEN_FORK_VERSION: a\n  AIKEN_FORK_VERSION: b\n",
    "env:\n  AIKEN_FORK_VERSION: a\n",
    (root) => assert.throws(() => pinnedAikenVersion(root), /found 2/u),
  );
});

const forkPin = ({
  repo = "https://example.invalid/aiken",
  tag = "midgard-abcdef0",
  rev = "abcdef0123456789abcdef0123456789abcdef01",
  version = "aiken v9.9.9+abcdef0",
} = {}) =>
  [
    "env:",
    `  AIKEN_FORK_REPO: ${repo}`,
    `  AIKEN_FORK_TAG: ${tag}`,
    `  AIKEN_FORK_REV: ${rev}`,
    `  AIKEN_FORK_VERSION: ${version}`,
    "",
  ].join("\n");

test("reads the fork's build coordinates the workflows agree on", () => {
  const fork = pinnedAikenFork();
  assert.match(fork.rev, /^[0-9a-f]{40}$/u);
  assert.equal(fork.version, pinnedAikenVersion());
  withWorkflows(forkPin(), forkPin(), (root) =>
    assert.deepEqual(pinnedAikenFork(root), {
      repo: "https://example.invalid/aiken",
      tag: "midgard-abcdef0",
      rev: "abcdef0123456789abcdef0123456789abcdef01",
      version: "aiken v9.9.9+abcdef0",
    }),
  );
});

test("refuses fork coordinates the workflows disagree on or that contradict each other", () => {
  withWorkflows(forkPin(), forkPin({ tag: "midgard-other" }), (root) =>
    assert.throws(() => pinnedAikenFork(root), /disagree on AIKEN_FORK_TAG/u),
  );
  const shortRev = forkPin({ rev: "abcdef0" });
  withWorkflows(shortRev, shortRev, (root) =>
    assert.throws(() => pinnedAikenFork(root), /full 40-hex commit/u),
  );
  const wrongVersion = forkPin({ version: "aiken v9.9.9+1234567" });
  withWorkflows(wrongVersion, wrongVersion, (root) =>
    assert.throws(() => pinnedAikenFork(root), /does not name AIKEN_FORK_REV/u),
  );
});

test("--print-pin prints the repository's fork coordinates as KEY=value lines", () => {
  const run = spawnSync(process.execPath, [script, "--print-pin"], {
    encoding: "utf8",
  });
  assert.equal(run.status, 0, run.stderr);
  const printed = Object.fromEntries(
    run.stdout
      .trim()
      .split("\n")
      .map((line) => [
        line.slice(0, line.indexOf("=")),
        line.slice(line.indexOf("=") + 1),
      ]),
  );
  const fork = pinnedAikenFork();
  assert.deepEqual(printed, {
    AIKEN_FORK_REPO: fork.repo,
    AIKEN_FORK_TAG: fork.tag,
    AIKEN_FORK_REV: fork.rev,
    AIKEN_FORK_VERSION: fork.version,
  });
});
