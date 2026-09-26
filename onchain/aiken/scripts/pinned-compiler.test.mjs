import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import { pinnedAikenVersion } from "./pinned-compiler.mjs";

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
