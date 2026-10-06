#!/usr/bin/env node

// Decides whether a Node CI run builds the verbose-traced blueprints and runs
// the traced-refusal check (`pnpm --dir demo/midgard-fault-proofs run
// test:traced-refusals`). The check is slow and only a change to one of its
// inputs can change its outcome, so a pull request runs it only when it
// touches one of the paths below. Every other event (a push to main, a manual
// dispatch) always runs it.
//
// The inputs are everything a pinned refusal's outcome depends on:
//   - the validators, their compiler and its install: onchain/, the
//     deployment profiles the blueprint is built under, the fork build
//     script, the workflow that pins the fork and the setup action that
//     installs it and restores the prebuilt blueprints;
//   - the check itself: the fault-proofs package, which holds the runner,
//     its plan of which cases run each pin, the traced-blueprint builder,
//     every `refusedBy` pin (in test files and support modules alike) and
//     the `expectOnchainRefusal` helper that checks them, and this file;
//   - what builds each pinned negative's transaction: the workspace packages
//     the test files that run a pin import (the plain run only shows that
//     *some* validator refused, so an off-chain change can move a refusal to
//     another check, and only this check notices), and the dependency pins,
//     patches and vendored evaluator they run on.
// Measured: the 26 test files that run a pin reach midgard-core,
// midgard-sdk, midgard-validation, lucid-midgard, midgard-node and
// midgard-test-support, and no other workspace package. A pinned test that
// starts importing another workspace package must add it here.
//
// On a pull request the checkout is the merge commit, so its first parent is
// the base branch and `HEAD^1..HEAD` is exactly what merging would change. It
// needs `fetch-depth: 2`. When the merge commit or the diff cannot be read
// the check runs: an unreadable diff never skips it.
//
// Writes `run=true` or `run=false` to $GITHUB_OUTPUT and prints why.
//
// Usage: node scripts/ci/traced-refusal-inputs.mjs

import { spawnSync } from "node:child_process";
import { appendFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

// Directory prefixes end in `/`; anything else names one file.
export const tracedRefusalInputs = [
  "onchain/",
  "config/deployments/",
  "scripts/ci/build-aiken-fork.sh",
  "scripts/ci/traced-refusal-inputs.mjs",
  ".github/workflows/midgard-node-ci.yml",
  ".github/actions/demo-setup/",
  "demo/midgard-fault-proofs/",
  "demo/midgard-core/",
  "demo/midgard-sdk/",
  "demo/midgard-validation/",
  "demo/lucid-midgard/",
  "demo/midgard-node/",
  "demo/midgard-test-support/",
  "demo/scripts/lib/",
  "demo/scripts/deployment-profiles.mjs",
  "demo/package.json",
  "demo/pnpm-lock.yaml",
  "demo/pnpm-workspace.yaml",
  "demo/.nvmrc",
  "demo/patches/",
  "demo/vendor/",
];

export const isTracedRefusalInput = (path) =>
  tracedRefusalInputs.some((input) =>
    input.endsWith("/") ? path.startsWith(input) : path === input,
  );

/**
 * The decision for one event: `{ run, reason, matched }`. `git` runs a git
 * command and returns `{ status, stdout }`.
 */
export const decide = ({ event, git }) => {
  if (event !== "pull_request") {
    return {
      run: true,
      reason: `event '${String(event)}' is not a pull request; the check always runs`,
      matched: [],
    };
  }
  const parents = git(["rev-list", "--parents", "--max-count=1", "HEAD"]);
  const commits = parents.status === 0 ? parents.stdout.trim().split(" ") : [];
  if (commits.length !== 3) {
    return {
      run: true,
      reason:
        "HEAD is not a readable two-parent merge commit (check out the pull request merge ref with fetch-depth: 2); running the check",
      matched: [],
      warn: true,
    };
  }
  const diff = git(["diff", "--name-only", "--no-renames", "HEAD^1", "HEAD"]);
  if (diff.status !== 0) {
    return {
      run: true,
      reason: "could not diff HEAD^1..HEAD; running the check",
      matched: [],
      warn: true,
    };
  }
  const changed = diff.stdout.split("\n").filter((line) => line.length > 0);
  const matched = changed.filter(isTracedRefusalInput);
  return matched.length > 0
    ? {
        run: true,
        reason: `${String(matched.length)} of ${String(changed.length)} changed files are inputs of the traced-refusal check`,
        matched,
      }
    : {
        run: false,
        reason: `none of the ${String(changed.length)} changed files is an input of the traced-refusal check; skipping it`,
        matched,
      };
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  if (process.argv.length > 2) {
    console.error("usage: traced-refusal-inputs.mjs");
    process.exit(2);
  }
  const verdict = decide({
    event: process.env.GITHUB_EVENT_NAME,
    git: (args) => {
      const result = spawnSync("git", args, { encoding: "utf8" });
      return { status: result.status, stdout: result.stdout ?? "" };
    },
  });
  console.log(verdict.warn ? `::warning::${verdict.reason}` : verdict.reason);
  for (const path of verdict.matched) console.log(`  ${path}`);
  const output = process.env.GITHUB_OUTPUT;
  if (output !== undefined && output !== "") {
    appendFileSync(output, `run=${String(verdict.run)}\n`);
  } else {
    console.log(`run=${String(verdict.run)}`);
  }
}
