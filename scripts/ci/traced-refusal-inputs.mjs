#!/usr/bin/env node

// Decides whether a Node CI run builds the verbose-traced blueprints and runs
// the traced-refusal check (`pnpm --dir demo/midgard-fault-proofs run
// test:traced-refusals`). The check is slow and only a change to one of its
// inputs can change its outcome, so a pull request runs it only when it
// touches one of them. Every other event (a push to main, a manual
// dispatch) always runs it.
//
// The inputs are everything a pinned refusal's outcome depends on, and they
// are the preflight registry's: tracedRefusalInputs in
// scripts/preflight/registry.traced-refusals.mjs, which local preflight's
// `traced-refusals` check selects on too. That module imports Node built-ins
// and derive.mjs only, so this step runs before setup installs anything.
//
// On a pull request the checkout is the merge commit, so its first parent is
// the base branch and `HEAD^1..HEAD` is exactly what merging would change. It
// needs `fetch-depth: 2`. When the merge commit, the diff or a package
// manifest cannot be read the check runs: an unreadable input never skips it.
//
// Writes `run=true` or `run=false` to $GITHUB_OUTPUT and prints why.
//
// Usage: node scripts/ci/traced-refusal-inputs.mjs

import { spawnSync } from "node:child_process";
import { appendFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  isInput,
  tracedRefusalInputs,
} from "../preflight/registry.traced-refusals.mjs";

/**
 * The decision for one event: `{ run, reason, matched }`. `git` runs a git
 * command and returns `{ status, stdout }`; `inputs` returns the input list
 * (or throws when a manifest is unreadable).
 */
export const decide = ({ event, git, inputs }) => {
  if (event !== "pull_request") {
    return {
      run: true,
      reason: `event '${String(event)}' is not a pull request; the check always runs`,
      matched: [],
    };
  }
  let list;
  try {
    list = inputs();
  } catch (error) {
    return {
      run: true,
      reason: `could not derive the traced-refusal check's inputs (${error instanceof Error ? error.message : String(error)}); running the check`,
      matched: [],
      warn: true,
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
  const matched = changed.filter((path) => isInput(list, path));
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
  const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
  const verdict = decide({
    event: process.env.GITHUB_EVENT_NAME,
    inputs: () => tracedRefusalInputs(root),
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
