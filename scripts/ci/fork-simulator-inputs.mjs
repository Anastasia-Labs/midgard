#!/usr/bin/env node

// Decides whether a Node CI run runs the L1 fork simulator and shadow-diff
// suites (`pnpm --dir demo -r --if-present run test:fork-sim`). A pull
// request runs them only when it touches one of their inputs; every other
// event (a push to main, a manual dispatch) always runs them.
//
// The inputs are derived, not listed by hand, so a role package that plugs
// its projection or comparator into the simulator is covered the moment it
// declares the script:
//   - every workspace package whose package.json defines `test:fork-sim`
//     (today midgard-l1-follower; C1, W1 and N1 add theirs), and every
//     workspace package those depend on, transitively, through a
//     `workspace:` dependency or devDependency;
//   - the follower and the node transport whatever they declare (the
//     simulator drives the follower's store through the transport's frame
//     client and fake sidecar);
//   - what installs and pins them: the workspace manifest, lockfile, Node
//     version, patches and vendored packages, the demo setup action, this
//     workflow, this file and the workspace reader it shares with preflight.
//
// On a pull request the checkout is the merge commit, so its first parent is
// the base branch and `HEAD^1..HEAD` is exactly what merging would change. It
// needs `fetch-depth: 2`. When the merge commit, the diff or a package
// manifest cannot be read the suites run: an unreadable input never skips
// them.
//
// Writes `run=true` or `run=false` to $GITHUB_OUTPUT and prints why.
//
// Usage: node scripts/ci/fork-simulator-inputs.mjs

import { spawnSync } from "node:child_process";
import { appendFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  workspaceDependencyClosure,
  workspacePackages,
} from "../preflight/derive.mjs";

export const FORK_SIM_SCRIPT = "test:fork-sim";

// Directory prefixes end in `/`; anything else names one file.
export const fixedInputs = [
  "scripts/ci/fork-simulator-inputs.mjs",
  "scripts/preflight/derive.mjs",
  ".github/workflows/midgard-node-ci.yml",
  ".github/actions/demo-setup/",
  "demo/midgard-l1-follower/",
  "demo/l1-node-transport/",
  "demo/package.json",
  "demo/pnpm-lock.yaml",
  "demo/pnpm-workspace.yaml",
  "demo/.nvmrc",
  "demo/patches/",
  "demo/vendor/",
];

/**
 * Every input path, from the workspace packages (`workspacePackages` in
 * scripts/preflight/derive.mjs, which refuses a workspace it cannot read
 * literally). A runner's dependency that is not a workspace package throws.
 */
export const forkSimulatorInputs = (packages) => {
  const byName = new Map(packages.map((pkg) => [pkg.name, pkg]));
  const runners = packages
    .filter((pkg) => pkg.scripts[FORK_SIM_SCRIPT] !== undefined)
    .map((pkg) => pkg.name)
    .sort();
  const reached = workspaceDependencyClosure(packages, runners);
  const packageInputs = [...reached].sort().map((name) => {
    const pkg = byName.get(name);
    if (pkg === undefined)
      throw new Error(
        `workspace dependency ${name} is not a workspace package`,
      );
    return `${pkg.directory}/`;
  });
  return {
    runners,
    inputs: [...new Set([...fixedInputs, ...packageInputs])],
  };
};

export const isInput = (inputs, path) =>
  inputs.some((input) =>
    input.endsWith("/") ? path.startsWith(input) : path === input,
  );

/**
 * The decision for one event: `{ run, reason, matched }`. `git` runs a git
 * command and returns `{ status, stdout }`; `inputs` returns the input list
 * (or throws when a manifest is unreadable).
 */
export const decide = ({ event, git, inputs }) => {
  if (event !== "pull_request") {
    return {
      run: true,
      reason: `event '${String(event)}' is not a pull request; the fork simulator always runs`,
      matched: [],
    };
  }
  let list;
  try {
    list = inputs();
  } catch (error) {
    return {
      run: true,
      reason: `could not derive the fork simulator's inputs (${error instanceof Error ? error.message : String(error)}); running it`,
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
        "HEAD is not a readable two-parent merge commit (check out the pull request merge ref with fetch-depth: 2); running the fork simulator",
      matched: [],
      warn: true,
    };
  }
  const diff = git(["diff", "--name-only", "--no-renames", "HEAD^1", "HEAD"]);
  if (diff.status !== 0) {
    return {
      run: true,
      reason: "could not diff HEAD^1..HEAD; running the fork simulator",
      matched: [],
      warn: true,
    };
  }
  const changed = diff.stdout.split("\n").filter((line) => line.length > 0);
  const matched = changed.filter((path) => isInput(list, path));
  return matched.length > 0
    ? {
        run: true,
        reason: `${String(matched.length)} of ${String(changed.length)} changed files are inputs of the fork simulator`,
        matched,
      }
    : {
        run: false,
        reason: `none of the ${String(changed.length)} changed files is an input of the fork simulator; skipping it`,
        matched,
      };
};

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);

export const repositoryInputs = (root = repositoryRoot) =>
  forkSimulatorInputs(workspacePackages(root));

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  if (process.argv.length > 2) {
    console.error("usage: fork-simulator-inputs.mjs");
    process.exit(2);
  }
  const verdict = decide({
    event: process.env.GITHUB_EVENT_NAME,
    inputs: () => repositoryInputs().inputs,
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
