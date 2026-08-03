#!/usr/bin/env node

// GOAL_SPEC §13.2 minimum direct toolchain verification:
//
//   # The normative technical specification (§1 item 8) must build from the
//   # repository root. Skip it when technical-spec/ is unchanged in the diff.
//   make spec
//
// The verification plan may only carry unconditional commands, so the
// condition lives here: this wrapper decides whether `technical-spec/` changed
// and either runs `make spec` from the repository root or records an explicit
// skip. Both outcomes are exit 0 only when they are honest — a `make spec`
// failure propagates its exit code, and an undecidable diff base degrades to
// the worktree comparison it can actually make rather than passing silently.

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const SPEC_PATH = "technical-spec";

const git = (...args) =>
  spawnSync("git", args, {
    cwd: repoRoot,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  });

const gitLines = (...args) => {
  const result = git(...args);
  if (result.status !== 0) {
    return null;
  }
  return result.stdout.split("\n").filter(Boolean);
};

const resolveDiffBase = () => {
  const declared = process.env.MIDGARD_GOAL_SPEC_DIFF_BASE;
  const candidates = declared === undefined ? [] : [declared];
  candidates.push("origin/main", "main");
  for (const candidate of candidates) {
    const mergeBase = git("merge-base", candidate, "HEAD");
    if (mergeBase.status === 0) {
      return { ref: candidate, commit: mergeBase.stdout.trim() };
    }
  }
  return null;
};

const base = resolveDiffBase();
const committedChanges =
  base === null
    ? null
    : gitLines("diff", "--name-only", `${base.commit}..HEAD`, "--", SPEC_PATH);
const worktreeChanges = gitLines(
  "status",
  "--porcelain=v1",
  "--",
  SPEC_PATH,
)?.map((line) => line.slice(3));

if (
  worktreeChanges === undefined ||
  (base !== null && committedChanges === null)
) {
  process.stderr.write(
    "unable to inspect the technical-spec diff; refusing to claim a skip\n",
  );
  process.exit(1);
}

const changed = [...new Set([...(committedChanges ?? []), ...worktreeChanges])]
  .sort()
  .slice(0, 20);

if (changed.length === 0) {
  process.stdout.write(
    `${JSON.stringify({
      command: ["make", "spec"],
      status: "SKIPPED",
      reason: `${SPEC_PATH}/ is unchanged in the diff`,
      diffBase: base === null ? "worktree-only" : base.ref,
    })}\n`,
  );
  process.exit(0);
}

process.stdout.write(
  `${JSON.stringify({
    command: ["make", "spec"],
    status: "REQUIRED",
    diffBase: base === null ? "worktree-only" : base.ref,
    changed,
  })}\n`,
);
const build = spawnSync("make", ["spec"], {
  cwd: repoRoot,
  env: process.env,
  stdio: "inherit",
});
process.exit(
  typeof build.status === "number"
    ? build.status
    : typeof build.signal === "string"
      ? 128
      : 1,
);
