#!/usr/bin/env node

// The body of every workflow's summary gate job. GitHub reports a job whose
// dependencies failed as "skipped", and a skipped required check counts as
// passing, so a gate that merely `needs:` its jobs reports green exactly when
// something upstream broke. The gate job therefore runs with
// `if: ${{ !cancelled() }}` and hands its `needs` context to this script, which
// fails unless every dependency's result is on an explicit allowlist.
//
// Usage (in the gate job):
//   env:
//     NEEDS: ${{ toJSON(needs) }}
//   run: node scripts/ci/check-needs-results.mjs --allow success
//
// Fails by default: a missing, empty or unparseable NEEDS, a missing --allow,
// or any result outside the allowlist is exit 1. `lint-workflows.mjs` checks
// every gate job calls it this way.

import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const allowableResults = new Set(["success", "skipped"]);

export const judgeNeeds = (needsJson, allowed) => {
  const failures = [];
  if (allowed.length === 0) {
    return { ok: false, lines: ["no --allow list given; refusing to pass"] };
  }
  for (const result of allowed) {
    if (!allowableResults.has(result)) {
      failures.push(
        `--allow ${result} is not an acceptable job result; only ${[...allowableResults].join(", ")} may be allowed`,
      );
    }
  }
  let needs;
  try {
    needs = JSON.parse(needsJson ?? "");
  } catch {
    return {
      ok: false,
      lines: [...failures, "NEEDS is missing or not JSON; refusing to pass"],
    };
  }
  if (needs === null || typeof needs !== "object" || Array.isArray(needs)) {
    return {
      ok: false,
      lines: [...failures, "NEEDS is not an object; refusing to pass"],
    };
  }
  const entries = Object.entries(needs);
  if (entries.length === 0) {
    failures.push("NEEDS names no jobs; a gate over nothing cannot pass");
  }
  const lines = [];
  for (const [job, value] of entries) {
    const result = value?.result;
    const accepted = typeof result === "string" && allowed.includes(result);
    lines.push(`${accepted ? "ok  " : "FAIL"} ${job}: ${String(result)}`);
    if (!accepted) failures.push(`${job} finished '${String(result)}'`);
  }
  return { ok: failures.length === 0, lines: [...lines, ...failures] };
};

const parseAllow = (argv) => {
  const index = argv.indexOf("--allow");
  if (index === -1 || argv[index + 1] === undefined) return [];
  return argv[index + 1]
    .split(",")
    .map((value) => value.trim())
    .filter((value) => value.length > 0);
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  const verdict = judgeNeeds(process.env.NEEDS, parseAllow(process.argv));
  for (const line of verdict.lines) console.log(line);
  if (!verdict.ok) {
    console.error("summary gate: FAILED");
    process.exitCode = 1;
  } else console.log("summary gate: every job passed");
}
