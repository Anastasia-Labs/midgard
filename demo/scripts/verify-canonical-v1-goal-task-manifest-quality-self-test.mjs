#!/usr/bin/env node

// Negative self-test for the F05 manifest-quality gate's blocked-on
// reconciliation. The gate is tested by behavior, not by reading its source:
// each hostile mutation is written to a throwaway copy of the manifest, the
// real gate is invoked against that copy through its `--manifest-under-test=`
// hook, and the run must exit non-zero with the expected diagnostic category.
// A clean copy is run first, so a gate that rejected everything (or nothing)
// could not pass this suite.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  cpSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(scriptDir, "../..");
const gatePath = resolve(
  scriptDir,
  "verify-canonical-v1-goal-task-manifest-quality.mjs",
);
const manifestPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json",
);
const templatesPath = resolve(repoRoot, "docs/exec-plans/templates");
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const clone = (value) => structuredClone(value);
const rowOf = (candidate, id) => {
  const row = candidate.tasks.find((task) => task.id === id);
  assert.ok(row !== undefined, `${id} must exist in the manifest under test`);
  return row;
};

const workspace = mkdtempSync(resolve(tmpdir(), "f05-quality-self-test-"));
const candidateManifestPath = resolve(workspace, "manifest.json");
const candidateTemplatesPath = resolve(workspace, "templates");
cpSync(templatesPath, candidateTemplatesPath, { recursive: true });

const runGate = (candidate) => {
  writeFileSync(
    candidateManifestPath,
    `${JSON.stringify(candidate, null, 2)}\n`,
  );
  const result = spawnSync(
    process.execPath,
    [
      gatePath,
      "--json",
      `--manifest-under-test=${candidateManifestPath}`,
      `--templates-under-test=${candidateTemplatesPath}`,
    ],
    { encoding: "utf8" },
  );
  assert.ok(
    result.status !== null,
    `gate did not run: ${result.error?.message ?? "unknown failure"}`,
  );
  return { status: result.status, report: JSON.parse(result.stdout) };
};

// Positive control: the manifest as published must be accepted.
const control = runGate(clone(manifest));
assert.equal(
  control.status,
  0,
  `published manifest must pass: ${JSON.stringify(control.report.summary)}`,
);
assert.equal(control.report.summary.defects, 0);

let rejectedMutations = 0;
const mustReject = (label, category, mutate) => {
  const candidate = clone(manifest);
  mutate(candidate);
  const { status, report } = runGate(candidate);
  assert.notEqual(status, 0, `${label}: gate exited 0 on a seeded defect`);
  assert.ok(
    Array.isArray(report.findings[category]) &&
      report.findings[category].length > 0,
    `${label}: expected category ${category}, got ${JSON.stringify(report.summary.categories)}`,
  );
  process.stdout.write(
    `rejected: ${label} -> exit ${status}, ${category}: ${report.findings[category][0].id} | ${report.findings[category][0].detail}\n`,
  );
  rejectedMutations += 1;
};

// The exact defect V-7 found: an understated manifest-wide blocked-on claim.
mustReject(
  "F05 claims 1 current non-PASS dependency while C26 is PARTIAL",
  "blockedOnClaimMismatch",
  (candidate) => {
    const f05 = rowOf(candidate, "F05");
    f05.expectedNonzeroCounts = f05.expectedNonzeroCounts.replace(
      /blockedOn contains exactly \d+ current non-PASS dependenc(?:y|ies)[^.]*/,
      "blockedOn contains exactly 1 current non-PASS dependency F41 with 0 stale PASS blockers",
    );
  },
);

// Right cardinality, wrong identities: counting alone must not satisfy it.
mustReject(
  "F05 claims the right count but names the wrong dependencies",
  "blockedOnClaimMismatch",
  (candidate) => {
    const f05 = rowOf(candidate, "F05");
    f05.expectedNonzeroCounts = f05.expectedNonzeroCounts.replace(
      /blockedOn contains exactly \d+ current non-PASS dependenc(?:y|ies)[^.]*/,
      "blockedOn contains exactly 2 current non-PASS dependencies C21, C29 with 0 stale PASS blockers",
    );
  },
);

// Deleting the claim must not disable the rule that checks it.
mustReject(
  "the reconciled claim is removed from the manifest entirely",
  "missingBlockedOnReconciliationClaim",
  (candidate) => {
    const f05 = rowOf(candidate, "F05");
    f05.expectedNonzeroCounts = f05.expectedNonzeroCounts.replace(
      /and blockedOn contains exactly \d+ current non-PASS dependenc(?:y|ies)[^.]*/,
      "and 0 stale PASS blockers are present",
    );
  },
);

// blockedOn contents themselves must stay complete against the queue.
mustReject(
  "C30 drops PARTIAL dependency C26 from blockedOn",
  "omittedCurrentBlocker",
  (candidate) => {
    const c30 = rowOf(candidate, "C30");
    c30.blockedOn = c30.blockedOn.filter((dependency) => dependency !== "C26");
  },
);

// The prose form of the same understatement.
mustReject(
  "C30 prose omits C26 from its only-current-blockers sentence",
  "blockedOnlyClaimMismatch",
  (candidate) => {
    const c30 = rowOf(candidate, "C30");
    c30.blockedBecause = c30.blockedBecause.replace(
      "C25, C26, C29",
      "C25, C29",
    );
  },
);

// Decorated statuses must be classified by base role: a naive `=== "PASS"`
// comparison would let `PASS (LOCAL_PASS; Q57/QG3 owns LIVE)` through.
mustReject(
  "a decorated-PASS row (Q44) is listed as a current blocker",
  "staleBlockedOnPass",
  (candidate) => {
    const f05 = rowOf(candidate, "F05");
    f05.blockedOn = [...f05.blockedOn, "Q44"];
  },
);

// Audit scope must come from the repository, never from the artifact under
// audit (V-6, issue #530). The gate derives its expected task set from the
// authoritative Goal specification, so emptying or trimming the manifest may
// not shrink the audit: it must fail closed on cardinality and on every task
// identity the specification requires.
mustReject(
  "the manifest publishes no tasks at all",
  "taskCardinality",
  (candidate) => {
    candidate.tasks = [];
  },
);

mustReject(
  "the manifest drops a single authoritative task",
  "missingAuthoritativeTaskId",
  (candidate) => {
    candidate.tasks = candidate.tasks.filter((task) => task.id !== "C30");
  },
);

rmSync(workspace, { recursive: true, force: true });
process.stdout.write(
  `goal:tasks:quality:self-test: PASS\ncontrol runs accepted: 1; hostile mutations rejected: ${rejectedMutations}\n`,
);
