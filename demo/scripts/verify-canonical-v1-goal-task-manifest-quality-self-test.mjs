#!/usr/bin/env node

// Negative self-test for the F05 manifest-quality gate's blocked-on
// reconciliation and for its focused-citation resolution rules (issue #534).
// The gate is tested by behavior, not by reading its source:
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

// The exact defect V-7 found: a stale manifest-wide blocked-on claim. The
// seed reinstates the pre-promotion wording (C26 went PASS in the 2026-08-06
// owner round), which the derivation must now reject.
mustReject(
  "F05 claims C26 is still a current non-PASS dependency after its promotion",
  "blockedOnClaimMismatch",
  (candidate) => {
    const f05 = rowOf(candidate, "F05");
    f05.expectedNonzeroCounts = f05.expectedNonzeroCounts.replace(
      /blockedOn contains exactly \d+ current non-PASS dependenc(?:y|ies)[^.]*/,
      "blockedOn contains exactly 2 current non-PASS dependencies C26, F41 with 0 stale PASS blockers",
    );
  },
);

// Right cardinality, wrong identities: counting alone must not satisfy it.
mustReject(
  "F05 claims the right count but names the wrong dependency",
  "blockedOnClaimMismatch",
  (candidate) => {
    const f05 = rowOf(candidate, "F05");
    f05.expectedNonzeroCounts = f05.expectedNonzeroCounts.replace(
      /blockedOn contains exactly \d+ current non-PASS dependenc(?:y|ies)[^.]*/,
      "blockedOn contains exactly 1 current non-PASS dependency C21 with 0 stale PASS blockers",
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

// blockedOn contents themselves must stay complete against the queue. F41 is
// the queue-recorded IN_PROGRESS dependency (post-C26-promotion, the only
// current non-PASS blocker with a first-queue row).
mustReject(
  "F05 drops queue-recorded non-PASS dependency F41 from blockedOn",
  "omittedCurrentBlocker",
  (candidate) => {
    const f05 = rowOf(candidate, "F05");
    f05.blockedOn = f05.blockedOn.filter((dependency) => dependency !== "F41");
  },
);

// The prose form of the same understatement.
mustReject(
  "F05 prose omits F41 from its only-current-blockers sentence",
  "blockedOnlyClaimMismatch",
  (candidate) => {
    const f05 = rowOf(candidate, "F05");
    f05.blockedBecause = f05.blockedBecause.replace(
      "non-PASS queue rows: F41",
      "non-PASS queue rows: Q02",
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

// Issue #534. Focused citations are evidence only when the module they name
// exists. The exact defect: C61's citation lost its `.test` suffix, so it
// named nothing, collected nothing, and still read as a green focused gate.
mustReject(
  "an executable focused command cites a module absent from the tree",
  "unresolvableFocusedModuleCitation",
  (candidate) => {
    const c61 = rowOf(candidate, "C61");
    c61.focusedCommands = c61.focusedCommands.map((command) =>
      command.replace(
        "midgard/transaction_root_v1_golden.test",
        "midgard/transaction_root_v1_golden",
      ),
    );
  },
);

// The planned/executable split must be one-way: a prescription for an unbuilt
// module may never also be published as an executable binding.
mustReject(
  "a planned citation is republished as an executable focused command",
  "plannedFocusedCommandCountedAsEvidence",
  (candidate) => {
    const q42 = rowOf(candidate, "Q42");
    q42.focusedCommands = [
      ...q42.focusedCommands,
      q42.plannedFocusedCommands[0],
    ];
  },
);

// Once the row is built, the prescription must be promoted rather than left
// parked where no gate executes it.
mustReject(
  "a planned citation names a module that now exists in the tree",
  "plannedFocusedCommandModuleExists",
  (candidate) => {
    const q42 = rowOf(candidate, "Q42");
    q42.plannedFocusedCommands = q42.plannedFocusedCommands.map((command) =>
      command.replace(
        "fraud_proofs/cross_block_duplicate_event/step_01",
        "midgard/transaction_root_v1_golden.test",
      ),
    );
  },
);

// A planned citation is still a lease claim: a row may only prescribe an
// unbuilt module it actually owns in writablePaths.
mustReject(
  "a planned citation names a module the row does not own",
  "plannedFocusedCommandOutsideLease",
  (candidate) => {
    const q42 = rowOf(candidate, "Q42");
    q42.plannedFocusedCommands = q42.plannedFocusedCommands.map((command) =>
      command.replace(
        "fraud_proofs/cross_block_duplicate_event/step_01",
        "fraud_proofs/invented_family/step_09",
      ),
    );
  },
);

// The convention only protects readers who are told about it.
mustReject(
  "the manifest carries planned citations without documenting the convention",
  "undocumentedPlannedFocusedCommandConvention",
  (candidate) => {
    candidate.note = candidate.note.replaceAll(
      "plannedFocusedCommands",
      "focused prescriptions",
    );
  },
);

rmSync(workspace, { recursive: true, force: true });
process.stdout.write(
  `goal:tasks:quality:self-test: PASS\ncontrol runs accepted: 1; hostile mutations rejected: ${rejectedMutations}\n`,
);
