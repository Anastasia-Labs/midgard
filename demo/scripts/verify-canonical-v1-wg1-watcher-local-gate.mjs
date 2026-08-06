#!/usr/bin/env node
/**
 * WG1 — local watcher gate, plus the consolidated IG3 watcher security-gate
 * review.
 *
 * This verifier does NOT decide that the watcher is locally ready. It decides
 * that the published WG1 artifact tells the truth about what has and has not
 * been measured. That inversion is deliberate and is the fail-closed reading of
 * GOAL_SPEC §11 IG3 ("production readiness remains false while ...") and of the
 * WG1 row's own "0 waivers or inferred readiness" contract: a gate that can
 * only be green by claiming PASS would create pressure to claim PASS.
 *
 * Concretely it rejects:
 *   - any WG1 condition marked PASS while any predecessor it depends on is not
 *     measured PASS (readiness inferred from an unmeasured predecessor);
 *   - any waiver of any kind;
 *   - a gate status of PASS unless all five predecessors and all six conditions
 *     are independently measured PASS;
 *   - a W44 binding whose digests or counts do not reproduce from the tree.
 */

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");

const execGit = (args, encoding, quiet = false) =>
  execFileSync("git", args, {
    cwd: repositoryRoot,
    encoding,
    maxBuffer: 128 * 1024 * 1024,
    stdio: quiet ? ["ignore", "pipe", "ignore"] : ["ignore", "pipe", "inherit"],
  });

const readIndexedFile = (path, encoding = "buffer", quiet = false) =>
  execGit(["show", `:${path}`], encoding, quiet);

const fail = (message) => {
  throw new Error(`WG1 watcher local gate verification failed: ${message}`);
};

const GATE_PATH =
  "docs/exec-plans/evidence/canonical-v1-watcher-local-gate-v1.json";
const W44_PATH =
  "docs/exec-plans/evidence/testnet/w44-crash-rollback-matrix-v1.json";
const W44_TEST_PATH =
  "demo/midgard-watcher/tests/crash-rollback-matrix.test.ts";

/** The six WG1 conditions, in the exact GOAL_SPEC §10.5 WG1 order. */
const WG1_CONDITION_IDS = [
  "independent",
  "deterministic",
  "complete_for_every_enabled_family_locally",
  "restart_rollback_safe",
  "observable",
  "ready_for_final_deployment",
];

/** The four IG3 watcher security-gate clauses, in GOAL_SPEC §11 IG3 order. */
const IG3_CONDITION_IDS = [
  "no_enabled_feature_maps_to_unprovable_gap",
  "no_proof_input_requires_private_operator_state",
  "finality_and_rollback_are_defined",
  "maturity_budget_accommodates_the_complete_path",
];

const WG1_PREDECESSOR_IDS = ["W40", "W41", "W42", "W43", "W44"];

const STATUSES = new Set(["PASS", "BLOCKED"]);

const gate = JSON.parse(readIndexedFile(GATE_PATH, "utf8"));

if (
  gate.schemaVersion !== "midgard-canonical-v1-watcher-local-gate-v1" ||
  !Array.isArray(gate.goalIds) ||
  gate.goalIds.length !== 2 ||
  !gate.goalIds.includes("WG1") ||
  !gate.goalIds.includes("IG3")
) {
  fail("the gate artifact must be the WG1 + IG3 consolidated review");
}

if (!Array.isArray(gate.waivers) || gate.waivers.length !== 0) {
  fail("WG1 admits no waivers; the waivers array must exist and be empty");
}

/* ------------------------------------------------------------------ */
/* Predecessor inventory: exactly W40-W44, each measured.             */
/* ------------------------------------------------------------------ */

if (
  !Array.isArray(gate.predecessors) ||
  gate.predecessors.length !== WG1_PREDECESSOR_IDS.length
) {
  fail(
    `predecessor inventory must be exactly ${String(WG1_PREDECESSOR_IDS.length)} rows (W40-W44)`,
  );
}
const predecessorStatus = new Map();
gate.predecessors.forEach((row, index) => {
  const expectedId = WG1_PREDECESSOR_IDS[index];
  if (row?.id !== expectedId) {
    fail(`predecessor ${String(index)} must be ${expectedId}`);
  }
  if (!STATUSES.has(row.status)) {
    fail(`${expectedId} status must be measured as PASS or BLOCKED`);
  }
  if (typeof row.measurement !== "string" || row.measurement.length === 0) {
    fail(`${expectedId} must record how its status was measured`);
  }
  if (row.status !== "PASS") {
    if (
      !Array.isArray(row.missingSurfaces) ||
      row.missingSurfaces.length === 0
    ) {
      fail(`${expectedId} is not PASS and must name its missing surfaces`);
    }
    for (const surface of row.missingSurfaces) {
      let present = true;
      try {
        readIndexedFile(surface, "buffer", true);
      } catch {
        present = false;
      }
      if (present) {
        fail(
          `${expectedId} claims ${surface} is missing but it exists in the index`,
        );
      }
    }
  }
  predecessorStatus.set(expectedId, row.status);
});

const passingPredecessors = [...predecessorStatus.values()].filter(
  (status) => status === "PASS",
).length;

/* ------------------------------------------------------------------ */
/* WG1 conditions: exactly six, none PASS by inference.               */
/* ------------------------------------------------------------------ */

if (
  !Array.isArray(gate.wg1Conditions) ||
  gate.wg1Conditions.length !== WG1_CONDITION_IDS.length
) {
  fail(
    `WG1 must enumerate exactly ${String(WG1_CONDITION_IDS.length)} conditions`,
  );
}
let passingConditions = 0;
gate.wg1Conditions.forEach((condition, index) => {
  const expectedId = WG1_CONDITION_IDS[index];
  if (condition?.id !== expectedId) {
    fail(`WG1 condition ${String(index)} must be ${expectedId}`);
  }
  if (!STATUSES.has(condition.status)) {
    fail(`${expectedId} status must be measured as PASS or BLOCKED`);
  }
  if (
    !Array.isArray(condition.dependsOn) ||
    condition.dependsOn.some((id) => !WG1_PREDECESSOR_IDS.includes(id))
  ) {
    fail(`${expectedId} must depend only on the declared W40-W44 predecessors`);
  }
  if (
    typeof condition.evidence !== "string" ||
    condition.evidence.length === 0
  ) {
    fail(`${expectedId} must cite the evidence it was measured from`);
  }
  if (condition.status === "PASS") {
    const unmet = condition.dependsOn.filter(
      (id) => predecessorStatus.get(id) !== "PASS",
    );
    if (unmet.length > 0) {
      fail(
        `${expectedId} is PASS while ${unmet.join(", ")} is not measured PASS: readiness may never be inferred`,
      );
    }
    passingConditions += 1;
  } else if (
    !Array.isArray(condition.blockedOn) ||
    condition.blockedOn.length === 0
  ) {
    fail(`${expectedId} is BLOCKED and must name what blocks it`);
  }
});

/* ------------------------------------------------------------------ */
/* IG3 consolidated security-gate review: exactly four clauses.       */
/* ------------------------------------------------------------------ */

if (
  !Array.isArray(gate.ig3SecurityConditions) ||
  gate.ig3SecurityConditions.length !== IG3_CONDITION_IDS.length
) {
  fail(
    `the IG3 review must enumerate exactly ${String(IG3_CONDITION_IDS.length)} security conditions`,
  );
}
let holdingSecurityConditions = 0;
gate.ig3SecurityConditions.forEach((condition, index) => {
  const expectedId = IG3_CONDITION_IDS[index];
  if (condition?.id !== expectedId) {
    fail(`IG3 condition ${String(index)} must be ${expectedId}`);
  }
  if (!STATUSES.has(condition.status)) {
    fail(`${expectedId} status must be measured as PASS or BLOCKED`);
  }
  if (
    typeof condition.evidence !== "string" ||
    condition.evidence.length === 0
  ) {
    fail(`${expectedId} must cite the evidence it was measured from`);
  }
  if (condition.status === "PASS") {
    holdingSecurityConditions += 1;
  } else if (
    !Array.isArray(condition.blockedOn) ||
    condition.blockedOn.length === 0
  ) {
    fail(`${expectedId} is BLOCKED and must name what blocks it`);
  }
});

if (gate.productionReadiness !== false) {
  fail(
    "IG3 keeps production readiness false until every security condition and every WG1 predecessor is measured PASS",
  );
}

/* ------------------------------------------------------------------ */
/* W44 binding: digests and counts must reproduce from the tree.      */
/* ------------------------------------------------------------------ */

const binding = gate.w44Binding;
if (binding?.artifactPath !== W44_PATH || binding?.testPath !== W44_TEST_PATH) {
  fail("the W44 binding must name the exact W44 artifact and test surfaces");
}
const sha256Of = (path) =>
  createHash("sha256").update(readIndexedFile(path)).digest("hex");
const artifactSha256 = sha256Of(W44_PATH);
const testSha256 = sha256Of(W44_TEST_PATH);
if (
  binding.artifactSha256 !== artifactSha256 ||
  binding.testSha256 !== testSha256
) {
  fail(
    `W44 binding digests are stale: artifact ${artifactSha256}, test ${testSha256}`,
  );
}

const w44 = JSON.parse(readIndexedFile(W44_PATH, "utf8"));
const expectedCaseCounts = {
  total: 17,
  beforeAfterCrashPoints: 14,
  lifecycleTransitions: 7,
  ordinaryL1Rollback: 1,
  rollbackDeeperThanFinalityDepthWithinCardanoK: 1,
  configuredSourceInconsistency: 1,
};
for (const [key, expected] of Object.entries(expectedCaseCounts)) {
  if (w44.caseCounts?.[key] !== expected) {
    fail(`W44 caseCounts.${key} must be exactly ${String(expected)}`);
  }
}
if (
  w44.caseCounts.beforeAfterCrashPoints !==
  w44.caseCounts.lifecycleTransitions * 2
) {
  fail(
    "W44 must carry exactly one before and one after crash point per transition",
  );
}
if (
  w44.caseCounts.total !==
  w44.caseCounts.beforeAfterCrashPoints +
    w44.caseCounts.ordinaryL1Rollback +
    w44.caseCounts.rollbackDeeperThanFinalityDepthWithinCardanoK +
    w44.caseCounts.configuredSourceInconsistency
) {
  fail("W44 case decomposition does not sum to its declared total");
}
if (
  !Array.isArray(w44.lifecycleTransitions) ||
  w44.lifecycleTransitions.length !== w44.caseCounts.lifecycleTransitions
) {
  fail("W44 must enumerate every lifecycle transition it crashed at");
}
const runner = w44.runnerReport;
if (
  runner?.numTotalTests !== expectedCaseCounts.total ||
  runner?.numPassedTests !== expectedCaseCounts.total ||
  runner?.numFailedTests !== 0 ||
  runner?.numPendingTests !== 0 ||
  runner?.numTodoTests !== 0 ||
  !Array.isArray(runner.cases) ||
  runner.cases.length !== expectedCaseCounts.total ||
  runner.cases.some(({ status }) => status !== "passed")
) {
  fail("W44 runner report must be an exact 17/17 pass with no skipped work");
}
for (const [key, expected] of Object.entries({
  doubleSubmits: 0,
  duplicateRewards: 0,
  lostEvidence: 0,
  falseVerifiedStates: 0,
  unrecoverableWorkflows: 0,
  manualSurgeryRecoverySteps: 0,
})) {
  if (w44.zeroDefectInvariants?.[key] !== expected) {
    fail(`W44 zeroDefectInvariants.${key} must be exactly ${String(expected)}`);
  }
}
if (
  !(
    w44.depthBinding?.measuredDeepRollbackDepth >
    w44.depthBinding?.configuredFinalityDepth
  ) ||
  !(w44.depthBinding.measuredDeepRollbackDepth <= w44.depthBinding?.cardanoK) ||
  w44.depthBinding.cardanoK !== 2160
) {
  fail(
    "W44 must prove a rollback strictly deeper than the configured finality depth and within Cardano k = 2160",
  );
}
if (!Array.isArray(w44.scopeLimits) || w44.scopeLimits.length === 0) {
  fail("W44 must publish its scope limits rather than imply total coverage");
}

/* ------------------------------------------------------------------ */
/* Gate status.                                                        */
/* ------------------------------------------------------------------ */

const everythingMeasuredPass =
  passingPredecessors === WG1_PREDECESSOR_IDS.length &&
  passingConditions === WG1_CONDITION_IDS.length &&
  holdingSecurityConditions === IG3_CONDITION_IDS.length;

if (!STATUSES.has(gate.gateStatus)) {
  fail("gateStatus must be measured as PASS or BLOCKED");
}
if (gate.gateStatus === "PASS" && !everythingMeasuredPass) {
  fail(
    "gateStatus is PASS while a predecessor, WG1 condition or IG3 condition is not measured PASS",
  );
}
if (gate.gateStatus !== "PASS" && everythingMeasuredPass) {
  fail(
    "every predecessor and condition is measured PASS but gateStatus is not PASS: the artifact is stale",
  );
}

process.stdout.write(
  [
    "Canonical V1 WG1 watcher local gate verified.",
    `gateStatus: ${gate.gateStatus}`,
    `predecessors PASS: ${String(passingPredecessors)}/${String(WG1_PREDECESSOR_IDS.length)}`,
    `WG1 conditions PASS: ${String(passingConditions)}/${String(WG1_CONDITION_IDS.length)}`,
    `IG3 security conditions PASS: ${String(holdingSecurityConditions)}/${String(IG3_CONDITION_IDS.length)}`,
    `waivers: ${String(gate.waivers.length)}`,
    `production readiness: ${String(gate.productionReadiness)}`,
    `W44 bound: ${String(w44.caseCounts.total)}/${String(w44.caseCounts.total)} cases, 0 failures`,
    "",
  ].join("\n"),
);
