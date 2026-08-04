#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  absoluteArg,
  evaluateClosureIdentity,
  evaluateClosureIdentityArtifacts,
  sameSourceIdentity,
  SHA256,
  sha256File,
} from "./phase3-architecture-g-closure-lib.mjs";

export const PHASE3_FINAL_TREE_SCHEMA =
  "midgard-phase3-architecture-g-final-tree-suite-v1";
export const PHASE3_FINAL_TREE_SCENARIO =
  "phase3-architecture-g-final-tree-crash-recovery-v1";
export const PHASE3_FINAL_TREE_AUTHORIZATION =
  "architecture-g-final-tree-isolated-v1";

export const PHASE3_FINAL_TREE_SUITES = Object.freeze(
  [
    {
      id: "locked-native-owner",
      argv: ["pnpm", "run", "native:mpf-owner:check"],
      coverage: ["locked_native", "native_owner_unit"],
    },
    {
      id: "adversarial-differential",
      argv: ["pnpm", "run", "test:mpf:differential"],
      coverage: ["root_differential", "proof_differential"],
    },
    {
      id: "native-crash-promotion-replay",
      argv: ["pnpm", "run", "test:mpf-native-owner"],
      coverage: [
        "malformed_frame",
        "child_crash_restart",
        "stale_epoch",
        "atomic_promotion_before_batch",
        "atomic_promotion_after_batch_before_ack",
        "post_submit_journal_replay",
      ],
    },
    {
      id: "journal-database-recovery",
      argv: [
        "pnpm",
        "exec",
        "vitest",
        "run",
        "tests/database.test.ts",
        "--reporter=default",
      ],
      coverage: ["journal_atomic_prepare", "journal_process_kill_recovery"],
    },
    {
      id: "shared-phase4-focused",
      argv: [
        "pnpm",
        "exec",
        "vitest",
        "run",
        "tests/canonical-journal-recovery.test.ts",
        "tests/commit-block-header-state-queue-tail.test.ts",
        "tests/commit-recovery-planner.test.ts",
        "tests/commit-worker-failure-lease-classification.test.ts",
        "tests/confirmation-finalization-race.test.ts",
        "tests/foreign-base-rebinding.test.ts",
        "tests/phase4-process-isolation.test.ts",
        "tests/phase4-t1-recovery.test.ts",
        "tests/pipelined-commit-process-harness.test.ts",
        "tests/speculative-commit-planner.test.ts",
        "tests/speculative-commit-safety-guard.test.ts",
        "tests/speculative-commit-worker-session.test.ts",
        "tests/speculative-mpf-lifecycle.test.ts",
        "--reporter=default",
      ],
      coverage: ["shared_phase4_lifecycle", "shared_phase4_process_harness"],
    },
  ].map((suite) =>
    Object.freeze({
      ...suite,
      argv: Object.freeze([...suite.argv]),
      coverage: Object.freeze([...suite.coverage]),
    }),
  ),
);

const requiredCoverage = new Set(
  PHASE3_FINAL_TREE_SUITES.flatMap(({ coverage }) => coverage),
);

const artifactReasons = (artifact, label, { checkArtifacts }) => {
  const reasons = [];
  if (
    typeof artifact?.path !== "string" ||
    !path.isAbsolute(artifact.path) ||
    !SHA256.test(artifact?.sha256 ?? "") ||
    !Number.isSafeInteger(artifact?.bytes) ||
    artifact.bytes < 0
  ) {
    return [`${label} artifact identity is malformed`];
  }
  if (checkArtifacts) {
    if (!fs.existsSync(artifact.path))
      reasons.push(`${label} artifact is missing`);
    else {
      const stat = fs.lstatSync(artifact.path);
      if (!stat.isFile() || stat.isSymbolicLink()) {
        reasons.push(`${label} artifact is not a regular file`);
      } else {
        if (stat.size !== artifact.bytes)
          reasons.push(`${label} byte count changed`);
        if (sha256File(artifact.path) !== artifact.sha256) {
          reasons.push(`${label} SHA-256 changed`);
        }
      }
    }
  }
  return reasons;
};

export const evaluatePhase3FinalTreeReport = (
  report,
  { checkArtifacts = true } = {},
) => {
  const reasons = [];
  if (report?.schemaVersion !== PHASE3_FINAL_TREE_SCHEMA) {
    reasons.push("unexpected final-tree report schema");
  }
  if (report?.scenario !== PHASE3_FINAL_TREE_SCENARIO) {
    reasons.push("unexpected final-tree scenario");
  }
  if (
    report?.authorization !== PHASE3_FINAL_TREE_AUTHORIZATION ||
    !/^midgard_phase3_arch_g_final_tree_[a-z0-9_]+$/u.test(
      report?.database?.name ?? "",
    ) ||
    !["127.0.0.1", "localhost"].includes(report?.database?.host)
  ) {
    reasons.push(
      "final-tree database is not explicitly authorized and isolated",
    );
  }
  reasons.push(...evaluateClosureIdentity(report?.identity));
  if (checkArtifacts) {
    reasons.push(...evaluateClosureIdentityArtifacts(report?.identity));
  }
  if (
    !sameSourceIdentity(report?.identity?.source, report?.sourceAtCompletion)
  ) {
    reasons.push("source tree changed while the final-tree suites ran");
  }
  if (
    !Number.isSafeInteger(report?.startedAtMs) ||
    !Number.isSafeInteger(report?.completedAtMs) ||
    report.completedAtMs < report.startedAtMs
  ) {
    reasons.push("invalid final-tree run interval");
  }
  const suites = Array.isArray(report?.suites) ? report.suites : [];
  if (suites.length !== PHASE3_FINAL_TREE_SUITES.length) {
    reasons.push("final-tree suite cardinality is not exact");
  }
  const observedCoverage = new Set();
  for (const [index, expected] of PHASE3_FINAL_TREE_SUITES.entries()) {
    const actual = suites[index];
    if (
      actual?.id !== expected.id ||
      JSON.stringify(actual?.argv) !== JSON.stringify(expected.argv) ||
      !Array.isArray(actual?.coverage) ||
      JSON.stringify(actual.coverage) !== JSON.stringify(expected.coverage)
    ) {
      reasons.push(`suite ${expected.id} command or coverage drifted`);
      continue;
    }
    for (const value of actual.coverage) observedCoverage.add(value);
    if (
      actual?.exitCode !== 0 ||
      actual?.signal !== null ||
      actual?.timedOut !== false ||
      actual?.completed !== true
    ) {
      reasons.push(`suite ${expected.id} did not complete successfully`);
    }
    if (
      !Number.isSafeInteger(actual?.startedAtMs) ||
      !Number.isSafeInteger(actual?.completedAtMs) ||
      actual.completedAtMs < actual.startedAtMs
    ) {
      reasons.push(`suite ${expected.id} has an invalid interval`);
    }
    reasons.push(
      ...artifactReasons(actual?.stdout, `${expected.id} stdout`, {
        checkArtifacts,
      }),
      ...artifactReasons(actual?.stderr, `${expected.id} stderr`, {
        checkArtifacts,
      }),
    );
  }
  for (const value of requiredCoverage) {
    if (!observedCoverage.has(value)) reasons.push(`missing coverage ${value}`);
  }
  if (report?.verdict !== "passed")
    reasons.push("report verdict is not passed");
  return { passed: reasons.length === 0, reasons };
};

const isMain = process.argv[1] === fileURLToPath(import.meta.url);
if (isMain) {
  try {
    const reportPath = absoluteArg(process.argv.slice(2), "--report");
    const report = JSON.parse(fs.readFileSync(reportPath, "utf8"));
    const result = evaluatePhase3FinalTreeReport(report);
    process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
    if (!result.passed) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exitCode = 1;
  }
}
