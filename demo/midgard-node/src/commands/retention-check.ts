import {
  daRetentionPruneDecisionV1,
  MIDGARD_RETENTION_WINDOW_V1,
  retentionDeadlineAlertV1,
  type RetentionPruneReasonCodeV1,
} from "@al-ft/midgard-core";

/**
 * Executable retention deadline alert (GOAL_SPEC 9.4 / Q54).
 *
 * Pure evaluator, in the shape of `evaluateReadiness`: no IO, no clock, no
 * database. Callers supply the observed retained records and the evaluator
 * reports whether any still-challengeable record has burned through its alert
 * headroom, so the CLI verb can exit nonzero.
 */

/** One retained DA record as observed by the caller. */
export type RetentionCheckRecord = {
  readonly headerHash: string;
  /** Block END TIME in ms. `null`/`undefined` means unknown (fails closed). */
  readonly blockEndTimeMs?: number | null;
  /** L1 state-queue header status, if known. Unknown fails closed. */
  readonly headerStatus?: string | null;
  /**
   * Deployment fingerprint the record was written under. When the caller
   * supplies `expectedDeploymentFingerprint`, a mismatch retains and alerts.
   */
  readonly deploymentFingerprint?: string | null;
};

export type RetentionCheckInput = {
  readonly nowMillis: number;
  readonly records: readonly RetentionCheckRecord[];
  /** Defaults to the derived operational margin (388_800_000 ms). */
  readonly alertThresholdMs?: number;
  /** Deployed retention in whole days; defaults to the derived 15. */
  readonly retentionDays?: number;
  readonly expectedDeploymentFingerprint?: string;
};

export type RetentionCheckFinding = {
  readonly headerHash: string;
  readonly reasonCode:
    | RetentionPruneReasonCodeV1
    | "deployment_fingerprint_mismatch";
  readonly remainingMs: number | null;
  readonly headroomMs: number | null;
};

export type RetentionCheckResult = {
  readonly ok: boolean;
  readonly alertThresholdMs: number;
  readonly requiredRetentionMs: number;
  readonly deployedRetentionMs: number;
  readonly marginMs: number;
  readonly checked: number;
  readonly stillChallengeable: number;
  readonly alerts: readonly RetentionCheckFinding[];
  readonly reasons: readonly string[];
};

/**
 * Evaluates retention deadlines for the supplied records.
 *
 * A record alerts when it is still challengeable AND its remaining time to the
 * challengeability deadline is at or below `alertThresholdMs`, or when its
 * deployment fingerprint does not match the expected one (in which case the
 * record is both retained and flagged).
 */
export const evaluateRetentionCheck = (
  input: RetentionCheckInput,
): RetentionCheckResult => {
  const alertThresholdMs =
    input.alertThresholdMs ?? MIDGARD_RETENTION_WINDOW_V1.marginMs;
  if (!Number.isSafeInteger(alertThresholdMs) || alertThresholdMs < 0) {
    throw new Error("alertThresholdMs must be a non-negative safe integer");
  }
  const retentionDays =
    input.retentionDays ?? MIDGARD_RETENTION_WINDOW_V1.retentionDays;

  const alerts: RetentionCheckFinding[] = [];
  const reasons: string[] = [];
  let stillChallengeable = 0;

  for (const record of input.records) {
    const decision = daRetentionPruneDecisionV1(
      {
        headerHash: record.headerHash,
        blockEndTimeMs: record.blockEndTimeMs ?? null,
      },
      {
        nowMs: input.nowMillis,
        retentionDays,
        headerStatus: record.headerStatus ?? undefined,
      },
    );

    const fingerprintMismatch =
      input.expectedDeploymentFingerprint !== undefined &&
      record.deploymentFingerprint !== input.expectedDeploymentFingerprint;

    if (decision.decision === "retain") {
      stillChallengeable += 1;
    }

    if (fingerprintMismatch) {
      // Fail closed: an unrecognised deployment fingerprint means the record's
      // window cannot be reasoned about, so retain it and alert.
      alerts.push({
        headerHash: record.headerHash,
        reasonCode: "deployment_fingerprint_mismatch",
        remainingMs: decision.remainingMs ?? null,
        headroomMs: null,
      });
      reasons.push(
        `retention_deployment_fingerprint_mismatch:${record.headerHash}`,
      );
      continue;
    }

    if (decision.decision !== "retain") {
      continue;
    }

    if (
      typeof record.blockEndTimeMs !== "number" ||
      decision.reasonCode === "missing_block_end_time" ||
      decision.reasonCode === "header_status_unknown"
    ) {
      // Retained for a fail-closed reason with no computable deadline: report
      // it so an operator sees the unresolvable record, but do not fabricate a
      // remaining-time number.
      alerts.push({
        headerHash: record.headerHash,
        reasonCode: decision.reasonCode,
        remainingMs: null,
        headroomMs: null,
      });
      reasons.push(`retention_indeterminate:${decision.reasonCode}`);
      continue;
    }

    const alert = retentionDeadlineAlertV1({
      nowMs: input.nowMillis,
      blockEndTimeMs: record.blockEndTimeMs,
      retentionDays,
      alertThresholdMs,
      headerHash: record.headerHash,
    });
    if (alert.alerting) {
      alerts.push({
        headerHash: record.headerHash,
        reasonCode: decision.reasonCode,
        remainingMs: alert.remainingMs,
        headroomMs: alert.headroomMs,
      });
      reasons.push(
        `retention_deadline_imminent:${record.headerHash}:${alert.remainingMs.toString()}`,
      );
    }
  }

  return {
    ok: alerts.length === 0,
    alertThresholdMs,
    requiredRetentionMs: MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs,
    deployedRetentionMs: MIDGARD_RETENTION_WINDOW_V1.deployedRetentionMs,
    marginMs: MIDGARD_RETENTION_WINDOW_V1.marginMs,
    checked: input.records.length,
    stillChallengeable,
    alerts,
    reasons,
  };
};

/** Process exit code for a retention check result: 0 clean, 1 alerting. */
export const retentionCheckExitCode = (result: RetentionCheckResult): number =>
  result.ok ? 0 : 1;
