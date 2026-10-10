import {
  daRetentionPruneDecision,
  MIDGARD_RETENTION_WINDOW,
  requireRetentionAlertThresholdMs,
  retentionDeadlineAlert,
  type RetentionHeaderStatus,
  type RetentionPruneReasonCode,
  type RetentionQueueReference,
} from "@al-ft/midgard-core";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { fetchRetentionL1View } from "../fibers/retention-sweeper.js";
import { newestTerminalOutcome } from "../l1-queue-terminals/index.js";
import { parseNonNegativeIntegerOption } from "./cli-options.js";

/**
 * Executable retention deadline alert (GOAL_SPEC 9.4 / Q54).
 *
 * `evaluateRetentionCheck` is a pure evaluator, in the shape of
 * `evaluateReadiness`: no IO, no clock, no database. Callers supply the
 * observed retained records and the evaluator reports whether any
 * still-challengeable record has burned through its alert headroom, so the CLI
 * verb can exit nonzero. `retentionCheckProgram` gathers those records from the
 * database and the node's authenticated L1 view.
 *
 * The deadline alert is opt-in: it is evaluated only when the operator passes
 * `alertThresholdMs` (`--alert-threshold-ms`). Without one no record raises a
 * deadline alert, because pruning never removes still-challengeable evidence.
 * With one, every merged payload is listed during its last `alertThresholdMs`
 * on its normal way to pruning, so a listed record is not a fault.
 */

/** One retained DA record as observed by the caller. */
export type RetentionCheckRecord = {
  readonly headerHash: string;
  /** Block END TIME in ms. */
  readonly blockEndTimeMs: number;
  /** Authenticated terminal status, or `unobserved` when none is recorded. */
  readonly headerStatus: RetentionHeaderStatus | "unobserved";
  /** Where the header sits in the caller's authenticated L1 view. */
  readonly queueReference: RetentionQueueReference;
  /**
   * Deployment fingerprint the record was written under. When the caller
   * supplies `expectedDeploymentFingerprint`, a mismatch is reported.
   */
  readonly deploymentFingerprint?: string | null;
};

export type RetentionCheckInput = {
  readonly nowMillis: number;
  readonly records: readonly RetentionCheckRecord[];
  /**
   * Remaining time at or below which a still-challengeable record alerts.
   * No default: when absent, no record raises a deadline alert.
   */
  readonly alertThresholdMs?: number;
  /** Deployed retention in whole days; defaults to the derived 15. */
  readonly retentionDays?: number;
  readonly expectedDeploymentFingerprint?: string;
};

export type RetentionCheckFinding = {
  readonly headerHash: string;
  readonly reasonCode:
    | RetentionPruneReasonCode
    | "deployment_fingerprint_mismatch";
  readonly remainingMs: number;
  readonly headroomMs: number | null;
};

export type RetentionCheckResult = {
  readonly ok: boolean;
  /** The configured threshold, or null when the deadline alert is off. */
  readonly alertThresholdMs: number | null;
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
 * A record alerts when a threshold is supplied, it is still challengeable, AND
 * its remaining time to the challengeability deadline is at or below
 * `alertThresholdMs`; or when its
 * deployment fingerprint does not match the expected one. The mismatch is a
 * diagnostic only; it never changes the retention decision.
 */
export const evaluateRetentionCheck = (
  input: RetentionCheckInput,
): RetentionCheckResult => {
  const alertThresholdMs = input.alertThresholdMs;
  if (
    alertThresholdMs !== undefined &&
    (!Number.isSafeInteger(alertThresholdMs) || alertThresholdMs < 0)
  ) {
    throw new Error("alertThresholdMs must be a non-negative safe integer");
  }
  const retentionDays =
    input.retentionDays ?? MIDGARD_RETENTION_WINDOW.retentionDays;

  const alerts: RetentionCheckFinding[] = [];
  const reasons: string[] = [];
  let stillChallengeable = 0;

  for (const record of input.records) {
    const decision = daRetentionPruneDecision({
      nowMs: input.nowMillis,
      blockEndTimeMs: record.blockEndTimeMs,
      headerStatus: record.headerStatus,
      queueReference: record.queueReference,
      retentionDays,
    });

    if (
      input.expectedDeploymentFingerprint !== undefined &&
      record.deploymentFingerprint !== input.expectedDeploymentFingerprint
    ) {
      alerts.push({
        headerHash: record.headerHash,
        reasonCode: "deployment_fingerprint_mismatch",
        remainingMs: decision.remainingMs,
        headroomMs: null,
      });
      reasons.push(
        `retention_deployment_fingerprint_mismatch:${record.headerHash}`,
      );
    }

    if (decision.reasonCode !== "still_challengeable") {
      continue;
    }
    stillChallengeable += 1;
    if (alertThresholdMs === undefined) {
      continue;
    }

    const alert = retentionDeadlineAlert({
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
    alertThresholdMs: alertThresholdMs ?? null,
    requiredRetentionMs: MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    deployedRetentionMs: MIDGARD_RETENTION_WINDOW.deployedRetentionMs,
    marginMs: MIDGARD_RETENTION_WINDOW.marginMs,
    checked: input.records.length,
    stillChallengeable,
    alerts,
    reasons,
  };
};

/**
 * Parses `--alert-threshold-ms`: absent leaves the deadline alert off. A value
 * at or above the merged-payload window (horizon minus block maturity) is
 * refused, because every merged payload would alert against it from the moment
 * it merges.
 */
export const parseRetentionAlertThresholdOption = (
  value: string | undefined,
): number | undefined =>
  value === undefined
    ? undefined
    : requireRetentionAlertThresholdMs(
        parseNonNegativeIntegerOption(value, "--alert-threshold-ms"),
        "--alert-threshold-ms",
      );

/** Process exit code for a retention check result: 0 clean, 1 alerting. */
export const retentionCheckExitCode = (result: RetentionCheckResult): number =>
  result.ok ? 0 : 1;

/**
 * Reads every retained DA payload, the newest terminal outcome a landed tx
 * gave its header (`node_l1_queue_terminals`), and its place in the landed
 * L1 state queue, then evaluates.
 */
export const retentionCheckProgram = (alertThresholdMs?: number) =>
  Effect.gen(function* () {
    const view = yield* fetchRetentionL1View;
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      readonly header_hash: Buffer;
      readonly block_end_time: Date;
      readonly terminal_outcome: "merged" | "removed" | null;
    }>`
      SELECT payload.header_hash, payload.block_end_time,
        ${newestTerminalOutcome(sql, "payload.header_hash")} AS terminal_outcome
      FROM da_payloads payload`;
    const confirmedHeadHash = view.confirmedHeadHash.toString("hex");
    const liveQueueHeaderHashes = new Set(
      view.liveQueueHeaderHashes.map((hash) => hash.toString("hex")),
    );
    return evaluateRetentionCheck({
      nowMillis: Date.now(),
      alertThresholdMs,
      records: rows.map((row) => {
        const headerHash = row.header_hash.toString("hex");
        return {
          headerHash,
          blockEndTimeMs: row.block_end_time.getTime(),
          headerStatus: row.terminal_outcome ?? "unobserved",
          queueReference:
            headerHash === confirmedHeadHash
              ? "confirmed_head"
              : liveQueueHeaderHashes.has(headerHash)
                ? "live_in_queue"
                : "none",
        };
      }),
    });
  });
