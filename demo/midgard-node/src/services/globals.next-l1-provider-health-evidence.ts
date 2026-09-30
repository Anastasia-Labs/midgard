import { Metric } from "effect";

import type { SubmitSlotSnapshot } from "../local-ogmios-slot.js";
import { SerializedStateQueueUTxO } from "../workers/utils/commit-block-header.js";

export type CommitPipelinePhase =
  | "idle"
  | "scheduler_alignment"
  | "speculative_build"
  | "mutation_worker";

export type CommitSubmitWake = {
  readonly confirmedHeaderHash: string;
  /** Canonical latest state-queue tip observed with confirmedHeaderHash. */
  readonly confirmedTip: SerializedStateQueueUTxO;
  readonly confirmationObservedAtMs: number;
  readonly confirmationWaitMs: number;
};

export type AdmissionBacklogGaugeState = {
  readonly ADMISSION_BACKLOG_BASE: bigint;
  readonly ADMISSION_BACKLOG_LOCAL_DELTA: bigint;
  /**
   * Slots reserved by concurrent submit handlers before their durable INSERT
   * finishes. Keeping these separate prevents an in-flight reservation from
   * being erased by a live-count refresh.
   */
  readonly ADMISSION_BACKLOG_IN_FLIGHT: bigint;
  readonly ADMISSION_BACKLOG_REFRESHED_AT: number;
};

export type MempoolLedgerDelta = {
  readonly version: number;
  readonly full: boolean;
  readonly upserts: ReadonlyArray<readonly [outRefHex: string, output: Buffer]>;
  readonly deletes: readonly string[];
};

export type MempoolLedgerDeltaLog = {
  readonly version: number;
  readonly entries: readonly MempoolLedgerDelta[];
};

export type L1ProviderHealthEvidence = {
  readonly evidenceRevision: number;
  readonly lastObservationKind:
    | "exact_success"
    | "exact_failure"
    | "direct_success"
    | "direct_failure"
    | null;
  readonly lastExactEvidenceRevision: number;
  readonly lastExactObservationKind: "exact_success" | "exact_failure" | null;
  readonly lastSuccessAtMs: number;
  /** Last success from the exact HubOracle + local-Ogmios readiness query. */
  readonly lastExactSuccessAtMs: number;
  readonly lastExactFailureAtMs: number;
  readonly lastExactFailure: string | null;
  readonly lastSuccessKind: "exact" | "direct" | null;
  readonly lastFailureAtMs: number;
  readonly lastFailure: string | null;
  readonly lastOgmiosSlot: SubmitSlotSnapshot | null;
};

export const nextL1ProviderHealthEvidence = ({
  current,
  healthy,
  error,
  observedAtMs,
  ogmiosSlot,
  successKind,
}: {
  readonly current: L1ProviderHealthEvidence;
  readonly healthy: boolean;
  readonly error?: string;
  readonly observedAtMs: number;
  readonly ogmiosSlot?: SubmitSlotSnapshot;
  readonly successKind: "exact" | "direct";
}): L1ProviderHealthEvidence => {
  const evidenceRevision = current.evidenceRevision + 1;
  const lastObservationKind =
    successKind === "exact"
      ? healthy
        ? ("exact_success" as const)
        : ("exact_failure" as const)
      : healthy
        ? ("direct_success" as const)
        : ("direct_failure" as const);

  return healthy
    ? (() => {
        const exactFailureIsUnrecovered =
          successKind === "direct" &&
          current.lastExactObservationKind === "exact_failure";
        return {
          evidenceRevision,
          lastObservationKind,
          lastExactEvidenceRevision:
            successKind === "exact"
              ? evidenceRevision
              : current.lastExactEvidenceRevision,
          lastExactObservationKind:
            successKind === "exact"
              ? "exact_success"
              : current.lastExactObservationKind,
          lastSuccessAtMs: observedAtMs,
          lastExactSuccessAtMs:
            successKind === "exact"
              ? observedAtMs
              : current.lastExactSuccessAtMs,
          lastExactFailureAtMs: current.lastExactFailureAtMs,
          lastExactFailure: current.lastExactFailure,
          lastSuccessKind: successKind,
          lastFailureAtMs: current.lastFailureAtMs,
          lastFailure: exactFailureIsUnrecovered
            ? current.lastExactFailure
            : null,
          lastOgmiosSlot: ogmiosSlot ?? current.lastOgmiosSlot,
        };
      })()
    : (() => {
        const failure = error ?? "L1 provider probe failed";
        return {
          ...current,
          evidenceRevision,
          lastObservationKind,
          lastExactEvidenceRevision:
            successKind === "exact"
              ? evidenceRevision
              : current.lastExactEvidenceRevision,
          lastExactObservationKind:
            successKind === "exact"
              ? "exact_failure"
              : current.lastExactObservationKind,
          lastExactFailureAtMs:
            successKind === "exact"
              ? observedAtMs
              : current.lastExactFailureAtMs,
          lastExactFailure:
            successKind === "exact" ? failure : current.lastExactFailure,
          lastFailureAtMs: observedAtMs,
          lastFailure: failure,
        };
      })();
};

/**
 * Health of the operator's attestation-timeout correction step, read by
 * readiness. The step logs and retries a transient failure, so without this a
 * correction that fails every tick leaves the node reporting Ready.
 */
export type AttestationTimeoutCorrectionHealth = {
  /** When the step last moved a correction forward: a step that completed, or
   * a removal transaction the correction confirmed mid-step (a correction with
   * several descendants to prune spends several confirmations in one step).
   * Starts at node startup, like the worker heartbeats. */
  readonly lastProgressAtMs: number;
  /** When the step last read and classified the state queue. Starts at node
   * startup. */
  readonly lastQueueReadAtMs: number;
  /** The correction and confirmed-removal count last credited as progress, so
   * a step that re-saves an unchanged journal is not mistaken for progress. */
  readonly correctionProgress: {
    readonly targetHeaderHash: string;
    readonly confirmedRemovals: number;
  } | null;
  readonly lastFailureAtMs: number;
  readonly lastError: string | null;
  /** Failed steps since the last successful one. */
  readonly consecutiveFailures: number;
  /** The oldest unattested state-queue header and its DA-attestation deadline
   * at the step's last queue read; null when the queue held none. */
  readonly oldestUnattestedHeader: {
    readonly headerHash: string;
    readonly deadlineMs: number;
  } | null;
};

export const DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS = 180_000;

export class L1ControlPlaneTimeoutError extends Error {
  readonly scope: string;
  readonly maxHoldMs: number;

  constructor(scope: string, maxHoldMs: number) {
    super(`L1 control-plane scope ${scope} exceeded ${maxHoldMs.toString()}ms`);
    this.name = "L1ControlPlaneTimeoutError";
    this.scope = scope;
    this.maxHoldMs = maxHoldMs;
  }
}

export const l1ControlPlaneWaitTimer = Metric.timer(
  "l1_control_plane_wait_ms",
  "Time waiting for the process-wide L1 control-plane permit",
);

export const l1ControlPlaneHoldTimer = Metric.timer(
  "l1_control_plane_hold_ms",
  "Time holding the process-wide L1 control-plane permit",
);

export const l1ControlPlaneAcquisitionCounter = Metric.counter(
  "l1_control_plane_acquisitions_total",
  { description: "L1 control-plane permit acquisitions" },
);

export const l1ControlPlaneTimeoutCounter = Metric.counter(
  "l1_control_plane_timeouts_total",
  { description: "Bounded L1 control-plane holds that timed out" },
);
