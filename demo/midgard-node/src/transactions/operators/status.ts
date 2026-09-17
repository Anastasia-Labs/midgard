/**
 * The operator status report behind `operator-status` and
 * `GET /operator/status`. Directory membership comes from the SDK's pure
 * status query; the inactivity block comes from the same takeover planner the
 * watchdog uses, so what the report predicts is what the watchdog will do.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { readOperatorWatchdogRecord } from "../../fibers/operator-watchdog-policy.js";
import { currentTimeMsForLucidOrEmulatorFallback } from "../register-active-operator/clock.js";

export type OperatorStatusReport = {
  readonly operatorKeyHash: string;
  readonly state: SDK.OperatorState;
  readonly duplicate: boolean;
  readonly memberships: readonly SDK.OperatorDirectoryOccupancyKind[];
  readonly bondLovelace: string | null;
  readonly inactivityStrikes: number | null;
  readonly maxInactivityStrikes: number;
  readonly bondUnlockTime: string | null;
  readonly bondRecoverable: boolean;
  /** Earliest validity lower bound a bond-recovery transaction may carry. */
  readonly bondRecoverableFrom: string | null;
  readonly registeredActivationTime: string | null;
  readonly activationTimeReached: boolean;
  readonly scheduler: {
    readonly currentOperator: string | null;
    readonly shiftStartTime: string | null;
    readonly holdsShift: boolean;
    readonly shiftAgeMs: number | null;
  };
  /** `null` when the scheduler names nobody. */
  readonly inactivity: {
    readonly thresholdTime: string | null;
    readonly thresholdSource: string | null;
    /** How long the shift has been strikable; `0` before the threshold. */
    readonly missedAgeMs: number;
    /** Earliest validity lower bound the successor's strike may carry. */
    readonly nextTakeoverTime: string | null;
    /** When every other active node's watchdog, at the local patience, acts. */
    readonly nextPatienceTakeoverTime: string | null;
    readonly strikesExhausted: boolean;
    /** Why no strike can be built right now, when that is the case. */
    readonly blocked: string | null;
  } | null;
  /** The local node's watchdog; never describes another node. */
  readonly watchdog: {
    readonly enabled: boolean;
    readonly patienceMs: number;
    readonly lastTakeoverTxHash: string | null;
    readonly lastTakeoverAt: string | null;
    readonly lastTakeoverKind: "strike" | "force_retire" | null;
    readonly lastSkipReason: string | null;
    readonly lastSkipAt: string | null;
  };
  readonly asOf: string;
};

export type OperatorStatusInput = {
  readonly operatorKeyHash: string;
  readonly watchdog: {
    readonly enabled: boolean;
    readonly patienceMs: number;
  };
  readonly nowMs?: bigint;
  readonly snapshot?: SDK.OperatorDirectorySnapshot;
};

const isoOf = (ms: number | null): string | null =>
  ms === null ? null : new Date(ms).toISOString();

const inactivityBlock = (
  plan: SDK.InactivityTakeoverPlan,
  nowMs: bigint,
  status: SDK.OperatorStatus,
  patienceMs: number,
): OperatorStatusReport["inactivity"] => {
  if (plan.kind === "no-shift") {
    return null;
  }
  if (plan.kind === "blocked") {
    return {
      thresholdTime: null,
      thresholdSource: null,
      missedAgeMs: 0,
      nextTakeoverTime: null,
      nextPatienceTakeoverTime: null,
      strikesExhausted: status.forcedRetirementEligible,
      blocked: `${plan.reason}: ${plan.detail}`,
    };
  }
  const thresholdMs = plan.thresholdMs;
  const missed = nowMs > thresholdMs ? Number(nowMs - thresholdMs) : 0;
  return {
    thresholdTime: thresholdMs.toString(),
    thresholdSource:
      plan.kind === "strikes-exhausted" ? null : plan.thresholdSource,
    missedAgeMs: missed,
    // A forced retirement has no designated successor; every node waits.
    nextTakeoverTime:
      plan.kind === "strikes-exhausted" ? null : (thresholdMs + 1n).toString(),
    nextPatienceTakeoverTime: (
      thresholdMs +
      1n +
      BigInt(patienceMs)
    ).toString(),
    strikesExhausted: plan.kind === "strikes-exhausted",
    blocked: null,
  };
};

/**
 * Builds the report from a directory snapshot; pure apart from the local
 * watchdog record.
 */
export const deriveOperatorStatusReport = (
  snapshot: SDK.OperatorDirectorySnapshot,
  input: Omit<OperatorStatusInput, "snapshot" | "nowMs"> & {
    readonly nowMs: bigint;
  },
): OperatorStatusReport => {
  const { operatorKeyHash, nowMs } = input;
  const status = SDK.deriveOperatorStatus(snapshot, operatorKeyHash, nowMs, {
    maxInactivityStrikes: SDK.MAX_INACTIVITY_STRIKES,
  });
  const plan = SDK.planInactivityTakeover({
    snapshot,
    nowMs,
    params: {
      ...SDK.DEFAULT_INACTIVITY_TIMING_PARAMETERS,
      maxInactivityStrikes: SDK.MAX_INACTIVITY_STRIKES,
    },
  });
  const scheduled = SDK.schedulerCurrentOperator(snapshot.scheduler);
  const record = readOperatorWatchdogRecord();
  return {
    operatorKeyHash,
    state: status.state,
    duplicate: status.duplicate,
    memberships: status.occupancies,
    bondLovelace: status.bondLovelace?.toString() ?? null,
    inactivityStrikes:
      status.inactivityStrikes === null
        ? null
        : Number(status.inactivityStrikes),
    maxInactivityStrikes: Number(SDK.MAX_INACTIVITY_STRIKES),
    bondUnlockTime: status.bondUnlockTime?.toString() ?? null,
    bondRecoverable: status.bondRecoveryAllowedNow,
    bondRecoverableFrom: status.bondRecoveryAllowedFrom?.toString() ?? null,
    registeredActivationTime:
      status.registeredActivationTime?.toString() ?? null,
    activationTimeReached: status.activationTimeReached,
    scheduler: {
      currentOperator: scheduled?.operator ?? null,
      shiftStartTime: scheduled?.startTime.toString() ?? null,
      holdsShift: status.holdsShift,
      shiftAgeMs: status.shiftAgeMs === null ? null : Number(status.shiftAgeMs),
    },
    inactivity: inactivityBlock(plan, nowMs, status, input.watchdog.patienceMs),
    watchdog: {
      enabled: input.watchdog.enabled,
      patienceMs: input.watchdog.patienceMs,
      lastTakeoverTxHash: record.lastTakeoverTxHash,
      lastTakeoverAt: isoOf(record.lastTakeoverAt),
      lastTakeoverKind: record.lastTakeoverKind,
      lastSkipReason: record.lastSkipReason,
      lastSkipAt: isoOf(record.lastSkipAt),
    },
    asOf: new Date(Number(nowMs)).toISOString(),
  };
};

export const operatorStatusProgram = (
  lucid: LucidEvolution,
  contracts: SDK.OperatorDirectoryValidators,
  input: OperatorStatusInput,
): Effect.Effect<
  OperatorStatusReport,
  SDK.OperatorDirectorySnapshotError | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const snapshot =
      input.snapshot ??
      (yield* SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts));
    const nowMs = input.nowMs ?? currentTimeMsForLucidOrEmulatorFallback(lucid);
    return deriveOperatorStatusReport(snapshot, { ...input, nowMs });
  });
