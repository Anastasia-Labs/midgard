import {
  buildMidgardLedgerOutputScanTrace,
  buildMidgardLedgerOutputValueTrace,
} from "@al-ft/midgard-core";
import { REGISTRATION_DURATION_MS } from "@al-ft/midgard-sdk";

/** Only the single lovelace-only deposit used by transitionTrace is audited here. */
export const TRANSITION_TRACE_JOURNEY_PLAN = Object.freeze({
  category: "transitionTrace",
  depositOutputs: 1,
  outputBytes: 41,
  scanPrimitiveSteps: 3,
  scanStepsPerTransaction: 4,
  valuePrimitiveSteps: 2,
  valueStepsPerTransaction: 4,
  proofPreimagePublications: 1,
  outputPreimagePublications: 1,
  checkpointPhases: Object.freeze([6, 0, 2, 7, 10, 8, 3, 9, 4, 5]),
  // Init, proof preimage, route, output preimage, nine checkpoints, final, remove.
  dependentTransactions: 15,
  // Registration, activation, appointment, and honest commitment; retained runs may skip earlier actions.
  successorTransactions: 4,
});

/**
 * Finite bound for every automatic family without an audited plan. The largest
 * installed proof chain (executionNativeScriptInvalid: init, thirteen steps,
 * removal, and its publications) stays well under this bound; a family that
 * needs more must publish an audited plan like transitionTrace instead of
 * widening the bound. The correction stage additionally fails as soon as the
 * workflow journal stops making durable progress for one transaction
 * allowance, so this bound only caps a family that keeps progressing.
 */
export const GENERIC_JOURNEY_PLAN = Object.freeze({
  category: "generic",
  dependentTransactions: 24,
  // Registration, activation, appointment, and honest commitment.
  successorTransactions: 4,
});

export type JourneyTimingPlan =
  | typeof TRANSITION_TRACE_JOURNEY_PLAN
  | typeof GENERIC_JOURNEY_PLAN;

/** Refuse to reuse the audited plan if the actual projected output has drifted. */
export const verifyTransitionTraceJourneyOutputPlan = (
  outputCbor: Uint8Array,
) => {
  const scan = buildMidgardLedgerOutputScanTrace(outputCbor);
  const value = buildMidgardLedgerOutputValueTrace({
    assets: scan.steps.flatMap((step) =>
      step.asset === null ? [] : [step.asset],
    ),
    lovelace: scan.terminal.lovelace,
  });
  const plan = TRANSITION_TRACE_JOURNEY_PLAN;
  if (
    outputCbor.length !== plan.outputBytes ||
    scan.steps.length !== plan.scanPrimitiveSteps ||
    value.steps.length !== plan.valuePrimitiveSteps
  ) {
    throw new Error(
      "Transition-trace output differs from its audited journey timing plan",
    );
  }
  return plan;
};

export interface JourneyCadence {
  slotLengthSeconds: number;
  activeSlotsCoeff: number;
  /** Release finality depth; it budgets the final evidence stamp only. */
  confirmationDepth: number;
  /**
   * Depth at which the watcher acts on an observation. Every action stage
   * (fault staging, the proof chain, the honest successor, healthy
   * processing) is budgeted from this depth. Defaults to the release depth,
   * which is the pre-inclusion-gating behavior.
   */
  actionDepth?: number;
  /** Separate preparation budget; retained fixture retries need no fresh preparation. */
  fixtureStagingAllowanceMs?: number;
}

export const HEALTHY_REPLAY_BLOCK_ALLOWANCE_MS = 10_000;

export const MAX_TIMER_MS = 2_147_483_647;

/** Fixed workload allowance, including blocks arriving while the serial replay catches up. */
export const healthyJourneyReplayTiming = (input: {
  tipBlockNo: number;
  slotLengthSeconds: number;
  activeSlotsCoeff: number;
  beforeReplayAllowanceMs: number;
  observationAllowanceMs: number;
}) => {
  const {
    tipBlockNo,
    slotLengthSeconds,
    activeSlotsCoeff,
    beforeReplayAllowanceMs,
    observationAllowanceMs,
  } = input;
  if (
    !Number.isSafeInteger(tipBlockNo) ||
    tipBlockNo < 0 ||
    !Number.isFinite(slotLengthSeconds) ||
    slotLengthSeconds <= 0 ||
    !Number.isFinite(activeSlotsCoeff) ||
    activeSlotsCoeff <= 0 ||
    activeSlotsCoeff > 1 ||
    !Number.isSafeInteger(beforeReplayAllowanceMs) ||
    beforeReplayAllowanceMs < 0 ||
    !Number.isSafeInteger(observationAllowanceMs) ||
    observationAllowanceMs < 0
  )
    throw new Error("Invalid healthy replay workload or cadence");
  const arrivingBlocksPerMs = activeSlotsCoeff / (slotLengthSeconds * 1000);
  const replayUtilization =
    arrivingBlocksPerMs * HEALTHY_REPLAY_BLOCK_ALLOWANCE_MS;
  if (replayUtilization >= 1)
    throw new Error(
      "Healthy replay allowance cannot catch up with configured block production",
    );
  // Origin is a conservative upper bound. Receipt/finality cursors do not identify
  // callbacks still draining inside a native replay batch and must not be subtracted.
  const initialBacklogBlocks = tipBlockNo + 1;
  const blocksBeforeReplay = Math.ceil(
    arrivingBlocksPerMs * beforeReplayAllowanceMs,
  );
  // One extra block covers rounding of arrivals during the replay interval.
  const timeoutMs = Math.ceil(
    (HEALTHY_REPLAY_BLOCK_ALLOWANCE_MS *
      (initialBacklogBlocks + blocksBeforeReplay + 1) +
      observationAllowanceMs) /
      (1 - replayUtilization),
  );
  if (!Number.isSafeInteger(timeoutMs) || timeoutMs > MAX_TIMER_MS)
    throw new Error("Healthy replay timing exceeds the supported timer range");
  return Object.freeze({
    replayOrigin: "origin" as const,
    capturedTipBlockNo: tipBlockNo,
    initialBacklogBlocks,
    blockAllowanceMs: HEALTHY_REPLAY_BLOCK_ALLOWANCE_MS,
    arrivingBlocksPerMs,
    replayUtilization,
    beforeReplayAllowanceMs,
    observationAllowanceMs,
    blocksBeforeReplay,
    blocksDuringReplay: Math.ceil(arrivingBlocksPerMs * timeoutMs),
    timeoutMs,
  });
};

export const requireHealthyReplayFitsDeadline = (input: {
  deadlineMonotonicMs: number;
  nowMonotonicMs: number;
  timeoutMs: number;
}) => {
  const remainingMs = Math.floor(
    input.deadlineMonotonicMs - input.nowMonotonicMs,
  );
  if (
    !Number.isFinite(remainingMs) ||
    !Number.isSafeInteger(input.timeoutMs) ||
    input.timeoutMs <= 0 ||
    remainingMs < input.timeoutMs
  )
    throw new Error(
      `Healthy replay requires ${input.timeoutMs}ms but the fixed outer deadline has ${remainingMs}ms remaining`,
    );
  return remainingMs;
};

export const journeyTimingForPlan = (
  plan: JourneyTimingPlan,
  {
    slotLengthSeconds,
    activeSlotsCoeff,
    confirmationDepth,
    actionDepth = confirmationDepth,
    fixtureStagingAllowanceMs = 0,
  }: JourneyCadence,
) => {
  if (
    !Number.isFinite(slotLengthSeconds) ||
    slotLengthSeconds <= 0 ||
    !Number.isFinite(activeSlotsCoeff) ||
    activeSlotsCoeff <= 0 ||
    activeSlotsCoeff > 1 ||
    !Number.isSafeInteger(confirmationDepth) ||
    confirmationDepth < 1 ||
    !Number.isSafeInteger(actionDepth) ||
    actionDepth < 1 ||
    actionDepth > confirmationDepth ||
    !Number.isSafeInteger(fixtureStagingAllowanceMs) ||
    fixtureStagingAllowanceMs < 0
  ) {
    throw new Error("Invalid journey cadence or staging allowance");
  }
  const idealDepthMs = (depth: number) =>
    (depth * slotLengthSeconds * 1000) / activeSlotsCoeff;
  // An action waits for inclusion at the action depth, not for release finality.
  const expectedConfirmationPerTransactionMs = idealDepthMs(actionDepth);
  // Two times the ideal cadence mean is a finite allowance, not an inclusion
  // guarantee. Node outages or sustained underproduction still terminate at
  // the outer deadline.
  const confirmationAllowancePerTransactionMs = Math.ceil(
    2 * expectedConfirmationPerTransactionMs,
  );
  const allowances = Object.freeze({
    transactionBuildMs: 120_000,
    transactionRpcMs: 30_000,
    authorityStartupMs: 30_000,
    watcherStartupMs: 600_000,
    automaticDecisionMs: 900_000,
    healthySuccessorObservationMs: 1_800_000,
    successorRegistrationMs: Number(REGISTRATION_DURATION_MS),
    fixtureStagingMs: fixtureStagingAllowanceMs,
    /**
     * The one release-finality wait in a journey: the native recorder
     * authenticates the included terminal's transactions at the release depth
     * and stamps it as the anchor. `completed`, journaled only beyond the
     * recovery horizon, is not awaited. The terminal is already on chain, so
     * this is a single confirmation window at the release depth plus its RPC
     * allowance.
     */
    finalizedEvidenceStampMs:
      Math.ceil(2 * idealDepthMs(confirmationDepth)) + 30_000,
  });
  const transactionAllowanceMs =
    confirmationAllowancePerTransactionMs +
    allowances.transactionBuildMs +
    allowances.transactionRpcMs;
  const correctionTimeoutMs =
    plan.dependentTransactions * transactionAllowanceMs;
  const journeyTimeoutMs =
    correctionTimeoutMs +
    plan.successorTransactions * transactionAllowanceMs +
    allowances.authorityStartupMs +
    allowances.watcherStartupMs +
    allowances.automaticDecisionMs +
    allowances.healthySuccessorObservationMs +
    allowances.successorRegistrationMs +
    allowances.fixtureStagingMs +
    allowances.finalizedEvidenceStampMs;
  // Node timers above this limit wrap down to approximately one millisecond.
  if (
    !Number.isSafeInteger(journeyTimeoutMs) ||
    journeyTimeoutMs > 2_147_483_647
  ) {
    throw new Error("Journey timing exceeds the supported timer range");
  }
  return {
    plan,
    cadence: {
      slotLengthSeconds,
      activeSlotsCoeff,
      confirmationDepth,
      actionDepth,
    },
    expectedConfirmationMs:
      plan.dependentTransactions * expectedConfirmationPerTransactionMs,
    confirmationAllowanceMs:
      plan.dependentTransactions * confirmationAllowancePerTransactionMs,
    /** Build, submit, and confirm one dependent transaction; also the progress allowance. */
    transactionAllowanceMs,
    correctionTimeoutMs,
    journeyTimeoutMs,
    allowances,
  };
};

export type JourneyTiming = ReturnType<typeof journeyTimingForPlan>;

export const transitionTraceJourneyTiming = (cadence: JourneyCadence) =>
  journeyTimingForPlan(TRANSITION_TRACE_JOURNEY_PLAN, cadence);

export const genericJourneyTiming = (cadence: JourneyCadence) =>
  journeyTimingForPlan(GENERIC_JOURNEY_PLAN, cadence);

export const journeyTimingForCategory = (
  category: string,
  cadence: JourneyCadence,
) =>
  category === TRANSITION_TRACE_JOURNEY_PLAN.category
    ? transitionTraceJourneyTiming(cadence)
    : genericJourneyTiming(cadence);

export const object = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
};

export interface ReadJourneyTimingOptions {
  authenticatedConfirmationDepth?: number;
  /** Depth the journey watcher acts at; the manifest depth when omitted. */
  actionDepth?: number;
  fixtureStagingAllowanceMs?: number;
}
