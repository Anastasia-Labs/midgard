import {
  buildMidgardLedgerOutputScanTrace,
  buildMidgardLedgerOutputValueTrace,
} from "@al-ft/midgard-core";
import {
  REGISTRATION_DURATION_MS,
  USER_EVENT_TX_TTL_MS,
} from "@al-ft/midgard-sdk";
import { protectedPredecessorHoldMs } from "midgard-watcher/tests/support/history-predecessor-hold";

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
 * installed automatic proof chain (executionNativeScriptInvalid's per-purpose
 * routes: 20 to 30 dependent transactions from init through the terminal,
 * plus the removal) stays under this bound; a family that needs more must
 * publish an audited plan like transitionTrace instead of widening the bound.
 * The correction stage additionally fails as soon as the workflow journal
 * stops making durable progress for one transaction allowance, so this bound
 * only caps a family that keeps progressing.
 */
export const GENERIC_JOURNEY_PLAN = Object.freeze({
  category: "generic",
  dependentTransactions: 32,
  // Registration, activation, appointment, and honest commitment.
  successorTransactions: 4,
});

/**
 * validationTraceDispute: the generic transaction bound, plus the one wait no
 * transaction of ours ends. Operator-2's node is stopped through the fault
 * phase, so nobody defends the case the watcher opens. The case validator
 * lets the watcher close it Convicted only past its defense deadline, the
 * side clock C plus the longest move validity range L_d after the case
 * starts (`validationTraceDefenseDeadlineMs`, validation-case-v1), and the
 * watcher journals nothing while it waits.
 */
export const VALIDATION_TRACE_DISPUTE_JOURNEY_PLAN = Object.freeze({
  category: "validationTraceDispute",
  dependentTransactions: GENERIC_JOURNEY_PLAN.dependentTransactions,
  successorTransactions: GENERIC_JOURNEY_PLAN.successorTransactions,
  awaitsUndefendedCase: true,
});

export type JourneyTimingPlan =
  | typeof TRANSITION_TRACE_JOURNEY_PLAN
  | typeof GENERIC_JOURNEY_PLAN
  | typeof VALIDATION_TRACE_DISPUTE_JOURNEY_PLAN;

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
  /**
   * The deployment profile's dispute clocks; a plan that waits out an
   * undefended case needs them.
   */
  disputeTiming?: Readonly<{
    dispute_side_clock_ms: number;
    dispute_max_validity_range_ms: number;
  }>;
  /**
   * The deployment profile's name and the fault families it leaves out of
   * its fraud-proof catalogue; a family it leaves out has no journey.
   */
  deploymentProfile?: Readonly<{
    name: string;
    excluded_fraud_proof_categories: readonly string[];
  }>;
}

/** How long an undefended case stays open: its defense window, C + L_d. */
const undefendedCaseWaitMs = (
  plan: JourneyTimingPlan,
  disputeTiming: JourneyCadence["disputeTiming"],
) => {
  if (!("awaitsUndefendedCase" in plan)) return 0;
  if (disputeTiming === undefined)
    throw new Error(
      `${plan.category} journey timing needs the deployment's dispute clocks`,
    );
  const { dispute_side_clock_ms, dispute_max_validity_range_ms } =
    disputeTiming;
  if (
    !Number.isSafeInteger(dispute_side_clock_ms) ||
    dispute_side_clock_ms < 0 ||
    !Number.isSafeInteger(dispute_max_validity_range_ms) ||
    dispute_max_validity_range_ms < 0
  )
    throw new Error(`Invalid dispute clocks for ${plan.category}`);
  return dispute_side_clock_ms + dispute_max_validity_range_ms;
};

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
    disputeTiming,
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
    /**
     * An undefended case's defense window, C + L_d; zero for every other
     * plan. The progress gate holds the journal's silence open from the
     * case's start to that deadline (`journeyProgressClock`).
     */
    undefendedCaseMs: undefendedCaseWaitMs(plan, disputeTiming),
  });
  const transactionAllowanceMs =
    confirmationAllowancePerTransactionMs +
    allowances.transactionBuildMs +
    allowances.transactionRpcMs;
  const correctionTimeoutMs =
    plan.dependentTransactions * transactionAllowanceMs +
    allowances.undefendedCaseMs;
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

/**
 * How long an attached stack's staging holds operator-2's shift, from the
 * stack tail it reads to its fault's commit: for each block it commits, the
 * commit and the stack coordinator's attestation of it, an init and then an
 * apply. Each is one dependent transaction at the journey's action depth.
 * The fault's own attestation falls outside the shift, but it is counted as
 * a block like the others. The staging publishes its user events one after
 * another, then waits once, until the fault may include the last of them and
 * before our node would. It holds each event transaction's validity; each
 * later event's build may first hold until the event before it leaves its
 * history protection; and then the one event wait.
 */
export const stackStagingShiftWorkMs = (
  timing: Pick<JourneyTiming, "transactionAllowanceMs">,
  blocks: number,
  events: Readonly<{
    count: number;
    eventWaitMs: number;
    /** The deployment's event-history protection duration. */
    protectionMs: number;
  }> = {
    count: 0,
    eventWaitMs: 0,
    protectionMs: 0,
  },
) => {
  if (!Number.isSafeInteger(blocks) || blocks < 1)
    throw new Error(
      `Stack staging commits at least one whole block, not ${blocks.toString()} blocks`,
    );
  if (!Number.isSafeInteger(events.count) || events.count < 0)
    throw new Error(
      `Stack staging publishes a whole number of user events, not ${events.count.toString()}`,
    );
  const commitMs = timing.transactionAllowanceMs;
  const attestationMs = 2 * timing.transactionAllowanceMs;
  const eventsMs =
    events.count === 0
      ? 0
      : events.count * USER_EVENT_TX_TTL_MS +
        (events.count - 1) * protectedPredecessorHoldMs(events.protectionMs) +
        events.eventWaitMs;
  return blocks * (attestationMs + commitMs) + eventsMs;
};

/**
 * Operator-2's shift work for one family's staging under the deployment
 * profile's event wait. Refuses, naming the family, before any L1 work when
 * no shift of the profile holds it: our node would commit in between and the
 * fault's commit would be rejected.
 */
export const stackStagingShiftCover = (
  input: Readonly<{
    category: string;
    timing: Pick<JourneyTiming, "transactionAllowanceMs">;
    historyBlocks: number;
    userEvents: number;
    /** The deployment's event-history protection duration. */
    eventHistoryProtectionMs: number;
    /** The deployment profile's timing. */
    profile: Readonly<{ event_wait_ms: number; operator_shift_ms: number }>;
  }>,
) => {
  const { profile } = input;
  const coverMs = stackStagingShiftWorkMs(
    input.timing,
    1 + input.historyBlocks,
    {
      count: input.userEvents,
      eventWaitMs: profile.event_wait_ms,
      protectionMs: input.eventHistoryProtectionMs,
    },
  );
  if (coverMs > profile.operator_shift_ms)
    throw new Error(
      `${input.category} staging holds operator-2's shift for ${coverMs.toString()} ms (${(1 + input.historyBlocks).toString()} blocks, ${input.userEvents.toString()} user events and one ${profile.event_wait_ms.toString()} ms event wait), longer than this profile's ${profile.operator_shift_ms.toString()} ms shift; the drill cannot stage it on this stack`,
    );
  return coverMs;
};

export const transitionTraceJourneyTiming = (cadence: JourneyCadence) =>
  journeyTimingForPlan(TRANSITION_TRACE_JOURNEY_PLAN, cadence);

export const genericJourneyTiming = (cadence: JourneyCadence) =>
  journeyTimingForPlan(GENERIC_JOURNEY_PLAN, cadence);

/**
 * The audited plan for transitionTrace, the undefended-case plan for
 * validationTraceDispute, and the finite generic plan for every other family.
 * Refuses, naming the family and profile, a family the deployment's profile
 * leaves out of its catalogue: its watcher never proves it, so a journey
 * would stage the fault on L1 and only then fail.
 */
export const journeyTimingForCategory = (
  category: string,
  cadence: JourneyCadence,
) => {
  const profile = cadence.deploymentProfile;
  if (profile?.excluded_fraud_proof_categories.includes(category))
    throw new Error(
      `${category} is not installed on deployment profile ${profile.name}: the profile leaves it out of its fraud-proof catalogue, so the drill cannot run it on this deployment`,
    );
  return category === TRANSITION_TRACE_JOURNEY_PLAN.category
    ? transitionTraceJourneyTiming(cadence)
    : category === VALIDATION_TRACE_DISPUTE_JOURNEY_PLAN.category
      ? journeyTimingForPlan(VALIDATION_TRACE_DISPUTE_JOURNEY_PLAN, cadence)
      : genericJourneyTiming(cadence);
};

/**
 * A family's whole time limit on an attached stack (`journey.test.ts`): its
 * computed budget, plus up to two of operator-2's shifts, since the drill
 * stages only in operator-2's shift: up to one waiting for its turn and one
 * holding the staging. A refusal-only family's age-out (two attestation
 * timeouts) fits in the budget it replaces.
 */
export const stackJourneyTimeoutMs = (
  timing: Pick<JourneyTiming, "journeyTimeoutMs">,
  profile: Readonly<{ operator_shift_ms: number }>,
) => {
  const limitMs = timing.journeyTimeoutMs + 2 * profile.operator_shift_ms;
  if (!Number.isSafeInteger(limitMs) || limitMs > MAX_TIMER_MS)
    throw new Error(
      `A stack journey limit of ${limitMs.toString()} ms exceeds the supported timer range`,
    );
  return limitMs;
};

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
