import {
  REGISTRATION_DURATION_MS,
  USER_EVENT_TX_TTL_MS,
} from "@al-ft/midgard-sdk";
import { protectedPredecessorHoldMs } from "midgard-watcher/tests/support/history-predecessor-hold";

import { stackUserEventFloorOffsetMs } from "./stack-user-event-floor.js";

/**
 * Every automatic family's plan. A family's proof chain takes as many
 * dependent transactions as its data needs (scan steps loop over the data,
 * per-purpose routes vary), so no plan counts them: the correction ends only
 * when the workflow journal stops making progress by the watcher's own rule
 * (`verifyJourneyCorrection`), and the family's time limit does not run
 * while it waits (`journeyFamilyDeadline`). The plan budgets the rest of the
 * journey.
 */
export const GENERIC_JOURNEY_PLAN = Object.freeze({
  category: "generic",
  // Registration, activation, appointment, and honest commitment.
  successorTransactions: 4,
});

/**
 * validationTraceDispute: the generic plan, plus the one wait no
 * transaction of ours ends. Operator-2's node is stopped through the fault
 * phase, so nobody defends the case the watcher opens. The case validator
 * lets the watcher close it Convicted only past its defense deadline, the
 * side clock C plus the longest move validity range L_d after the case
 * starts (`validationTraceDefenseDeadlineMs`, validation-case-v1), and the
 * watcher journals nothing while it waits.
 */
export const VALIDATION_TRACE_DISPUTE_JOURNEY_PLAN = Object.freeze({
  category: "validationTraceDispute",
  successorTransactions: GENERIC_JOURNEY_PLAN.successorTransactions,
  awaitsUndefendedCase: true,
});

export type JourneyTimingPlan =
  | typeof GENERIC_JOURNEY_PLAN
  | typeof VALIDATION_TRACE_DISPUTE_JOURNEY_PLAN;

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
  /**
   * The family's staging, when it runs on the family's clock with no other
   * term covering it (a devnet's): what it commits and publishes, and the
   * deployment's event wait and event-history protection. A stack's limit
   * covers staging with its shift cover instead (`stackJourneyTimeoutMs`).
   */
  fixtureStaging?: Readonly<{
    historyBlocks?: number;
    userEvents?: number;
    eventWaitMs: number;
    /** The longer of the deposit and withdrawal histories' protection. */
    eventHistoryProtectionMs: number;
  }>;
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

/**
 * The watcher follower's cost to replay one L1 block, as an allowance. Measured
 * on the local devnet (seven catch-ups from the L1 origin, 1,119 blocks
 * including every deployment publication): at most 22.6 ms per block averaged
 * over any 0.5 s readiness interval, and no block took longer than the 519 ms
 * between two readiness reports. One second covers that bound with margin and
 * keeps replay utilization at f or below for one-second slots.
 */
export const HEALTHY_REPLAY_BLOCK_ALLOWANCE_MS = 1_000;

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

export const journeyTimingForPlan = (
  plan: JourneyTimingPlan,
  {
    slotLengthSeconds,
    activeSlotsCoeff,
    confirmationDepth,
    actionDepth = confirmationDepth,
    fixtureStaging,
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
    actionDepth > confirmationDepth ||
    actionDepth < 1
  ) {
    throw new Error("Invalid journey cadence");
  }
  const idealDepthMs = (depth: number) =>
    (depth * slotLengthSeconds * 1000) / activeSlotsCoeff;
  // An action waits for inclusion at the action depth, not for release finality.
  const expectedConfirmationPerTransactionMs = idealDepthMs(actionDepth);
  // Two times the ideal cadence mean is a finite allowance, not an inclusion
  // guarantee. Node outages or sustained underproduction still end at the
  // family's limit (`journeyFamilyDeadline`).
  const confirmationAllowancePerTransactionMs = Math.ceil(
    2 * expectedConfirmationPerTransactionMs,
  );
  const transactionBuildMs = 120_000;
  const transactionRpcMs = 30_000;
  const transactionAllowanceMs =
    confirmationAllowancePerTransactionMs +
    transactionBuildMs +
    transactionRpcMs;
  const allowances = Object.freeze({
    transactionBuildMs,
    transactionRpcMs,
    authorityStartupMs: 30_000,
    watcherStartupMs: 600_000,
    automaticDecisionMs: 900_000,
    healthySuccessorObservationMs: 1_800_000,
    successorRegistrationMs: Number(REGISTRATION_DURATION_MS),
    /**
     * The family's staging, from its plan (`stackStagingShiftWorkMs`, the
     * same work a stack's shift holds): its history blocks and fault, each
     * committed and attested, and its user events. Zero when the limit
     * covers staging otherwise.
     */
    fixtureStagingMs:
      fixtureStaging === undefined
        ? 0
        : stackStagingShiftWorkMs(
            { transactionAllowanceMs },
            1 + (fixtureStaging.historyBlocks ?? 0),
            {
              count: fixtureStaging.userEvents ?? 0,
              eventWaitMs: fixtureStaging.eventWaitMs,
              protectionMs: fixtureStaging.eventHistoryProtectionMs,
            },
          ),
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
     * case's start to that deadline (`journeyProgressClock`); the family's
     * limit does not run meanwhile.
     */
    undefendedCaseMs: undefendedCaseWaitMs(plan, disputeTiming),
  });
  // The correction, with any undefended case it waits out, is not budgeted
  // here: the progress gate ends it, and the family's limit pauses for it.
  const journeyTimeoutMs =
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
    /** Build, submit, and confirm one dependent transaction. */
    transactionAllowanceMs,
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
 * history protection; and then the one event wait. None of its events is
 * built before `floorMs` into the shift (`stackUserEventFloorOffsetMs`); its
 * history blocks, committed before its events, run meanwhile. A devnet's
 * staging is the same work, with no floor, on the family's own clock
 * (`fixtureStagingMs`).
 */
export const stackStagingShiftWorkMs = (
  timing: Readonly<{ transactionAllowanceMs: number }>,
  blocks: number,
  events: Readonly<{
    count: number;
    eventWaitMs: number;
    /** The deployment's event-history protection duration. */
    protectionMs: number;
    /** How far into the shift the first event may be built; none by default. */
    floorMs?: number;
  }> = {
    count: 0,
    eventWaitMs: 0,
    protectionMs: 0,
  },
): number => {
  if (!Number.isSafeInteger(blocks) || blocks < 1)
    throw new Error(
      `Stack staging commits at least one whole block, not ${blocks.toString()} blocks`,
    );
  if (!Number.isSafeInteger(events.count) || events.count < 0)
    throw new Error(
      `Stack staging publishes a whole number of user events, not ${events.count.toString()}`,
    );
  const floorMs = events.floorMs ?? 0;
  if (!Number.isSafeInteger(floorMs) || floorMs < 0)
    throw new Error(
      `A user event floor is a whole number of milliseconds, not ${floorMs.toString()}`,
    );
  const commitMs = timing.transactionAllowanceMs;
  const attestationMs = 2 * timing.transactionAllowanceMs;
  const blockMs = attestationMs + commitMs;
  if (events.count === 0) return blocks * blockMs;
  const eventsMs =
    events.count * USER_EVENT_TX_TTL_MS +
    (events.count - 1) * protectedPredecessorHoldMs(events.protectionMs) +
    events.eventWaitMs;
  return Math.max((blocks - 1) * blockMs, floorMs) + eventsMs + blockMs;
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
    profile: Readonly<{
      event_wait_ms: number;
      operator_shift_ms: number;
      user_events_negligence_timeout_ms: number;
    }>;
  }>,
) => {
  const { profile } = input;
  const floorMs = stackUserEventFloorOffsetMs({
    eventTtlMs: USER_EVENT_TX_TTL_MS,
    eventWaitMs: profile.event_wait_ms,
    negligenceTimeoutMs: profile.user_events_negligence_timeout_ms,
    shiftDurationMs: profile.operator_shift_ms,
  });
  const coverMs = stackStagingShiftWorkMs(
    input.timing,
    1 + input.historyBlocks,
    {
      count: input.userEvents,
      eventWaitMs: profile.event_wait_ms,
      protectionMs: input.eventHistoryProtectionMs,
      floorMs,
    },
  );
  if (coverMs > profile.operator_shift_ms)
    throw new Error(
      `${input.category} staging holds operator-2's shift for ${coverMs.toString()} ms (${(1 + input.historyBlocks).toString()} blocks, ${input.userEvents.toString()} user events built from ${floorMs.toString()} ms into the shift and one ${profile.event_wait_ms.toString()} ms event wait), longer than this profile's ${profile.operator_shift_ms.toString()} ms shift; the drill cannot stage it on this stack`,
    );
  return coverMs;
};

export const genericJourneyTiming = (cadence: JourneyCadence) =>
  journeyTimingForPlan(GENERIC_JOURNEY_PLAN, cadence);

/**
 * The undefended-case plan for validationTraceDispute and the generic plan
 * for every other family.
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
  return category === VALIDATION_TRACE_DISPUTE_JOURNEY_PLAN.category
    ? journeyTimingForPlan(VALIDATION_TRACE_DISPUTE_JOURNEY_PLAN, cadence)
    : genericJourneyTiming(cadence);
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
  /**
   * The family's staged plan, budgeted in its limit (`fixtureStagingMs`)
   * with the manifest's event wait and event-history protection. Only a
   * devnet passes it: a stack's limit covers staging with its shift cover.
   */
  fixtureStaging?: Readonly<{ historyBlocks?: number; userEvents?: number }>;
}
