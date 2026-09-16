import { readFile } from "node:fs/promises";
import { isAbsolute, join } from "node:path";

import {
  buildMidgardLedgerOutputScanTrace,
  buildMidgardLedgerOutputValueTrace,
} from "@al-ft/midgard-core";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
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

export type TransitionTraceJourneyCadence = JourneyCadence;

export const HEALTHY_REPLAY_BLOCK_ALLOWANCE_MS = 10_000;
const MAX_TIMER_MS = 2_147_483_647;

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
     * The one release-finality wait in a journey: the completed terminal is
     * re-observed at the release depth and stamped as the finalized anchor.
     * The terminal is already on chain, so this is a single confirmation
     * window at the release depth plus its RPC allowance.
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

const object = (value: unknown, label: string): Record<string, unknown> => {
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

/** Safe at Vitest module load: reads public configuration only, without providers or keys. */
export const readJourneyCadence = async (
  runDirectory: string,
  options: ReadJourneyTimingOptions = {},
): Promise<JourneyCadence> => {
  if (!isAbsolute(runDirectory))
    throw new Error("Journey run directory must be absolute");
  const [genesisBytes, manifestBytes] = await Promise.all([
    readFile(join(runDirectory, "genesis/shelley-genesis.json"), "utf8"),
    readFile(join(runDirectory, "deploymentInfo/manifest.json"), "utf8"),
  ]);
  const genesis = object(JSON.parse(genesisBytes), "Shelley genesis");
  const manifest = verifyFinalizedDeploymentManifest(JSON.parse(manifestBytes));
  const finality = object(manifest.l1Finality, "Deployment finality policy");
  const confirmationDepth = finality.confirmationDepth;
  if (
    typeof genesis.slotLength !== "number" ||
    typeof genesis.activeSlotsCoeff !== "number" ||
    typeof confirmationDepth !== "number"
  ) {
    throw new Error(
      "Journey configuration omitted numeric cadence or finality depth",
    );
  }
  if (
    options.authenticatedConfirmationDepth !== undefined &&
    options.authenticatedConfirmationDepth !== confirmationDepth
  ) {
    throw new Error(
      "Journey timing differs from authenticated release finality depth",
    );
  }
  return {
    slotLengthSeconds: genesis.slotLength,
    activeSlotsCoeff: genesis.activeSlotsCoeff,
    confirmationDepth,
    actionDepth: options.actionDepth,
    fixtureStagingAllowanceMs: options.fixtureStagingAllowanceMs,
  };
};

/** The audited plan for transitionTrace; the finite generic plan for every other family. */
export const readJourneyTiming = async (
  runDirectory: string,
  category: string,
  options: ReadJourneyTimingOptions = {},
) =>
  journeyTimingForCategory(
    category,
    await readJourneyCadence(runDirectory, options),
  );

export const readTransitionTraceJourneyTiming = async (
  runDirectory: string,
  options: ReadJourneyTimingOptions = {},
) => readJourneyTiming(runDirectory, "transitionTrace", options);

export interface JourneyTimingTip {
  blockNo: number;
  slot: number;
  blockHash: string;
}

/** Same atomic chain-sync tip query used by the production Ogmios source. */
const readJourneyTimingTip = async (
  runDirectory: string,
): Promise<JourneyTimingTip> => {
  const lines = (await readFile(join(runDirectory, "run.env"), "utf8")).split(
    "\n",
  );
  const port = lines
    .find((line) => line.startsWith("MIDGARD_PHASE4_OGMIOS_PORT="))
    ?.split("=")[1];
  if (
    port === undefined ||
    !/^\d+$/.test(port) ||
    Number(port) < 1 ||
    Number(port) > 65535
  )
    throw new Error("Journey timing requires its actual local Ogmios port");
  const socket = new WebSocket(`ws://127.0.0.1:${port}`);
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    return await new Promise<JourneyTimingTip>((resolve, reject) => {
      timer = setTimeout(
        () => reject(new Error("Journey timing tip query timed out")),
        10_000,
      );
      socket.addEventListener(
        "error",
        () => reject(new Error("Journey timing tip query failed")),
        { once: true },
      );
      socket.addEventListener(
        "close",
        () =>
          reject(
            new Error("Journey timing tip query closed before its response"),
          ),
        { once: true },
      );
      socket.addEventListener(
        "open",
        () =>
          socket.send(
            JSON.stringify({
              jsonrpc: "2.0",
              id: "journey-timing-tip",
              method: "findIntersection",
              params: { points: ["origin"] },
            }),
          ),
        { once: true },
      );
      socket.addEventListener(
        "message",
        (event) => {
          try {
            if (typeof event.data !== "string" || event.data.length > 16_384)
              throw new Error(
                "Journey timing tip response is not bounded JSON",
              );
            const response = object(
              JSON.parse(event.data),
              "Ogmios timing response",
            );
            if (
              response.id !== "journey-timing-tip" ||
              response.error !== undefined
            )
              throw new Error("Ogmios refused the journey timing tip query");
            const result = object(
              response.result,
              "Ogmios timing intersection",
            );
            if (result.intersection !== "origin")
              throw new Error("Ogmios timing query did not intersect origin");
            const tip = object(result.tip, "Ogmios timing tip");
            if (
              typeof tip.height !== "number" ||
              !Number.isSafeInteger(tip.height) ||
              tip.height < 0 ||
              typeof tip.slot !== "number" ||
              !Number.isSafeInteger(tip.slot) ||
              tip.slot < 0 ||
              typeof tip.id !== "string" ||
              !/^[0-9a-f]{64}$/.test(tip.id)
            )
              throw new Error(
                "Ogmios timing tip omitted actual block number, slot, or hash",
              );
            resolve({ blockNo: tip.height, slot: tip.slot, blockHash: tip.id });
          } catch (cause) {
            reject(cause instanceof Error ? cause : new Error(String(cause)));
          }
        },
        { once: true },
      );
    });
  } finally {
    if (timer !== undefined) clearTimeout(timer);
    socket.close();
  }
};

export const journeyExecutionTiming = (
  timing: JourneyTiming,
  capturedTip: JourneyTimingTip,
  capturedAtMonotonicMs: number,
) => {
  const beforeReplayAllowanceMs =
    timing.journeyTimeoutMs - timing.allowances.healthySuccessorObservationMs;
  const healthyReplay = healthyJourneyReplayTiming({
    ...timing.cadence,
    tipBlockNo: capturedTip.blockNo,
    beforeReplayAllowanceMs,
    // Native recorder startup and final operations/provider checks remain bounded.
    observationAllowanceMs: 90_000,
  });
  const journeyTimeoutMs = beforeReplayAllowanceMs + healthyReplay.timeoutMs;
  if (
    !Number.isSafeInteger(journeyTimeoutMs) ||
    journeyTimeoutMs > MAX_TIMER_MS ||
    !Number.isFinite(capturedAtMonotonicMs) ||
    capturedAtMonotonicMs < 0
  )
    throw new Error(
      "Journey execution timing exceeds the supported timer range",
    );
  return Object.freeze({
    ...timing,
    capturedTip: Object.freeze({ ...capturedTip }),
    capturedAtMonotonicMs,
    healthyReplay,
    journeyTimeoutMs,
    deadlineMonotonicMs: capturedAtMonotonicMs + journeyTimeoutMs,
  });
};

export const transitionTraceJourneyExecutionTiming = journeyExecutionTiming;

export type JourneyExecutionTiming = ReturnType<typeof journeyExecutionTiming>;
export type TransitionTraceJourneyExecutionTiming = JourneyExecutionTiming;

/** Suite-load planning opens only a bounded read-only RPC and reads public configuration. */
export const readJourneyExecutionTiming = async (
  runDirectory: string,
  category: string,
  options: ReadJourneyTimingOptions = {},
) => {
  const timing = await readJourneyTiming(runDirectory, category, options);
  const capturedTip = await readJourneyTimingTip(runDirectory);
  return journeyExecutionTiming(timing, capturedTip, performance.now());
};

export const readTransitionTraceJourneyExecutionTiming = async (
  runDirectory: string,
  options: ReadJourneyTimingOptions = {},
) => readJourneyExecutionTiming(runDirectory, "transitionTrace", options);
