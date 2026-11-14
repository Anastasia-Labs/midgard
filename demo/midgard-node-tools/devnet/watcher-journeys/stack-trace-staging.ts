import * as SDK from "@al-ft/midgard-sdk";
import { protectedPredecessorHoldMs } from "midgard-watcher/tests/support/history-predecessor-hold";
import {
  EMPTY_PREDECESSOR_INTERVAL_MS,
  type PublishedDepositTraceAttachedStack,
  stagePublishedDepositTrace,
} from "midgard-watcher/tests/support/published-deposit-trace";
import { publishedDepositTraceQueue } from "midgard-watcher/tests/support/published-deposit-trace.queue-tail";

import type { JourneyFixtureStage } from "./fixture.js";
import { lateShiftMinimumLeadMs } from "./honest-successor.observables.js";
import { readStackJourneyTiming } from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import type { StackJourneyContext } from "./stack-context.js";
import { stackJourneyDa } from "./stack-journey-da.js";
import {
  stackLateShiftGate,
  stackUserEventPublicationGate,
} from "./stack-late-shift.js";

const CATEGORY = "transitionTrace";

/**
 * Operator-2's shift work for the transition trace on an attached stack,
 * from its shift wait to the end of its fault's commit. The trace first
 * publishes a predecessor deposit: its build holds out the protection of the
 * history before it, and the empty block may close only once that deposit
 * is due. The trace's own deposit holds out the predecessor deposit's
 * protection; the fault's commit then waits for the trace deposit's
 * admission and for the empty block's commit and attestation (an init and
 * an apply). The commit itself starts no later than the shortest late-shift
 * lead before the shift ends. Refuses, before any L1 work, when one shift of
 * the profile cannot hold it.
 */
export const stackTraceShiftWorkMs = (
  input: Readonly<{
    /** How long one dependent transaction takes to land. */
    transactionAllowanceMs: number;
    eventWaitMs: number;
    /** The deposit history's protection duration. */
    eventHistoryProtectionMs: number;
    operatorShiftMs: number;
  }>,
) => {
  const txMs = input.transactionAllowanceMs;
  const holdMs = protectedPredecessorHoldMs(input.eventHistoryProtectionMs);
  // A deposit's validity ends within its time to live, slot-aligned.
  const validityMs = SDK.USER_EVENT_TX_TTL_MS + 1_000;
  const admissionMs = validityMs + input.eventWaitMs;
  const traceDepositAfterMs =
    holdMs +
    Math.max(
      txMs,
      admissionMs - EMPTY_PREDECESSOR_INTERVAL_MS + 1_000,
      validityMs + holdMs,
    );
  const faultCommitAfterMs = Math.max(4 * txMs, admissionMs);
  const workMs =
    traceDepositAfterMs + faultCommitAfterMs + lateShiftMinimumLeadMs(txMs);
  if (workMs > input.operatorShiftMs)
    throw new Error(
      `${CATEGORY} staging holds operator-2's shift for ${workMs.toString()} ms (a predecessor deposit, the trace deposit's ${input.eventWaitMs.toString()} ms event wait and a late-shift fault commit), longer than this profile's ${input.operatorShiftMs.toString()} ms shift; the drill cannot stage it on this stack`,
    );
  return workMs;
};

/**
 * The transition trace's attached stack: the stack committee attests its
 * blocks, it holds operator-2's shift for `stackTraceShiftWorkMs`, its trace
 * deposit, which only the fault delivers, waits for the publication floor
 * (`stackUserEventPublicationGate`), and its fault commits in the late-shift
 * window. Refuses, before any L1 work, when
 * the late-shift lead is unset or too short, or when no shift holds the
 * trace.
 */
export const stackTraceAttachedStack = async (
  context: StackJourneyContext,
  input: Pick<JourneyFixtureStage, "readConfirmedTransaction" | "onStage"> &
    Readonly<{ directory: string; checkpointPath: string }>,
): Promise<PublishedDepositTraceAttachedStack> => {
  const { onStage } = input;
  const timing = await readStackJourneyTiming(
    {
      nodeConfigPath: context.layout.node.nodeConfigPath,
      manifestPath: context.layout.manifestPath,
    },
    CATEGORY,
    { actionDepth: JOURNEY_ACTION_DEPTH },
  );
  const beforeFaultCommit = stackLateShiftGate(context, {
    category: CATEGORY,
    commitAllowanceMs: timing.transactionAllowanceMs,
    directory: input.directory,
    onStage,
  });
  // The cover already lets the trace deposit's build run as late as its
  // `traceDepositAfterMs` into the shift, which is past the floor.
  const userEvents = stackUserEventPublicationGate(context, {
    commitAllowanceMs: timing.transactionAllowanceMs,
    onStage,
  });
  const { recipe } = SDK.requireEventHistoryContracts(
    context.deployment.contracts,
  ).deposit;
  const shiftWorkMs = stackTraceShiftWorkMs({
    transactionAllowanceMs: timing.transactionAllowanceMs,
    eventWaitMs: SDK.EVENT_WAIT_DURATION_MS,
    eventHistoryProtectionMs: Number(recipe.protectionDurationMs),
    operatorShiftMs: Number(SDK.SHIFT_DURATION_MS),
  });
  const da = stackJourneyDa(context, {
    readConfirmedTransaction: input.readConfirmedTransaction,
    onStage,
  });
  return {
    attest: (block) =>
      da.publish(block, { role: "history", expectation: "attestation" }, [
        input.checkpointPath,
      ]),
    shiftWorkMs,
    beforeFaultCommit,
    beforeTraceDeposit: (operatorVkey, tailEndTimeMs) =>
      userEvents.beforeEvent(operatorVkey, BigInt(tailEndTimeMs)),
  };
};

/**
 * The transition trace staged on an attached stack's live tail. The trace
 * reads the tail's ledger inside operator-2's shift, after onboarding it.
 * The replay is brought to that tail first, outside the shift, so the read
 * inside it only follows the blocks the stack appended since. A fresh trace
 * reads the queue's current tail; a resumed one the tail it extended, which
 * the drill's own blocks may now follow. A tail without a replayed ledger is
 * refused before operator-2 is onboarded.
 */
export const stageAttachedTransitionTrace = async (
  deployment: Parameters<typeof stagePublishedDepositTrace>[0],
  options: Parameters<typeof stagePublishedDepositTrace>[1],
) => {
  const { readBase, ledgerPairs } = publishedDepositTraceQueue({
    contracts: deployment.contracts,
    lucid: () => deployment.operatorLucid,
    liveTail: options.liveTail,
  });
  if (options.resume === undefined) await readBase();
  else await ledgerPairs(options.resume.predecessor.header.prevHeaderHash);
  return stagePublishedDepositTrace(deployment, options);
};
