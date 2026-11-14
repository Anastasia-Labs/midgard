import type * as SDK from "@al-ft/midgard-sdk";

import { eventKeyPhase } from "./reconstruct.js";

type BandCounts = Pick<
  SDK.Header,
  | "withdrawalCount"
  | "forcedTransactionCount"
  | "l2TransactionCount"
  | "depositCount"
  | "totalEventCount"
  | "transitionStepCount"
>;

/**
 * Twin of `proof.ak` `phase_for_step_index`: the header counts lay the trace
 * out in bands (withdrawals, then forced transactions, then L2 transactions,
 * then deposits). The on-chain function fails its `expect`s outside
 * `[0, total_event_count)`, past the last deposit, or on a negative count, so
 * no band proof exists there; this returns `undefined` for exactly those
 * inputs and leaves them to the count checks.
 */
export const phaseForStepIndex = (
  header: BandCounts,
  stepIndex: bigint,
): SDK.TransitionPhase | undefined => {
  if (
    header.withdrawalCount < 0n ||
    header.forcedTransactionCount < 0n ||
    header.l2TransactionCount < 0n ||
    header.depositCount < 0n ||
    header.totalEventCount < 0n ||
    header.transitionStepCount < 0n ||
    stepIndex < 0n ||
    stepIndex >= header.totalEventCount
  ) {
    return undefined;
  }
  const withdrawalsEnd = header.withdrawalCount;
  const forcedEnd = withdrawalsEnd + header.forcedTransactionCount;
  const l2End = forcedEnd + header.l2TransactionCount;
  if (stepIndex < withdrawalsEnd) return "Withdrawal";
  if (stepIndex < forcedEnd) return "ForcedTransaction";
  if (stepIndex < l2End) return "L2Transaction";
  return stepIndex < l2End + header.depositCount ? "Deposit" : undefined;
};

export type TraceStepPhaseFault = {
  readonly invariant:
    | "event_to_step_phase_band"
    | "trace_phase_matches_event_key";
  readonly diagnostic: string;
};

/**
 * Twin of `proof.ak` `trace_has_bad_phase`: a trace step is provably
 * misplaced when its phase differs from the band its index falls in, or from
 * the phase its own event key names. The on-chain disjunction evaluates the
 * band first, so neither disjunct is provable where the band is undefined.
 */
export const traceStepPhaseFault = (
  header: BandCounts,
  step: Pick<SDK.TransitionStep, "step_index" | "phase" | "event_key">,
): TraceStepPhaseFault | undefined => {
  const band = phaseForStepIndex(header, step.step_index);
  if (band === undefined) return undefined;
  const at = `Trace step ${step.step_index.toString()} has phase ${step.phase}`;
  if (step.phase !== band) {
    return {
      invariant: "event_to_step_phase_band",
      diagnostic: `${at}, but the header counts place it in the ${band} band.`,
    };
  }
  const keyPhase = eventKeyPhase(step.event_key);
  if (keyPhase !== step.phase) {
    return {
      invariant: "trace_phase_matches_event_key",
      diagnostic: `${at}, but its event key names phase ${keyPhase}.`,
    };
  }
  return undefined;
};
