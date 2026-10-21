/** The validation-trace dispute's wait on its counterparty: a case of ours
 * that nobody has defended waits for a Defend until its defense deadline,
 * computed from the case datum with the case validator's own formula. The
 * run reports that wait as `awaiting_counterparty` with the deadline, so the
 * session hands its turn back at once and the watcher treats the objective as
 * healthy until the deadline passes. A defended thread of ours waits the same
 * way on its defender's move, until the defender's clock runs out.
 *
 * Four other states are named for readiness, so a session hands its turn
 * back at once instead of polling into a continuation stall: a thread that
 * needs a one-step resolution the runner does not drive, a thread lost, a
 * case a defender won, and a move whose fee would pass the funding permit's
 * per-transaction fee ceiling, the last an alarm. */
import {
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
  MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";

import type {
  FraudProofWorkflowNamedState,
  FraudProofWorkflowRunResult,
} from "../workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { VALIDATION_TRACE_MOVE_FEE_OVER_CAP } from "./move-fee.js";
import {
  VALIDATION_TRACE_DEFENDER_WON,
  VALIDATION_TRACE_ONE_STEP_REQUIRED,
  VALIDATION_TRACE_THREAD_AWAITING,
  VALIDATION_TRACE_THREAD_LOST,
} from "./threads.js";

/** Our case exists and nobody has defended it yet. */
export const VALIDATION_TRACE_CASE_OPEN = "validation-trace-case-open";

/**
 * `defense_deadline` (validation-case-v1): the last Defend lands with its
 * lower bound at `started_at + C`; a close whose lower bound is past
 * `started_at + C + L_d` follows every Defend.
 */
export const validationTraceDefenseDeadlineMs = (caseDatum: SDK.Case): number =>
  Number(caseDatum.started_at) +
  MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS +
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS;

const CASE_OPEN_REASON = new RegExp(
  `^${VALIDATION_TRACE_CASE_OPEN}: case [0-9a-f]+ is open at ([0-9a-f]{64}#[0-9]+) until its defense deadline ([0-9]+)$`,
  "u",
);

const THREAD_AWAITING_REASON = new RegExp(
  `^${VALIDATION_TRACE_THREAD_AWAITING}: thread [0-9a-f]+ at ([0-9a-f]{64}#[0-9]+) awaits its defender until ([0-9]+)$`,
  "u",
);

/** The pending reason of an undefended case of ours. */
export const validationTraceCaseOpenReason = ({
  caseUnit,
  outRef,
  caseDatum,
}: {
  readonly caseUnit: string;
  readonly outRef: string;
  readonly caseDatum: SDK.Case;
}): string =>
  `${VALIDATION_TRACE_CASE_OPEN}: case ${caseUnit} is open at ${outRef} until its defense deadline ${validationTraceDefenseDeadlineMs(caseDatum).toString()}`;

export type ValidationTraceAwaitingCounterparty = Omit<
  Extract<
    FraudProofWorkflowRunResult,
    { readonly kind: "pending" | "stalled" }
  >,
  "kind"
> & {
  readonly kind: "awaiting_counterparty";
  /**
   * When this side may move again, in ms: the case's defense deadline, or
   * the end of a thread's defender clock.
   */
  readonly responseDeadline: number;
  /** The case output a Defend would spend, or the thread awaiting a move. */
  readonly outRef: string;
};

/**
 * A run pending on an undefended case of ours, or on a thread's defender, as
 * `awaiting_counterparty` with its deadline; every other result unchanged.
 */
export const awaitingValidationTraceCounterparty = (
  result: FraudProofWorkflowRunResult,
): FraudProofWorkflowRunResult | ValidationTraceAwaitingCounterparty => {
  if (result.kind !== "pending" || result.resumeOnObservation === true)
    return result;
  const match =
    CASE_OPEN_REASON.exec(result.reason) ??
    THREAD_AWAITING_REASON.exec(result.reason);
  if (match === null) return result;
  return Object.freeze({
    ...result,
    kind: "awaiting_counterparty",
    outRef: match[1]!,
    responseDeadline: Number(match[2]!),
  });
};

/**
 * The run states named for readiness: a thread at ReadyForOneStep waits on
 * a one-step resolution the runner does not drive, a lost thread has no move
 * of ours left, a case a defender won has none either, and a wallet-paid
 * move whose fee would pass the funding permit's per-transaction fee ceiling
 * cannot be sent at all, which the operator must see.
 */
const NAMED_STATES: readonly FraudProofWorkflowNamedState[] = Object.freeze([
  Object.freeze({ name: VALIDATION_TRACE_ONE_STEP_REQUIRED, alarm: false }),
  Object.freeze({ name: VALIDATION_TRACE_THREAD_LOST, alarm: false }),
  Object.freeze({ name: VALIDATION_TRACE_DEFENDER_WON, alarm: false }),
  Object.freeze({ name: VALIDATION_TRACE_MOVE_FEE_OVER_CAP, alarm: true }),
]);

/**
 * A run of the dispute as its caller sees it: a wait on the counterparty as
 * `awaiting_counterparty`, a named state as a pending run that hands its
 * turn back at once under that state, and every other result unchanged.
 */
export const validationTraceRunState = (
  result: FraudProofWorkflowRunResult,
): FraudProofWorkflowRunResult | ValidationTraceAwaitingCounterparty => {
  const awaiting = awaitingValidationTraceCounterparty(result);
  if (awaiting !== result || result.kind !== "pending") return awaiting;
  const namedState = NAMED_STATES.find(({ name }) =>
    result.reason.startsWith(`${name}: `),
  );
  return namedState === undefined
    ? result
    : Object.freeze({ ...result, resumeOnObservation: true, namedState });
};
