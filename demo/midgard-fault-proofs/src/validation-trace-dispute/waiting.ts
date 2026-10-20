/** The validation-trace dispute's wait on its counterparty: a case of ours
 * that nobody has defended waits for a Defend until its defense deadline,
 * computed from the case datum with the case validator's own formula. The
 * run reports that wait as `awaiting_counterparty` with the deadline, so the
 * session hands its turn back at once and the watcher treats the objective as
 * healthy until the deadline passes. */
import {
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
  MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";

import type { FraudProofWorkflowRunResult } from "../workflow/orchestrator.fraud-proof-workflow-run-result.js";

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
  /** When this side may move again: the case's defense deadline, in ms. */
  readonly responseDeadline: number;
  /** The case output the counterparty's Defend would spend. */
  readonly outRef: string;
};

/**
 * A run pending on an undefended case of ours as `awaiting_counterparty`
 * with the case's defense deadline; every other result unchanged.
 */
export const awaitingValidationTraceCounterparty = (
  result: FraudProofWorkflowRunResult,
): FraudProofWorkflowRunResult | ValidationTraceAwaitingCounterparty => {
  if (result.kind !== "pending" || result.resumeOnObservation === true)
    return result;
  const match = CASE_OPEN_REASON.exec(result.reason);
  if (match === null) return result;
  return Object.freeze({
    ...result,
    kind: "awaiting_counterparty",
    outRef: match[1]!,
    responseDeadline: Number(match[2]!),
  });
};
