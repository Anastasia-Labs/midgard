/**
 * Funded actions whose attempts are mutually exclusive by a protocol input,
 * not by a forced funding input.
 *
 * Owner ruling (whichever lands wins): a replacement must be mutually
 * exclusive with each superseded attempt, which the permit and the
 * reservation store otherwise ensure by forcing a funding input the
 * superseded attempt holds into the replacement. A validation-trace thread
 * move needs none: every attempt at it spends the same thread, and a move
 * spends no funding input of the wallet, so a superseded attempt's wallet
 * inputs are never forced into it. A move can only be built once the case
 * our claim opened exists, which already consumed that claim; no superseded
 * attempt over it can land beside the move.
 */
import {
  isValidationTraceMove,
  VALIDATION_TRACE_DISPUTE_ACTION,
  VALIDATION_TRACE_STAGES,
  type ValidationTraceStage,
} from "../validation-trace-dispute/actions.js";

/**
 * The protocol input every attempt at the funded action `actionInput` of
 * workflow `category` spends (a validation-trace thread move's thread), or
 * null when it has none and the ordinary overlap rule applies. The caller
 * checks the transaction spends it: the reservation store over the signed
 * inputs, the permit through the move authority's walk, which binds the
 * thread to this input and refuses a move that omits it.
 */
export const workflowFundingExclusiveProtocolInput = ({
  category,
  actionInput,
}: {
  readonly category: unknown;
  readonly actionInput: unknown;
}): string | null => {
  if (category !== "validationTraceDispute") return null;
  if (typeof actionInput !== "object" || actionInput === null) return null;
  const { schemaVersion, stage, consumes } = actionInput as Readonly<
    Record<string, unknown>
  >;
  return schemaVersion === VALIDATION_TRACE_DISPUTE_ACTION &&
    (VALIDATION_TRACE_STAGES as readonly unknown[]).includes(stage) &&
    isValidationTraceMove(stage as ValidationTraceStage) &&
    typeof consumes === "string"
    ? consumes
    : null;
};
