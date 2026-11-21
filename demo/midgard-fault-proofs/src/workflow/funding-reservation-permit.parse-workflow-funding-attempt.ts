import { parseWorkflowFundingPreparedTransition } from "./funding-reservation-permit.parse-workflow-funding-prepared-transition.js";
import {
  DIGEST,
  exact,
  isPlainObject,
  type WorkflowFundingRecordedAttempt,
  type WorkflowFundingSupersededAttempt,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import { parseSignedWorkflowTransactionRetirement } from "./signed-transaction-retirement.js";

const attemptKeys = ["transition", "workflowId", "intentSequence", "actionId"];

const parseAttempt = (
  record: Readonly<Record<string, unknown>>,
): WorkflowFundingRecordedAttempt => {
  const transition = parseWorkflowFundingPreparedTransition(record.transition);
  if (
    typeof record.workflowId !== "string" ||
    !DIGEST.test(record.workflowId) ||
    typeof record.intentSequence !== "number" ||
    !Number.isSafeInteger(record.intentSequence) ||
    record.intentSequence < 1 ||
    typeof record.actionId !== "string" ||
    record.actionId.length === 0 ||
    record.actionId.trim() !== record.actionId
  )
    throw new Error("funding recorded attempt is malformed");
  return Object.freeze({
    transition,
    workflowId: record.workflowId,
    intentSequence: record.intentSequence,
    actionId: record.actionId,
  });
};

/** A signed attempt as the reservation store records it. */
export const parseWorkflowFundingRecordedAttempt = (
  value: unknown,
): WorkflowFundingRecordedAttempt =>
  parseAttempt(exact(value, attemptKeys, "funding recorded attempt"));

/** A superseded attempt, with its retirement once it has one. */
export const parseWorkflowFundingSupersededAttempt = (
  value: unknown,
): WorkflowFundingSupersededAttempt => {
  const record = exact(
    value,
    [
      ...attemptKeys,
      ...(isPlainObject(value) && "retirement" in value ? ["retirement"] : []),
    ],
    "funding superseded attempt",
  );
  const attempt = parseAttempt(record);
  return record.retirement === undefined
    ? attempt
    : Object.freeze({
        ...attempt,
        retirement: parseSignedWorkflowTransactionRetirement(
          record.retirement,
          attempt.transition.transactionHash,
        ),
      });
};
