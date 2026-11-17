import {
  requireExactKeys,
  requireRecord,
} from "./journal.fraud-proof-workflow-journal-event.js";
import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowIdentity,
  normalizeFraudProofWorkflowIdentity,
} from "./journal.fraud-proof-workflow-terminal.js";
export const requireTxHash = (value: string, field: string): void => {
  if (!/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
};
export const requireOutRef = (value: string, field: string): void => {
  if (!/^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${field} must be a canonical transaction outRef`);
  }
};

export const expectedWorkflowIdentityMatches = (
  workflowId: string,
  expectedIdentity?: FraudProofWorkflowIdentity,
): boolean =>
  expectedIdentity === undefined ||
  computeFraudProofWorkflowId(
    normalizeFraudProofWorkflowIdentity(expectedIdentity),
  ) === workflowId;

/** A journal entry's identity target: a state-queue header or a settlement
 * claim, each with exactly its own keys. */
export const requireJournalTarget = (
  value: unknown,
  sequence: number,
): void => {
  const target = requireRecord(
    value,
    `journal entry ${sequence.toString()} identity target`,
  );
  if (target.kind === "state_queue_header") {
    requireExactKeys(
      target,
      ["kind", "headerHash"],
      `journal entry ${sequence.toString()} state-queue target`,
    );
  } else if (target.kind === "settlement_claim") {
    requireExactKeys(
      target,
      ["kind", "claimId"],
      `journal entry ${sequence.toString()} settlement target`,
    );
  } else {
    throw new Error(
      `journal entry ${sequence.toString()} has an unknown target kind`,
    );
  }
};
