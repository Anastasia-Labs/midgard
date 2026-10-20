import {
  handoffKeys,
  parseHandoffIdentity,
} from "./funding-reservation-permit.parse-workflow-funding-prepared-transition.js";
import {
  DIGEST,
  exact,
  isPlainObject,
  type WorkflowFundingAbandonmentHandoff,
  type WorkflowFundingCompletionHandoff,
  type WorkflowFundingJournalHandoff,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  type FraudProofWorkflowJournalEntry,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "./journal.js";
import {
  parseSignedWorkflowTransactionRetirement,
  type SignedWorkflowTransactionRetirement,
} from "./signed-transaction-retirement.js";

export const parseWorkflowFundingAbandonmentHandoff = (
  value: unknown,
): WorkflowFundingAbandonmentHandoff => {
  const record = exact(
    value,
    [...handoffKeys, "submissionIntent", "reconciliation"],
    "funding abandonment handoff",
  );
  const identity = parseHandoffIdentity(record);
  const keys = ["kind", "actionId", "actionInput", "attempt", "txHash"];
  if (
    isPlainObject(record.submissionIntent) &&
    "durableRecovery" in record.submissionIntent
  )
    keys.push("durableRecovery");
  const intent = exact(
    record.submissionIntent,
    keys,
    "funding abandoned submission intent",
  );
  const reconciliation = exact(
    record.reconciliation,
    [
      "kind",
      "actionId",
      "outcome",
      "txHash",
      ...(isPlainObject(record.reconciliation) &&
      "retirement" in record.reconciliation
        ? ["retirement"]
        : []),
    ],
    "funding abandonment reconciliation",
  );
  if (
    intent.kind !== "submission_intent" ||
    typeof intent.actionId !== "string" ||
    intent.actionId.length === 0 ||
    intent.actionId.trim() !== intent.actionId ||
    typeof intent.txHash !== "string" ||
    !DIGEST.test(intent.txHash) ||
    typeof intent.attempt !== "number" ||
    !Number.isSafeInteger(intent.attempt) ||
    intent.attempt < 1 ||
    !isPlainObject(intent.actionInput) ||
    (intent.durableRecovery !== undefined &&
      !isPlainObject(intent.durableRecovery)) ||
    reconciliation.kind !== "reconciled" ||
    reconciliation.outcome !== "not_found" ||
    reconciliation.actionId !== intent.actionId ||
    reconciliation.txHash !== intent.txHash
  )
    throw new Error("funding abandonment changed its exact submission intent");
  return Object.freeze({
    ...identity,
    submissionIntent: Object.freeze({
      kind: "submission_intent",
      actionId: intent.actionId,
      txHash: intent.txHash,
      attempt: intent.attempt,
      actionInput: normalizeJournalJson(
        intent.actionInput,
      ) as JournalJsonObject,
      ...(intent.durableRecovery === undefined
        ? {}
        : {
            durableRecovery: normalizeJournalJson(
              intent.durableRecovery,
            ) as JournalJsonObject,
          }),
    }),
    reconciliation: Object.freeze({
      kind: "reconciled",
      actionId: intent.actionId,
      outcome: "not_found",
      txHash: intent.txHash,
      ...(reconciliation.retirement === undefined
        ? {}
        : {
            retirement: parseSignedWorkflowTransactionRetirement(
              reconciliation.retirement,
              intent.txHash,
            ),
          }),
    }),
  });
};

export const createWorkflowFundingAbandonmentHandoff = (input: {
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly transactionHash: string;
  readonly retirement?: SignedWorkflowTransactionRetirement;
}): WorkflowFundingAbandonmentHandoff => {
  const first = input.entries[0],
    prepared = input.entries[1];
  const intent = [...input.entries]
    .reverse()
    .map(({ event }) => event)
    .find(
      (event) =>
        event.kind === "submission_intent" &&
        event.txHash === input.transactionHash,
    );
  if (
    first === undefined ||
    prepared?.event.kind !== "prepared" ||
    intent?.kind !== "submission_intent"
  )
    throw new Error(
      "funding abandonment requires its exact prepared execution and intent",
    );
  const handoff = parseWorkflowFundingAbandonmentHandoff({
    workflowId: first.workflowId,
    identity: first.identity,
    preparedArtifactDigest: prepared.event.artifactDigest,
    expectedJournalSequence: input.entries.length,
    submissionIntent: intent,
    reconciliation: {
      kind: "reconciled",
      actionId: intent.actionId,
      outcome: "not_found",
      txHash: intent.txHash,
      ...(input.retirement === undefined
        ? {}
        : { retirement: input.retirement }),
    },
  });
  assertWorkflowFundingAbandonmentHandoffJournal({
    handoff,
    entries: input.entries,
  });
  return handoff;
};

/** Returns whether the one intended outcome already reached the durable journal. */
export const assertWorkflowFundingAbandonmentHandoffJournal = (input: {
  readonly handoff: WorkflowFundingAbandonmentHandoff;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
}): boolean => {
  const handoff = parseWorkflowFundingAbandonmentHandoff(input.handoff);
  assertHandoffJournal(handoff, input.entries);
  const prefix = input.entries.slice(0, handoff.expectedJournalSequence);
  const latest = prefix
    .map(({ event }) => event)
    .reverse()
    .find(
      (event) =>
        event.kind === "submission_intent" &&
        event.txHash === handoff.submissionIntent.txHash,
    );
  if (
    latest === undefined ||
    journalJsonDigest(normalizeJournalJson(latest)) !==
      journalJsonDigest(normalizeJournalJson(handoff.submissionIntent))
  )
    throw new Error(
      "funding abandonment differs from its journaled signed intent",
    );
  const intentIndex =
    prefix.length -
    1 -
    [...prefix]
      .reverse()
      .findIndex(
        ({ event }) =>
          event.kind === "submission_intent" &&
          event.txHash === handoff.submissionIntent.txHash,
      );
  const reopenedFromEnd = [...prefix]
    .reverse()
    .findIndex(
      ({ event }) =>
        event.kind === "reobserved" &&
        event.txHash === handoff.submissionIntent.txHash,
    );
  const reopenedIndex =
    reopenedFromEnd < 0 ? -1 : prefix.length - 1 - reopenedFromEnd;
  if (
    prefix
      .slice(Math.max(intentIndex, reopenedIndex) + 1)
      .some(
        ({ event }) =>
          event.kind === "confirmed" ||
          (event.kind === "reconciled" && event.outcome !== "pending"),
      )
  )
    throw new Error(
      "funding abandonment intent was already resolved before its handoff",
    );
  const tail = input.entries
    .slice(handoff.expectedJournalSequence)
    .filter(({ event }) => event.kind !== "stalled");
  if (
    tail.length > 1 ||
    (tail[0] !== undefined &&
      journalJsonDigest(normalizeJournalJson(tail[0].event)) !==
        journalJsonDigest(normalizeJournalJson(handoff.reconciliation)))
  )
    throw new Error("funding abandonment has an unrelated journal suffix");
  return tail.length === 1;
};

export const parseWorkflowFundingCompletionHandoff = (
  value: unknown,
): WorkflowFundingCompletionHandoff => {
  const record = exact(
    value,
    [...handoffKeys, "completion"],
    "funding completion handoff",
  );
  const identity = parseHandoffIdentity(record);
  const completion = exact(
    record.completion,
    ["kind", "terminal", "terminalDigest"],
    "funding completion event",
  );
  if (
    (completion.kind !== "completed" &&
      completion.kind !== "terminal_included") ||
    !isPlainObject(completion.terminal) ||
    typeof completion.terminalDigest !== "string" ||
    !DIGEST.test(completion.terminalDigest) ||
    journalJsonDigest(normalizeJournalJson(completion.terminal)) !==
      completion.terminalDigest
  )
    throw new Error("funding completion handoff changed its terminal digest");
  // Journal validation and independent native terminal verification are required
  // before this event can be appended; the storage boundary admits only its bytes.
  return Object.freeze({
    ...identity,
    completion: Object.freeze({
      kind: completion.kind,
      terminal: structuredClone(
        completion.terminal,
      ) as WorkflowFundingCompletionHandoff["completion"]["terminal"],
      terminalDigest: completion.terminalDigest,
    }),
  });
};

export const assertHandoffJournal = (
  handoff: WorkflowFundingJournalHandoff,
  entries: readonly FraudProofWorkflowJournalEntry[],
): void => {
  validateFraudProofWorkflowJournal({
    workflowId: handoff.workflowId,
    entries,
    expectedIdentity: handoff.identity,
  });
  if (
    entries.length < handoff.expectedJournalSequence ||
    entries[1]?.event.kind !== "prepared" ||
    entries[1].event.artifactDigest !== handoff.preparedArtifactDigest
  )
    throw new Error(
      "funding handoff differs from its existing prepared workflow",
    );
};
