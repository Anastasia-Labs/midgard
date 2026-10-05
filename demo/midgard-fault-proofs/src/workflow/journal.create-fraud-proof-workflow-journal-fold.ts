import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalFold,
  requireExactKeys,
  requireOptionalExactKeys,
  requireRecord,
  validateWorkflowId,
} from "./journal.fraud-proof-workflow-journal-event.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  isFinalWorkflowCompletion,
  journalJsonDigest,
  normalizeJournalJson,
} from "./journal.fraud-proof-workflow-terminal.js";
import {
  expectedWorkflowIdentityMatches,
  requireOutRef,
  requireTxHash,
} from "./journal.require-transaction-identity.js";
import {
  validateJournalRetirement,
  validateRetiredJournalAttempt,
} from "./signed-transaction-retirement-journal.js";

export const createFraudProofWorkflowJournalFold = ({
  workflowId,
  expectedIdentity,
}: {
  readonly workflowId: string;
  readonly expectedIdentity?: FraudProofWorkflowIdentity;
}): FraudProofWorkflowJournalFold => {
  validateWorkflowId(workflowId);
  const expectedIdentityMatches = expectedWorkflowIdentityMatches(
    workflowId,
    expectedIdentity,
  );
  const latestPreflightByAction = new Map<
    string,
    Extract<
      FraudProofWorkflowJournalEvent,
      { readonly kind: "preflight_passed" }
    >
  >();
  const latestIntentByAction = new Map<
    string,
    Extract<
      FraudProofWorkflowJournalEvent,
      { readonly kind: "submission_intent" }
    >
  >();
  const unresolvedSubmissionByAction = new Map<
    string,
    "intent" | "submitted" | "ambiguous" | "pending" | "reconciled_confirmed"
  >();
  const confirmedReconciliationByAction = new Map<string, string>();
  const confirmedTransactionHashes = new Set<string>();
  const attemptsByAction = new Map<string, number>();
  const broadcastsByTransaction = new Map<string, number>();
  const knownSignedIntentHashes = new Set<string>();
  let completed = false;
  let previousEvent: FraudProofWorkflowJournalEvent | undefined;
  let length = 0;
  const step = (
    entry: FraudProofWorkflowJournalEntry,
    sequence: number,
  ): void => {
    if (completed && entry.event.kind !== "signed_attempt_retired") {
      throw new Error("journal contains an event after terminal completion");
    }
    requireExactKeys(
      entry,
      [
        "schemaVersion",
        "workflowId",
        "identity",
        "sequence",
        "recordedAt",
        "event",
      ],
      `journal entry ${sequence.toString()}`,
    );
    requireOptionalExactKeys(
      entry.identity,
      ["schemaVersion", "deploymentFingerprint", "category", "target"],
      ["decisionDigest"],
      `journal entry ${sequence.toString()} identity`,
    );
    const target = requireRecord(
      entry.identity.target,
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
    if (entry.schemaVersion !== FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION) {
      throw new Error(`journal entry ${sequence.toString()} has wrong schema`);
    }
    if (entry.sequence !== sequence) {
      throw new Error(
        `journal entry sequence gap: expected=${sequence.toString()} actual=${String(entry.sequence)}`,
      );
    }
    if (entry.workflowId !== workflowId) {
      throw new Error(
        `journal entry ${sequence.toString()} changed workflowId`,
      );
    }
    if (computeFraudProofWorkflowId(entry.identity) !== workflowId) {
      throw new Error(
        `journal entry ${sequence.toString()} identity does not derive workflowId`,
      );
    }
    if (!expectedIdentityMatches) {
      throw new Error(
        `journal entry ${sequence.toString()} does not match requested workflow identity`,
      );
    }
    if (
      !Number.isFinite(Date.parse(entry.recordedAt)) ||
      new Date(entry.recordedAt).toISOString() !== entry.recordedAt
    ) {
      throw new Error(
        `journal entry ${sequence.toString()} recordedAt is invalid`,
      );
    }
    const event = entry.event;
    const previous = previousEvent;
    if (event.kind !== "stalled") previousEvent = event;
    const eventRecord = requireRecord(
      event,
      `journal entry ${sequence.toString()} event`,
    );
    if (sequence === 0 && eventRecord.kind !== "started") {
      throw new Error("journal must begin with exactly one started event");
    }
    if (sequence > 0 && eventRecord.kind === "started") {
      throw new Error("journal contains a duplicate started event");
    }
    if (sequence === 1 && eventRecord.kind !== "prepared") {
      throw new Error("journal second event must be the prepared artifact");
    }
    if (sequence > 1 && eventRecord.kind === "prepared") {
      throw new Error("journal contains a duplicate prepared artifact");
    }
    if (eventRecord.kind === "started") {
      requireExactKeys(event, ["kind"], "journal started event");
      return;
    }
    if (event.kind === "prepared") {
      if (previous?.kind !== "started") {
        throw new Error(
          "journal prepared artifact must immediately follow started",
        );
      }
      requireExactKeys(
        event,
        ["kind", "artifact", "artifactDigest"],
        "journal prepared event",
      );
      requireRecord(event.artifact, "journal prepared artifact");
      normalizeJournalJson(event.artifact, "journal prepared artifact");
      if (!/^[0-9a-f]{64}$/u.test(event.artifactDigest)) {
        throw new Error("journal prepared artifactDigest must be 32-byte hex");
      }
      if (journalJsonDigest(event.artifact) !== event.artifactDigest) {
        throw new Error(
          `journal entry ${sequence.toString()} prepared artifact digest mismatch`,
        );
      }
      return;
    }
    if (event.kind === "preflight_passed") {
      if (unresolvedSubmissionByAction.size > 0) {
        throw new Error(
          "journal cannot preflight another submission before reconciling the unresolved intent",
        );
      }
      requireExactKeys(
        event,
        ["kind", "actionId", "txHash", "localEvaluator", "referenceScripts"],
        "journal preflight event",
      );
      if (
        event.actionId.trim().length === 0 ||
        event.actionId.trim() !== event.actionId ||
        event.localEvaluator.trim().length === 0
      ) {
        throw new Error(
          "journal preflight actionId/evaluator must be canonical non-empty strings",
        );
      }
      requireTxHash(event.txHash, "journal preflight txHash");
      if (!Array.isArray(event.referenceScripts)) {
        throw new Error("journal preflight referenceScripts must be an array");
      }
      const roles = new Set<string>();
      for (const reference of event.referenceScripts) {
        requireExactKeys(
          reference,
          ["role", "outRef", "scriptHash"],
          "journal preflight reference script",
        );
        if (
          reference.role.trim().length === 0 ||
          reference.role.trim() !== reference.role ||
          roles.has(reference.role)
        ) {
          throw new Error(
            "journal preflight reference-script roles must be unique canonical strings",
          );
        }
        roles.add(reference.role);
        requireOutRef(
          reference.outRef,
          "journal preflight reference-script outRef",
        );
        if (!/^[0-9a-f]{56}$/u.test(reference.scriptHash)) {
          throw new Error(
            "journal preflight reference-script hash must be 28-byte hex",
          );
        }
      }
      latestPreflightByAction.set(event.actionId, event);
      return;
    }
    if (event.kind === "submission_intent") {
      if (
        previous?.kind !== "preflight_passed" ||
        previous.actionId !== event.actionId
      ) {
        throw new Error(
          "journal submission intent must immediately follow its preflight",
        );
      }
      requireOptionalExactKeys(
        event,
        ["kind", "actionId", "actionInput", "attempt", "txHash"],
        ["durableRecovery"],
        "journal submission intent",
      );
      requireRecord(event.actionInput, "journal action input");
      normalizeJournalJson(event.actionInput, "journal action input");
      if (event.durableRecovery !== undefined) {
        requireRecord(event.durableRecovery, "journal durable recovery");
        normalizeJournalJson(event.durableRecovery, "journal durable recovery");
      }
      if (
        event.actionId.trim().length === 0 ||
        event.actionId.trim() !== event.actionId ||
        !Number.isSafeInteger(event.attempt) ||
        event.attempt < 1
      ) {
        throw new Error("journal submission intent fields are not canonical");
      }
      requireTxHash(event.txHash, "journal intent txHash");
      const preflight = latestPreflightByAction.get(event.actionId);
      if (preflight === undefined || preflight.txHash !== event.txHash) {
        throw new Error(
          `journal intent ${event.actionId} lacks a matching exact-body preflight`,
        );
      }
      const expectedAttempt = (attemptsByAction.get(event.actionId) ?? 0) + 1;
      if (event.attempt !== expectedAttempt) {
        throw new Error(
          `journal intent ${event.actionId} attempt mismatch: expected=${expectedAttempt.toString()} actual=${event.attempt.toString()}`,
        );
      }
      attemptsByAction.set(event.actionId, event.attempt);
      knownSignedIntentHashes.add(event.txHash);
      latestIntentByAction.set(event.actionId, event);
      broadcastsByTransaction.set(event.txHash, 1);
      unresolvedSubmissionByAction.set(event.actionId, "intent");
      return;
    }
    if (event.kind === "rebroadcast_intent") {
      requireExactKeys(
        event,
        ["kind", "actionId", "txHash", "attempt"],
        "journal rebroadcast intent",
      );
      const intent = latestIntentByAction.get(event.actionId);
      if (
        unresolvedSubmissionByAction.size !== 1 ||
        !unresolvedSubmissionByAction.has(event.actionId) ||
        unresolvedSubmissionByAction.get(event.actionId) ===
          "reconciled_confirmed" ||
        intent?.txHash !== event.txHash ||
        !Number.isSafeInteger(event.attempt) ||
        event.attempt !== (broadcastsByTransaction.get(event.txHash) ?? 0) + 1
      )
        throw new Error(
          "journal rebroadcast differs from its unresolved exact transaction",
        );
      broadcastsByTransaction.set(event.txHash, event.attempt);
      return;
    }
    if (event.kind === "submitted" || event.kind === "submission_ambiguous") {
      if (
        previous?.kind !== "submission_intent" ||
        previous.actionId !== event.actionId ||
        previous.attempt !== event.attempt
      ) {
        throw new Error(
          `journal ${event.kind} must immediately follow its durable intent`,
        );
      }
      if (event.kind === "submitted") {
        requireExactKeys(
          event,
          ["kind", "actionId", "attempt", "txHash"],
          "journal submitted event",
        );
      } else {
        requireOptionalExactKeys(
          event,
          ["kind", "actionId", "attempt", "detail"],
          ["txHash"],
          "journal ambiguous-submission event",
        );
        if (event.detail.trim().length === 0) {
          throw new Error(
            "journal ambiguous-submission detail must not be empty",
          );
        }
      }
      if (!Number.isSafeInteger(event.attempt) || event.attempt < 1) {
        throw new Error(
          "journal submission attempt must be a positive integer",
        );
      }
      const intent = latestIntentByAction.get(event.actionId);
      if (intent === undefined || intent.attempt !== event.attempt) {
        throw new Error(
          `journal ${event.kind} ${event.actionId} lacks its matching durable intent`,
        );
      }
      if (event.txHash !== undefined) {
        requireTxHash(event.txHash, `journal ${event.kind} txHash`);
        if (event.txHash !== intent.txHash) {
          throw new Error(
            `journal ${event.kind} ${event.actionId} changed the intended transaction hash`,
          );
        }
      }
      unresolvedSubmissionByAction.set(
        event.actionId,
        event.kind === "submitted" ? "submitted" : "ambiguous",
      );
      return;
    }
    if (event.kind === "reobserved") {
      requireExactKeys(
        event,
        ["kind", "actionId", "txHash"],
        "journal reobservation",
      );
      requireTxHash(event.txHash, "journal reobservation txHash");
      const intent = latestIntentByAction.get(event.actionId);
      if (intent?.txHash !== event.txHash)
        throw new Error("journal reobservation lacks its exact signed intent");
      // Submission history remains intact. Only the current reconciliation cursor
      // moves back; later attempts must be observed again before they are reused.
      unresolvedSubmissionByAction.clear();
      unresolvedSubmissionByAction.set(event.actionId, "pending");
      confirmedReconciliationByAction.delete(event.actionId);
      confirmedTransactionHashes.delete(event.txHash);
      return;
    }
    if (event.kind === "signed_attempt_retired")
      return validateRetiredJournalAttempt(event, knownSignedIntentHashes);
    if (event.kind === "reconciled") {
      const unresolvedState = unresolvedSubmissionByAction.get(event.actionId);
      if (
        unresolvedSubmissionByAction.size !== 1 ||
        unresolvedState === undefined ||
        unresolvedState === "reconciled_confirmed"
      ) {
        throw new Error(
          "journal reconciliation must follow an unresolved matching submission",
        );
      }
      requireOptionalExactKeys(
        event,
        ["kind", "actionId", "outcome"],
        ["txHash", "retirement"],
        "journal reconciliation event",
      );
      if (
        event.outcome !== "confirmed" &&
        event.outcome !== "pending" &&
        event.outcome !== "not_found"
      ) {
        throw new Error("journal reconciliation outcome is unknown");
      }
      const intent = latestIntentByAction.get(event.actionId);
      if (intent === undefined) {
        throw new Error(
          `journal reconciliation ${event.actionId} lacks a durable intent`,
        );
      }
      if (event.txHash !== undefined) {
        requireTxHash(event.txHash, "journal reconciliation txHash");
        if (event.txHash !== intent.txHash) {
          throw new Error(
            `journal reconciliation ${event.actionId} changed the intended transaction hash`,
          );
        }
      }
      if (event.retirement !== undefined) validateJournalRetirement(event);
      if (event.outcome === "confirmed") {
        if (event.txHash === undefined) {
          throw new Error(
            `journal confirmed reconciliation ${event.actionId} omitted txHash`,
          );
        }
        confirmedReconciliationByAction.set(event.actionId, event.txHash);
        unresolvedSubmissionByAction.set(
          event.actionId,
          "reconciled_confirmed",
        );
      } else if (event.outcome === "pending") {
        unresolvedSubmissionByAction.set(event.actionId, "pending");
      } else {
        unresolvedSubmissionByAction.delete(event.actionId);
      }
      return;
    }
    if (event.kind === "confirmed") {
      if (
        unresolvedSubmissionByAction.size !== 1 ||
        unresolvedSubmissionByAction.get(event.actionId) !==
          "reconciled_confirmed"
      ) {
        throw new Error(
          "journal confirmation must follow matching confirmed reconciliation",
        );
      }
      requireExactKeys(
        event,
        ["kind", "actionId", "txHash"],
        "journal confirmation event",
      );
      requireTxHash(event.txHash, "journal confirmed txHash");
      if (
        confirmedReconciliationByAction.get(event.actionId) !== event.txHash
      ) {
        throw new Error(
          `journal confirmation ${event.actionId} lacks matching authenticated reconciliation`,
        );
      }
      confirmedTransactionHashes.add(event.txHash);
      unresolvedSubmissionByAction.delete(event.actionId);
      return;
    }
    if (event.kind === "completed" || event.kind === "terminal_included") {
      requireExactKeys(
        event,
        ["kind", "terminal", "terminalDigest"],
        "journal completed event",
      );
      requireExactKeys(
        event.terminal,
        [
          "schemaVersion",
          "category",
          "headerHash",
          "proofToken",
          "correction",
          "economics",
          "observedAt",
        ],
        "journal terminal",
      );
      requireExactKeys(
        event.terminal.proofToken,
        ["unit", "outRef", "createdByTxHash", "retainedAtFinalState"],
        "journal terminal proof token",
      );
      requireExactKeys(
        event.terminal.correction,
        [
          "removalTxHash",
          "removedStateQueueOutRef",
          "fraudulentHeaderAbsent",
          "referencedProofTokenOutRef",
        ],
        "journal terminal correction",
      );
      requireExactKeys(
        event.terminal.economics,
        [
          "operatorCredential",
          "proverCredential",
          "operatorBondInputOutRef",
          "operatorBondInputLovelace",
          "slashedLovelace",
          "proverRewardOutputOutRef",
          "proverRewardLovelace",
          "removalFeeLovelace",
          "duplicateRewardAbsent",
        ],
        "journal terminal economics",
      );
      requireExactKeys(
        event.terminal.observedAt,
        ["slot", "blockHash", "confirmationDepth"],
        "journal terminal observation",
      );
      if (
        event.terminal.schemaVersion !==
        FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION
      ) {
        throw new Error("journal terminal has an unsupported schema");
      }
      const terminalJson = normalizeJournalJson(
        event.terminal,
        "journal terminal",
      );
      if (!/^[0-9a-f]{64}$/u.test(event.terminalDigest)) {
        throw new Error("journal terminalDigest must be 32-byte hex");
      }
      if (journalJsonDigest(terminalJson) !== event.terminalDigest) {
        throw new Error(
          `journal entry ${sequence.toString()} terminal digest mismatch`,
        );
      }
      if (
        entry.identity.target.kind !== "state_queue_header" ||
        event.terminal.category !== entry.identity.category ||
        event.terminal.headerHash !== entry.identity.target.headerHash
      ) {
        throw new Error("journal terminal does not match workflow identity");
      }
      if (!/^(?:[0-9a-f]{2}){28,60}$/u.test(event.terminal.proofToken.unit)) {
        throw new Error(
          "journal terminal contains a malformed proof-token unit",
        );
      }
      requireOutRef(
        event.terminal.proofToken.outRef,
        "journal terminal proof-token outRef",
      );
      requireOutRef(
        event.terminal.correction.removedStateQueueOutRef,
        "journal terminal removed state-queue outRef",
      );
      requireOutRef(
        event.terminal.correction.referencedProofTokenOutRef,
        "journal terminal referenced proof-token outRef",
      );
      if (event.terminal.economics.operatorBondInputOutRef !== null) {
        requireOutRef(
          event.terminal.economics.operatorBondInputOutRef,
          "journal terminal operator-bond input outRef",
        );
      }
      if (event.terminal.economics.proverRewardOutputOutRef !== null) {
        requireOutRef(
          event.terminal.economics.proverRewardOutputOutRef,
          "journal terminal prover-reward output outRef",
        );
      }
      if (
        event.terminal.correction.fraudulentHeaderAbsent !== true ||
        event.terminal.economics.duplicateRewardAbsent !== true ||
        !/^[0-9a-f]{56}$/u.test(event.terminal.economics.operatorCredential) ||
        !/^[0-9a-f]{56}$/u.test(event.terminal.economics.proverCredential) ||
        !/^(0|[1-9][0-9]*)$/u.test(
          event.terminal.economics.operatorBondInputLovelace,
        ) ||
        !/^(0|[1-9][0-9]*)$/u.test(event.terminal.economics.slashedLovelace) ||
        !/^(0|[1-9][0-9]*)$/u.test(
          event.terminal.economics.proverRewardLovelace,
        ) ||
        !/^(0|[1-9][0-9]*)$/u.test(
          event.terminal.economics.removalFeeLovelace,
        ) ||
        !/^(0|[1-9][0-9]*)$/u.test(event.terminal.observedAt.slot) ||
        !/^[0-9a-f]{64}$/u.test(event.terminal.observedAt.blockHash) ||
        !Number.isSafeInteger(event.terminal.observedAt.confirmationDepth) ||
        event.terminal.observedAt.confirmationDepth < 1
      ) {
        throw new Error("journal terminal facts are not canonical");
      }
      const proofCreationTxHash = event.terminal.proofToken.createdByTxHash;
      const removalTxHash = event.terminal.correction.removalTxHash;
      requireTxHash(
        proofCreationTxHash,
        "journal terminal proof creation txHash",
      );
      requireTxHash(removalTxHash, "journal terminal removal txHash");
      if (
        proofCreationTxHash === removalTxHash ||
        !confirmedTransactionHashes.has(proofCreationTxHash) ||
        !confirmedTransactionHashes.has(removalTxHash)
      ) {
        throw new Error(
          "journal terminal requires distinct confirmed proof creation and removal transactions",
        );
      }
      if (
        event.terminal.proofToken.retainedAtFinalState !== true ||
        event.terminal.correction.referencedProofTokenOutRef !==
          event.terminal.proofToken.outRef ||
        "spentByTxHash" in event.terminal.proofToken ||
        "proofTokenSpent" in event.terminal.correction
      ) {
        throw new Error(
          "journal terminal must retain and exactly reference the permanent proof token",
        );
      }
      completed = isFinalWorkflowCompletion(event);
      return;
    }
    if (event.kind === "stalled") {
      requireExactKeys(event, ["kind", "reason"], "journal stalled event");
      if (event.reason.trim().length === 0) {
        throw new Error("journal stalled reason must not be empty");
      }
      return;
    }
    throw new Error(
      `journal entry ${sequence.toString()} has unknown event kind: ${String(eventRecord.kind)}`,
    );
  };
  return {
    get length() {
      return length;
    },
    push(entry) {
      step(entry, length);
      length += 1;
    },
  };
};
