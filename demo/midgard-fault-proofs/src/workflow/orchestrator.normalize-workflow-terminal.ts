import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowTerminal,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import {
  type FraudProofWorkflowAction,
  type FraudProofWorkflowPreflight,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  normalizeOutRef,
  normalizeTxHash,
} from "./orchestrator.immutable-fraud-proof-workflow-registry.js";
import { type VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import { copyWorkflowPreflightTransaction } from "./transaction-boundary.js";

export const validateAction = (
  action: FraudProofWorkflowAction,
): FraudProofWorkflowAction => {
  if (
    action.actionId.length === 0 ||
    action.actionId.trim() !== action.actionId
  ) {
    throw new Error("workflow actionId must be a canonical non-empty string");
  }
  return {
    actionId: action.actionId,
    input: normalizeJournalJson(
      action.input,
      `workflow action ${action.actionId}`,
    ) as JournalJsonObject,
  };
};

export const validatePreflight = ({
  action,
  preflight,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly preflight: FraudProofWorkflowPreflight;
}): FraudProofWorkflowPreflight => {
  if (preflight.actionId !== action.actionId) {
    throw new Error("workflow preflight returned a different actionId");
  }
  const txHash = normalizeTxHash(
    preflight.txHash,
    "workflow preflight transaction hash",
  );
  if (
    preflight.localUplcEvaluation.status !== "passed" ||
    preflight.localUplcEvaluation.evaluator.trim().length === 0
  ) {
    throw new Error(
      "workflow submission requires a passed local UPLC evaluation",
    );
  }
  if (
    preflight.scriptExecution === "reference_scripts" &&
    preflight.referenceScripts.length === 0
  ) {
    throw new Error(
      "script-executing workflow submission requires reference scripts",
    );
  }
  if (
    preflight.scriptExecution === "none" &&
    preflight.referenceScripts.length !== 0
  ) {
    throw new Error(
      "script-free workflow submission reported reference scripts",
    );
  }
  const roles = new Set<string>();
  for (const reference of preflight.referenceScripts) {
    if (reference.role.trim().length === 0 || roles.has(reference.role)) {
      throw new Error(
        "workflow reference-script roles must be unique and non-empty",
      );
    }
    roles.add(reference.role);
    normalizeOutRef(reference.outRef, "workflow reference-script outRef");
    if (!/^[0-9a-f]{56}$/u.test(reference.scriptHash)) {
      throw new Error("workflow reference-script hash must be 28-byte hex");
    }
  }
  return copyWorkflowPreflightTransaction({
    from: preflight,
    to: {
      ...preflight,
      txHash,
      ...(preflight.durableRecovery === undefined
        ? {}
        : {
            durableRecovery: normalizeJournalJson(
              preflight.durableRecovery,
              `workflow preflight ${action.actionId} durable recovery`,
            ) as JournalJsonObject,
          }),
    },
  });
};

const normalizeNonNegativeLovelace = (value: string, field: string): string => {
  if (!/^(0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${field} must be canonical non-negative lovelace`);
  }
  return value;
};

export const normalizeWorkflowTerminal = ({
  identity,
  terminal,
  entries,
  releaseFinality,
  inclusionOnly = false,
}: {
  readonly identity: FraudProofWorkflowIdentity;
  readonly terminal: FraudProofWorkflowTerminal;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly inclusionOnly?: boolean;
}): FraudProofWorkflowTerminal => {
  if (terminal.schemaVersion !== FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION) {
    throw new Error("workflow terminal has an unsupported schema");
  }
  if (
    identity.target.kind !== "state_queue_header" ||
    terminal.category !== identity.category ||
    terminal.headerHash !== identity.target.headerHash
  ) {
    throw new Error("workflow terminal does not match workflow identity");
  }
  const createdByTxHash = normalizeTxHash(
    terminal.proofToken.createdByTxHash,
    "terminal proof-token creation transaction hash",
  );
  const removalTxHash = normalizeTxHash(
    terminal.correction.removalTxHash,
    "terminal removal transaction hash",
  );
  if (createdByTxHash === removalTxHash) {
    throw new Error(
      "terminal proof-token creation and removal must be distinct transactions",
    );
  }
  const confirmed = new Set(
    entries
      .filter(
        (
          entry,
        ): entry is FraudProofWorkflowJournalEntry & {
          readonly event: Extract<
            FraudProofWorkflowJournalEvent,
            { readonly kind: "confirmed" }
          >;
        } => entry.event.kind === "confirmed",
      )
      .map((entry) => entry.event.txHash),
  );
  if (!confirmed.has(createdByTxHash) || !confirmed.has(removalTxHash)) {
    throw new Error(
      "terminal proof-token creation and removal must both be confirmed in this workflow journal",
    );
  }
  if (!/^(?:[0-9a-f]{2}){28,60}$/u.test(terminal.proofToken.unit)) {
    throw new Error("terminal proof-token unit must be canonical hex");
  }
  normalizeOutRef(terminal.proofToken.outRef, "terminal proof-token outRef");
  normalizeOutRef(
    terminal.correction.removedStateQueueOutRef,
    "terminal removed state-queue outRef",
  );
  normalizeOutRef(
    terminal.correction.referencedProofTokenOutRef,
    "terminal referenced proof-token outRef",
  );
  if (
    terminal.correction.referencedProofTokenOutRef !==
    terminal.proofToken.outRef
  ) {
    throw new Error(
      "terminal removal did not reference the retained proof token",
    );
  }
  if (
    terminal.proofToken.retainedAtFinalState !== true ||
    "spentByTxHash" in terminal.proofToken ||
    "proofTokenSpent" in terminal.correction
  ) {
    throw new Error(
      "terminal must prove the permanent proof token remains unspent",
    );
  }
  if (
    terminal.correction.fraudulentHeaderAbsent !== true ||
    terminal.economics.duplicateRewardAbsent !== true
  ) {
    throw new Error(
      "workflow terminal omitted mandatory correction/economic facts",
    );
  }
  if (
    !/^[0-9a-f]{56}$/u.test(terminal.economics.operatorCredential) ||
    !/^[0-9a-f]{56}$/u.test(terminal.economics.proverCredential)
  ) {
    throw new Error("terminal economic credentials must be canonical hex");
  }
  if (terminal.economics.operatorBondInputOutRef !== null) {
    normalizeOutRef(
      terminal.economics.operatorBondInputOutRef,
      "terminal operator-bond input outRef",
    );
  }
  if (terminal.economics.proverRewardOutputOutRef !== null) {
    normalizeOutRef(
      terminal.economics.proverRewardOutputOutRef,
      "terminal prover-reward output outRef",
    );
  }
  normalizeNonNegativeLovelace(
    terminal.economics.operatorBondInputLovelace,
    "terminal operatorBondInputLovelace",
  );
  normalizeNonNegativeLovelace(
    terminal.economics.slashedLovelace,
    "terminal slashedLovelace",
  );
  normalizeNonNegativeLovelace(
    terminal.economics.proverRewardLovelace,
    "terminal proverRewardLovelace",
  );
  normalizeNonNegativeLovelace(
    terminal.economics.removalFeeLovelace,
    "terminal removalFeeLovelace",
  );
  if (
    (terminal.economics.operatorBondInputOutRef === null) !==
      (terminal.economics.operatorBondInputLovelace === "0") ||
    (terminal.economics.proverRewardOutputOutRef === null) !==
      (terminal.economics.proverRewardLovelace === "0")
  ) {
    throw new Error(
      "terminal economic output references do not match their lovelace amounts",
    );
  }
  if (!/^(0|[1-9][0-9]*)$/u.test(terminal.observedAt.slot)) {
    throw new Error("terminal chain-point slot must be canonical");
  }
  if (!/^[0-9a-f]{64}$/u.test(terminal.observedAt.blockHash)) {
    throw new Error("terminal chain-point block hash must be 32-byte hex");
  }
  if (
    !Number.isSafeInteger(terminal.observedAt.confirmationDepth) ||
    terminal.observedAt.confirmationDepth <
      (inclusionOnly ? 1 : releaseFinality.policy.confirmationDepth)
  ) {
    throw new Error(
      `terminal observation confirmation depth is below the release threshold: required=${releaseFinality.policy.confirmationDepth.toString()} actual=${String(terminal.observedAt.confirmationDepth)} policy=${releaseFinality.policyDigest}`,
    );
  }
  return {
    ...terminal,
    proofToken: {
      ...terminal.proofToken,
      createdByTxHash,
    },
    correction: {
      ...terminal.correction,
      removalTxHash,
    },
  };
};

export const lastActionEvent = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
): FraudProofWorkflowJournalEvent | undefined =>
  [...entries]
    .reverse()
    .map((entry) => entry.event)
    .find((event) => "actionId" in event && event.actionId === actionId);

export const lastKnownTxHash = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
): string | undefined => {
  let latestIntentIndex = -1;
  for (let index = entries.length - 1; index >= 0; index -= 1) {
    const event = entries[index]!.event;
    if (event.kind === "submission_intent" && event.actionId === actionId) {
      latestIntentIndex = index;
      break;
    }
  }
  if (latestIntentIndex < 0) {
    return undefined;
  }
  for (let index = entries.length - 1; index >= latestIntentIndex; index -= 1) {
    const entry = entries[index]!;
    const event = entry.event;
    if (!("actionId" in event) || event.actionId !== actionId) {
      continue;
    }
    if ("txHash" in event && event.txHash !== undefined) {
      return event.txHash;
    }
  }
  return undefined;
};
