import { createHash } from "node:crypto";

import { CML, coreToUtxo } from "@lucid-evolution/lucid";

import {
  ACTION_KIND,
  canonicalOutRefs,
  DIGEST,
  exact,
  isPlainObject,
  OUT_REF,
  reservedInput,
  type WorkflowFundingJournalHandoff,
  type WorkflowFundingPreparedTransition,
  type WorkflowFundingSubmissionHandoff,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeFraudProofWorkflowIdentity,
  normalizeJournalJson,
} from "./journal.js";
import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowPreflight,
} from "./orchestrator.js";

export const parseWorkflowFundingPreparedTransition = (
  value: unknown,
): WorkflowFundingPreparedTransition => {
  const record = exact(
    value,
    [
      "actionKind",
      "signedTransactionCborHex",
      "transactionHash",
      "transactionBodySha256",
      "consumedOutRefs",
      "producedInputs",
    ],
    "funding prepared transition",
  );
  if (
    typeof record.actionKind !== "string" ||
    !ACTION_KIND.test(record.actionKind) ||
    typeof record.signedTransactionCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(record.signedTransactionCborHex) ||
    typeof record.transactionHash !== "string" ||
    !DIGEST.test(record.transactionHash) ||
    typeof record.transactionBodySha256 !== "string" ||
    !DIGEST.test(record.transactionBodySha256) ||
    !Array.isArray(record.consumedOutRefs) ||
    record.consumedOutRefs.some((outRef) => typeof outRef !== "string") ||
    !Array.isArray(record.producedInputs)
  )
    throw new Error("funding prepared transition is malformed");
  const transaction = CML.Transaction.from_cbor_hex(
    record.signedTransactionCborHex,
  );
  if (
    CML.hash_transaction(transaction.body()).to_hex() !==
      record.transactionHash ||
    createHash("sha256")
      .update(transaction.body().to_cbor_bytes())
      .digest("hex") !== record.transactionBodySha256
  )
    throw new Error(
      "funding prepared transaction bytes changed their identity",
    );
  const consumedOutRefs = canonicalOutRefs(
    record.consumedOutRefs as string[],
    "funding prepared consumed inputs",
  );
  const inputs = transaction.body().inputs();
  const actual = new Set(
    Array.from({ length: inputs.len() }, (_, index) => {
      const input = inputs.get(index);
      return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
    }),
  );
  if (consumedOutRefs.some((outRef) => !actual.has(outRef)))
    throw new Error(
      "funding prepared consumed input is outside the signed transaction",
    );
  const producedInputs = record.producedInputs.map((input, index) =>
    reservedInput(input, `funding produced input ${index.toString()}`),
  );
  canonicalOutRefs(
    producedInputs.map(({ outRef }) => outRef),
    "funding prepared produced inputs",
  );
  const transactionHash = record.transactionHash;
  const outputs = transaction.body().outputs();
  for (const produced of producedInputs) {
    const index = Number(produced.outRef.split("#")[1]);
    if (
      !produced.outRef.startsWith(`${transactionHash}#`) ||
      !Number.isSafeInteger(index) ||
      index >= outputs.len() ||
      produced.role !== "funding"
    )
      throw new Error("funding produced input belongs to another transaction");
    const decoded = coreToUtxo(
      CML.TransactionUnspentOutput.new(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(transactionHash),
          BigInt(index),
        ),
        outputs.get(index),
      ),
    );
    const assets = Object.entries(decoded.assets)
      .filter(([unit]) => unit !== "lovelace")
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([unit, quantity]) => ({ unit, quantity: quantity.toString() }));
    if (
      decoded.assets.lovelace?.toString() !== produced.lovelace ||
      journalJsonDigest(assets) !== journalJsonDigest(produced.assets)
    )
      throw new Error("funding produced input changed its signed output value");
  }
  return Object.freeze({
    actionKind: record.actionKind,
    signedTransactionCborHex: record.signedTransactionCborHex,
    transactionHash: record.transactionHash,
    transactionBodySha256: record.transactionBodySha256,
    consumedOutRefs,
    producedInputs: Object.freeze(producedInputs),
  });
};

export const parseHandoffIdentity = (
  record: Readonly<Record<string, unknown>>,
): WorkflowFundingJournalHandoff => {
  const identity = normalizeFraudProofWorkflowIdentity(
    record.identity as FraudProofWorkflowIdentity,
  );
  if (
    record.workflowId !== computeFraudProofWorkflowId(identity) ||
    typeof record.preparedArtifactDigest !== "string" ||
    !DIGEST.test(record.preparedArtifactDigest) ||
    typeof record.expectedJournalSequence !== "number" ||
    !Number.isSafeInteger(record.expectedJournalSequence) ||
    record.expectedJournalSequence < 2
  )
    throw new Error("funding journal handoff identity is malformed");
  return Object.freeze({
    workflowId: record.workflowId,
    identity,
    preparedArtifactDigest: record.preparedArtifactDigest,
    expectedJournalSequence: record.expectedJournalSequence,
  });
};

export const handoffKeys = [
  "workflowId",
  "identity",
  "preparedArtifactDigest",
  "expectedJournalSequence",
];

export const parseWorkflowFundingSubmissionHandoff = (
  value: unknown,
): WorkflowFundingSubmissionHandoff => {
  const record = exact(
    value,
    [...handoffKeys, "preflight", "submissionIntent"],
    "funding submission handoff",
  );
  const identity = parseHandoffIdentity(record);
  const preflight = exact(
    record.preflight,
    ["kind", "actionId", "txHash", "localEvaluator", "referenceScripts"],
    "funding handoff preflight",
  );
  const intentKeys = ["kind", "actionId", "actionInput", "attempt", "txHash"];
  if (
    isPlainObject(record.submissionIntent) &&
    "durableRecovery" in record.submissionIntent
  )
    intentKeys.push("durableRecovery");
  const intent = exact(
    record.submissionIntent,
    intentKeys,
    "funding handoff submission intent",
  );
  if (
    preflight.kind !== "preflight_passed" ||
    intent.kind !== "submission_intent" ||
    typeof preflight.actionId !== "string" ||
    preflight.actionId.length === 0 ||
    preflight.actionId.trim() !== preflight.actionId ||
    intent.actionId !== preflight.actionId ||
    typeof preflight.txHash !== "string" ||
    !DIGEST.test(preflight.txHash) ||
    intent.txHash !== preflight.txHash ||
    typeof preflight.localEvaluator !== "string" ||
    preflight.localEvaluator.trim().length === 0 ||
    !Array.isArray(preflight.referenceScripts) ||
    typeof intent.attempt !== "number" ||
    !Number.isSafeInteger(intent.attempt) ||
    intent.attempt < 1 ||
    !isPlainObject(intent.actionInput) ||
    (intent.durableRecovery !== undefined &&
      !isPlainObject(intent.durableRecovery))
  )
    throw new Error("funding handoff changed its evaluated action identity");
  const roles = new Set<string>();
  const referenceScripts = preflight.referenceScripts.map((value) => {
    const reference = exact(
      value,
      ["role", "outRef", "scriptHash"],
      "funding handoff reference",
    );
    if (
      typeof reference.role !== "string" ||
      reference.role.length === 0 ||
      reference.role.trim() !== reference.role ||
      roles.has(reference.role) ||
      typeof reference.outRef !== "string" ||
      !OUT_REF.test(reference.outRef) ||
      typeof reference.scriptHash !== "string" ||
      !/^[0-9a-f]{56}$/u.test(reference.scriptHash)
    )
      throw new Error("funding handoff reference identity is malformed");
    roles.add(reference.role);
    return Object.freeze({
      role: reference.role,
      outRef: reference.outRef,
      scriptHash: reference.scriptHash,
    });
  });
  return Object.freeze({
    ...identity,
    preflight: Object.freeze({
      kind: "preflight_passed",
      actionId: preflight.actionId,
      txHash: preflight.txHash,
      localEvaluator: preflight.localEvaluator,
      referenceScripts: Object.freeze(referenceScripts),
    }),
    submissionIntent: Object.freeze({
      kind: "submission_intent",
      actionId: preflight.actionId,
      txHash: preflight.txHash,
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
  });
};

export const createWorkflowFundingSubmissionHandoff = (input: {
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly action: FraudProofWorkflowAction;
  readonly preflight: FraudProofWorkflowPreflight;
  readonly attempt: number;
}): WorkflowFundingSubmissionHandoff => {
  const first = input.entries[0];
  const prepared = input.entries[1];
  if (first === undefined || prepared?.event.kind !== "prepared")
    throw new Error(
      "funding submission requires its existing prepared journal",
    );
  return parseWorkflowFundingSubmissionHandoff({
    workflowId: first.workflowId,
    identity: first.identity,
    preparedArtifactDigest: prepared.event.artifactDigest,
    expectedJournalSequence: input.entries.length,
    preflight: {
      kind: "preflight_passed",
      actionId: input.action.actionId,
      txHash: input.preflight.txHash,
      localEvaluator: input.preflight.localUplcEvaluation.evaluator,
      referenceScripts: input.preflight.referenceScripts,
    },
    submissionIntent: {
      kind: "submission_intent",
      actionId: input.action.actionId,
      actionInput: input.action.input,
      txHash: input.preflight.txHash,
      attempt: input.attempt,
      ...(input.preflight.durableRecovery === undefined
        ? {}
        : { durableRecovery: input.preflight.durableRecovery }),
    },
  });
};
