import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  assetsToValue,
  CML,
  coreToTxOutput,
  coreToUtxo,
  type LucidEvolution,
  type TxSigned,
  type UTxO,
  utxoToCore,
  validatorToScriptHash,
  type WalletApi,
} from "@lucid-evolution/lucid";

import { readFraudSlashFundingAuthority } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  assertWorkflowActuationPermitIdentity,
  type WorkflowActuationPermit,
  workflowActuationPermitIsReconciliationOnly,
} from "./actuation-permit.js";
import type { WorkflowAdapterRunner } from "./adapters.js";
import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeFraudProofWorkflowIdentity,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "./journal.js";
import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowPreflight,
} from "./orchestrator.js";
import {
  assertWorkflowRuntimeFundingPolicyRunner,
  readWorkflowRuntimeFundingPolicy,
  workflowRuntimeFundingMinimumFee,
  type WorkflowRuntimeFundingPolicy,
} from "./runtime-funding-policy.js";
import {
  workflowPreflightTransaction,
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const WORKFLOW_FUNDING_RESERVATION_PERMIT =
  "midgard-production-workflow-funding-reservation-permit-v1" as const;

const DIGEST = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const ACTION_KIND = /^[a-z][a-zA-Z0-9_.:-]{0,127}$/u;

export type WorkflowFundingReservedInput = Readonly<{
  outRef: string;
  role: "funding" | "collateral";
  lovelace: string;
  assets: readonly Readonly<{ unit: string; quantity: string }>[];
}>;

export type WorkflowFundingReservationSnapshot = Readonly<{
  reservationId: string;
  deploymentFingerprint: string;
  decisionDigest: string;
  policyDigest: string;
  reservationBasisDigest: string;
  rollbackGeneration: string;
  revision: string;
  walletAddress: string;
  fundingPaymentKeyHash: string;
  state: "active" | "released" | "conflict";
  activeInputs: readonly WorkflowFundingReservedInput[];
}>;

export type WorkflowFundingPreparedTransition = Readonly<{
  actionKind: string;
  signedTransactionCborHex: string;
  transactionHash: string;
  transactionBodySha256: string;
  consumedOutRefs: readonly string[];
  producedInputs: readonly WorkflowFundingReservedInput[];
}>;

type WorkflowFundingJournalHandoff = Readonly<{
  workflowId: string;
  identity: FraudProofWorkflowIdentity;
  preparedArtifactDigest: string;
  expectedJournalSequence: number;
}>;

export type WorkflowFundingSubmissionHandoff = WorkflowFundingJournalHandoff &
  Readonly<{
    preflight: Extract<
      FraudProofWorkflowJournalEvent,
      { kind: "preflight_passed" }
    >;
    submissionIntent: Extract<
      FraudProofWorkflowJournalEvent,
      { kind: "submission_intent" }
    >;
  }>;

export type WorkflowFundingCompletionHandoff = WorkflowFundingJournalHandoff &
  Readonly<{
    completion: Extract<FraudProofWorkflowJournalEvent, { kind: "completed" }>;
  }>;

export type WorkflowFundingAbandonmentHandoff = WorkflowFundingJournalHandoff &
  Readonly<{
    submissionIntent: WorkflowFundingSubmissionHandoff["submissionIntent"];
    reconciliation: Extract<
      FraudProofWorkflowJournalEvent,
      { kind: "reconciled" }
    > &
      Readonly<{ outcome: "not_found"; txHash: string }>;
  }>;

/**
 * Durable watcher-owned authority. The production application supplies this
 * port from its authenticated SQLite reservation store and local-node UTxO
 * source; workflow constructors never accept reservation data from config.
 */
export interface WorkflowFundingReservationPort {
  load(): Promise<unknown>;
  readPendingTransition(): Promise<unknown>;
  readPendingHandoff(): Promise<unknown>;
  readCompletionHandoff(): Promise<unknown>;
  readAbandonmentHandoff(): Promise<unknown>;
  resolveInputs(outRefs: readonly string[]): Promise<readonly UTxO[]>;
  /** Exact confirmed output from this reservation, or null if it has no lineage. */
  resolveConfirmedInput(input: { readonly outRef: string }): Promise<unknown>;
  resolveProtocolInputAuthority(input: {
    readonly deploymentFingerprint: string;
    readonly outRef: string;
    readonly semanticRole: "protocol_state";
  }): Promise<unknown>;
  prepare(input: {
    readonly expectedRevision: string;
    readonly transition: WorkflowFundingPreparedTransition;
    readonly handoff: WorkflowFundingSubmissionHandoff;
  }): Promise<unknown>;
  confirm(input: {
    readonly expectedRevision: string;
    readonly transactionHash: string;
  }): Promise<unknown>;
  abandon(input: {
    readonly expectedRevision: string;
    readonly transactionHash: string;
    readonly handoff: WorkflowFundingAbandonmentHandoff;
  }): Promise<unknown>;
  acknowledgeAbandonment(input: {
    readonly expectedRevision: string;
    readonly handoff: WorkflowFundingAbandonmentHandoff;
  }): Promise<unknown>;
  markConflict(input: {
    readonly expectedRevision: string;
    readonly code: "unexpected_spend" | "reservation_collision";
  }): Promise<unknown>;
  release(input: {
    readonly expectedRevision: string;
    readonly handoff: WorkflowFundingCompletionHandoff;
  }): Promise<unknown>;
}

export interface WorkflowFundingReservationPermit {
  readonly permitVersion: typeof WORKFLOW_FUNDING_RESERVATION_PERMIT;
}

type PermitState = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly policy: WorkflowRuntimeFundingPolicy | undefined;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly port: WorkflowFundingReservationPort;
  readonly maximumCollateralInputs: number;
  snapshot: WorkflowFundingReservationSnapshot;
  resolvedInputs: ReadonlyMap<string, UTxO>;
  boundJournal: object | undefined;
  currentActionKind: string | undefined;
  currentActionDigest: string | undefined;
  currentFundingOutRefs: readonly string[];
  currentCollateralOutRefs: readonly string[];
  pendingTransactionHash: string | undefined;
  preparedTransaction:
    | Readonly<{ signed: TxSigned; cborHex: string }>
    | undefined;
};

const admittedPermits = new WeakMap<object, PermitState>();
const journalPermits = new WeakMap<object, PermitState>();

const isPlainObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  Object.getPrototypeOf(value) === Object.prototype &&
  Reflect.ownKeys(value).length === Object.keys(value).length;

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (!isPlainObject(value)) throw new Error(`${label} is not a plain object`);
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
  return value;
};

const canonicalOutRefs = (
  values: readonly string[],
  label: string,
): readonly string[] => {
  if (
    values.some((value) => !OUT_REF.test(value)) ||
    values.some(
      (value, index) =>
        index > 0 && values[index - 1]!.localeCompare(value) >= 0,
    )
  ) {
    throw new Error(`${label} must be canonical, ordered, and unique`);
  }
  return Object.freeze([...values]);
};

const reservedInput = (
  value: unknown,
  label: string,
): WorkflowFundingReservedInput => {
  const record = exact(value, ["outRef", "role", "lovelace", "assets"], label);
  if (
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    (record.role !== "funding" && record.role !== "collateral") ||
    typeof record.lovelace !== "string" ||
    !NATURAL.test(record.lovelace) ||
    BigInt(record.lovelace) <= 0n ||
    !Array.isArray(record.assets)
  ) {
    throw new Error(`${label} is invalid`);
  }
  const assets = record.assets.map((entry, index) => {
    const asset = exact(
      entry,
      ["unit", "quantity"],
      `${label}.assets[${index.toString()}]`,
    );
    if (
      typeof asset.unit !== "string" ||
      !/^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u.test(asset.unit) ||
      typeof asset.quantity !== "string" ||
      !NATURAL.test(asset.quantity) ||
      BigInt(asset.quantity) <= 0n
    ) {
      throw new Error(`${label}.assets[${index.toString()}] is invalid`);
    }
    return Object.freeze({ unit: asset.unit, quantity: asset.quantity });
  });
  if (
    assets.some(
      (asset, index) =>
        index > 0 && assets[index - 1]!.unit.localeCompare(asset.unit) >= 0,
    ) ||
    (record.role === "collateral" && assets.length !== 0)
  ) {
    throw new Error(`${label} asset set is invalid`);
  }
  return Object.freeze({
    outRef: record.outRef,
    role: record.role,
    lovelace: record.lovelace,
    assets: Object.freeze(assets),
  });
};

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

const parseHandoffIdentity = (
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

const handoffKeys = [
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
    ["kind", "actionId", "outcome", "txHash"],
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
    }),
  });
};

export const createWorkflowFundingAbandonmentHandoff = (input: {
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly transactionHash: string;
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
    .find((event) => event.kind === "submission_intent");
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
      .findIndex(({ event }) => event.kind === "submission_intent");
  if (
    prefix
      .slice(intentIndex + 1)
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
    completion.kind !== "completed" ||
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
      kind: "completed",
      terminal: structuredClone(
        completion.terminal,
      ) as WorkflowFundingCompletionHandoff["completion"]["terminal"],
      terminalDigest: completion.terminalDigest,
    }),
  });
};

const assertHandoffJournal = (
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

/** Validate the durable prefix and return only the lifecycle records lost in a crash. */
export const reconcileWorkflowFundingSubmissionHandoff = (input: {
  readonly handoff: WorkflowFundingSubmissionHandoff;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
}): readonly FraudProofWorkflowJournalEvent[] => {
  const handoff = parseWorkflowFundingSubmissionHandoff(input.handoff);
  assertHandoffJournal(handoff, input.entries);
  const observed = input.entries
    .slice(handoff.expectedJournalSequence)
    .map(({ event }) => event)
    .filter(({ kind }) => kind !== "stalled");
  const expected = [handoff.preflight, handoff.submissionIntent];
  for (
    let index = 0;
    index < Math.min(expected.length, observed.length);
    index++
  ) {
    if (
      journalJsonDigest(normalizeJournalJson(observed[index])) !==
      journalJsonDigest(normalizeJournalJson(expected[index]))
    )
      throw new Error(
        "funding handoff conflicts with an existing journal action",
      );
  }
  for (const event of observed.slice(expected.length)) {
    if (
      !("actionId" in event) ||
      event.actionId !== handoff.submissionIntent.actionId ||
      !("txHash" in event) ||
      event.txHash !== handoff.submissionIntent.txHash ||
      (event.kind !== "submitted" &&
        event.kind !== "submission_ambiguous" &&
        event.kind !== "rebroadcast_intent" &&
        !(event.kind === "reconciled" && event.outcome === "pending"))
    )
      throw new Error("funding handoff has an unrelated journal suffix");
  }
  return Object.freeze(
    expected.slice(Math.min(expected.length, observed.length)),
  );
};

export const assertWorkflowFundingCompletionHandoffJournal = (input: {
  readonly handoff: WorkflowFundingCompletionHandoff;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
}): void => {
  const handoff = parseWorkflowFundingCompletionHandoff(input.handoff);
  assertHandoffJournal(handoff, input.entries);
  const tail = input.entries
    .slice(handoff.expectedJournalSequence)
    .filter(({ event }) => event.kind !== "stalled");
  if (
    tail.length > 1 ||
    (tail[0] !== undefined &&
      journalJsonDigest(normalizeJournalJson(tail[0].event)) !==
        journalJsonDigest(normalizeJournalJson(handoff.completion)))
  )
    throw new Error(
      "funding completion handoff conflicts with its journal suffix",
    );
};

const parseSnapshot = (value: unknown): WorkflowFundingReservationSnapshot => {
  const record = exact(
    value,
    [
      "reservationId",
      "deploymentFingerprint",
      "decisionDigest",
      "policyDigest",
      "reservationBasisDigest",
      "rollbackGeneration",
      "revision",
      "walletAddress",
      "fundingPaymentKeyHash",
      "state",
      "activeInputs",
    ],
    "production funding reservation snapshot",
  );
  if (
    typeof record.reservationId !== "string" ||
    !DIGEST.test(record.reservationId) ||
    typeof record.deploymentFingerprint !== "string" ||
    !DIGEST.test(record.deploymentFingerprint) ||
    typeof record.decisionDigest !== "string" ||
    !DIGEST.test(record.decisionDigest) ||
    typeof record.policyDigest !== "string" ||
    !DIGEST.test(record.policyDigest) ||
    typeof record.reservationBasisDigest !== "string" ||
    !DIGEST.test(record.reservationBasisDigest) ||
    typeof record.rollbackGeneration !== "string" ||
    !NATURAL.test(record.rollbackGeneration) ||
    typeof record.revision !== "string" ||
    !NATURAL.test(record.revision) ||
    typeof record.walletAddress !== "string" ||
    record.walletAddress.length === 0 ||
    typeof record.fundingPaymentKeyHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(record.fundingPaymentKeyHash) ||
    (record.state !== "active" &&
      record.state !== "released" &&
      record.state !== "conflict") ||
    !Array.isArray(record.activeInputs)
  ) {
    throw new Error("production funding reservation snapshot is invalid");
  }
  const activeInputs = record.activeInputs.map((entry, index) =>
    reservedInput(
      entry,
      `production funding reservation snapshot.activeInputs[${index.toString()}]`,
    ),
  );
  canonicalOutRefs(
    activeInputs.map(({ outRef }) => outRef),
    "production funding reservation active inputs",
  );
  if (record.state === "active" && activeInputs.length === 0) {
    throw new Error("active production funding reservation has no inputs");
  }
  // Conflicted leases stay quarantined until canonical lineage is resolved.
  if (record.state === "released" && activeInputs.length !== 0) {
    throw new Error("released production funding reservation retains inputs");
  }
  return Object.freeze({
    reservationId: record.reservationId,
    deploymentFingerprint: record.deploymentFingerprint,
    decisionDigest: record.decisionDigest,
    policyDigest: record.policyDigest,
    reservationBasisDigest: record.reservationBasisDigest,
    rollbackGeneration: record.rollbackGeneration,
    revision: record.revision,
    walletAddress: record.walletAddress,
    fundingPaymentKeyHash: record.fundingPaymentKeyHash,
    state: record.state,
    activeInputs: Object.freeze(activeInputs),
  });
};

const assertSnapshotInputBounds = ({
  snapshot,
  maximumCollateralInputs,
}: {
  readonly snapshot: WorkflowFundingReservationSnapshot;
  readonly maximumCollateralInputs: number;
}): void => {
  if (
    snapshot.activeInputs.filter(({ role }) => role === "collateral").length >
    maximumCollateralInputs
  ) {
    throw new Error(
      "production funding reservation exceeds its collateral input bound",
    );
  }
};

const parseStateSnapshot = (
  state: PermitState,
  value: unknown,
): WorkflowFundingReservationSnapshot => {
  const snapshot = parseSnapshot(value);
  assertSnapshotInputBounds({
    snapshot,
    maximumCollateralInputs: state.maximumCollateralInputs,
  });
  return snapshot;
};

const exactUtxos = ({
  snapshot,
  utxos,
}: {
  readonly snapshot: WorkflowFundingReservationSnapshot;
  readonly utxos: readonly UTxO[];
}): ReadonlyMap<string, UTxO> => {
  const resolved = new Map<string, UTxO>();
  for (const utxo of utxos) {
    const outRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (!OUT_REF.test(outRef) || resolved.has(outRef)) {
      throw new Error("production funding resolver returned malformed inputs");
    }
    resolved.set(outRef, utxo);
  }
  const expected = snapshot.activeInputs.map(({ outRef }) => outRef);
  const actual = [...resolved.keys()].sort();
  if (
    actual.length !== expected.length ||
    actual.some((outRef, index) => outRef !== expected[index])
  ) {
    throw new Error(
      "production funding resolver changed the reserved input set",
    );
  }
  for (const reserved of snapshot.activeInputs) {
    const utxo = resolved.get(reserved.outRef)!;
    if (utxo.address !== snapshot.walletAddress) {
      throw new Error("production funding resolver returned a foreign address");
    }
    const lovelace = utxo.assets.lovelace;
    if (lovelace?.toString() !== reserved.lovelace) {
      throw new Error("production funding resolver changed reserved lovelace");
    }
    const actualAssets = Object.entries(utxo.assets)
      .filter(([unit]) => unit !== "lovelace")
      .sort(([left], [right]) => left.localeCompare(right));
    if (
      actualAssets.length !== reserved.assets.length ||
      actualAssets.some(
        ([unit, quantity], index) =>
          unit !== reserved.assets[index]!.unit ||
          quantity.toString() !== reserved.assets[index]!.quantity,
      )
    ) {
      throw new Error("production funding resolver changed reserved assets");
    }
  }
  return resolved;
};

const resolveExactOutRefs = async ({
  port,
  outRefs,
  label,
}: {
  readonly port: WorkflowFundingReservationPort;
  readonly outRefs: readonly string[];
  readonly label: string;
}): Promise<ReadonlyMap<string, UTxO>> => {
  const resolved = new Map<string, UTxO>();
  for (const utxo of await port.resolveInputs(outRefs)) {
    const outRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (!OUT_REF.test(outRef) || resolved.has(outRef)) {
      throw new Error(`${label} resolver returned malformed inputs`);
    }
    resolved.set(outRef, utxo);
  }
  if (
    resolved.size !== outRefs.length ||
    outRefs.some((outRef) => !resolved.has(outRef))
  ) {
    throw new Error(`${label} resolver changed the exact input set`);
  }
  return resolved;
};

const exactResolvedOutputCbor = (utxo: UTxO): string =>
  utxoToCore(utxo).output().to_canonical_cbor_hex();

const parseConfirmedActionOutput = (
  value: unknown,
): Readonly<{
  sourceActionKind: string;
  sourceOutputIndex: number;
  outRef: string;
  resolvedOutputCborHex: string;
}> => {
  const record = exact(
    value,
    [
      "sourceActionKind",
      "sourceOutputIndex",
      "outRef",
      "resolvedOutputCborHex",
    ],
    "confirmed production funding action output",
  );
  if (
    typeof record.sourceActionKind !== "string" ||
    !ACTION_KIND.test(record.sourceActionKind) ||
    !Number.isSafeInteger(record.sourceOutputIndex) ||
    (record.sourceOutputIndex as number) < 0 ||
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    typeof record.resolvedOutputCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(record.resolvedOutputCborHex)
  ) {
    throw new Error("confirmed production funding action output is invalid");
  }
  const output = CML.TransactionOutput.from_cbor_hex(
    record.resolvedOutputCborHex,
  );
  if (output.to_canonical_cbor_hex() !== record.resolvedOutputCborHex) {
    throw new Error(
      "confirmed production funding action output is not canonical",
    );
  }
  return Object.freeze({
    sourceActionKind: record.sourceActionKind,
    sourceOutputIndex: record.sourceOutputIndex as number,
    outRef: record.outRef,
    resolvedOutputCborHex: record.resolvedOutputCborHex,
  });
};

const parseProtocolInputAuthority = (
  value: unknown,
): Readonly<{
  deploymentFingerprint: string;
  outRef: string;
  semanticRole: "protocol_state";
  resolvedOutputCborHex: string;
}> => {
  const record = exact(
    value,
    [
      "deploymentFingerprint",
      "outRef",
      "semanticRole",
      "resolvedOutputCborHex",
    ],
    "production protocol input authority",
  );
  if (
    typeof record.deploymentFingerprint !== "string" ||
    !DIGEST.test(record.deploymentFingerprint) ||
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    record.semanticRole !== "protocol_state" ||
    typeof record.resolvedOutputCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(record.resolvedOutputCborHex)
  ) {
    throw new Error("production protocol input authority is invalid");
  }
  const output = CML.TransactionOutput.from_cbor_hex(
    record.resolvedOutputCborHex,
  );
  if (output.to_canonical_cbor_hex() !== record.resolvedOutputCborHex) {
    throw new Error("production protocol input authority is not canonical");
  }
  return Object.freeze({
    deploymentFingerprint: record.deploymentFingerprint,
    outRef: record.outRef,
    semanticRole: "protocol_state",
    resolvedOutputCborHex: record.resolvedOutputCborHex,
  });
};

const refresh = async (state: PermitState): Promise<void> => {
  const snapshot = parseStateSnapshot(state, await state.port.load());
  if (
    snapshot.reservationId !== state.snapshot.reservationId ||
    snapshot.deploymentFingerprint !== state.snapshot.deploymentFingerprint ||
    snapshot.decisionDigest !== state.snapshot.decisionDigest ||
    snapshot.policyDigest !== state.snapshot.policyDigest ||
    snapshot.reservationBasisDigest !== state.snapshot.reservationBasisDigest ||
    snapshot.rollbackGeneration !== state.snapshot.rollbackGeneration ||
    snapshot.walletAddress !== state.snapshot.walletAddress ||
    snapshot.fundingPaymentKeyHash !== state.snapshot.fundingPaymentKeyHash
  ) {
    throw new Error("production funding reservation identity changed");
  }
  const outRefs = snapshot.activeInputs.map(({ outRef }) => outRef);
  state.snapshot = snapshot;
  state.resolvedInputs = exactUtxos({
    snapshot,
    utxos: await state.port.resolveInputs(outRefs),
  });
};

const actionKind = (action: FraudProofWorkflowAction): string => {
  const value =
    typeof action.input.actionKind === "string"
      ? action.input.actionKind
      : action.input.stage;
  if (typeof value !== "string" || !ACTION_KIND.test(value)) {
    throw new Error(
      "production workflow action omitted its stable action kind",
    );
  }
  return value;
};

const stateForJournal = (journal: object): PermitState | undefined =>
  journalPermits.get(journal);

export const createWorkflowFundingReservationPermit = async ({
  category,
  runner,
  policy,
  actuationPermit,
  rollbackGeneration,
  port,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: WorkflowAdapterRunner;
  readonly policy: WorkflowRuntimeFundingPolicy;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly rollbackGeneration: string;
  readonly port: WorkflowFundingReservationPort;
}): Promise<WorkflowFundingReservationPermit> => {
  const actuation = assertWorkflowActuationPermitIdentity({
    permit: actuationPermit,
    category,
    rollbackGeneration,
  });
  assertWorkflowRuntimeFundingPolicyRunner({ policy, runner, category });
  const funding = readWorkflowRuntimeFundingPolicy(policy);
  const snapshot = parseSnapshot(await port.load());
  const maximumCollateralInputs = Number(funding.maximumCollateralInputs);
  if (
    snapshot.deploymentFingerprint !== actuation.deploymentFingerprint ||
    snapshot.deploymentFingerprint !== funding.deploymentFingerprint ||
    snapshot.decisionDigest !== actuation.executionDecisionDigest ||
    snapshot.rollbackGeneration !== rollbackGeneration ||
    snapshot.policyDigest !== funding.policyDigest ||
    snapshot.fundingPaymentKeyHash !== funding.fundingPaymentKeyHash ||
    (snapshot.state !== "active" && snapshot.state !== "released")
  )
    throw new Error(
      "production funding reservation does not match its runner authority",
    );
  if (snapshot.state === "released") {
    const handoff = parseWorkflowFundingCompletionHandoff(
      await port.readCompletionHandoff(),
    );
    if (
      handoff.identity.deploymentFingerprint !==
        snapshot.deploymentFingerprint ||
      handoff.identity.decisionDigest !== snapshot.decisionDigest ||
      handoff.identity.category !== category ||
      handoff.identity.target.kind !== "state_queue_header" ||
      handoff.identity.target.headerHash !== actuation.headerHash
    )
      throw new Error(
        "released funding reservation has no exact terminal recovery identity",
      );
  }
  const address = CML.Address.from_bech32(
    snapshot.walletAddress,
  ).to_raw_bytes();
  if (
    address.length !== 29 ||
    address[0]! >> 4 !== 6 ||
    Buffer.from(address.subarray(1)).toString("hex") !==
      funding.fundingPaymentKeyHash
  )
    throw new Error(
      "production funding reservation has a foreign wallet credential",
    );
  assertSnapshotInputBounds({ snapshot, maximumCollateralInputs });
  const permit: WorkflowFundingReservationPermit = Object.freeze({
    permitVersion: WORKFLOW_FUNDING_RESERVATION_PERMIT,
  });
  admittedPermits.set(permit, {
    category,
    policy,
    actuationPermit,
    port,
    maximumCollateralInputs,
    snapshot,
    // A restarted workflow must first reconcile its durable pending intent.
    // Its inputs may already be consumed by that exact confirmed transaction.
    // Only begin/recheck, after journal reconciliation, demand live UTxOs.
    resolvedInputs: new Map(),
    boundJournal: undefined,
    currentActionKind: undefined,
    currentActionDigest: undefined,
    currentFundingOutRefs: Object.freeze([]),
    currentCollateralOutRefs: Object.freeze([]),
    pendingTransactionHash: undefined,
    preparedTransaction: undefined,
  });
  return permit;
};

export const bindWorkflowFundingReservationJournal = <Journal extends object>({
  journal,
  permit,
}: {
  readonly journal: Journal;
  readonly permit: WorkflowFundingReservationPermit;
}): Journal => {
  const state = admittedPermits.get(permit);
  if (
    permit.permitVersion !== WORKFLOW_FUNDING_RESERVATION_PERMIT ||
    state === undefined
  ) {
    throw new Error("production funding reservation permit was not admitted");
  }
  if (journalPermits.has(journal)) {
    throw new Error(
      "workflow journal already has funding reservation authority",
    );
  }
  if (state.boundJournal !== undefined) {
    throw new Error(
      "production funding reservation permit is already bound to a workflow journal",
    );
  }
  state.boundJournal = journal;
  journalPermits.set(journal, state);
  return journal;
};

const assertFundingSubmissionAuthority = (state: PermitState): void => {
  if (workflowActuationPermitIsReconciliationOnly(state.actuationPermit))
    throw new Error(
      "reconciliation-only funding authority cannot spend or sign",
    );
};

export const beginWorkflowFundingReservationAction = async ({
  journal,
  action,
}: {
  readonly journal: object;
  readonly action: FraudProofWorkflowAction;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  if (state.policy === undefined)
    throw new Error("test-only funding permit cannot build transactions");
  assertFundingSubmissionAuthority(state);
  if ((await state.port.readAbandonmentHandoff()) !== null)
    throw new Error(
      "funding abandonment outcome awaits journal acknowledgment",
    );
  await refresh(state);
  assertFundingSubmissionAuthority(state);
  if (state.snapshot.state !== "active")
    throw new Error("production funding reservation is not active");
  state.currentActionKind = actionKind(action);
  state.currentActionDigest = computeDeploymentManifestJsonDigest(action);
  // The real builder selects from durable leased candidates; admission below
  // derives the exact consumed subset from its signed transaction.
  state.currentFundingOutRefs = Object.freeze(
    state.snapshot.activeInputs
      .filter(({ role }) => role === "funding")
      .map(({ outRef }) => outRef),
  );
  state.currentCollateralOutRefs = Object.freeze(
    state.snapshot.activeInputs
      .filter(({ role }) => role === "collateral")
      .map(({ outRef }) => outRef),
  );
};

const bodySha256 = (signed: TxSigned): string =>
  createHash("sha256")
    .update(Buffer.from(signed.toTransaction().body().to_cbor_hex(), "hex"))
    .digest("hex");

const addAssets = (
  totals: Map<string, bigint>,
  assets: Readonly<Record<string, bigint>>,
): void => {
  for (const [unit, quantity] of Object.entries(assets)) {
    totals.set(unit, (totals.get(unit) ?? 0n) + quantity);
  }
};

const isProtocolFundingContract = (contract: {
  readonly role: string;
}): boolean =>
  contract.role === "protocol_state" || contract.role === "correction_lock";

const assertRuntimeTransactionBound = async ({
  state,
  action,
  signed,
  bodyInputs,
  fundingOutRefs,
  collateralOutRefs,
}: {
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
  readonly signed: TxSigned;
  readonly bodyInputs: readonly string[];
  readonly fundingOutRefs: readonly string[];
  readonly collateralOutRefs: readonly string[];
}): Promise<void> => {
  if (state.policy === undefined)
    throw new Error("runtime funding policy is missing");
  const policy = readWorkflowRuntimeFundingPolicy(state.policy);
  const parameters = policy.protocolParameters;
  const transaction = signed.toTransaction();
  const body = transaction.body();
  const witnesses = transaction.witness_set();
  const signedBytes = BigInt(transaction.to_cbor_hex().length / 2);
  const contracts = new Map(
    policy.contracts.map((contract) => [contract.address, contract]),
  );
  const slash = readFraudSlashFundingAuthority(signed);
  if (slash !== null) {
    const actuation = assertWorkflowActuationPermitIdentity({
      permit: state.actuationPermit,
      category: state.category,
      rollbackGeneration: state.snapshot.rollbackGeneration,
    });
    const economics = policy.economics.policy;
    const bond =
      BigInt(economics.requiredBondLovelace) -
      (slash.tranche === "full"
        ? 0n
        : BigInt(economics.inactivitySlashingPenaltyLovelace));
    const reward = BigInt(economics.fraudProverRewardLovelace);
    if (
      state.currentActionKind !== "remove" ||
      actionKind(action) !== "remove" ||
      action.input.stage !== "remove" ||
      state.currentActionDigest !==
        computeDeploymentManifestJsonDigest(action) ||
      action.input.nextRemovalOutRef !== slash.removedStateQueueOutRef ||
      action.input.fraudProofOutRef !== slash.fraudProofOutRef ||
      slash.category !== state.category ||
      slash.headerHash !== actuation.headerHash ||
      slash.deploymentFingerprint !== policy.deploymentFingerprint ||
      slash.economicsPolicyDigest !== policy.economicsPolicyDigest ||
      slash.rewardAddress !== state.snapshot.walletAddress ||
      BigInt(slash.operatorBondLovelace) !== bond ||
      BigInt(slash.rewardLovelace) !== reward ||
      BigInt(slash.exactFeeLovelace) !== bond - reward ||
      body.fee() !== bond - reward ||
      fundingOutRefs.length !== 0 ||
      slash.inputs.length !== bodyInputs.length ||
      slash.inputs.some((input, index) => input.outRef !== bodyInputs[index]) ||
      !bodyInputs.includes(slash.operatorOutRef) ||
      !bodyInputs.includes(slash.removedStateQueueOutRef) ||
      !workflowTransactionReferenceInputOutRefs(signed).includes(
        slash.fraudProofOutRef,
      )
    )
      throw new Error(
        "fraud slash funding authority differs from its exact removal action or economics",
      );
  }
  if (signedBytes > BigInt(parameters.maxTxSize))
    throw new Error("signed transaction exceeds protocol maxTxSize");
  if (!transaction.is_valid())
    throw new Error("funding transaction declares script failure");
  const bodyHash = CML.hash_transaction(body);
  if (signed.toHash().toLowerCase() !== bodyHash.to_hex())
    throw new Error("funding transaction hash differs from its actual body");
  const vkeys = witnesses.vkeywitnesses();
  let fundingSignature = false;
  for (let index = 0; index < (vkeys?.len() ?? 0); index += 1) {
    const witness = vkeys!.get(index);
    if (
      witness.vkey().hash().to_hex() === policy.fundingPaymentKeyHash &&
      witness
        .vkey()
        .verify(bodyHash.to_raw_bytes(), witness.ed25519_signature())
    )
      fundingSignature = true;
  }
  if (!fundingSignature)
    throw new Error("funding transaction lacks the reserved wallet signature");
  if (
    (witnesses.native_scripts()?.len() ?? 0) +
      (witnesses.plutus_v1_scripts()?.len() ?? 0) +
      (witnesses.plutus_v2_scripts()?.len() ?? 0) +
      (witnesses.plutus_v3_scripts()?.len() ?? 0) !==
    0
  )
    throw new Error("funding transaction embeds executable script witnesses");
  let memory = 0n,
    steps = 0n;
  const redeemers = witnesses.redeemers()?.to_flat_format();
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    memory += redeemers!.get(index).ex_units().mem();
    steps += redeemers!.get(index).ex_units().steps();
  }
  if (
    memory > BigInt(parameters.maxTxExUnits.memory) ||
    steps > BigInt(parameters.maxTxExUnits.steps)
  )
    throw new Error("funding transaction exceeds protocol maxTxExUnits");
  const references = await resolveExactOutRefs({
    port: state.port,
    outRefs: [...workflowTransactionReferenceInputOutRefs(signed)].sort(),
    label: "runtime funding reference inputs",
  });
  let referenceScriptBytes = 0n;
  const scriptIdentities = new Map(
    policy.referenceScripts.map(({ outRef, scriptHash }) => [
      outRef,
      scriptHash,
    ]),
  );
  for (const [outRef, reference] of references) {
    const expected = scriptIdentities.get(outRef);
    if (reference.scriptRef == null) {
      if (expected !== undefined)
        throw new Error(
          "funding transaction lost its deployed reference script",
        );
      continue;
    }
    if (expected !== validatorToScriptHash(reference.scriptRef))
      throw new Error(
        "funding transaction uses an ungoverned reference script",
      );
    referenceScriptBytes += BigInt(reference.scriptRef.script.length / 2);
  }
  if (
    referenceScriptBytes >
    BigInt(parameters.referenceScriptFee.maximumSizeBytes)
  )
    throw new Error("funding transaction exceeds reference-script byte limit");
  const minimumFee = workflowRuntimeFundingMinimumFee({
    parameters,
    transactionBytes: signedBytes,
    memory,
    steps,
    referenceScriptBytes,
  });
  if (
    body.fee() < minimumFee ||
    (slash === null && body.fee() > BigInt(policy.maximumFeeLovelace))
  )
    throw new Error(
      "funding transaction fee is outside the live protocol funding bounds",
    );
  const scriptExecution = (redeemers?.len() ?? 0) !== 0;
  const collateral = await resolveExactOutRefs({
    port: state.port,
    outRefs: collateralOutRefs,
    label: "runtime funding collateral inputs",
  });
  const collateralValue = [...collateral.values()].reduce((total, input) => {
    if (
      input.address !== state.snapshot.walletAddress ||
      Object.keys(input.assets).some((unit) => unit !== "lovelace")
    )
      throw new Error("funding collateral is not reserved-wallet pure Ada");
    return total + (input.assets.lovelace ?? 0n);
  }, 0n);
  const declaredCollateral = body.total_collateral();
  const collateralReturn = body.collateral_return();
  if (scriptExecution) {
    const percentage =
      (body.fee() * BigInt(parameters.collateralPercentage) + 99n) / 100n;
    const floor = BigInt(policy.economics.policy.proverCollateralFloorLovelace);
    const reservedCollateral = state.snapshot.activeInputs
      .filter(({ role }) => role === "collateral")
      .reduce((sum, input) => sum + BigInt(input.lovelace), 0n);
    if (reservedCollateral < floor)
      throw new Error(
        "funding collateral reservation is below the release floor",
      );
    if (
      collateralOutRefs.length === 0 ||
      collateralOutRefs.length > state.maximumCollateralInputs ||
      declaredCollateral === undefined ||
      declaredCollateral < percentage ||
      declaredCollateral >
        BigInt(
          slash === null
            ? policy.maximumCollateralLovelace
            : policy.maximumSlashCollateralLovelace,
        ) ||
      collateralValue < declaredCollateral
    )
      throw new Error(
        "funding transaction collateral is outside the exact funding bounds",
      );
    if (
      collateralReturn !== undefined &&
      (collateralReturn.address().to_bech32() !==
        state.snapshot.walletAddress ||
        collateralReturn.amount().has_multiassets())
    )
      throw new Error("funding collateral return escapes the reserved wallet");
    if (
      collateralReturn !== undefined &&
      collateralReturn.amount().coin() <
        CML.min_ada_required(
          collateralReturn,
          BigInt(parameters.coinsPerUtxoByte),
        )
    )
      throw new Error("funding collateral return is below exact min-Ada");
    if (
      collateralValue !==
      declaredCollateral + (collateralReturn?.amount().coin() ?? 0n)
    )
      throw new Error("funding collateral value is not conserved");
  } else if (
    collateralOutRefs.length !== 0 ||
    declaredCollateral !== undefined ||
    collateralReturn !== undefined
  )
    throw new Error(
      "non-script funding transaction unexpectedly declares collateral",
    );
  const allInputs = await resolveExactOutRefs({
    port: state.port,
    outRefs: bodyInputs,
    label: "runtime funding ordinary inputs",
  });
  const inputAssets = new Map<string, bigint>();
  const walletAssets = new Map<string, bigint>();
  let releasedCustody = 0n;
  let protocolInputLovelace = 0n;
  for (const [outRef, input] of allInputs) {
    addAssets(inputAssets, input.assets);
    if (input.address === state.snapshot.walletAddress) {
      if (slash !== null)
        throw new Error("fraud slash cannot consume ordinary wallet funding");
      if (!fundingOutRefs.includes(outRef))
        throw new Error(
          "signed transaction consumed an unreserved wallet input",
        );
      addAssets(walletAssets, input.assets);
      continue;
    }
    const contract = contracts.get(input.address);
    if (contract === undefined)
      throw new Error(
        "funding transaction consumed an ungoverned contract input",
      );
    if (
      slash !== null &&
      slash.inputs.find((value) => value.outRef === outRef)
        ?.resolvedOutputCborHex !== exactResolvedOutputCbor(input)
    ) {
      throw new Error(
        "fraud slash protocol input changed after local evaluation",
      );
    }
    if (
      slash !== null &&
      outRef === slash.operatorOutRef &&
      (input.assets.lovelace ?? 0n).toString() !== slash.operatorBondLovelace
    ) {
      throw new Error(
        "fraud slash operator bond differs from its authenticated tranche",
      );
    }
    // Slashing always reacquires live protocol authority, even for an output
    // whose earlier transaction already appears in this workflow's lineage.
    const lineage =
      slash === null
        ? await state.port.resolveConfirmedInput({ outRef })
        : null;
    if (lineage !== null) {
      const confirmed = parseConfirmedActionOutput(lineage);
      if (
        confirmed.outRef !== outRef ||
        confirmed.resolvedOutputCborHex !== exactResolvedOutputCbor(input)
      )
        throw new Error(
          "funding released custody differs from its exact confirmed lineage",
        );
      if (!isProtocolFundingContract(contract))
        releasedCustody += input.assets.lovelace ?? 0n;
      else protocolInputLovelace += input.assets.lovelace ?? 0n;
      continue;
    }
    if (!isProtocolFundingContract(contract))
      throw new Error(
        "funding released custody lacks confirmed workflow lineage",
      );
    const authority = parseProtocolInputAuthority(
      await state.port.resolveProtocolInputAuthority({
        deploymentFingerprint: policy.deploymentFingerprint,
        outRef,
        semanticRole: "protocol_state",
      }),
    );
    if (
      authority.deploymentFingerprint !== policy.deploymentFingerprint ||
      authority.outRef !== outRef ||
      authority.resolvedOutputCborHex !== exactResolvedOutputCbor(input)
    )
      throw new Error(
        "funding protocol input differs from its deployment-bound authority",
      );
    protocolInputLovelace += input.assets.lovelace ?? 0n;
  }
  const outputAssets = new Map<string, bigint>();
  const walletOutputAssets = new Map<string, bigint>();
  const outputs = body.outputs();
  let custody = 0n,
    custodyAllocation = 0n,
    protocolOutputs = 0n,
    protocolMinimum = 0n;
  let rewardOutputs = 0;
  for (let index = 0; index < outputs.len(); index += 1) {
    const raw = outputs.get(index);
    const output = coreToTxOutput(raw);
    const minimum = CML.min_ada_required(
      raw,
      BigInt(parameters.coinsPerUtxoByte),
    );
    if (
      raw.amount().coin() < minimum ||
      BigInt(raw.amount().to_canonical_cbor_hex().length / 2) >
        BigInt(parameters.maxValueSize)
    )
      throw new Error("funding output violates exact min-Ada or maxValueSize");
    addAssets(outputAssets, output.assets);
    if (output.address === state.snapshot.walletAddress) {
      if (slash !== null) {
        rewardOutputs += 1;
        if (
          raw.amount().coin().toString() !== slash.rewardLovelace ||
          raw.amount().has_multiassets() ||
          raw.datum() !== undefined ||
          raw.script_ref() !== undefined
        ) {
          throw new Error(
            "fraud slash reward differs from exact release economics",
          );
        }
      }
      addAssets(walletOutputAssets, output.assets);
      continue;
    }
    const contract = contracts.get(output.address);
    if (contract === undefined)
      throw new Error("funding output escapes the governed contract roster");
    if (isProtocolFundingContract(contract)) {
      protocolOutputs += raw.amount().coin();
      protocolMinimum += minimum;
    } else {
      custody += raw.amount().coin();
      // Operator stake and slashing rewards never authorize prover topups.
      // Existing custody is accounted from exact confirmed lineage above.
      custodyAllocation += minimum;
    }
  }
  if (custody > releasedCustody + custodyAllocation)
    throw new Error(
      `funding transaction exceeds its governed custody allocation: actual=${custody.toString()} allowed=${(releasedCustody + custodyAllocation).toString()}`,
    );
  const protocolTopup =
    protocolOutputs > protocolInputLovelace
      ? protocolOutputs - protocolInputLovelace
      : 0n;
  if (
    slash !== null &&
    (rewardOutputs !== 1 ||
      custody !== 0n ||
      releasedCustody !== 0n ||
      protocolTopup !== 0n ||
      protocolInputLovelace !==
        protocolOutputs + body.fee() + BigInt(slash.rewardLovelace))
  )
    throw new Error(
      "fraud slash must preserve protocol state and fund only its exact fee and reward",
    );
  if (protocolTopup > protocolMinimum)
    throw new Error(
      "funding transaction exceeds exact protocol min-Ada topups",
    );
  const walletSpent =
    (walletAssets.get("lovelace") ?? 0n) -
    (walletOutputAssets.get("lovelace") ?? 0n);
  const custodyIncrease =
    custody > releasedCustody ? custody - releasedCustody : 0n;
  if (walletSpent > body.fee() + custodyIncrease + protocolTopup)
    throw new Error(
      "signed transaction loses reserved wallet value outside fees and governed custody",
    );
  for (const [unit, quantity] of walletAssets) {
    if (unit !== "lovelace" && (walletOutputAssets.get(unit) ?? 0n) < quantity)
      throw new Error("signed transaction loses reserved wallet native assets");
  }
  const mint = body.mint();
  if (mint !== undefined) {
    const policies = mint.keys();
    for (let i = 0; i < policies.len(); i += 1) {
      const policyHash = policies.get(i);
      const assets = mint.get_assets(policyHash)!;
      const names = assets.keys();
      for (let j = 0; j < names.len(); j += 1) {
        const name = names.get(j),
          unit = policyHash.to_hex() + name.to_hex();
        inputAssets.set(
          unit,
          (inputAssets.get(unit) ?? 0n) + assets.get(name)!,
        );
      }
    }
  }
  // Proof workflows may use withdraw-zero yielding, but never withdraw funds
  // or certify/deposit stake through the prover funding surface.
  const withdrawals = body.withdrawals();
  if (withdrawals !== undefined) {
    const keys = withdrawals.keys();
    for (let index = 0; index < keys.len(); index += 1)
      if (withdrawals.get(keys.get(index)) !== 0n)
        throw new Error("funding transaction includes a nonzero withdrawal");
  }
  if ((body.certs()?.len() ?? 0) !== 0 || body.donation() !== undefined)
    throw new Error("funding transaction changes unrelated ledger accounting");
  outputAssets.set(
    "lovelace",
    (outputAssets.get("lovelace") ?? 0n) + body.fee(),
  );
  for (const unit of new Set([...inputAssets.keys(), ...outputAssets.keys()]))
    if ((inputAssets.get(unit) ?? 0n) !== (outputAssets.get(unit) ?? 0n))
      throw new Error(
        "funding transaction does not conserve exact ledger value",
      );
};

const producedFundingInputs = ({
  signed,
  walletAddress,
}: {
  readonly signed: TxSigned;
  readonly walletAddress: string;
}): readonly WorkflowFundingReservedInput[] => {
  const body = signed.toTransaction().body();
  const outputs = body.outputs();
  const transactionHash = signed.toHash().toLowerCase();
  const produced: WorkflowFundingReservedInput[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    if (output.address().to_bech32() !== walletAddress) continue;
    const utxo = CML.TransactionUnspentOutput.new(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(transactionHash),
        BigInt(index),
      ),
      output,
    );
    const decoded = coreToUtxo(utxo);
    const assets = Object.entries(decoded.assets)
      .filter(([unit]) => unit !== "lovelace")
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([unit, quantity]) =>
        Object.freeze({ unit, quantity: quantity.toString() }),
      );
    produced.push(
      Object.freeze({
        outRef: `${transactionHash}#${index.toString()}`,
        role: "funding" as const,
        lovelace: decoded.assets.lovelace!.toString(),
        assets: Object.freeze(assets),
      }),
    );
  }
  if (produced.length === 0) {
    throw new Error("production transaction omitted reserved-wallet change");
  }
  return Object.freeze(produced);
};

export const prepareWorkflowFundingReservationTransaction = async ({
  journal,
  action,
  preflight,
  handoff,
}: {
  readonly journal: object;
  readonly action: FraudProofWorkflowAction;
  readonly preflight: object;
  readonly handoff: WorkflowFundingSubmissionHandoff;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  const signed = workflowPreflightTransaction(preflight);
  if (signed === undefined) {
    throw new Error(
      "production preflight omitted its captured signed transaction",
    );
  }
  const kind = actionKind(action);
  if (state.currentActionKind !== kind) {
    throw new Error(
      "production funding action changed after reservation admission",
    );
  }
  const candidates = canonicalOutRefs(
    state.currentFundingOutRefs,
    "reserved funding inputs",
  );
  const collateralCandidates = canonicalOutRefs(
    state.currentCollateralOutRefs,
    "reserved collateral inputs",
  );
  const bodyInputs = [...workflowTransactionInputOutRefs(signed)].sort();
  const collateralOutRefs = [
    ...workflowTransactionCollateralInputOutRefs(signed),
  ].sort();
  const fundingOutRefs = Object.freeze(
    bodyInputs.filter((outRef) => candidates.includes(outRef)),
  );
  if (
    bodyInputs.some((outRef) => collateralCandidates.includes(outRef)) ||
    collateralOutRefs.some((outRef) => !collateralCandidates.includes(outRef))
  )
    throw new Error(
      "signed transaction changed reserved ordinary/collateral separation",
    );
  await assertRuntimeTransactionBound({
    state,
    action,
    signed,
    bodyInputs,
    fundingOutRefs,
    collateralOutRefs,
  });
  const transactionHash = signed.toHash().toLowerCase();
  const admittedHandoff = parseWorkflowFundingSubmissionHandoff(handoff);
  if (
    admittedHandoff.identity.deploymentFingerprint !==
      state.snapshot.deploymentFingerprint ||
    admittedHandoff.identity.decisionDigest !== state.snapshot.decisionDigest ||
    admittedHandoff.submissionIntent.txHash !== transactionHash ||
    admittedHandoff.submissionIntent.actionId !== action.actionId ||
    journalJsonDigest(admittedHandoff.submissionIntent.actionInput) !==
      journalJsonDigest(action.input)
  )
    throw new Error("funding handoff changed the evaluated workflow action");
  const transition = Object.freeze({
    actionKind: kind,
    signedTransactionCborHex: signed.toTransaction().to_cbor_hex(),
    transactionHash,
    transactionBodySha256: bodySha256(signed),
    consumedOutRefs: fundingOutRefs,
    producedInputs: producedFundingInputs({
      signed,
      walletAddress: state.snapshot.walletAddress,
    }),
  });
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.prepare({
      expectedRevision: state.snapshot.revision,
      transition,
      handoff: admittedHandoff,
    }),
  );
  state.pendingTransactionHash = transactionHash;
  state.preparedTransaction = Object.freeze({
    signed,
    cborHex: transition.signedTransactionCborHex,
  });
};

export const assertWorkflowFundingReservationReadyToSubmit = async ({
  journal,
  transactionHash,
}: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  assertFundingSubmissionAuthority(state);
  if ((await state.port.readAbandonmentHandoff()) !== null)
    throw new Error(
      "funding abandonment outcome awaits journal acknowledgment",
    );
  const expectedRevision = state.snapshot.revision;
  const expectedPending = state.pendingTransactionHash;
  await refresh(state);
  if (state.preparedTransaction !== undefined) {
    const { signed, cborHex } = state.preparedTransaction;
    if (
      signed.toHash().toLowerCase() !== transactionHash ||
      signed.toTransaction().to_cbor_hex() !== cborHex
    ) {
      throw new Error("prepared funding transaction changed before submission");
    }
    readFraudSlashFundingAuthority(signed);
  }
  if (
    state.snapshot.state !== "active" ||
    state.snapshot.revision !== expectedRevision ||
    expectedPending !== transactionHash ||
    state.pendingTransactionHash !== transactionHash
  ) {
    throw new Error("production funding reservation changed before submission");
  }
};

/** Read durable signed material without requiring its already-spent inputs to remain live. */
export const readWorkflowFundingRecovery = async (
  journal: object,
): Promise<
  Readonly<{
    transition: WorkflowFundingPreparedTransition | null;
    submissionHandoff: WorkflowFundingSubmissionHandoff | null;
    completionHandoff: WorkflowFundingCompletionHandoff | null;
    abandonmentHandoff: WorkflowFundingAbandonmentHandoff | null;
  }>
> => {
  const state = stateForJournal(journal);
  if (state === undefined)
    return {
      transition: null,
      submissionHandoff: null,
      completionHandoff: null,
      abandonmentHandoff: null,
    };
  const rawTransition = await state.port.readPendingTransition();
  let transition =
    rawTransition === null
      ? null
      : parseWorkflowFundingPreparedTransition(rawTransition);
  const rawSubmission = await state.port.readPendingHandoff();
  let submissionHandoff: WorkflowFundingSubmissionHandoff | null = null;
  if (rawSubmission !== null) {
    const record = exact(
      rawSubmission,
      ["transition", "handoff"],
      "funding pending handoff record",
    );
    const savedTransition = parseWorkflowFundingPreparedTransition(
      record.transition,
    );
    if (
      transition === null ||
      computeDeploymentManifestJsonDigest(savedTransition) !==
        computeDeploymentManifestJsonDigest(transition)
    )
      throw new Error(
        "funding handoff differs from its pending signed transaction",
      );
    submissionHandoff = parseWorkflowFundingSubmissionHandoff(record.handoff);
    if (
      submissionHandoff.submissionIntent.txHash !== transition.transactionHash
    )
      throw new Error("funding handoff changed its transaction hash");
  }
  const rawAbandonment = await state.port.readAbandonmentHandoff();
  let abandonmentHandoff: WorkflowFundingAbandonmentHandoff | null = null;
  if (rawAbandonment !== null) {
    if (transition !== null || submissionHandoff !== null)
      throw new Error(
        "funding recovery has both pending and abandoned transactions",
      );
    const record = exact(
      rawAbandonment,
      ["transition", "handoff"],
      "funding abandonment record",
    );
    transition = parseWorkflowFundingPreparedTransition(record.transition);
    abandonmentHandoff = parseWorkflowFundingAbandonmentHandoff(record.handoff);
    if (
      abandonmentHandoff.submissionIntent.txHash !== transition.transactionHash
    )
      throw new Error(
        "funding abandonment changed its signed transaction hash",
      );
  }
  const rawCompletion = await state.port.readCompletionHandoff();
  const completionHandoff =
    rawCompletion === null
      ? null
      : parseWorkflowFundingCompletionHandoff(rawCompletion);
  for (const handoff of [
    submissionHandoff,
    completionHandoff,
    abandonmentHandoff,
  ]) {
    if (
      handoff !== null &&
      (handoff.identity.deploymentFingerprint !==
        state.snapshot.deploymentFingerprint ||
        handoff.identity.decisionDigest !== state.snapshot.decisionDigest ||
        handoff.identity.category !== state.category)
    )
      throw new Error(
        "funding recovery handoff has a foreign workflow identity",
      );
  }
  if (transition !== null && completionHandoff !== null)
    throw new Error(
      "released funding recovery retains an unresolved transaction",
    );
  state.pendingTransactionHash = transition?.transactionHash;
  return Object.freeze({
    transition,
    submissionHandoff,
    completionHandoff,
    abandonmentHandoff,
  });
};

const applyTransition = async ({
  journal,
  outcome,
  transactionHash,
}: {
  readonly journal: object;
  readonly outcome: "confirmed" | "conflict";
  readonly transactionHash: string;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  if (
    state.pendingTransactionHash !== undefined &&
    state.pendingTransactionHash !== transactionHash
  ) {
    throw new Error(
      "funding reservation reconciliation changed transaction hash",
    );
  }
  const next =
    outcome === "confirmed"
      ? await state.port.confirm({
          expectedRevision: state.snapshot.revision,
          transactionHash,
        })
      : await state.port.markConflict({
          expectedRevision: state.snapshot.revision,
          code: "unexpected_spend",
        });
  state.snapshot = parseStateSnapshot(state, next);
  state.pendingTransactionHash = undefined;
  state.preparedTransaction = undefined;
  state.currentActionKind = undefined;
  state.currentActionDigest = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
};

export const confirmWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => await applyTransition({ ...input, outcome: "confirmed" });

export const abandonWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
  readonly handoff: WorkflowFundingAbandonmentHandoff;
}): Promise<void> => {
  const state = stateForJournal(input.journal);
  if (state === undefined) return;
  const handoff = parseWorkflowFundingAbandonmentHandoff(input.handoff);
  if (
    handoff.submissionIntent.txHash !== input.transactionHash ||
    (state.pendingTransactionHash !== undefined &&
      state.pendingTransactionHash !== input.transactionHash)
  )
    throw new Error(
      "funding abandonment changed its exact transaction identity",
    );
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.abandon({
      expectedRevision: state.snapshot.revision,
      transactionHash: input.transactionHash,
      handoff,
    }),
  );
  state.preparedTransaction = undefined;
  state.currentActionKind = undefined;
  state.currentActionDigest = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
  // The recorded bytes remain available until the exact journal outcome is acknowledged.
  state.pendingTransactionHash = input.transactionHash;
};

export const acknowledgeWorkflowFundingAbandonment = async (input: {
  readonly journal: FraudProofWorkflowJournalStore;
  readonly handoff: WorkflowFundingAbandonmentHandoff;
}): Promise<void> => {
  const state = stateForJournal(input.journal);
  if (state === undefined) return;
  const handoff = parseWorkflowFundingAbandonmentHandoff(input.handoff);
  if (
    !assertWorkflowFundingAbandonmentHandoffJournal({
      handoff,
      entries: await input.journal.load(handoff.workflowId),
    })
  )
    throw new Error(
      "funding abandonment outcome is not durable in its journal",
    );
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.acknowledgeAbandonment({
      expectedRevision: state.snapshot.revision,
      handoff,
    }),
  );
  state.pendingTransactionHash = undefined;
};

export const conflictWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => await applyTransition({ ...input, outcome: "conflict" });

export const releaseWorkflowFundingReservation = async ({
  journal,
  handoff,
}: {
  readonly journal: object;
  readonly handoff: WorkflowFundingCompletionHandoff;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.release({
      expectedRevision: state.snapshot.revision,
      handoff: parseWorkflowFundingCompletionHandoff(handoff),
    }),
  );
  state.currentActionKind = undefined;
  state.currentActionDigest = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
  state.pendingTransactionHash = undefined;
  state.preparedTransaction = undefined;
};

const balanceCbor = (utxos: readonly UTxO[]): string => {
  const assets: Record<string, bigint> = {};
  for (const utxo of utxos) {
    for (const [unit, quantity] of Object.entries(utxo.assets)) {
      assets[unit] = (assets[unit] ?? 0n) + quantity;
    }
  }
  return assetsToValue(assets).to_cbor_hex();
};

/**
 * Returns a signer whose wallet API exposes exactly the currently reserved
 * ordinary and collateral UTxOs. Signing and submission still delegate to the
 * original enterprise-key wallet.
 */
export const restrictWorkflowFundingSigner = ({
  signer,
  permit,
}: {
  readonly signer: ResolvedProverSigner;
  readonly permit: WorkflowFundingReservationPermit;
}): ResolvedProverSigner => {
  const state = admittedPermits.get(permit);
  if (state === undefined) {
    throw new Error("production funding reservation permit was not admitted");
  }
  if (
    signer.address !== state.snapshot.walletAddress ||
    signer.paymentKeyHash !== state.snapshot.fundingPaymentKeyHash
  ) {
    throw new Error("production signer differs from funding reservation");
  }
  return Object.freeze({
    ...signer,
    selectWallet: (lucid: LucidEvolution): void => {
      assertFundingSubmissionAuthority(state);
      if (state.currentActionKind === undefined) {
        throw new Error(
          "production signer used before a reserved action began",
        );
      }
      signer.selectWallet(lucid);
      const original = lucid.wallet();
      const funding = state.currentFundingOutRefs.map(
        (outRef) => state.resolvedInputs.get(outRef)!,
      );
      const collateral = state.currentCollateralOutRefs.map(
        (outRef) => state.resolvedInputs.get(outRef)!,
      );
      const addressHex = CML.Address.from_bech32(signer.address).to_hex();
      const api: WalletApi = Object.freeze({
        getNetworkId: async () =>
          CML.Address.from_bech32(signer.address).to_raw_bytes()[0]! & 0x0f,
        getUtxos: async () =>
          funding.map((utxo) => utxoToCore(utxo).to_cbor_hex()),
        getBalance: async () => balanceCbor(funding),
        getUsedAddresses: async () => [addressHex],
        getUnusedAddresses: async () => [],
        getChangeAddress: async () => addressHex,
        getRewardAddresses: async () => [],
        signTx: async (tx) => {
          assertFundingSubmissionAuthority(state);
          return (
            await original.signTx(CML.Transaction.from_cbor_hex(tx))
          ).to_cbor_hex();
        },
        signData: async (address, payload) =>
          await original.signMessage(
            CML.Address.from_hex(address).to_bech32(),
            payload,
          ),
        submitTx: async (tx) => {
          assertFundingSubmissionAuthority(state);
          return await original.submitTx(tx);
        },
        getCollateral: async () =>
          collateral.map((utxo) => utxoToCore(utxo).to_cbor_hex()),
        experimental: Object.freeze({
          getCollateral: async () =>
            collateral.map((utxo) => utxoToCore(utxo).to_cbor_hex()),
          on: () => undefined,
          off: () => undefined,
        }),
      });
      lucid.selectWallet.fromAPI(api);
    },
  });
};

/** Test-only identity seam for runtime lifecycle tests that never build a tx. */
export const unsafeCreateWorkflowFundingReservationPermitForTest = ({
  category,
  actuationPermit,
  deploymentFingerprint,
  decisionDigest,
  rollbackGeneration,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly deploymentFingerprint: string;
  readonly decisionDigest: string;
  readonly rollbackGeneration: string;
}): WorkflowFundingReservationPermit => {
  if (process.env.NODE_ENV !== "test") {
    throw new Error("unsafe funding reservation permit is test-only");
  }
  const identity = assertWorkflowActuationPermitIdentity({
    permit: actuationPermit,
    category,
    rollbackGeneration,
  });
  if (
    identity.deploymentFingerprint !== deploymentFingerprint ||
    identity.decisionDigest !== decisionDigest
  ) {
    throw new Error("test funding reservation identity mismatch");
  }
  const permit: WorkflowFundingReservationPermit = Object.freeze({
    permitVersion: WORKFLOW_FUNDING_RESERVATION_PERMIT,
  });
  const snapshot: WorkflowFundingReservationSnapshot = Object.freeze({
    reservationId: "01".repeat(32),
    deploymentFingerprint,
    decisionDigest,
    policyDigest: "02".repeat(32),
    reservationBasisDigest: "03".repeat(32),
    rollbackGeneration,
    revision: "0",
    walletAddress: "test-only-no-wallet",
    fundingPaymentKeyHash: "04".repeat(28),
    state: "active",
    activeInputs: Object.freeze([]),
  });
  const port: WorkflowFundingReservationPort = Object.freeze({
    load: async () => snapshot,
    readPendingTransition: async () => null,
    readPendingHandoff: async () => null,
    readCompletionHandoff: async () => null,
    readAbandonmentHandoff: async () => null,
    acknowledgeAbandonment: async () => snapshot,
    resolveInputs: async () => [],
    resolveConfirmedInput: async () => {
      throw new Error("unsafe test permit has no confirmed action lineage");
    },
    resolveProtocolInputAuthority: async () => {
      throw new Error("unsafe test permit has no protocol input authority");
    },
    prepare: async () => snapshot,
    confirm: async () => snapshot,
    abandon: async () => snapshot,
    markConflict: async () => snapshot,
    release: async () => snapshot,
  });
  admittedPermits.set(permit, {
    category,
    policy: undefined,
    actuationPermit,
    port,
    maximumCollateralInputs: 0,
    snapshot,
    resolvedInputs: new Map(),
    boundJournal: undefined,
    currentActionKind: undefined,
    currentActionDigest: undefined,
    currentFundingOutRefs: Object.freeze([]),
    currentCollateralOutRefs: Object.freeze([]),
    pendingTransactionHash: undefined,
    preparedTransaction: undefined,
  });
  return permit;
};

export const unsafeWorkflowFundingReservationSelectedOutRefsForTest = (
  permit: WorkflowFundingReservationPermit,
): Readonly<{
  fundingOutRefs: readonly string[];
  collateralOutRefs: readonly string[];
}> => {
  if (process.env.NODE_ENV !== "test") {
    throw new Error("unsafe funding reservation inspection is test-only");
  }
  const state = admittedPermits.get(permit);
  if (state === undefined) {
    throw new Error("production funding reservation permit was not admitted");
  }
  return Object.freeze({
    fundingOutRefs: Object.freeze([...state.currentFundingOutRefs]),
    collateralOutRefs: Object.freeze([...state.currentCollateralOutRefs]),
  });
};
