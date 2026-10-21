/** The journaled actions of the validation-trace dispute: Init the thread,
 * Open the claim, VerifySource it into a case and its bond, close the
 * convicted case into its fraud-proof record, and the challenger's moves on
 * a defended case's threads: reveal a midpoint, time out a defender whose
 * clock ran out, enter a thread ready for its one step into the resolution
 * and resolve it hop by hop to the award, and close a thread won either way
 * into the case. Each action is keyed by the
 * output of ours it consumes, and its durable recovery names the outputs of
 * our own signed body it leaves, by out-ref, which every later read
 * follows. A move's recovery also keeps its exact signed bytes: the
 * defender may spend a move's successor at once, so a move is reconciled
 * from those bytes, never from its outputs. */
import { CML, type TxOutput, type UTxO } from "@lucid-evolution/lucid";

import {
  exact,
  OUT_REF,
  sha256,
  TX_HASH,
} from "../workflow/field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import type {
  FraudProofWorkflowJournalEntry,
  JournalJsonObject,
} from "../workflow/journal.js";
import { latestSubmissionIntent } from "../workflow/orchestrator.fraud-proof-workflow-run-result.js";
import type { FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import { claimOwnOutputs } from "../workflow/own-outputs.js";
import { inspectSignedWorkflowTransaction } from "../workflow/signed-transaction-reconciliation.js";
import type { LocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";

export const VALIDATION_TRACE_DISPUTE_ACTION =
  "midgard-validation-trace-dispute-action-v1" as const;

export const VALIDATION_TRACE_DISPUTE_RECOVERY =
  "midgard-validation-trace-dispute-recovery-v1" as const;

export const VALIDATION_TRACE_STAGES = [
  "init",
  "open",
  "verify_source",
  "close",
  "reveal",
  "timeout",
  "close_thread",
  "enter_resolution",
  "resolve",
  "close_awarded",
] as const;

export type ValidationTraceStage = (typeof VALIDATION_TRACE_STAGES)[number];

/** The challenger's moves on a defended case's threads. */
export const isValidationTraceMove = (stage: ValidationTraceStage): boolean =>
  stage !== "init" &&
  stage !== "open" &&
  stage !== "verify_source" &&
  stage !== "close";

/** The outputs each stage leaves, in the order its recovery lists them. */
export const VALIDATION_TRACE_STAGE_ROLES = {
  init: ["thread"],
  open: ["opener"],
  verify_source: ["case", "bond"],
  close: ["proof"],
  reveal: ["thread"],
  timeout: ["thread"],
  close_thread: ["case", "receipt"],
  enter_resolution: ["thread"],
  resolve: ["thread"],
  close_awarded: ["case", "receipt"],
} as const satisfies Record<ValidationTraceStage, readonly string[]>;

export type ValidationTraceOutputRole =
  (typeof VALIDATION_TRACE_STAGE_ROLES)[ValidationTraceStage][number];

const UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){1,32}$/u;

export type ParsedValidationTraceAction = Readonly<{
  stage: ValidationTraceStage;
  /**
   * Init: the thread of ours it replaces after that one vanished (null
   * first). Open: our thread it spends. VerifySource: our opener it spends.
   * Close: our case it spends, beside the case's bond. Reveal and timeout:
   * the defended thread at the game they spend. Close thread: the thread at
   * the timeout validator it closes into the case. Enter resolution: the
   * thread at the game ready for its one step. Resolve: the thread at the
   * one-step holder it moves on. Close awarded: the thread at the award it
   * closes into the case.
   */
  consumes: string | null;
}>;

export const validationTraceActionId = ({
  stage,
  consumes,
}: ParsedValidationTraceAction): string =>
  `validation-trace-${stage.replaceAll("_", "-")}:${consumes ?? "first"}`;

export const validationTraceAction = (
  parsed: ParsedValidationTraceAction,
): FraudProofWorkflowAction =>
  Object.freeze({
    actionId: validationTraceActionId(parsed),
    input: Object.freeze({
      schemaVersion: VALIDATION_TRACE_DISPUTE_ACTION,
      stage: parsed.stage,
      consumes: parsed.consumes,
    }),
  });

/** The exact fields of a validation-trace action, or throws. */
export const parseValidationTraceAction = (
  action: FraudProofWorkflowAction,
): ParsedValidationTraceAction => {
  const input = exact(
    action.input,
    ["schemaVersion", "stage", "consumes"],
    "validation-trace action",
  );
  const stage = input.stage as ValidationTraceStage;
  const consumes = input.consumes;
  if (
    input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION ||
    !VALIDATION_TRACE_STAGES.includes(stage) ||
    !(
      (consumes === null && stage === "init") ||
      (typeof consumes === "string" && OUT_REF.test(consumes))
    )
  )
    throw new Error("validation-trace action is malformed");
  const parsed = Object.freeze({ stage, consumes: consumes as string | null });
  if (action.actionId !== validationTraceActionId(parsed))
    throw new Error("validation-trace action changed identity");
  return parsed;
};

/** One output of ours a stage left, as its durable recovery names it. */
export type ValidationTraceOutputClaim = Readonly<{
  role: ValidationTraceOutputRole;
  outRef: string;
  address: string;
  datumCborSha256: string;
  unit: string;
}>;

/** The content digest of an inline datum, over its canonical encoding. */
export const validationTraceDatumDigest = (datumCbor: string): string =>
  sha256(CML.PlutusData.from_cbor_hex(datumCbor).to_canonical_cbor_hex());

/** Whether `utxo` is the claimed output, with the content it was claimed with. */
export const isClaimedOutput = (
  claim: ValidationTraceOutputClaim,
  utxo: UTxO | TxOutput | undefined,
): boolean =>
  utxo !== undefined &&
  utxo.address === claim.address &&
  typeof utxo.datum === "string" &&
  utxo.scriptRef == null &&
  validationTraceDatumDigest(utxo.datum) === claim.datumCborSha256 &&
  utxo.assets[claim.unit] === 1n;

/**
 * The durable recovery of a captured stage transaction: each output of our
 * own signed body the stage leaves (`slots`, in role order), by out-ref, and
 * for a move its exact signed bytes.
 */
export const validationTraceRecovery = ({
  stage,
  transaction,
  slots,
}: {
  readonly stage: ValidationTraceStage;
  readonly transaction: LocallyEvaluatedTransaction;
  readonly slots: readonly Readonly<{ address: string; unit: string }>[];
}): JournalJsonObject => {
  const roles = VALIDATION_TRACE_STAGE_ROLES[stage];
  if (slots.length !== roles.length)
    throw new Error(`validation-trace ${stage} names the wrong outputs`);
  const claims = claimOwnOutputs({
    signed: transaction.signed,
    slots: slots.map(({ address, unit }) => ({
      address,
      matches: (output: TxOutput) =>
        typeof output.datum === "string" &&
        output.scriptRef == null &&
        output.assets[unit] === 1n,
    })),
  });
  return Object.freeze({
    validationTrace: Object.freeze({
      schemaVersion: VALIDATION_TRACE_DISPUTE_RECOVERY,
      stage,
      outputs: Object.freeze(
        claims.map(({ outRef, output }, index) => {
          if (!outRef.startsWith(`${transaction.txHash}#`))
            throw new Error(
              `validation-trace ${stage} body is not the captured one`,
            );
          return Object.freeze({
            role: roles[index]!,
            outRef,
            address: slots[index]!.address,
            datumCborSha256: validationTraceDatumDigest(output.datum!),
            unit: slots[index]!.unit,
          });
        }),
      ),
      ...(isValidationTraceMove(stage)
        ? { signedTransactionCborHex: transaction.signed.toCBOR() }
        : {}),
    }),
  });
};

/** The outputs an intent of `stage` for `txHash` journaled, or throws. */
export const parseValidationTraceRecovery = ({
  value,
  stage,
  txHash,
}: {
  readonly value: JournalJsonObject | undefined;
  readonly stage: ValidationTraceStage;
  readonly txHash: string;
}): readonly ValidationTraceOutputClaim[] => {
  const outer = exact(value, ["validationTrace"], "validation-trace recovery");
  const parsed = exact(
    outer.validationTrace,
    [
      "schemaVersion",
      "stage",
      "outputs",
      ...(isValidationTraceMove(stage) ? ["signedTransactionCborHex"] : []),
    ],
    "validation-trace recovery payload",
  );
  const roles = VALIDATION_TRACE_STAGE_ROLES[stage];
  if (
    !TX_HASH.test(txHash) ||
    parsed.schemaVersion !== VALIDATION_TRACE_DISPUTE_RECOVERY ||
    parsed.stage !== stage ||
    !Array.isArray(parsed.outputs) ||
    parsed.outputs.length !== roles.length
  )
    throw new Error("validation-trace recovery changed identity");
  return Object.freeze(
    (parsed.outputs as readonly unknown[]).map((raw, index) => {
      const output = exact(
        raw,
        ["role", "outRef", "address", "datumCborSha256", "unit"],
        "validation-trace recovery output",
      );
      if (
        output.role !== roles[index] ||
        typeof output.outRef !== "string" ||
        !OUT_REF.test(output.outRef) ||
        !output.outRef.startsWith(`${txHash}#`) ||
        typeof output.address !== "string" ||
        typeof output.datumCborSha256 !== "string" ||
        !/^[0-9a-f]{64}$/u.test(output.datumCborSha256) ||
        typeof output.unit !== "string" ||
        !UNIT.test(output.unit)
      )
        throw new Error("validation-trace recovery output changed identity");
      return Object.freeze({
        role: roles[index]!,
        outRef: output.outRef,
        address: output.address,
        datumCborSha256: output.datumCborSha256,
        unit: output.unit,
      });
    }),
  );
};

/** The exact signed bytes a move's intent for `txHash` journaled, or throws. */
export const recordedValidationTraceMoveBytes = ({
  value,
  txHash,
}: {
  readonly value: JournalJsonObject | undefined;
  readonly txHash: string;
}): string => {
  const outer = exact(value, ["validationTrace"], "validation-trace recovery");
  const { signedTransactionCborHex } = outer.validationTrace as Readonly<{
    signedTransactionCborHex?: unknown;
  }>;
  if (typeof signedTransactionCborHex !== "string")
    throw new Error("validation-trace move recovery omits its signed bytes");
  inspectSignedWorkflowTransaction({
    transactionHash: txHash,
    signedTransactionCborHex,
  });
  return signedTransactionCborHex;
};

/** One confirmed stage transaction of the journal and the outputs it left. */
export type ConfirmedValidationTraceStage = Readonly<{
  action: ParsedValidationTraceAction;
  txHash: string;
  outputs: readonly ValidationTraceOutputClaim[];
  /** When the confirmation was journaled (ISO time). */
  confirmedAt: string;
}>;

/**
 * The journal's confirmed open-step transactions, oldest confirmation
 * first. A confirmation a later reobservation undid (a rollback) is left
 * out until it is confirmed again.
 */
export const confirmedValidationTraceStages = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): readonly ConfirmedValidationTraceStage[] => {
  const latest = new Map<string, FraudProofWorkflowJournalEntry>();
  for (const entry of entries) {
    const { event } = entry;
    if (event.kind === "confirmed" || event.kind === "reobserved") {
      latest.delete(event.actionId);
      latest.set(event.actionId, entry);
    }
  }
  const confirmed: ConfirmedValidationTraceStage[] = [];
  for (const [actionId, entry] of latest) {
    if (entry.event.kind !== "confirmed") continue;
    const intent = latestSubmissionIntent(entries, actionId);
    if (
      intent === undefined ||
      intent.txHash !== entry.event.txHash ||
      intent.actionInput.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION
    )
      continue;
    const action = parseValidationTraceAction({
      actionId,
      input: intent.actionInput,
    });
    confirmed.push(
      Object.freeze({
        action,
        txHash: intent.txHash,
        outputs: parseValidationTraceRecovery({
          value: intent.durableRecovery,
          stage: action.stage,
          txHash: intent.txHash,
        }),
        confirmedAt: entry.recordedAt,
      }),
    );
  }
  return Object.freeze(confirmed);
};
