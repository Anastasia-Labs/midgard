/** The journaled actions of the validation-trace open step: Init the thread,
 * Open the claim, and VerifySource it into a case and its bond. Each action
 * is keyed by the output of ours it consumes, and its durable recovery names
 * the outputs of our own signed body it leaves, by out-ref, which every later
 * read follows. */
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
import type { LocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";

export const VALIDATION_TRACE_DISPUTE_ACTION =
  "midgard-validation-trace-dispute-action-v1" as const;

export const VALIDATION_TRACE_DISPUTE_RECOVERY =
  "midgard-validation-trace-dispute-recovery-v1" as const;

export const VALIDATION_TRACE_OPEN_STAGES = [
  "init",
  "open",
  "verify_source",
] as const;

export type ValidationTraceOpenStage =
  (typeof VALIDATION_TRACE_OPEN_STAGES)[number];

/** The outputs each stage leaves, in the order its recovery lists them. */
export const VALIDATION_TRACE_STAGE_ROLES = {
  init: ["thread"],
  open: ["opener"],
  verify_source: ["case", "bond"],
} as const satisfies Record<ValidationTraceOpenStage, readonly string[]>;

export type ValidationTraceOutputRole =
  (typeof VALIDATION_TRACE_STAGE_ROLES)[ValidationTraceOpenStage][number];

const UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){1,32}$/u;

export type ParsedValidationTraceAction = Readonly<{
  stage: ValidationTraceOpenStage;
  /**
   * Init: the thread of ours it replaces after that one vanished (null
   * first). Open: our thread it spends. VerifySource: our opener it spends.
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
  const stage = input.stage as ValidationTraceOpenStage;
  const consumes = input.consumes;
  if (
    input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION ||
    !VALIDATION_TRACE_OPEN_STAGES.includes(stage) ||
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
 * own signed body the stage leaves (`slots`, in role order), by out-ref.
 */
export const validationTraceRecovery = ({
  stage,
  transaction,
  slots,
}: {
  readonly stage: ValidationTraceOpenStage;
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
  readonly stage: ValidationTraceOpenStage;
  readonly txHash: string;
}): readonly ValidationTraceOutputClaim[] => {
  const outer = exact(value, ["validationTrace"], "validation-trace recovery");
  const parsed = exact(
    outer.validationTrace,
    ["schemaVersion", "stage", "outputs"],
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
