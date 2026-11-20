import {
  advanceMidgardLedgerOutputScan,
  decodeMidgardFieldPreimage,
  finishMidgardLedgerOutputScan,
  initialMidgardLedgerOutputScanControl,
  midgardFieldCommitment,
  type MidgardLedgerOutputScanControl,
  MidgardLedgerOutputScanStages,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { type RejectionReason } from "@al-ft/midgard-sdk/core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
  VerdictSubjectSchema,
} from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import { claimedItem } from "../evidence/claimed-coordinate.js";
import {
  planTransactionOutputDatum,
  type TransactionOutputDatumPlan,
} from "./datum-plan.js";

export const TRANSACTION_OUTPUT_NON_CANONICAL_CATEGORY =
  "transactionOutputNonCanonical" as const;
export const TRANSACTION_OUTPUT_NON_CANONICAL_MAX_OUTPUT_BYTES = 16_384;
/** The terminal outcome of an output field 2 does not have (`rule.outcome_absent`). */
export const TRANSACTION_OUTPUT_OUTCOME_ABSENT = 3n;

const fail = (message: string): never => {
  throw new Error(`${TRANSACTION_OUTPUT_NON_CANONICAL_CATEGORY}: ${message}`);
};
export type TransactionOutputFinding = {
  readonly subject: VerdictSubject;
  readonly fieldIndex: 2;
  /**
   * Any integer the rejection codec admits; a rejection may name an output
   * field 2 lacks, a negative one included.
   */
  readonly itemIndex: bigint;
};

const reasonOutputIndex = (reason: RejectionReason | null): bigint => {
  if (
    reason === null ||
    typeof reason === "string" ||
    !("OutputNonCanonical" in reason)
  ) {
    return fail("typed rejection reason is not OutputNonCanonical");
  }
  return reason.OutputNonCanonical.output_index;
};

export const classifyTransactionOutputFinding = ({
  subject,
  fieldIndex,
  itemIndex,
}: TransactionOutputFinding): TransactionOutputFinding => {
  if (!verdictSubjectIsCanonical(subject))
    fail("verdict subject is not canonical");
  if (fieldIndex !== 2) fail("only field 2 belongs to this family");
  if (typeof itemIndex !== "bigint") fail("output index is not an integer");
  if (subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    const reason = subject.rejection_reason;
    if (reason === null) fail("forced subject omitted its reason");
    if (reasonOutputIndex(reason) !== itemIndex)
      fail("typed reason coordinate differs from output coordinate");
  } else if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    subject.rejection_reason !== null
  ) {
    fail("direction/reason polarity is invalid");
  } else if (itemIndex < 0n || itemIndex > BigInt(Number.MAX_SAFE_INTEGER)) {
    // An acceptance binds an output field 2 holds.
    fail("output index is invalid");
  }
  return Object.freeze({ subject, fieldIndex, itemIndex });
};

export type TransactionOutputEvidence = TransactionOutputFinding & {
  readonly fieldPreimageHex: string;
  readonly fieldCommitmentHex: string;
  readonly itemHex: string;
  readonly itemLength: number;
  readonly itemHash: string;
  readonly chunkHashes: readonly string[];
  readonly carriage: "Inline" | "RawUtxo" | "Certified";
  /** The output is canonical: its envelope and, if it has one, its datum. */
  readonly canonical: boolean;
  /** The envelope scan alone reached its canonical terminal. */
  readonly envelopeCanonical: boolean;
  readonly scanControls: readonly MidgardLedgerOutputScanControl[];
  /** The datum steps after a canonical envelope with an inline datum. */
  readonly datumPlan: TransactionOutputDatumPlan | null;
  readonly decisiveFaultHolds: boolean;
  /**
   * The rejection names an output field 2 does not have. Step 02 then reads
   * the field's count instead of an item and hands over to step 04.
   */
  readonly absent: boolean;
};

export const deriveTransactionOutputScan = (
  item: Buffer,
): {
  readonly canonical: boolean;
  readonly controls: readonly MidgardLedgerOutputScanControl[];
} => {
  let control = initialMidgardLedgerOutputScanControl();
  const controls: MidgardLedgerOutputScanControl[] = [control];
  for (let step = 0; step <= item.length + 32; step += 1) {
    const finished = finishMidgardLedgerOutputScan({
      control,
      totalLength: item.length,
    });
    if (finished !== null) {
      controls.push(finished);
      return Object.freeze({
        canonical: true,
        controls: Object.freeze(controls),
      });
    }
    const chunkStart = Math.floor(control.cursor / 4_095) * 4_095;
    const window = item.subarray(chunkStart, chunkStart + 8_190);
    const next = advanceMidgardLedgerOutputScan({
      control,
      totalLength: item.length,
      window,
      windowOffset: control.cursor - chunkStart,
    });
    if (next === null) {
      return Object.freeze({
        canonical: false,
        controls: Object.freeze(controls),
      });
    }
    controls.push(next);
    control = next;
    if (control.stage === MidgardLedgerOutputScanStages.Terminal) {
      return Object.freeze({
        canonical: true,
        controls: Object.freeze(controls),
      });
    }
  }
  return fail("output scan exceeded its strict progress bound");
};

export const prepareTransactionOutputEvidence = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: TransactionOutputFinding;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): TransactionOutputEvidence => {
  const finding = classifyTransactionOutputFinding(rawFinding);
  const actualCommitment =
    midgardFieldCommitment(fieldPreimage).toString("hex");
  if (actualCommitment !== committedFieldHashHex)
    fail("field commitment differs");
  const item = claimedItem(
    decodeMidgardFieldPreimage(fieldPreimage),
    finding.itemIndex,
  );
  if (item === undefined) {
    // Canonical validation reads no output at a position field 2 lacks, so
    // only a rejection naming one is wrong.
    if (finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION)
      fail("output coordinate is outside field 2");
    return Object.freeze({
      ...finding,
      fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
      fieldCommitmentHex: actualCommitment,
      itemHex: "",
      itemLength: 0,
      itemHash: "",
      chunkHashes: Object.freeze([]),
      carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
      canonical: true,
      envelopeCanonical: true,
      scanControls: Object.freeze([initialMidgardLedgerOutputScanControl()]),
      datumPlan: null,
      decisiveFaultHolds: false,
      absent: true,
    });
  }
  if (item.length > TRANSACTION_OUTPUT_NON_CANONICAL_MAX_OUTPUT_BYTES)
    fail("output item belongs to fieldItemWidthIllegal");
  const scan = deriveTransactionOutputScan(item);
  const terminal = scan.controls.at(-1)!;
  const datumPlan =
    scan.canonical && terminal.datumOffset >= 0
      ? planTransactionOutputDatum({
          item,
          datumOffset: terminal.datumOffset,
          datumLength: terminal.datumLength,
        })
      : null;
  const canonical = scan.canonical && (datumPlan?.canonical ?? true);
  return Object.freeze({
    ...finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actualCommitment,
    itemHex: item.toString("hex"),
    itemLength: item.length,
    itemHash: computeHash32(item).toString("hex"),
    chunkHashes: Object.freeze(
      Array.from({ length: Math.ceil(item.length / 4_095) }, (_, index) =>
        computeHash32(
          item.subarray(index * 4_095, (index + 1) * 4_095),
        ).toString("hex"),
      ),
    ),
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
    canonical,
    envelopeCanonical: scan.canonical,
    scanControls: scan.controls,
    datumPlan,
    decisiveFaultHolds: !canonical,
    absent: false,
  });
};

export const transactionOutputEvidenceCloses = (
  evidence: TransactionOutputEvidence,
): boolean =>
  terminalVerdictContradiction(evidence.subject, !evidence.canonical);

export const TransactionOutputVerdictSubjectSchema = VerdictSubjectSchema;
export const TransactionOutputBoundOutputSchema = Data.Object({
  subject: TransactionOutputVerdictSubjectSchema,
  output_index: Data.Integer(),
});

export const transactionOutputScanControlData = (
  control: MidgardLedgerOutputScanControl,
) => ({
  version: BigInt(control.version),
  stage: BigInt(control.stage),
  cursor: BigInt(control.cursor),
  map_entry_count: BigInt(control.mapEntryCount),
  optional_field_count: BigInt(control.optionalFieldCount),
  address: control.address.toString("hex"),
  lovelace: control.lovelace,
  cardano_value_size: BigInt(control.cardanoValueSize),
  policy_remaining: BigInt(control.policyRemaining),
  asset_remaining: BigInt(control.assetRemaining),
  policy_asset_cursor: BigInt(control.policyAssetCursor),
  previous_policy: control.previousPolicy.toString("hex"),
  current_policy: control.currentPolicy.toString("hex"),
  previous_asset_name: control.previousAssetName.toString("hex"),
  asset_count: BigInt(control.assetFrontier.count),
  asset_peaks: control.assetFrontier.peaks.map(({ height, hash }) => ({
    height: BigInt(height),
    hash: Buffer.from(hash).toString("hex"),
  })),
  datum_offset: BigInt(control.datumOffset),
  datum_length: BigInt(control.datumLength),
  payload_remaining: BigInt(control.payloadRemaining),
  reference_script_language: BigInt(control.referenceScriptLanguage),
  reference_script_item_offset: BigInt(control.referenceScriptItemOffset),
  reference_script_offset: BigInt(control.referenceScriptOffset),
  reference_script_length: BigInt(control.referenceScriptLength),
});

export const TRANSACTION_OUTPUT_NON_CANONICAL_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "step03Datum",
  "step04",
  "proven",
  "removed",
  "cancelled",
] as const;
export type TransactionOutputStage =
  (typeof TRANSACTION_OUTPUT_NON_CANONICAL_STAGES)[number];
