import {
  advanceMidgardLedgerOutputScanV1,
  decodeMidgardFieldPreimageV1,
  encodeMidgardLedgerOutputScanControlV1,
  finishMidgardLedgerOutputScanV1,
  initialMidgardLedgerOutputScanControlV1,
  midgardFieldCommitmentV1,
  type MidgardLedgerOutputScanControlV1,
  MidgardLedgerOutputScanStagesV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  type RejectionReasonV1,
  RejectionReasonV1Schema,
  terminalVerdictContradictionV1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const TRANSACTION_OUTPUT_NON_CANONICAL_CATEGORY_V1 =
  "transactionOutputNonCanonical" as const;
export const TRANSACTION_OUTPUT_NON_CANONICAL_PROPOSED_ID_V1 =
  "00000029" as const;
export const TRANSACTION_OUTPUT_NON_CANONICAL_OUTPUTS_FIELD_V1 = 2 as const;
export const TRANSACTION_OUTPUT_NON_CANONICAL_MAX_OUTPUT_BYTES_V1 = 16_384;

const fail = (message: string): never => {
  throw new Error(
    `${TRANSACTION_OUTPUT_NON_CANONICAL_CATEGORY_V1}: ${message}`,
  );
};
const exactIndex = (value: number, name: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) fail(`${name} is invalid`);
  return value;
};

export type TransactionOutputFindingV1 = {
  readonly subject: VerdictSubjectV1;
  readonly fieldIndex: 2;
  readonly itemIndex: number;
};

const reasonOutputIndexV1 = (reason: RejectionReasonV1 | null): number => {
  if (
    reason === null ||
    typeof reason === "string" ||
    !("OutputNonCanonical" in reason)
  ) {
    return fail("typed rejection reason is not OutputNonCanonical");
  }
  return exactIndex(
    Number(reason.OutputNonCanonical.output_index),
    "reason output index",
  );
};

export const classifyTransactionOutputFindingV1 = ({
  subject,
  fieldIndex,
  itemIndex,
}: TransactionOutputFindingV1): TransactionOutputFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(subject))
    fail("verdict subject is not canonical");
  if (fieldIndex !== 2) fail("only field 2 belongs to this family");
  exactIndex(itemIndex, "output index");
  if (subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1) {
    const reason = subject.rejection_reason;
    if (reason === null) fail("forced subject omitted its reason");
    if (reasonOutputIndexV1(reason) !== itemIndex)
      fail("typed reason coordinate differs from output coordinate");
  } else if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    subject.rejection_reason !== null
  ) {
    fail("direction/reason polarity is invalid");
  }
  return Object.freeze({ subject, fieldIndex, itemIndex });
};

export type TransactionOutputEvidenceV1 = TransactionOutputFindingV1 & {
  readonly fieldPreimageHex: string;
  readonly fieldCommitmentHex: string;
  readonly itemHex: string;
  readonly itemLength: number;
  readonly itemHash: string;
  readonly chunkHashes: readonly string[];
  readonly carriage: "Inline" | "RawUtxo" | "Certified";
  readonly canonical: boolean;
  readonly scanControls: readonly MidgardLedgerOutputScanControlV1[];
  readonly decisiveFaultHolds: boolean;
};

const deriveScanV1 = (
  item: Buffer,
): {
  readonly canonical: boolean;
  readonly controls: readonly MidgardLedgerOutputScanControlV1[];
} => {
  let control = initialMidgardLedgerOutputScanControlV1();
  const controls: MidgardLedgerOutputScanControlV1[] = [control];
  for (let step = 0; step <= item.length + 32; step += 1) {
    const finished = finishMidgardLedgerOutputScanV1({
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
    const next = advanceMidgardLedgerOutputScanV1({
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
    if (control.stage === MidgardLedgerOutputScanStagesV1.Terminal) {
      return Object.freeze({
        canonical: true,
        controls: Object.freeze(controls),
      });
    }
  }
  return fail("output scan exceeded its strict progress bound");
};

export const prepareTransactionOutputEvidenceV1 = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: TransactionOutputFindingV1;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): TransactionOutputEvidenceV1 => {
  const finding = classifyTransactionOutputFindingV1(rawFinding);
  const actualCommitment =
    midgardFieldCommitmentV1(fieldPreimage).toString("hex");
  if (actualCommitment !== committedFieldHashHex)
    fail("field commitment differs");
  const item = decodeMidgardFieldPreimageV1(fieldPreimage)[finding.itemIndex];
  if (item === undefined) fail("output coordinate is outside field 2");
  if (item.length > TRANSACTION_OUTPUT_NON_CANONICAL_MAX_OUTPUT_BYTES_V1)
    fail("output item belongs to fieldItemWidthIllegal");
  const scan = deriveScanV1(item);
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
    carriage: selectMidgardFieldCarriageTierV1(fieldPreimage.length),
    canonical: scan.canonical,
    scanControls: scan.controls,
    decisiveFaultHolds: !scan.canonical,
  });
};

export const transactionOutputEvidenceClosesV1 = (
  evidence: TransactionOutputEvidenceV1,
): boolean =>
  terminalVerdictContradictionV1(evidence.subject, !evidence.canonical);

export const TransactionOutputVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const TransactionOutputBoundOutputV1Schema = Data.Object({
  subject: TransactionOutputVerdictSubjectV1Schema,
  output_index: Data.Integer(),
});
export const encodeTransactionOutputBoundOutputV1 = (
  finding: TransactionOutputFindingV1,
): string => {
  const exact = classifyTransactionOutputFindingV1(finding);
  return Data.to(
    { subject: exact.subject, output_index: BigInt(exact.itemIndex) } as never,
    TransactionOutputBoundOutputV1Schema as never,
  );
};
export const encodeTransactionOutputScanControlV1 = (
  control: MidgardLedgerOutputScanControlV1,
): string => encodeMidgardLedgerOutputScanControlV1(control).toString("hex");

export const transactionOutputScanControlDataV1 = (
  control: MidgardLedgerOutputScanControlV1,
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

export const TRANSACTION_OUTPUT_NON_CANONICAL_STAGES_V1 = [
  "none",
  "step01",
  "step02",
  "step03",
  "step04",
  "proven",
  "removed",
  "cancelled",
] as const;
export type TransactionOutputStageV1 =
  (typeof TRANSACTION_OUTPUT_NON_CANONICAL_STAGES_V1)[number];
export type TransactionOutputJournalEntryV1 = {
  readonly sequence: number;
  readonly identity: string;
  readonly stage: TransactionOutputStageV1;
  readonly txHash: string;
  readonly outputReference: string | null;
};
export type TransactionOutputJournalV1 = {
  readonly load: (
    identity: string,
  ) => Promise<readonly TransactionOutputJournalEntryV1[]>;
  readonly append: (entry: TransactionOutputJournalEntryV1) => Promise<void>;
};
export const transactionOutputEvidenceIdentityV1 = (
  evidence: TransactionOutputEvidenceV1,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction,
    evidence.itemIndex,
    evidence.fieldCommitmentHex,
    evidence.itemHash,
  ].join(":");
const stageRank = (stage: TransactionOutputStageV1): number =>
  TRANSACTION_OUTPUT_NON_CANONICAL_STAGES_V1.indexOf(stage);
export const reconcileTransactionOutputJournalV1 = (
  identity: string,
  entries: readonly TransactionOutputJournalEntryV1[],
  observedStage: TransactionOutputStageV1,
): TransactionOutputStageV1 => {
  let sequence = -1;
  let last: TransactionOutputStageV1 = "none";
  for (const entry of entries) {
    if (entry.identity !== identity) fail("journal identity mismatch");
    if (entry.sequence !== sequence + 1)
      fail("journal sequence is not contiguous");
    if (entry.stage !== "cancelled" && stageRank(entry.stage) < stageRank(last))
      fail("journal stage regressed");
    sequence = entry.sequence;
    last = entry.stage;
  }
  if (observedStage === "cancelled" || observedStage === "removed")
    return observedStage;
  if (stageRank(observedStage) < stageRank(last))
    fail("chain state is behind durable journal");
  return observedStage;
};
export type TransactionOutputActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitStep04"
  | "removeDescendants"
  | "done";
export const nextTransactionOutputActionV1 = (
  stage: TransactionOutputStageV1,
): TransactionOutputActionV1 => {
  switch (stage) {
    case "none":
      return "submitInit";
    case "step01":
      return "submitStep01";
    case "step02":
      return "submitStep02";
    case "step03":
      return "submitStep03";
    case "step04":
      return "submitStep04";
    case "proven":
      return "removeDescendants";
    case "removed":
    case "cancelled":
      return "done";
  }
};
export type TransactionOutputSubmissionResultV1 = {
  readonly stage: TransactionOutputStageV1;
  readonly txHash: string;
  readonly outputReference: string | null;
};
export type TransactionOutputSubmissionAdapterV1 = {
  readonly observe: (identity: string) => Promise<TransactionOutputStageV1>;
  readonly submit: (
    action: Exclude<TransactionOutputActionV1, "done">,
    evidence: TransactionOutputEvidenceV1,
  ) => Promise<TransactionOutputSubmissionResultV1>;
  readonly cancel: (
    stage: "step01" | "step02" | "step03" | "step04",
    evidence: TransactionOutputEvidenceV1,
  ) => Promise<TransactionOutputSubmissionResultV1>;
};
export const runTransactionOutputProofV1 = async ({
  evidence,
  journal,
  submission,
}: {
  readonly evidence: TransactionOutputEvidenceV1;
  readonly journal: TransactionOutputJournalV1;
  readonly submission: TransactionOutputSubmissionAdapterV1;
}): Promise<TransactionOutputStageV1> => {
  if (!transactionOutputEvidenceClosesV1(evidence))
    fail("honest verdict cannot start a proof thread");
  const identity = transactionOutputEvidenceIdentityV1(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    const stage = reconcileTransactionOutputJournalV1(
      identity,
      entries,
      await submission.observe(identity),
    );
    const action = nextTransactionOutputActionV1(stage);
    if (action === "done") return stage;
    const result = await submission.submit(action, evidence);
    await journal.append({
      sequence: entries.length,
      identity,
      stage: result.stage,
      txHash: result.txHash,
      outputReference: result.outputReference,
    });
  }
};
