import {
  advanceMidgardLedgerOutputScan,
  decodeMidgardFieldPreimage,
  encodeMidgardLedgerOutputScanControl,
  finishMidgardLedgerOutputScan,
  initialMidgardLedgerOutputScanControl,
  midgardFieldCommitment,
  type MidgardLedgerOutputScanControl,
  MidgardLedgerOutputScanStages,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type RejectionReason,
  RejectionReasonSchema,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const TRANSACTION_OUTPUT_NON_CANONICAL_CATEGORY =
  "transactionOutputNonCanonical" as const;
export const TRANSACTION_OUTPUT_NON_CANONICAL_PROPOSED_ID = "00000029" as const;
export const TRANSACTION_OUTPUT_NON_CANONICAL_OUTPUTS_FIELD = 2 as const;
export const TRANSACTION_OUTPUT_NON_CANONICAL_MAX_OUTPUT_BYTES = 16_384;

const fail = (message: string): never => {
  throw new Error(`${TRANSACTION_OUTPUT_NON_CANONICAL_CATEGORY}: ${message}`);
};
const exactIndex = (value: number, name: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) fail(`${name} is invalid`);
  return value;
};

export type TransactionOutputFinding = {
  readonly subject: VerdictSubject;
  readonly fieldIndex: 2;
  readonly itemIndex: number;
};

const reasonOutputIndex = (reason: RejectionReason | null): number => {
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

export const classifyTransactionOutputFinding = ({
  subject,
  fieldIndex,
  itemIndex,
}: TransactionOutputFinding): TransactionOutputFinding => {
  if (!verdictSubjectIsCanonical(subject))
    fail("verdict subject is not canonical");
  if (fieldIndex !== 2) fail("only field 2 belongs to this family");
  exactIndex(itemIndex, "output index");
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
  readonly canonical: boolean;
  readonly scanControls: readonly MidgardLedgerOutputScanControl[];
  readonly decisiveFaultHolds: boolean;
};

const deriveScan = (
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
  const item = decodeMidgardFieldPreimage(fieldPreimage)[finding.itemIndex];
  if (item === undefined) fail("output coordinate is outside field 2");
  if (item.length > TRANSACTION_OUTPUT_NON_CANONICAL_MAX_OUTPUT_BYTES)
    fail("output item belongs to fieldItemWidthIllegal");
  const scan = deriveScan(item);
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
    canonical: scan.canonical,
    scanControls: scan.controls,
    decisiveFaultHolds: !scan.canonical,
  });
};

export const transactionOutputEvidenceCloses = (
  evidence: TransactionOutputEvidence,
): boolean =>
  terminalVerdictContradiction(evidence.subject, !evidence.canonical);

export const TransactionOutputVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const TransactionOutputBoundOutputSchema = Data.Object({
  subject: TransactionOutputVerdictSubjectSchema,
  output_index: Data.Integer(),
});
export const encodeTransactionOutputBoundOutput = (
  finding: TransactionOutputFinding,
): string => {
  const exact = classifyTransactionOutputFinding(finding);
  return Data.to(
    { subject: exact.subject, output_index: BigInt(exact.itemIndex) } as never,
    TransactionOutputBoundOutputSchema as never,
  );
};
export const encodeTransactionOutputScanControl = (
  control: MidgardLedgerOutputScanControl,
): string => encodeMidgardLedgerOutputScanControl(control).toString("hex");

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
  "step04",
  "proven",
  "removed",
  "cancelled",
] as const;
export type TransactionOutputStage =
  (typeof TRANSACTION_OUTPUT_NON_CANONICAL_STAGES)[number];
export type TransactionOutputJournalEntry = {
  readonly sequence: number;
  readonly identity: string;
  readonly stage: TransactionOutputStage;
  readonly txHash: string;
  readonly outputReference: string | null;
};
export type TransactionOutputJournal = {
  readonly load: (
    identity: string,
  ) => Promise<readonly TransactionOutputJournalEntry[]>;
  readonly append: (entry: TransactionOutputJournalEntry) => Promise<void>;
};
export const transactionOutputEvidenceIdentity = (
  evidence: TransactionOutputEvidence,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction,
    evidence.itemIndex,
    evidence.fieldCommitmentHex,
    evidence.itemHash,
  ].join(":");
const stageRank = (stage: TransactionOutputStage): number =>
  TRANSACTION_OUTPUT_NON_CANONICAL_STAGES.indexOf(stage);
export const reconcileTransactionOutputJournal = (
  identity: string,
  entries: readonly TransactionOutputJournalEntry[],
  observedStage: TransactionOutputStage,
): TransactionOutputStage => {
  let sequence = -1;
  let last: TransactionOutputStage = "none";
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
export type TransactionOutputAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitStep04"
  | "removeDescendants"
  | "done";
export const nextTransactionOutputAction = (
  stage: TransactionOutputStage,
): TransactionOutputAction => {
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
export type TransactionOutputSubmissionResult = {
  readonly stage: TransactionOutputStage;
  readonly txHash: string;
  readonly outputReference: string | null;
};
export type TransactionOutputSubmissionAdapter = {
  readonly observe: (identity: string) => Promise<TransactionOutputStage>;
  readonly submit: (
    action: Exclude<TransactionOutputAction, "done">,
    evidence: TransactionOutputEvidence,
  ) => Promise<TransactionOutputSubmissionResult>;
  readonly cancel: (
    stage: "step01" | "step02" | "step03" | "step04",
    evidence: TransactionOutputEvidence,
  ) => Promise<TransactionOutputSubmissionResult>;
};
export const runTransactionOutputProof = async ({
  evidence,
  journal,
  submission,
}: {
  readonly evidence: TransactionOutputEvidence;
  readonly journal: TransactionOutputJournal;
  readonly submission: TransactionOutputSubmissionAdapter;
}): Promise<TransactionOutputStage> => {
  if (!transactionOutputEvidenceCloses(evidence))
    fail("honest verdict cannot start a proof thread");
  const identity = transactionOutputEvidenceIdentity(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    const stage = reconcileTransactionOutputJournal(
      identity,
      entries,
      await submission.observe(identity),
    );
    const action = nextTransactionOutputAction(stage);
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
