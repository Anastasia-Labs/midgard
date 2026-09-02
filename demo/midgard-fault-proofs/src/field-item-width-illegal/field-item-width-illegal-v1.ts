import {
  decodeMidgardFieldPreimageV1,
  midgardFieldCommitmentV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
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

export const FIELD_ITEM_WIDTH_ILLEGAL_CATEGORY_V1 =
  "fieldItemWidthIllegal" as const;
export const FIELD_ITEM_WIDTH_ILLEGAL_PROPOSED_ID_V1 = "00000021" as const;
export const FIELD_ITEM_WIDTH_ILLEGAL_OUTPUTS_FIELD_V1 = 2;
export const FIELD_ITEM_WIDTH_ILLEGAL_MINT_FIELD_V1 = 5;
export const FIELD_ITEM_WIDTH_ILLEGAL_MAX_OUTPUT_BYTES_V1 = 16_384;

const fail = (message: string): never => {
  throw new Error(`${FIELD_ITEM_WIDTH_ILLEGAL_CATEGORY_V1}: ${message}`);
};

const exactIndex = (value: number, name: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    return fail(`${name} must be a non-negative safe integer`);
  }
  return value;
};

export const fieldItemWidthCoordinateIsSupportedV1 = (
  fieldIndex: number,
  itemIndex: number,
): boolean =>
  Number.isSafeInteger(itemIndex) &&
  itemIndex >= 0 &&
  (fieldIndex === FIELD_ITEM_WIDTH_ILLEGAL_OUTPUTS_FIELD_V1 ||
    fieldIndex === FIELD_ITEM_WIDTH_ILLEGAL_MINT_FIELD_V1);

export const fieldItemWidthIsIllegalV1 = (
  fieldIndex: number,
  itemWidth: number,
): boolean => {
  exactIndex(itemWidth, "item width");
  if (fieldIndex === FIELD_ITEM_WIDTH_ILLEGAL_OUTPUTS_FIELD_V1) {
    return itemWidth > FIELD_ITEM_WIDTH_ILLEGAL_MAX_OUTPUT_BYTES_V1;
  }
  if (fieldIndex === FIELD_ITEM_WIDTH_ILLEGAL_MINT_FIELD_V1) {
    return itemWidth === 0;
  }
  return fail(`field ${fieldIndex.toString()} is outside the width family`);
};

const fieldItemWidthReasonCoordinateV1 = (
  reason: RejectionReasonV1,
): { readonly fieldIndex: number; readonly itemIndex: number } => {
  if (typeof reason === "string" || !("FieldItemWidthIllegal" in reason)) {
    return fail("typed rejection reason is not FieldItemWidthIllegal");
  }
  const coordinate = reason.FieldItemWidthIllegal;
  const fieldIndex = Number(coordinate.field_index);
  const itemIndex = Number(coordinate.item_index);
  exactIndex(fieldIndex, "reason field index");
  exactIndex(itemIndex, "reason item index");
  return { fieldIndex, itemIndex };
};

export type FieldItemWidthFindingV1 = {
  readonly subject: VerdictSubjectV1;
  readonly fieldIndex: number;
  readonly itemIndex: number;
};

/** Strict classifier: this reason never falls back to validation dispute. */
export const classifyFieldItemWidthFindingV1 = ({
  subject,
  fieldIndex,
  itemIndex,
}: FieldItemWidthFindingV1): FieldItemWidthFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(subject)) {
    return fail("verdict subject is not canonical");
  }
  if (!fieldItemWidthCoordinateIsSupportedV1(fieldIndex, itemIndex)) {
    return fail("unsupported field/item coordinate");
  }
  if (subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1) {
    if (subject.rejection_reason === null) {
      return fail("wrongful-rejection subject carries no typed reason");
    }
    const exact = fieldItemWidthReasonCoordinateV1(subject.rejection_reason);
    if (exact.fieldIndex !== fieldIndex || exact.itemIndex !== itemIndex) {
      return fail("typed reason coordinate differs from finding coordinate");
    }
  } else if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity is invalid");
  }
  return Object.freeze({ subject, fieldIndex, itemIndex });
};

export type FieldItemWidthEvidenceV1 = FieldItemWidthFindingV1 & {
  readonly fieldPreimageHex: string;
  readonly fieldCommitmentHex: string;
  readonly itemHex: string;
  readonly itemWidth: number;
  readonly carriage: "Inline" | "RawUtxo" | "Certified";
  readonly decisiveFaultHolds: boolean;
};

/**
 * Prepare evidence only from retained canonical field bytes. The caller must
 * supply the commitment extracted from the authenticated compact transaction;
 * mismatches and out-of-range coordinates fail before submission.
 */
export const prepareFieldItemWidthEvidenceV1 = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: FieldItemWidthFindingV1;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): FieldItemWidthEvidenceV1 => {
  const finding = classifyFieldItemWidthFindingV1(rawFinding);
  if (!/^[0-9a-f]{64}$/u.test(committedFieldHashHex)) {
    return fail("committed field hash must be 32 bytes of lowercase hex");
  }
  const actualCommitment =
    midgardFieldCommitmentV1(fieldPreimage).toString("hex");
  if (actualCommitment !== committedFieldHashHex) {
    return fail("retained field preimage does not match committed field hash");
  }
  const items = decodeMidgardFieldPreimageV1(fieldPreimage);
  const item = items[finding.itemIndex];
  if (item === undefined) return fail("item coordinate is outside the field");
  const decisiveFaultHolds = fieldItemWidthIsIllegalV1(
    finding.fieldIndex,
    item.length,
  );
  return Object.freeze({
    ...finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actualCommitment,
    itemHex: item.toString("hex"),
    itemWidth: item.length,
    carriage: selectMidgardFieldCarriageTierV1(fieldPreimage.length),
    decisiveFaultHolds,
  });
};

export const fieldItemWidthEvidenceClosesV1 = (
  evidence: FieldItemWidthEvidenceV1,
): boolean =>
  terminalVerdictContradictionV1(
    evidence.subject,
    fieldItemWidthIsIllegalV1(evidence.fieldIndex, evidence.itemWidth),
  );

const VerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});

export const FieldItemWidthBoundCoordinateV1Schema = Data.Object({
  subject: VerdictSubjectV1Schema,
  field_index: Data.Integer(),
  item_index: Data.Integer(),
});

export const FieldItemWidthAuthenticatedWidthV1Schema = Data.Object({
  subject: VerdictSubjectV1Schema,
  field_index: Data.Integer(),
  item_index: Data.Integer(),
  item_width: Data.Integer(),
});

export const encodeFieldItemWidthBoundCoordinateV1 = (
  finding: FieldItemWidthFindingV1,
): string => {
  const exact = classifyFieldItemWidthFindingV1(finding);
  return Data.to(
    {
      subject: exact.subject,
      field_index: BigInt(exact.fieldIndex),
      item_index: BigInt(exact.itemIndex),
    } as never,
    FieldItemWidthBoundCoordinateV1Schema as never,
  );
};

export const encodeFieldItemWidthAuthenticatedWidthV1 = (
  evidence: FieldItemWidthEvidenceV1,
): string =>
  Data.to(
    {
      subject: evidence.subject,
      field_index: BigInt(evidence.fieldIndex),
      item_index: BigInt(evidence.itemIndex),
      item_width: BigInt(evidence.itemWidth),
    } as never,
    FieldItemWidthAuthenticatedWidthV1Schema as never,
  );

export const FIELD_ITEM_WIDTH_STAGES_V1 = [
  "none",
  "step01",
  "step02",
  "step03",
  "proven",
  "removed",
  "cancelled",
] as const;
export type FieldItemWidthStageV1 = (typeof FIELD_ITEM_WIDTH_STAGES_V1)[number];

export type FieldItemWidthJournalEntryV1 = {
  readonly sequence: number;
  readonly identity: string;
  readonly stage: FieldItemWidthStageV1;
  readonly txHash: string;
  readonly outputReference: string | null;
};

export type FieldItemWidthJournalV1 = {
  readonly load: (
    identity: string,
  ) => Promise<readonly FieldItemWidthJournalEntryV1[]>;
  readonly append: (entry: FieldItemWidthJournalEntryV1) => Promise<void>;
};

export const fieldItemWidthEvidenceIdentityV1 = (
  evidence: FieldItemWidthEvidenceV1,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction.toString(),
    evidence.fieldIndex.toString(),
    evidence.itemIndex.toString(),
    evidence.fieldCommitmentHex,
  ].join(":");

const stageRank = (stage: FieldItemWidthStageV1): number =>
  FIELD_ITEM_WIDTH_STAGES_V1.indexOf(stage);

export const reconcileFieldItemWidthJournalV1 = (
  identity: string,
  entries: readonly FieldItemWidthJournalEntryV1[],
  observedStage: FieldItemWidthStageV1,
): FieldItemWidthStageV1 => {
  let lastSequence = -1;
  let lastStage: FieldItemWidthStageV1 = "none";
  for (const entry of entries) {
    if (entry.identity !== identity) return fail("journal identity mismatch");
    if (entry.sequence !== lastSequence + 1)
      return fail("journal sequence is not contiguous");
    if (
      entry.stage !== "cancelled" &&
      stageRank(entry.stage) < stageRank(lastStage)
    ) {
      return fail("journal stage regressed");
    }
    lastSequence = entry.sequence;
    lastStage = entry.stage;
  }
  if (observedStage === "cancelled" || observedStage === "removed") {
    return observedStage;
  }
  if (stageRank(observedStage) < stageRank(lastStage)) {
    return fail("authenticated chain state is behind durable journal");
  }
  return observedStage;
};

export type FieldItemWidthActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "removeDescendants"
  | "done";

export const nextFieldItemWidthActionV1 = (
  stage: FieldItemWidthStageV1,
): FieldItemWidthActionV1 => {
  switch (stage) {
    case "none":
      return "submitInit";
    case "step01":
      return "submitStep01";
    case "step02":
      return "submitStep02";
    case "step03":
      return "submitStep03";
    case "proven":
      return "removeDescendants";
    case "removed":
    case "cancelled":
      return "done";
  }
};

export type FieldItemWidthSubmissionResultV1 = {
  readonly stage: FieldItemWidthStageV1;
  readonly txHash: string;
  readonly outputReference: string | null;
};

export type FieldItemWidthSubmissionAdapterV1 = {
  readonly observe: (identity: string) => Promise<FieldItemWidthStageV1>;
  readonly submit: (
    action: Exclude<FieldItemWidthActionV1, "done">,
    evidence: FieldItemWidthEvidenceV1,
  ) => Promise<FieldItemWidthSubmissionResultV1>;
  readonly cancel: (
    stage: "step01" | "step02" | "step03",
    evidence: FieldItemWidthEvidenceV1,
  ) => Promise<FieldItemWidthSubmissionResultV1>;
};

/** Durable, restart-safe runner; every decision is reconstructed from journal + chain. */
export const runFieldItemWidthProofV1 = async ({
  evidence,
  journal,
  submission,
}: {
  readonly evidence: FieldItemWidthEvidenceV1;
  readonly journal: FieldItemWidthJournalV1;
  readonly submission: FieldItemWidthSubmissionAdapterV1;
}): Promise<FieldItemWidthStageV1> => {
  if (!fieldItemWidthEvidenceClosesV1(evidence)) {
    return fail("honest verdict cannot start a proof thread");
  }
  const identity = fieldItemWidthEvidenceIdentityV1(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    const observed = await submission.observe(identity);
    const stage = reconcileFieldItemWidthJournalV1(identity, entries, observed);
    const action = nextFieldItemWidthActionV1(stage);
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
