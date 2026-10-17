import {
  decodeMidgardFieldPreimage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
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

export const FIELD_ITEM_WIDTH_ILLEGAL_CATEGORY =
  "fieldItemWidthIllegal" as const;
export const FIELD_ITEM_WIDTH_ILLEGAL_PROPOSED_ID = "00000021" as const;
export const FIELD_ITEM_WIDTH_ILLEGAL_OUTPUTS_FIELD = 2;
export const FIELD_ITEM_WIDTH_ILLEGAL_MINT_FIELD = 5;
export const FIELD_ITEM_WIDTH_ILLEGAL_MAX_OUTPUT_BYTES = 16_384;

const fail = (message: string): never => {
  throw new Error(`${FIELD_ITEM_WIDTH_ILLEGAL_CATEGORY}: ${message}`);
};

const exactIndex = (value: number, name: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    return fail(`${name} must be a non-negative safe integer`);
  }
  return value;
};

export const fieldItemWidthCoordinateIsSupported = (
  fieldIndex: number,
  itemIndex: number,
): boolean =>
  Number.isSafeInteger(itemIndex) &&
  itemIndex >= 0 &&
  (fieldIndex === FIELD_ITEM_WIDTH_ILLEGAL_OUTPUTS_FIELD ||
    fieldIndex === FIELD_ITEM_WIDTH_ILLEGAL_MINT_FIELD);

export const fieldItemWidthIsIllegal = (
  fieldIndex: number,
  itemWidth: number,
): boolean => {
  exactIndex(itemWidth, "item width");
  if (fieldIndex === FIELD_ITEM_WIDTH_ILLEGAL_OUTPUTS_FIELD) {
    return itemWidth > FIELD_ITEM_WIDTH_ILLEGAL_MAX_OUTPUT_BYTES;
  }
  if (fieldIndex === FIELD_ITEM_WIDTH_ILLEGAL_MINT_FIELD) {
    return itemWidth === 0;
  }
  return fail(`field ${fieldIndex.toString()} is outside the width family`);
};

const fieldItemWidthReasonCoordinate = (
  reason: RejectionReason,
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

export type FieldItemWidthFinding = {
  readonly subject: VerdictSubject;
  readonly fieldIndex: number;
  readonly itemIndex: number;
};

/** Strict classifier: this reason never falls back to validation dispute. */
export const classifyFieldItemWidthFinding = ({
  subject,
  fieldIndex,
  itemIndex,
}: FieldItemWidthFinding): FieldItemWidthFinding => {
  if (!verdictSubjectIsCanonical(subject)) {
    return fail("verdict subject is not canonical");
  }
  if (!fieldItemWidthCoordinateIsSupported(fieldIndex, itemIndex)) {
    return fail("unsupported field/item coordinate");
  }
  if (subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    if (subject.rejection_reason === null) {
      return fail("wrongful-rejection subject carries no typed reason");
    }
    const exact = fieldItemWidthReasonCoordinate(subject.rejection_reason);
    if (exact.fieldIndex !== fieldIndex || exact.itemIndex !== itemIndex) {
      return fail("typed reason coordinate differs from finding coordinate");
    }
  } else if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity is invalid");
  }
  return Object.freeze({ subject, fieldIndex, itemIndex });
};

export type FieldItemWidthEvidence = FieldItemWidthFinding & {
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
export const prepareFieldItemWidthEvidence = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: FieldItemWidthFinding;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): FieldItemWidthEvidence => {
  const finding = classifyFieldItemWidthFinding(rawFinding);
  if (!/^[0-9a-f]{64}$/u.test(committedFieldHashHex)) {
    return fail("committed field hash must be 32 bytes of lowercase hex");
  }
  const actualCommitment =
    midgardFieldCommitment(fieldPreimage).toString("hex");
  if (actualCommitment !== committedFieldHashHex) {
    return fail("retained field preimage does not match committed field hash");
  }
  const items = decodeMidgardFieldPreimage(fieldPreimage);
  const item = items[finding.itemIndex];
  if (item === undefined) return fail("item coordinate is outside the field");
  const decisiveFaultHolds = fieldItemWidthIsIllegal(
    finding.fieldIndex,
    item.length,
  );
  return Object.freeze({
    ...finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actualCommitment,
    itemHex: item.toString("hex"),
    itemWidth: item.length,
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
    decisiveFaultHolds,
  });
};

export const fieldItemWidthEvidenceCloses = (
  evidence: FieldItemWidthEvidence,
): boolean =>
  terminalVerdictContradiction(
    evidence.subject,
    fieldItemWidthIsIllegal(evidence.fieldIndex, evidence.itemWidth),
  );

const VerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

export const FieldItemWidthBoundCoordinateSchema = Data.Object({
  subject: VerdictSubjectSchema,
  field_index: Data.Integer(),
  item_index: Data.Integer(),
});

export const FieldItemWidthAuthenticatedWidthSchema = Data.Object({
  subject: VerdictSubjectSchema,
  field_index: Data.Integer(),
  item_index: Data.Integer(),
  item_width: Data.Integer(),
});

export const encodeFieldItemWidthBoundCoordinate = (
  finding: FieldItemWidthFinding,
): string => {
  const exact = classifyFieldItemWidthFinding(finding);
  return Data.to(
    {
      subject: exact.subject,
      field_index: BigInt(exact.fieldIndex),
      item_index: BigInt(exact.itemIndex),
    } as never,
    FieldItemWidthBoundCoordinateSchema as never,
  );
};

export const encodeFieldItemWidthAuthenticatedWidth = (
  evidence: FieldItemWidthEvidence,
): string =>
  Data.to(
    {
      subject: evidence.subject,
      field_index: BigInt(evidence.fieldIndex),
      item_index: BigInt(evidence.itemIndex),
      item_width: BigInt(evidence.itemWidth),
    } as never,
    FieldItemWidthAuthenticatedWidthSchema as never,
  );

export const FIELD_ITEM_WIDTH_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "proven",
  "removed",
  "cancelled",
] as const;
export type FieldItemWidthStage = (typeof FIELD_ITEM_WIDTH_STAGES)[number];

export type FieldItemWidthJournalEntry = {
  readonly sequence: number;
  readonly identity: string;
  readonly stage: FieldItemWidthStage;
  readonly txHash: string;
  readonly outputReference: string | null;
};

export type FieldItemWidthJournal = {
  readonly load: (
    identity: string,
  ) => Promise<readonly FieldItemWidthJournalEntry[]>;
  readonly append: (entry: FieldItemWidthJournalEntry) => Promise<void>;
};

export const fieldItemWidthEvidenceIdentity = (
  evidence: FieldItemWidthEvidence,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction.toString(),
    evidence.fieldIndex.toString(),
    evidence.itemIndex.toString(),
    evidence.fieldCommitmentHex,
  ].join(":");

const stageRank = (stage: FieldItemWidthStage): number =>
  FIELD_ITEM_WIDTH_STAGES.indexOf(stage);

export const reconcileFieldItemWidthJournal = (
  identity: string,
  entries: readonly FieldItemWidthJournalEntry[],
  observedStage: FieldItemWidthStage,
): FieldItemWidthStage => {
  let lastSequence = -1;
  let lastStage: FieldItemWidthStage = "none";
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

export type FieldItemWidthAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "removeDescendants"
  | "done";

export const nextFieldItemWidthAction = (
  stage: FieldItemWidthStage,
): FieldItemWidthAction => {
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

export type FieldItemWidthSubmissionResult = {
  readonly stage: FieldItemWidthStage;
  readonly txHash: string;
  readonly outputReference: string | null;
};

export type FieldItemWidthSubmissionAdapter = {
  readonly observe: (identity: string) => Promise<FieldItemWidthStage>;
  readonly submit: (
    action: Exclude<FieldItemWidthAction, "done">,
    evidence: FieldItemWidthEvidence,
  ) => Promise<FieldItemWidthSubmissionResult>;
  readonly cancel: (
    stage: "step01" | "step02" | "step03",
    evidence: FieldItemWidthEvidence,
  ) => Promise<FieldItemWidthSubmissionResult>;
};

/** Durable, restart-safe runner; every decision is reconstructed from journal + chain. */
export const runFieldItemWidthProof = async ({
  evidence,
  journal,
  submission,
}: {
  readonly evidence: FieldItemWidthEvidence;
  readonly journal: FieldItemWidthJournal;
  readonly submission: FieldItemWidthSubmissionAdapter;
}): Promise<FieldItemWidthStage> => {
  if (!fieldItemWidthEvidenceCloses(evidence)) {
    return fail("honest verdict cannot start a proof thread");
  }
  const identity = fieldItemWidthEvidenceIdentity(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    const observed = await submission.observe(identity);
    const stage = reconcileFieldItemWidthJournal(identity, entries, observed);
    const action = nextFieldItemWidthAction(stage);
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
