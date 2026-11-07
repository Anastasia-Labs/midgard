import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  H32Schema,
  NativeTxBodyCompactSchema,
  type VerdictSubject,
  VerdictSubjectSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  classifyFieldItemWidthFinding,
  FIELD_ITEM_WIDTH_ILLEGAL_CATEGORY,
  type FieldItemWidthEvidence,
  fieldItemWidthEvidenceCloses,
  type FieldItemWidthFinding,
} from "./field-item-width-illegal.evidence.js";
export * from "./field-item-width-illegal.evidence.js";

const fail = (message: string): never => {
  throw new Error(`${FIELD_ITEM_WIDTH_ILLEGAL_CATEGORY}: ${message}`);
};

const SubjectData = asDataType<VerdictSubject>(VerdictSubjectSchema);

const FieldItemWidthCompactSchema = Data.Object({
  body: NativeTxBodyCompactSchema,
  witness_set_hash: H32Schema,
  validity_code: Data.Nullable(Data.Integer()),
});

export const FieldItemWidthBoundCoordinateSchema = Data.Object({
  subject: VerdictSubjectSchema,
  field_index: Data.Integer(),
  item_index: Data.Integer(),
  compact: FieldItemWidthCompactSchema,
});
export type FieldItemWidthBoundCoordinate = Data.Static<
  typeof FieldItemWidthBoundCoordinateSchema
>;
export const FieldItemWidthBoundCoordinate =
  asDataType<FieldItemWidthBoundCoordinate>(
    FieldItemWidthBoundCoordinateSchema,
  );

const FieldItemWidthObservationSchema = Data.Enum([
  Data.Literal("ItemOutOfDomain"),
  Data.Object({
    ItemLegality: Data.Object({ item_is_legal: Data.Boolean() }),
  }),
]);

export const FieldItemWidthAuthenticatedItemSchema = Data.Object({
  subject: VerdictSubjectSchema,
  field_index: Data.Integer(),
  item_index: Data.Integer(),
  observation: FieldItemWidthObservationSchema,
});
export type FieldItemWidthAuthenticatedItem = Data.Static<
  typeof FieldItemWidthAuthenticatedItemSchema
>;
export const FieldItemWidthAuthenticatedItem =
  asDataType<FieldItemWidthAuthenticatedItem>(
    FieldItemWidthAuthenticatedItemSchema,
  );

export const fieldItemWidthBoundCoordinate = (
  finding: FieldItemWidthFinding,
  compact: FieldItemWidthBoundCoordinate["compact"],
): FieldItemWidthBoundCoordinate => {
  const exact = classifyFieldItemWidthFinding(finding);
  return {
    subject: exact.subject,
    field_index: exact.fieldIndex,
    item_index: exact.itemIndex,
    compact,
  };
};

export const fieldItemWidthAuthenticatedItem = (
  evidence: FieldItemWidthEvidence,
): FieldItemWidthAuthenticatedItem => ({
  subject: evidence.subject,
  field_index: evidence.fieldIndex,
  item_index: evidence.itemIndex,
  observation: evidence.observation,
});

export const encodeFieldItemWidthBoundCoordinate = (
  finding: FieldItemWidthFinding,
  compact: FieldItemWidthBoundCoordinate["compact"],
): string =>
  Data.to(
    fieldItemWidthBoundCoordinate(finding, compact),
    FieldItemWidthBoundCoordinate,
  );

export const encodeFieldItemWidthAuthenticatedItem = (
  evidence: FieldItemWidthEvidence,
): string =>
  Data.to(
    fieldItemWidthAuthenticatedItem(evidence),
    FieldItemWidthAuthenticatedItem,
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
    Data.to(evidence.subject, SubjectData),
    evidence.fieldIndex.toString(),
    evidence.itemIndex.toString(),
    evidence.fieldCommitmentHex ?? "StaticDomain",
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
