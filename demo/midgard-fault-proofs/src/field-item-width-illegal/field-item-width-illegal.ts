import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { H32Schema, NativeTxBodyCompactSchema } from "@al-ft/midgard-sdk/core";
import { VerdictSubjectSchema } from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import {
  classifyFieldItemWidthFinding,
  type FieldItemWidthEvidence,
  type FieldItemWidthFinding,
} from "./field-item-width-illegal.evidence.js";
export * from "./field-item-width-illegal.evidence.js";

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
