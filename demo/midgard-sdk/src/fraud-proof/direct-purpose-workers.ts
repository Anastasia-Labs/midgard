/** Physical direct-purpose worker twins; rejection-reason ABI stays global. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ProofSchema, ScriptHashSchema } from "../common.js";
import {
  DirectFieldCursorSchema,
  DirectMintPurposeStateSchema,
  DirectPurposeCoordinateSchema,
  DirectPurposeSelectionSchema,
  DirectScriptSourceSchema,
} from "./direct-script-derivation.js";
import { DirectSourceIndexSchema } from "./direct-source-index-state.js";
import { DirectSourceBoundIndexSchema } from "./direct-source-workers.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MembershipCarriageSchema,
} from "./native.js";

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };
const selected = {
  ...indices,
  purpose_index: Data.Integer(),
  opening: FieldOpeningSchema,
};

/** The selection door hands a spend selection to the spend door unchanged. */
export const DirectPurposeActionSchema = Data.Enum([
  Data.Object({ Spend: Data.Object(indices) }),
  Data.Object({ Observe: Data.Object(selected) }),
  Data.Object({ Mint: Data.Object(selected) }),
  Data.Object({ Receive: Data.Object(indices) }),
]);
export const DirectSpendSelectionArgsSchema = Data.Object({
  ...indices,
  raw_item_index: Data.Integer(),
  opening: FieldOpeningSchema,
  descriptor_cbor: Data.Bytes(),
  membership: MembershipCarriageSchema,
});
export const DirectMintWorkStateSchema = Data.Object({
  source_index: DirectSourceIndexSchema,
  scan: DirectMintPurposeStateSchema,
});
export const DirectSpendWorkStateSchema = Data.Object({
  source_index: DirectSourceIndexSchema,
  cursor: DirectFieldCursorSchema,
  raw_item_index: Data.Integer(),
  target: Data.Object({ script_hash: ScriptHashSchema, subject: Data.Bytes() }),
  rank: Data.Integer(),
  complete: Data.Boolean(),
});
export const DirectProgressArgsSchema = Data.Object({
  ...indices,
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
  budget: Data.Integer(),
});
export const DirectScanActionSchema = Data.Enum([
  Data.Object({ Scan: Data.Object({ args: DirectProgressArgsSchema }) }),
  Data.Object({ Finish: Data.Object(indices) }),
]);
export const DirectSourceSelectionSchema = Data.Object({
  selection: DirectPurposeSelectionSchema,
  source: Data.Nullable(DirectScriptSourceSchema),
});
export const DirectSourceLookupArgsSchema = Data.Object({
  ...indices,
  source: Data.Nullable(DirectScriptSourceSchema),
  proof: ProofSchema,
});
export const DirectPointerStateSchema = Data.Object({
  cursor: DirectFieldCursorSchema,
  coordinate: DirectPurposeCoordinateSchema,
  found: Data.Boolean(),
  complete: Data.Boolean(),
});
export type DirectPurposeAction = Data.Static<typeof DirectPurposeActionSchema>;
export const DirectPurposeAction = asDataType<DirectPurposeAction>(
  DirectPurposeActionSchema,
);
export type DirectMintWorkState = Data.Static<typeof DirectMintWorkStateSchema>;
export const DirectMintWorkState = asDataType<DirectMintWorkState>(
  DirectMintWorkStateSchema,
);
export type DirectSpendWorkState = Data.Static<
  typeof DirectSpendWorkStateSchema
>;
export const DirectSpendWorkState = asDataType<DirectSpendWorkState>(
  DirectSpendWorkStateSchema,
);
export type DirectProgressArgs = Data.Static<typeof DirectProgressArgsSchema>;
export const DirectProgressArgs = asDataType<DirectProgressArgs>(
  DirectProgressArgsSchema,
);
export type DirectScanAction = Data.Static<typeof DirectScanActionSchema>;
export const DirectScanAction = asDataType<DirectScanAction>(
  DirectScanActionSchema,
);
export type DirectSourceSelection = Data.Static<
  typeof DirectSourceSelectionSchema
>;
export const DirectSourceSelection = asDataType<DirectSourceSelection>(
  DirectSourceSelectionSchema,
);
export type DirectSourceLookupArgs = Data.Static<
  typeof DirectSourceLookupArgsSchema
>;
export const DirectSourceLookupArgs = asDataType<DirectSourceLookupArgs>(
  DirectSourceLookupArgsSchema,
);
export type DirectPointerState = Data.Static<typeof DirectPointerStateSchema>;
export const DirectPointerState = asDataType<DirectPointerState>(
  DirectPointerStateSchema,
);

export const DirectPurposeSelectDatumSchema = faultProofStepDatumSchema(
  DirectSourceBoundIndexSchema,
);
export type DirectPurposeSelectDatum = Data.Static<
  typeof DirectPurposeSelectDatumSchema
>;
export const DirectPurposeSelectDatum = asDataType<DirectPurposeSelectDatum>(
  DirectPurposeSelectDatumSchema,
);

export const DirectPurposeSelectRedeemerSchema = faultProofStepRedeemerSchema(
  DirectPurposeActionSchema,
);
export type DirectPurposeSelectRedeemer = Data.Static<
  typeof DirectPurposeSelectRedeemerSchema
>;
export const DirectPurposeSelectRedeemer =
  asDataType<DirectPurposeSelectRedeemer>(DirectPurposeSelectRedeemerSchema);

export const DirectSpendSelectRedeemerSchema = faultProofStepRedeemerSchema(
  DirectSpendSelectionArgsSchema,
);
export type DirectSpendSelectRedeemer = Data.Static<
  typeof DirectSpendSelectRedeemerSchema
>;
export const DirectSpendSelectRedeemer = asDataType<DirectSpendSelectRedeemer>(
  DirectSpendSelectRedeemerSchema,
);
export const DirectMintWorkDatumSchema = faultProofStepDatumSchema(
  DirectMintWorkStateSchema,
);
export type DirectMintWorkDatum = Data.Static<typeof DirectMintWorkDatumSchema>;
export const DirectMintWorkDatum = asDataType<DirectMintWorkDatum>(
  DirectMintWorkDatumSchema,
);

export const DirectMintGrammarRedeemerSchema = faultProofStepRedeemerSchema(
  DirectProgressArgsSchema,
);
export type DirectMintGrammarRedeemer = Data.Static<
  typeof DirectMintGrammarRedeemerSchema
>;
export const DirectMintGrammarRedeemer = asDataType<DirectMintGrammarRedeemer>(
  DirectMintGrammarRedeemerSchema,
);

export const DirectMintScanRedeemerSchema = faultProofStepRedeemerSchema(
  DirectScanActionSchema,
);
export type DirectMintScanRedeemer = Data.Static<
  typeof DirectMintScanRedeemerSchema
>;
export const DirectMintScanRedeemer = asDataType<DirectMintScanRedeemer>(
  DirectMintScanRedeemerSchema,
);

export const DirectSpendWorkDatumSchema = faultProofStepDatumSchema(
  DirectSpendWorkStateSchema,
);
export type DirectSpendWorkDatum = Data.Static<
  typeof DirectSpendWorkDatumSchema
>;
export const DirectSpendWorkDatum = asDataType<DirectSpendWorkDatum>(
  DirectSpendWorkDatumSchema,
);
export const DirectSpendScanRedeemerSchema = faultProofStepRedeemerSchema(
  DirectScanActionSchema,
);
export type DirectSpendScanRedeemer = Data.Static<
  typeof DirectSpendScanRedeemerSchema
>;
export const DirectSpendScanRedeemer = asDataType<DirectSpendScanRedeemer>(
  DirectSpendScanRedeemerSchema,
);

export const DirectSourceLookupDatumSchema = faultProofStepDatumSchema(
  DirectPurposeSelectionSchema,
);
export type DirectSourceLookupDatum = Data.Static<
  typeof DirectSourceLookupDatumSchema
>;
export const DirectSourceLookupDatum = asDataType<DirectSourceLookupDatum>(
  DirectSourceLookupDatumSchema,
);

export const DirectSourceLookupRedeemerSchema = faultProofStepRedeemerSchema(
  DirectSourceLookupArgsSchema,
);
export type DirectSourceLookupRedeemer = Data.Static<
  typeof DirectSourceLookupRedeemerSchema
>;
export const DirectSourceLookupRedeemer =
  asDataType<DirectSourceLookupRedeemer>(DirectSourceLookupRedeemerSchema);

export const DirectSourceSelectionDatumSchema = faultProofStepDatumSchema(
  DirectSourceSelectionSchema,
);
export type DirectSourceSelectionDatum = Data.Static<
  typeof DirectSourceSelectionDatumSchema
>;
export const DirectSourceSelectionDatum =
  asDataType<DirectSourceSelectionDatum>(DirectSourceSelectionDatumSchema);
