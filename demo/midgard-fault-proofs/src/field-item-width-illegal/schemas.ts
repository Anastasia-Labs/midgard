import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ProofThreadSubjectSourceSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  FieldItemWidthAuthenticatedItemSchema,
  FieldItemWidthBoundCoordinateSchema,
} from "./field-item-width-illegal.js";

export const FieldItemWidthStep01SourceSchema = ProofThreadSubjectSourceSchema;
export const FieldItemWidthStep01ArgsSchema = Data.Object({
  source: FieldItemWidthStep01SourceSchema,
  field_index: Data.Integer(),
  item_index: Data.Integer(),
});
export const FieldItemWidthStep01RedeemerSchema = faultProofStepRedeemerSchema(
  FieldItemWidthStep01ArgsSchema,
);

export const FieldItemWidthStep02DatumSchema = faultProofStepDatumSchema(
  FieldItemWidthBoundCoordinateSchema,
);
export const FieldItemWidthStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: Data.Nullable(FieldOpeningSchema),
  grammar_ref_input_index: Data.Nullable(Data.Integer()),
});
export const FieldItemWidthStep02RedeemerSchema = faultProofStepRedeemerSchema(
  FieldItemWidthStep02ArgsSchema,
);
export const FieldItemWidthStep03DatumSchema = faultProofStepDatumSchema(
  FieldItemWidthAuthenticatedItemSchema,
);
export const FieldItemWidthStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const FieldItemWidthStep03RedeemerSchema = faultProofStepRedeemerSchema(
  FieldItemWidthStep03ArgsSchema,
);
