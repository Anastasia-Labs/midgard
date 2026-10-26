/** Direct missing-redeemer wire: the observed route
 * (`missing-redeemer/step-0*.ak`).
 * Constructor and field order is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { DirectRedeemerScanStateSchema } from "./direct-claim.js";
import {
  DirectProgressArgsSchema,
  DirectScanActionSchema,
  DirectSourceSelectionSchema,
} from "./direct-purpose-workers.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const MissingRedeemerStartArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export type MissingRedeemerStartArgs = Data.Static<
  typeof MissingRedeemerStartArgsSchema
>;
export const MissingRedeemerStartArgs = asDataType<MissingRedeemerStartArgs>(
  MissingRedeemerStartArgsSchema,
);

export const MissingRedeemerStartDatumSchema = faultProofStepDatumSchema(
  DirectSourceSelectionSchema,
);
export type MissingRedeemerStartDatum = Data.Static<
  typeof MissingRedeemerStartDatumSchema
>;
export const MissingRedeemerStartDatum = asDataType<MissingRedeemerStartDatum>(
  MissingRedeemerStartDatumSchema,
);

export const MissingRedeemerStartRedeemerSchema = faultProofStepRedeemerSchema(
  MissingRedeemerStartArgsSchema,
);
export type MissingRedeemerStartRedeemer = Data.Static<
  typeof MissingRedeemerStartRedeemerSchema
>;
export const MissingRedeemerStartRedeemer =
  asDataType<MissingRedeemerStartRedeemer>(MissingRedeemerStartRedeemerSchema);

export const MissingRedeemerScanDatumSchema = faultProofStepDatumSchema(
  DirectRedeemerScanStateSchema,
);
export type MissingRedeemerScanDatum = Data.Static<
  typeof MissingRedeemerScanDatumSchema
>;
export const MissingRedeemerScanDatum = asDataType<MissingRedeemerScanDatum>(
  MissingRedeemerScanDatumSchema,
);

export const MissingRedeemerGrammarRedeemerSchema =
  faultProofStepRedeemerSchema(DirectProgressArgsSchema);
export type MissingRedeemerGrammarRedeemer = Data.Static<
  typeof MissingRedeemerGrammarRedeemerSchema
>;
export const MissingRedeemerGrammarRedeemer =
  asDataType<MissingRedeemerGrammarRedeemer>(
    MissingRedeemerGrammarRedeemerSchema,
  );

export const MissingRedeemerScanRedeemerSchema = faultProofStepRedeemerSchema(
  DirectScanActionSchema,
);
export type MissingRedeemerScanRedeemer = Data.Static<
  typeof MissingRedeemerScanRedeemerSchema
>;
export const MissingRedeemerScanRedeemer =
  asDataType<MissingRedeemerScanRedeemer>(MissingRedeemerScanRedeemerSchema);
