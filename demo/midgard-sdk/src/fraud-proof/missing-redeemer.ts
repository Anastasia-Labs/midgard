/** Direct missing-redeemer observed-route wire (`missing-redeemer/rule.ak`,
 * `missing-redeemer/step-0*.ak`). Constructor and field order is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  DirectPointerStateSchema,
  DirectProgressArgsSchema,
  DirectScanActionSchema,
  DirectSourceSelectionSchema,
} from "./direct-purpose-workers.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";
import { ProofThreadSubjectSourceSchema } from "./proof-thread-source.js";

export const MissingRedeemerStep01ArgsSchema = Data.Object({
  source: ProofThreadSubjectSourceSchema,
});
export const MissingRedeemerStartArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export const MissingRedeemerScanStateSchema = Data.Object({
  authenticated: DirectSourceSelectionSchema,
  scan: DirectPointerStateSchema,
});
export const MissingRedeemerDecisionStateSchema = Data.Object({
  authenticated: DirectSourceSelectionSchema,
  redeemer_missing: Data.Boolean(),
});
export const MissingRedeemerFinalizeArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type MissingRedeemerStep01Args = Data.Static<
  typeof MissingRedeemerStep01ArgsSchema
>;
export const MissingRedeemerStep01Args = asDataType<MissingRedeemerStep01Args>(
  MissingRedeemerStep01ArgsSchema,
);
export type MissingRedeemerStartArgs = Data.Static<
  typeof MissingRedeemerStartArgsSchema
>;
export const MissingRedeemerStartArgs = asDataType<MissingRedeemerStartArgs>(
  MissingRedeemerStartArgsSchema,
);
export type MissingRedeemerScanState = Data.Static<
  typeof MissingRedeemerScanStateSchema
>;
export const MissingRedeemerScanState = asDataType<MissingRedeemerScanState>(
  MissingRedeemerScanStateSchema,
);
export type MissingRedeemerDecisionState = Data.Static<
  typeof MissingRedeemerDecisionStateSchema
>;
export const MissingRedeemerDecisionState =
  asDataType<MissingRedeemerDecisionState>(MissingRedeemerDecisionStateSchema);
export type MissingRedeemerFinalizeArgs = Data.Static<
  typeof MissingRedeemerFinalizeArgsSchema
>;
export const MissingRedeemerFinalizeArgs =
  asDataType<MissingRedeemerFinalizeArgs>(MissingRedeemerFinalizeArgsSchema);

export const MissingRedeemerStep01RedeemerSchema = faultProofStepRedeemerSchema(
  MissingRedeemerStep01ArgsSchema,
);
export type MissingRedeemerStep01Redeemer = Data.Static<
  typeof MissingRedeemerStep01RedeemerSchema
>;
export const MissingRedeemerStep01Redeemer =
  asDataType<MissingRedeemerStep01Redeemer>(
    MissingRedeemerStep01RedeemerSchema,
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
  MissingRedeemerScanStateSchema,
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

export const MissingRedeemerFinalizeRedeemerSchema =
  faultProofStepRedeemerSchema(MissingRedeemerFinalizeArgsSchema);
export type MissingRedeemerFinalizeRedeemer = Data.Static<
  typeof MissingRedeemerFinalizeRedeemerSchema
>;
export const MissingRedeemerFinalizeRedeemer =
  asDataType<MissingRedeemerFinalizeRedeemer>(
    MissingRedeemerFinalizeRedeemerSchema,
  );
