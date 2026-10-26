/** Direct missing-redeemer wire: step-01, the family entry and the observed
 * route (`missing-redeemer/entry.ak`, `missing-redeemer/step-0*.ak`).
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
import { ProofThreadSubjectSourceSchema } from "./proof-thread-source.js";

export const MissingRedeemerStep01ArgsSchema = Data.Object({
  source: ProofThreadSubjectSourceSchema,
});
export const MissingRedeemerStartArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
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

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };

/** 0 Observe, 1 Claim. */
export const MissingRedeemerEntryActionSchema = Data.Enum([
  Data.Object({ Observe: Data.Object(indices) }),
  Data.Object({ Claim: Data.Object(indices) }),
]);
export type MissingRedeemerEntryAction = Data.Static<
  typeof MissingRedeemerEntryActionSchema
>;
export const MissingRedeemerEntryAction =
  asDataType<MissingRedeemerEntryAction>(MissingRedeemerEntryActionSchema);

export const MissingRedeemerEntryRedeemerSchema = faultProofStepRedeemerSchema(
  MissingRedeemerEntryActionSchema,
);
export type MissingRedeemerEntryRedeemer = Data.Static<
  typeof MissingRedeemerEntryRedeemerSchema
>;
export const MissingRedeemerEntryRedeemer =
  asDataType<MissingRedeemerEntryRedeemer>(MissingRedeemerEntryRedeemerSchema);
