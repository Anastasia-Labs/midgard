import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ProofThreadSubjectSourceSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { ObserversForbiddenStateSchema } from "./family.js";

export const ObserversForbiddenStep01SourceSchema =
  ProofThreadSubjectSourceSchema;
export const ObserversForbiddenStep01ArgsSchema = Data.Object({
  source: ObserversForbiddenStep01SourceSchema,
});
export const ObserversForbiddenStep01RedeemerSchema =
  faultProofStepRedeemerSchema(ObserversForbiddenStep01ArgsSchema);

export const ObserversForbiddenStep02DatumSchema = faultProofStepDatumSchema(
  ObserversForbiddenStateSchema,
);
export const ObserversForbiddenStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  observer_opening: FieldOpeningSchema,
});
export const ObserversForbiddenStep02RedeemerSchema =
  faultProofStepRedeemerSchema(ObserversForbiddenStep02ArgsSchema);
