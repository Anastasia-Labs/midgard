import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ProofThreadForcedSourcePayloadSchema,
  ProofThreadSubjectSourceSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { ZeroInputStateSchema } from "./family.js";

export const ZeroInputForcedSourcePayloadSchema =
  ProofThreadForcedSourcePayloadSchema;
export const ZeroInputStep01ArgsSchema = Data.Object({
  source: ProofThreadSubjectSourceSchema,
});
export const ZeroInputStep01RedeemerSchema = faultProofStepRedeemerSchema(
  ZeroInputStep01ArgsSchema,
);

export const ZeroInputStep02DatumSchema =
  faultProofStepDatumSchema(ZeroInputStateSchema);
export const ZeroInputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningSchema,
});
export const ZeroInputStep02RedeemerSchema = faultProofStepRedeemerSchema(
  ZeroInputStep02ArgsSchema,
);
