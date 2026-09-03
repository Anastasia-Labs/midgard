import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedInclusionTxSchema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { ZeroInputStateSchema } from "./family-v1.js";

export const ZeroInputStep01SourceSchema = Data.Enum([
  Data.Object({
    AcceptedSource: Data.Object({ inclusion: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    ForcedSource: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderSchema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxSchema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);
export const ZeroInputForcedSourcePayloadSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxSchema,
  ),
  direction: Data.Integer(),
});
export const ZeroInputStep01ArgsSchema = Data.Object({
  source: ZeroInputStep01SourceSchema,
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
