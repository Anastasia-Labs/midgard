import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningV1Schema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { ZeroInputStateV1Schema } from "./family-v1.js";

export const ZeroInputStep01SourceV1Schema = Data.Enum([
  Data.Object({
    AcceptedSource: Data.Object({ inclusion: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    ForcedSource: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderV1Schema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxV1Schema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);
export const ZeroInputForcedSourcePayloadV1Schema = Data.Object({
  header: HeaderV1Schema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
  direction: Data.Integer(),
});
export const ZeroInputStep01ArgsV1Schema = Data.Object({
  source: ZeroInputStep01SourceV1Schema,
});
export const ZeroInputStep01RedeemerV1Schema = faultProofStepRedeemerSchema(
  ZeroInputStep01ArgsV1Schema,
);

export const ZeroInputStep02DatumV1Schema = faultProofStepDatumSchema(
  ZeroInputStateV1Schema,
);
export const ZeroInputStep02ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningV1Schema,
});
export const ZeroInputStep02RedeemerV1Schema = faultProofStepRedeemerSchema(
  ZeroInputStep02ArgsV1Schema,
);
