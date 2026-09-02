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

import { ObserversForbiddenStateV1Schema } from "./family-v1.js";

export const ObserversForbiddenStep01SourceV1Schema = Data.Enum([
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
export const ObserversForbiddenStep01ArgsV1Schema = Data.Object({
  source: ObserversForbiddenStep01SourceV1Schema,
});
export const ObserversForbiddenStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(ObserversForbiddenStep01ArgsV1Schema);

export const ObserversForbiddenStep02DatumV1Schema = faultProofStepDatumSchema(
  ObserversForbiddenStateV1Schema,
);
export const ObserversForbiddenStep02ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  observer_opening: FieldOpeningV1Schema,
});
export const ObserversForbiddenStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(ObserversForbiddenStep02ArgsV1Schema);
