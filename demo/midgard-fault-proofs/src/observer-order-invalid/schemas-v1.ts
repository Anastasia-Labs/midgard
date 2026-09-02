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

import {
  ObserverOrderInvalidAuthenticationStateV1Schema,
  ObserverOrderInvalidDecisionStateV1Schema,
  ObserverOrderInvalidScanStateV1Schema,
} from "./family-v1.js";

export const ObserverOrderInvalidStep01SourceV1Schema = Data.Enum([
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

export const ObserverOrderInvalidStep01ArgsV1Schema = Data.Object({
  source: ObserverOrderInvalidStep01SourceV1Schema,
  observer_index: Data.Integer(),
});
export const ObserverOrderInvalidStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(ObserverOrderInvalidStep01ArgsV1Schema);

export const ObserverOrderInvalidStep02DatumV1Schema =
  faultProofStepDatumSchema(ObserverOrderInvalidAuthenticationStateV1Schema);
export const ObserverOrderInvalidStep02ActionV1Schema = Data.Enum([
  Data.Object({
    Authenticate: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
    }),
  }),
  Data.Literal("Reserved"),
]);
export const ObserverOrderInvalidStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(ObserverOrderInvalidStep02ActionV1Schema);

export const ObserverOrderInvalidStep03DatumV1Schema =
  faultProofStepDatumSchema(ObserverOrderInvalidScanStateV1Schema);
export const ObserverOrderInvalidStep03ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningV1Schema,
  checkpoint_bytes: Data.Bytes(),
  item_budget: Data.Integer(),
});
export const ObserverOrderInvalidStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(ObserverOrderInvalidStep03ArgsV1Schema);

export const ObserverOrderInvalidStep04DatumV1Schema =
  faultProofStepDatumSchema(ObserverOrderInvalidDecisionStateV1Schema);
export const ObserverOrderInvalidStep04ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const ObserverOrderInvalidStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(ObserverOrderInvalidStep04ArgsV1Schema);
