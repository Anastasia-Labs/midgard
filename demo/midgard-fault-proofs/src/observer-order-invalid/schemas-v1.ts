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

import {
  ObserverOrderInvalidAuthenticationStateSchema,
  ObserverOrderInvalidDecisionStateSchema,
  ObserverOrderInvalidScanStateSchema,
} from "./family-v1.js";

export const ObserverOrderInvalidStep01SourceSchema = Data.Enum([
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

export const ObserverOrderInvalidStep01ArgsSchema = Data.Object({
  source: ObserverOrderInvalidStep01SourceSchema,
  observer_index: Data.Integer(),
});
export const ObserverOrderInvalidStep01RedeemerSchema =
  faultProofStepRedeemerSchema(ObserverOrderInvalidStep01ArgsSchema);

export const ObserverOrderInvalidStep02DatumSchema = faultProofStepDatumSchema(
  ObserverOrderInvalidAuthenticationStateSchema,
);
export const ObserverOrderInvalidStep02ActionSchema = Data.Enum([
  Data.Object({
    Authenticate: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
    }),
  }),
  Data.Literal("Reserved"),
]);
export const ObserverOrderInvalidStep02RedeemerSchema =
  faultProofStepRedeemerSchema(ObserverOrderInvalidStep02ActionSchema);

export const ObserverOrderInvalidStep03DatumSchema = faultProofStepDatumSchema(
  ObserverOrderInvalidScanStateSchema,
);
export const ObserverOrderInvalidStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
  item_budget: Data.Integer(),
});
export const ObserverOrderInvalidStep03RedeemerSchema =
  faultProofStepRedeemerSchema(ObserverOrderInvalidStep03ArgsSchema);

export const ObserverOrderInvalidStep04DatumSchema = faultProofStepDatumSchema(
  ObserverOrderInvalidDecisionStateSchema,
);
export const ObserverOrderInvalidStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const ObserverOrderInvalidStep04RedeemerSchema =
  faultProofStepRedeemerSchema(ObserverOrderInvalidStep04ArgsSchema);
