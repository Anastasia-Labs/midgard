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
  MintDeclaredAssetLimitAuthenticationStateV1Schema,
  MintDeclaredAssetLimitDecisionStateV1Schema,
  MintDeclaredAssetLimitFoldStateV1Schema,
} from "./family-v1.js";

export const MintDeclaredAssetLimitStep01SourceV1Schema = Data.Enum([
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

export const MintDeclaredAssetLimitStep01ArgsV1Schema = Data.Object({
  source: MintDeclaredAssetLimitStep01SourceV1Schema,
  policy_index: Data.Integer(),
});
export const MintDeclaredAssetLimitStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(MintDeclaredAssetLimitStep01ArgsV1Schema);

export const MintDeclaredAssetLimitStep02DatumV1Schema =
  faultProofStepDatumSchema(MintDeclaredAssetLimitAuthenticationStateV1Schema);
export const MintDeclaredAssetLimitStep02ActionV1Schema = Data.Enum([
  Data.Object({
    AuthenticateDirect: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
    }),
  }),
  Data.Object({
    StartGrammar: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    ResumeGrammar: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    FinishGrammar: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      checkpoint_bytes: Data.Bytes(),
    }),
  }),
]);
export const MintDeclaredAssetLimitStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(MintDeclaredAssetLimitStep02ActionV1Schema);

export const MintDeclaredAssetLimitStep03DatumV1Schema =
  faultProofStepDatumSchema(MintDeclaredAssetLimitFoldStateV1Schema);
export const MintDeclaredAssetLimitStep03ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningV1Schema,
  checkpoint_bytes: Data.Bytes(),
  item_budget: Data.Integer(),
});
export const MintDeclaredAssetLimitStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(MintDeclaredAssetLimitStep03ArgsV1Schema);

export const MintDeclaredAssetLimitStep04DatumV1Schema =
  faultProofStepDatumSchema(MintDeclaredAssetLimitDecisionStateV1Schema);
export const MintDeclaredAssetLimitStep04ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const MintDeclaredAssetLimitStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(MintDeclaredAssetLimitStep04ArgsV1Schema);
