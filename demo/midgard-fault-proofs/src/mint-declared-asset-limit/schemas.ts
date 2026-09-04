import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  MintDeclaredAssetLimitAuthenticationStateSchema,
  MintDeclaredAssetLimitDecisionStateSchema,
  MintDeclaredAssetLimitFoldStateSchema,
} from "./family.js";

export const MintDeclaredAssetLimitStep01SourceSchema = Data.Enum([
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
        ForcedInclusionTxV1Schema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);

export const MintDeclaredAssetLimitStep01ArgsSchema = Data.Object({
  source: MintDeclaredAssetLimitStep01SourceSchema,
  policy_index: Data.Integer(),
});
export const MintDeclaredAssetLimitStep01RedeemerSchema =
  faultProofStepRedeemerSchema(MintDeclaredAssetLimitStep01ArgsSchema);

export const MintDeclaredAssetLimitStep02DatumSchema =
  faultProofStepDatumSchema(MintDeclaredAssetLimitAuthenticationStateSchema);
export const MintDeclaredAssetLimitStep02ActionSchema = Data.Enum([
  Data.Object({
    AuthenticateDirect: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    StartGrammar: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    ResumeGrammar: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    FinishGrammar: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
    }),
  }),
]);
export const MintDeclaredAssetLimitStep02RedeemerSchema =
  faultProofStepRedeemerSchema(MintDeclaredAssetLimitStep02ActionSchema);

export const MintDeclaredAssetLimitStep03DatumSchema =
  faultProofStepDatumSchema(MintDeclaredAssetLimitFoldStateSchema);
export const MintDeclaredAssetLimitStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
  item_budget: Data.Integer(),
});
export const MintDeclaredAssetLimitStep03RedeemerSchema =
  faultProofStepRedeemerSchema(MintDeclaredAssetLimitStep03ArgsSchema);

export const MintDeclaredAssetLimitStep04DatumSchema =
  faultProofStepDatumSchema(MintDeclaredAssetLimitDecisionStateSchema);
export const MintDeclaredAssetLimitStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const MintDeclaredAssetLimitStep04RedeemerSchema =
  faultProofStepRedeemerSchema(MintDeclaredAssetLimitStep04ArgsSchema);
