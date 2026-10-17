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
  MintItemBoundItemSchema,
  MintItemVerdictSubjectSchema,
} from "./mint-item-non-canonical.js";

export const MintItemScanControlSchema = Data.Object({
  stage: Data.Integer(),
  cursor: Data.Integer(),
  remaining: Data.Integer(),
  previous_policy: Data.Bytes(),
  previous_asset: Data.Nullable(Data.Bytes()),
});

export const MintItemStep01SourceSchema = Data.Enum([
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
export const MintItemStep01ArgsSchema = Data.Object({
  source: MintItemStep01SourceSchema,
  item_index: Data.Integer(),
});
export const MintItemStep01RedeemerSchema = faultProofStepRedeemerSchema(
  MintItemStep01ArgsSchema,
);

export const MintItemStep02DatumSchema = faultProofStepDatumSchema(
  MintItemBoundItemSchema,
);
export const MintItemStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export const MintItemStep02RedeemerSchema = faultProofStepRedeemerSchema(
  MintItemStep02ArgsSchema,
);
export const MintItemScanStateSchema = Data.Object({
  subject: MintItemVerdictSubjectSchema,
  item_index: Data.Integer(),
  item_length: Data.Integer(),
  item_hash: Data.Bytes(),
  chunk_hashes: Data.Array(Data.Bytes({ minLength: 32, maxLength: 32 })),
  control: MintItemScanControlSchema,
  outcome: Data.Integer(),
});
export const MintItemStep03DatumSchema = faultProofStepDatumSchema(
  MintItemScanStateSchema,
);
export const MintItemStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  window: Data.Bytes(),
});
export const MintItemStep03RedeemerSchema = faultProofStepRedeemerSchema(
  MintItemStep03ArgsSchema,
);
export const MintItemStep04DatumSchema = faultProofStepDatumSchema(
  MintItemScanStateSchema,
);
export const MintItemStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const MintItemStep04RedeemerSchema = faultProofStepRedeemerSchema(
  MintItemStep04ArgsSchema,
);
