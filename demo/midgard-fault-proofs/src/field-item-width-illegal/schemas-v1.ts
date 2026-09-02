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
  FieldItemWidthAuthenticatedWidthV1Schema,
  FieldItemWidthBoundCoordinateV1Schema,
} from "./field-item-width-illegal-v1.js";

export const FieldItemWidthStep01SourceV1Schema = Data.Enum([
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
export const FieldItemWidthStep01ArgsV1Schema = Data.Object({
  source: FieldItemWidthStep01SourceV1Schema,
  field_index: Data.Integer(),
  item_index: Data.Integer(),
});
export const FieldItemWidthStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(FieldItemWidthStep01ArgsV1Schema);

export const FieldItemWidthStep02DatumV1Schema = faultProofStepDatumSchema(
  FieldItemWidthBoundCoordinateV1Schema,
);
export const FieldItemWidthStep02ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningV1Schema,
});
export const FieldItemWidthStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(FieldItemWidthStep02ArgsV1Schema);
export const FieldItemWidthStep03DatumV1Schema = faultProofStepDatumSchema(
  FieldItemWidthAuthenticatedWidthV1Schema,
);
export const FieldItemWidthStep03ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const FieldItemWidthStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(FieldItemWidthStep03ArgsV1Schema);
