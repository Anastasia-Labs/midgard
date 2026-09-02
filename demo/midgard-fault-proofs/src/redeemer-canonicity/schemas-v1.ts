import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningV1Schema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonV1Schema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const RedeemerCanonicityVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});

export const RedeemerCanonicityBoundV1Schema = Data.Object({
  subject: RedeemerCanonicityVerdictSubjectV1Schema,
  witness_set_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  redeemer_index: Data.Integer(),
});

export const RedeemerCanonicityStep02StateV1Schema =
  RedeemerCanonicityBoundV1Schema;

export const RedeemerCanonicityTerminalV1Schema = Data.Object({
  bound: RedeemerCanonicityBoundV1Schema,
  canonical: Data.Boolean(),
});

export const RedeemerCanonicityStep01SourceV1Schema = Data.Enum([
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
export const RedeemerCanonicityStep01ArgsV1Schema = Data.Object({
  source: RedeemerCanonicityStep01SourceV1Schema,
  redeemer_index: Data.Integer(),
});
export const RedeemerCanonicityStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(RedeemerCanonicityStep01ArgsV1Schema);
export const RedeemerCanonicityStep02DatumV1Schema = faultProofStepDatumSchema(
  RedeemerCanonicityStep02StateV1Schema,
);
export const RedeemerCanonicityStep02ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningV1Schema,
});
export const RedeemerCanonicityStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(RedeemerCanonicityStep02ArgsV1Schema);
export const RedeemerCanonicityStep03DatumV1Schema = faultProofStepDatumSchema(
  RedeemerCanonicityTerminalV1Schema,
);
export const RedeemerCanonicityStep03ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const RedeemerCanonicityStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(RedeemerCanonicityStep03ArgsV1Schema);
