import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const RedeemerCanonicityVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

export const RedeemerCanonicityBoundSchema = Data.Object({
  subject: RedeemerCanonicityVerdictSubjectSchema,
  witness_set_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  redeemer_index: Data.Integer(),
});

export const RedeemerCanonicityStep02StateSchema =
  RedeemerCanonicityBoundSchema;

export const RedeemerCanonicityTerminalSchema = Data.Object({
  bound: RedeemerCanonicityBoundSchema,
  canonical: Data.Boolean(),
});

export const RedeemerCanonicityStep01SourceSchema = Data.Enum([
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
export const RedeemerCanonicityStep01ArgsSchema = Data.Object({
  source: RedeemerCanonicityStep01SourceSchema,
  redeemer_index: Data.Integer(),
});
export const RedeemerCanonicityStep01RedeemerSchema =
  faultProofStepRedeemerSchema(RedeemerCanonicityStep01ArgsSchema);
export const RedeemerCanonicityStep02DatumSchema = faultProofStepDatumSchema(
  RedeemerCanonicityStep02StateSchema,
);
export const RedeemerCanonicityStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export const RedeemerCanonicityStep02RedeemerSchema =
  faultProofStepRedeemerSchema(RedeemerCanonicityStep02ArgsSchema);
export const RedeemerCanonicityStep03DatumSchema = faultProofStepDatumSchema(
  RedeemerCanonicityTerminalSchema,
);
export const RedeemerCanonicityStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const RedeemerCanonicityStep03RedeemerSchema =
  faultProofStepRedeemerSchema(RedeemerCanonicityStep03ArgsSchema);
