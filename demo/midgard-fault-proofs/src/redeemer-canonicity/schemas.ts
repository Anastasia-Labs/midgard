import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedTransactionSourceMembershipProofSchema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  ProofThreadSubjectSourceSchema,
  StreamFrameSchema,
  VerdictSubjectSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const RedeemerCanonicityVerdictSubjectSchema = VerdictSubjectSchema;

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

/**
 * The step-01 source as retained in the workflow artifact, which keeps the
 * full forced-transaction membership.
 */
export const RedeemerCanonicityStep01SourceArtifactSchema = Data.Enum([
  Data.Object({
    AcceptedSource: Data.Object({ inclusion: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    ForcedSource: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderSchema,
      membership: ForcedTransactionSourceMembershipProofSchema,
    }),
  }),
]);
export const RedeemerCanonicityStep01ArgsSchema = Data.Object({
  source: ProofThreadSubjectSourceSchema,
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
  grammar_ref_input_index: Data.Nullable(Data.Integer()),
});
export const RedeemerCanonicityStep02RedeemerSchema =
  faultProofStepRedeemerSchema(RedeemerCanonicityStep02ArgsSchema);
export const RedeemerCanonicityDataScanStateSchema = Data.Object({
  bound: RedeemerCanonicityBoundSchema,
  data_length: Data.Integer(),
  chunk_hashes: Data.Array(Data.Bytes({ minLength: 32, maxLength: 32 })),
  cursor: Data.Integer(),
  stack_root: Data.Bytes(),
});
export const RedeemerCanonicityStep02DataDatumSchema =
  faultProofStepDatumSchema(RedeemerCanonicityDataScanStateSchema);
export const RedeemerCanonicityStep02DataArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  window: Data.Bytes(),
  revealed: Data.Array(StreamFrameSchema),
  tail_root: Data.Bytes(),
  node_budget: Data.Integer(),
});
export const RedeemerCanonicityStep02DataRedeemerSchema =
  faultProofStepRedeemerSchema(RedeemerCanonicityStep02DataArgsSchema);
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
