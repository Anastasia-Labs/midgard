import {
  BoundedItemChunkProofSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedInclusionTxSchema,
  HeaderSchema,
  MembershipCarriageSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ResolvedOutputVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const ResolvedOutputBoundInputSchema = Data.Object({
  subject: ResolvedOutputVerdictSubjectSchema,
  source_kind: Data.Integer(),
  input_index: Data.Integer(),
  prior_root: Data.Bytes(),
});
export const ResolvedOutputAuthenticatedOutRefSchema = Data.Object({
  subject: ResolvedOutputVerdictSubjectSchema,
  prior_root: Data.Bytes(),
  out_ref: OutputReferenceSchema,
});
export const ResolvedOutputCanonicalVerdictSchema = Data.Object({
  subject: ResolvedOutputVerdictSubjectSchema,
  output_is_non_canonical: Data.Boolean(),
});
const PeakSchema = Data.Object({ height: Data.Integer(), hash: Data.Bytes() });
export const ResolvedOutputScanControlSchema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  cursor: Data.Integer(),
  map_entry_count: Data.Integer(),
  optional_field_count: Data.Integer(),
  address: Data.Bytes(),
  lovelace: Data.Integer(),
  cardano_value_size: Data.Integer(),
  policy_remaining: Data.Integer(),
  asset_remaining: Data.Integer(),
  policy_asset_cursor: Data.Integer(),
  previous_policy: Data.Bytes(),
  current_policy: Data.Bytes(),
  previous_asset_name: Data.Bytes(),
  asset_count: Data.Integer(),
  asset_peaks: Data.Array(PeakSchema),
  datum_offset: Data.Integer(),
  datum_length: Data.Integer(),
  payload_remaining: Data.Integer(),
  reference_script_language: Data.Integer(),
  reference_script_item_offset: Data.Integer(),
  reference_script_offset: Data.Integer(),
  reference_script_length: Data.Integer(),
});
export const ResolvedOutputReconstructionSchema = Data.Object({
  subject: ResolvedOutputVerdictSubjectSchema,
  descriptor_cbor: Data.Bytes(),
  control: ResolvedOutputScanControlSchema,
});
export const ResolvedOutputStep01SourceSchema = Data.Enum([
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
export const ResolvedOutputStep01ArgsSchema = Data.Object({
  source: ResolvedOutputStep01SourceSchema,
  source_kind: Data.Integer(),
  input_index: Data.Integer(),
});
export const ResolvedOutputStep01RedeemerSchema = faultProofStepRedeemerSchema(
  ResolvedOutputStep01ArgsSchema,
);
export const ResolvedOutputStep02DatumSchema = faultProofStepDatumSchema(
  ResolvedOutputBoundInputSchema,
);
export const ResolvedOutputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export const ResolvedOutputStep02RedeemerSchema = faultProofStepRedeemerSchema(
  ResolvedOutputStep02ArgsSchema,
);
export const ResolvedOutputStep03DatumSchema = faultProofStepDatumSchema(
  ResolvedOutputAuthenticatedOutRefSchema,
);
export const ResolvedOutputStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  descriptor_cbor: Data.Bytes(),
  membership: MembershipCarriageSchema,
});
export const ResolvedOutputStep03RedeemerSchema = faultProofStepRedeemerSchema(
  ResolvedOutputStep03ArgsSchema,
);
export const ResolvedOutputStep04DatumSchema = faultProofStepDatumSchema(
  ResolvedOutputReconstructionSchema,
);
export const ResolvedOutputStep04ActionSchema = Data.Enum([
  Data.Object({
    Advance: Data.Object({
      chunk_proof: BoundedItemChunkProofSchema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
    }),
  }),
  Data.Literal("FinalizeCanonical"),
]);
export const ResolvedOutputStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  action: ResolvedOutputStep04ActionSchema,
});
export const ResolvedOutputStep04RedeemerSchema = faultProofStepRedeemerSchema(
  ResolvedOutputStep04ArgsSchema,
);
export const ResolvedOutputStep05DatumSchema = faultProofStepDatumSchema(
  ResolvedOutputCanonicalVerdictSchema,
);
export const ResolvedOutputStep05ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const ResolvedOutputStep05RedeemerSchema = faultProofStepRedeemerSchema(
  ResolvedOutputStep05ArgsSchema,
);
