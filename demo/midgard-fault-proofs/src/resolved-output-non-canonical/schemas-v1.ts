import {
  BoundedItemChunkProofV1Schema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningV1Schema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  MembershipCarriageSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonV1Schema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ResolvedOutputVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const ResolvedOutputBoundInputV1Schema = Data.Object({
  subject: ResolvedOutputVerdictSubjectV1Schema,
  source_kind: Data.Integer(),
  input_index: Data.Integer(),
  prior_root: Data.Bytes(),
});
export const ResolvedOutputAuthenticatedOutRefV1Schema = Data.Object({
  subject: ResolvedOutputVerdictSubjectV1Schema,
  prior_root: Data.Bytes(),
  out_ref: OutputReferenceSchema,
});
export const ResolvedOutputCanonicalVerdictV1Schema = Data.Object({
  subject: ResolvedOutputVerdictSubjectV1Schema,
  output_is_non_canonical: Data.Boolean(),
});
const PeakSchema = Data.Object({ height: Data.Integer(), hash: Data.Bytes() });
export const ResolvedOutputScanControlV1Schema = Data.Object({
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
export const ResolvedOutputReconstructionV1Schema = Data.Object({
  subject: ResolvedOutputVerdictSubjectV1Schema,
  descriptor_cbor: Data.Bytes(),
  control: ResolvedOutputScanControlV1Schema,
});
export const ResolvedOutputStep01SourceV1Schema = Data.Enum([
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
export const ResolvedOutputStep01ArgsV1Schema = Data.Object({
  source: ResolvedOutputStep01SourceV1Schema,
  source_kind: Data.Integer(),
  input_index: Data.Integer(),
});
export const ResolvedOutputStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(ResolvedOutputStep01ArgsV1Schema);
export const ResolvedOutputStep02DatumV1Schema = faultProofStepDatumSchema(
  ResolvedOutputBoundInputV1Schema,
);
export const ResolvedOutputStep02ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningV1Schema,
});
export const ResolvedOutputStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(ResolvedOutputStep02ArgsV1Schema);
export const ResolvedOutputStep03DatumV1Schema = faultProofStepDatumSchema(
  ResolvedOutputAuthenticatedOutRefV1Schema,
);
export const ResolvedOutputStep03ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  descriptor_cbor: Data.Bytes(),
  membership: MembershipCarriageSchema,
});
export const ResolvedOutputStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(ResolvedOutputStep03ArgsV1Schema);
export const ResolvedOutputStep04DatumV1Schema = faultProofStepDatumSchema(
  ResolvedOutputReconstructionV1Schema,
);
export const ResolvedOutputStep04ActionV1Schema = Data.Enum([
  Data.Object({
    Advance: Data.Object({
      chunk_proof: BoundedItemChunkProofV1Schema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
    }),
  }),
  Data.Literal("FinalizeCanonical"),
]);
export const ResolvedOutputStep04ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  action: ResolvedOutputStep04ActionV1Schema,
});
export const ResolvedOutputStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(ResolvedOutputStep04ArgsV1Schema);
export const ResolvedOutputStep05DatumV1Schema = faultProofStepDatumSchema(
  ResolvedOutputCanonicalVerdictV1Schema,
);
export const ResolvedOutputStep05ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const ResolvedOutputStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(ResolvedOutputStep05ArgsV1Schema);
