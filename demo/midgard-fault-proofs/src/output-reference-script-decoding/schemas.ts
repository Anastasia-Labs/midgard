import {
  BoundedItemChunkProofSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  NativeScriptFrameSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const OutputReferenceVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const OutputReferenceBoundOutputSchema = Data.Object({
  subject: OutputReferenceVerdictSubjectSchema,
  output_index: Data.Integer(),
  accused_class: Data.Integer(),
});
const PeakSchema = Data.Object({ height: Data.Integer(), hash: Data.Bytes() });
export const OutputReferenceOutputControlSchema = Data.Object({
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
export const OutputReferenceDescriptorStateSchema = Data.Object({
  bound: OutputReferenceBoundOutputSchema,
  item_length: Data.Integer(),
  item_hash: Data.Bytes(),
  chunk_hashes: Data.Array(Data.Bytes()),
  control: OutputReferenceOutputControlSchema,
  outcome: Data.Integer(),
});
export const OutputReferenceScanStateSchema = Data.Object({
  bound: OutputReferenceBoundOutputSchema,
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  control_cbor: Data.Bytes(),
  next_expected_script_hash: Data.Bytes(),
  checkpoint_hash: Data.Bytes(),
  result_class: Data.Integer(),
});
export const OutputReferenceStep01SourceSchema = Data.Enum([
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
export const OutputReferenceStep01RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    source: OutputReferenceStep01SourceSchema,
    output_index: Data.Integer(),
  }),
);
export const OutputReferenceStep02DatumSchema = faultProofStepDatumSchema(
  OutputReferenceBoundOutputSchema,
);
export const OutputReferenceStep02RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    opening: FieldOpeningSchema,
  }),
);
export const OutputReferenceStep03DatumSchema = faultProofStepDatumSchema(
  OutputReferenceDescriptorStateSchema,
);
export const OutputReferenceStep03RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    window: Data.Bytes(),
  }),
);
export const OutputReferenceStep04DatumSchema = faultProofStepDatumSchema(
  OutputReferenceDescriptorStateSchema,
);
export const OutputReferenceStep04RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    opening: FieldOpeningSchema,
  }),
);
export const OutputReferenceStep05DatumSchema = faultProofStepDatumSchema(
  OutputReferenceScanStateSchema,
);
export const OutputReferenceStep05RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    control_cbor: Data.Bytes(),
    chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
    next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
    frames: Data.Array(NativeScriptFrameSchema),
    step_budget: Data.Integer(),
  }),
);
export const OutputReferenceStep06DatumSchema = faultProofStepDatumSchema(
  OutputReferenceScanStateSchema,
);
export const OutputReferenceStep06RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
