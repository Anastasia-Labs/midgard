import {
  BoundedItemChunkProofV1Schema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningV1Schema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  NativeScriptFrameV1Schema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonV1Schema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const OutputReferenceVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const OutputReferenceBoundOutputV1Schema = Data.Object({
  subject: OutputReferenceVerdictSubjectV1Schema,
  output_index: Data.Integer(),
  accused_class: Data.Integer(),
});
const PeakSchema = Data.Object({ height: Data.Integer(), hash: Data.Bytes() });
export const OutputReferenceOutputControlV1Schema = Data.Object({
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
export const OutputReferenceDescriptorStateV1Schema = Data.Object({
  bound: OutputReferenceBoundOutputV1Schema,
  item_length: Data.Integer(),
  item_hash: Data.Bytes(),
  chunk_hashes: Data.Array(Data.Bytes()),
  control: OutputReferenceOutputControlV1Schema,
  outcome: Data.Integer(),
});
export const OutputReferenceScanStateV1Schema = Data.Object({
  bound: OutputReferenceBoundOutputV1Schema,
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  control_cbor: Data.Bytes(),
  next_expected_script_hash: Data.Bytes(),
  checkpoint_hash: Data.Bytes(),
  result_class: Data.Integer(),
});
export const OutputReferenceStep01SourceV1Schema = Data.Enum([
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
export const OutputReferenceStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: OutputReferenceStep01SourceV1Schema,
      output_index: Data.Integer(),
    }),
  );
export const OutputReferenceStep02DatumV1Schema = faultProofStepDatumSchema(
  OutputReferenceBoundOutputV1Schema,
);
export const OutputReferenceStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
    }),
  );
export const OutputReferenceStep03DatumV1Schema = faultProofStepDatumSchema(
  OutputReferenceDescriptorStateV1Schema,
);
export const OutputReferenceStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      window: Data.Bytes(),
    }),
  );
export const OutputReferenceStep04DatumV1Schema = faultProofStepDatumSchema(
  OutputReferenceDescriptorStateV1Schema,
);
export const OutputReferenceStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
    }),
  );
export const OutputReferenceStep05DatumV1Schema = faultProofStepDatumSchema(
  OutputReferenceScanStateV1Schema,
);
export const OutputReferenceStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      control_cbor: Data.Bytes(),
      chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
      frames: Data.Array(NativeScriptFrameV1Schema),
      step_budget: Data.Integer(),
    }),
  );
export const OutputReferenceStep06DatumV1Schema = faultProofStepDatumSchema(
  OutputReferenceScanStateV1Schema,
);
export const OutputReferenceStep06RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
