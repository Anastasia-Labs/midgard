import {
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  ForcedInclusionTxV1Schema,
  FrontierPeakSchema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  ProofSchema,
  RejectionReasonSchema,
  rootMembershipProofSchema,
  ValidationMachineStateSchema,
  ValidationTraceDescriptorSchema,
  ValidationTraceProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

const FrontierSchema = Data.Array(FrontierPeakSchema);
const NativeScriptsControlSchema = Data.Object({
  compact_cbor: Data.Bytes(),
  witness_set_compact_cbor: Data.Bytes(),
  field_preimage_lengths_cbor: Data.Bytes(),
  context_cbor: Data.Bytes(),
  resolved_input_count: Data.Integer(),
  resolved_inputs_accumulator: Data.Bytes(),
  spend_input_count: Data.Integer(),
  resolved_item_peaks: FrontierSchema,
  signer_count: Data.Integer(),
  signer_frontier_commitment: Data.Bytes(),
  source_count: Data.Integer(),
  source_peaks: FrontierSchema,
  redeemer_count: Data.Integer(),
  redeemer_peaks: FrontierSchema,
  purpose_count: Data.Integer(),
  purpose_peaks: FrontierSchema,
  output_count: Data.Integer(),
  output_peaks: FrontierSchema,
  output_descriptor_peaks: FrontierSchema,
  mint_count: Data.Integer(),
  mint_peaks: FrontierSchema,
  execution_count: Data.Integer(),
  execution_peaks: FrontierSchema,
  execution_cursor: Data.Integer(),
  language_bitmap: Data.Integer(),
  resolution_schedule_hash: Data.Bytes(),
});

export const DistinctAssetVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const DistinctAssetCoordinateSchema = Data.Object({
  fold: Data.Integer(),
  primary_index: Data.Integer(),
  asset_index: Data.Integer(),
});
export const DistinctAssetBoundSchema = Data.Object({
  subject: DistinctAssetVerdictSubjectSchema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  coordinate: DistinctAssetCoordinateSchema,
});
export const DistinctAssetValueAndMintControlSchema = Data.Object({
  native_control: NativeScriptsControlSchema,
  stage: Data.Integer(),
  replay_schedule_hash: Data.Bytes(),
  replay_cursor: Data.Integer(),
  replay_asset_cursor: Data.Integer(),
  replay_value_hash: Data.Bytes(),
  replay_accumulator: Data.Bytes(),
  replay_remaining_schedule_hash: Data.Bytes(),
  output_cursor: Data.Integer(),
  output_asset_cursor: Data.Integer(),
  mint_cursor: Data.Integer(),
  value_accumulator: Data.Object({
    lovelace_delta: Data.Integer(),
    asset_root: Data.Bytes(),
    seen_asset_count: Data.Integer(),
    nonzero_asset_count: Data.Integer(),
  }),
});
export const DistinctAssetFoldStateSchema = Data.Object({
  bound: DistinctAssetBoundSchema,
  control: Data.Nullable(DistinctAssetValueAndMintControlSchema),
  stage: Data.Integer(),
  decisive_fault_holds: Data.Nullable(Data.Boolean()),
});
export const DistinctAssetStep02DatumSchema = faultProofStepDatumSchema(
  DistinctAssetBoundSchema,
);
export const DistinctAssetStep01SourceSchema = Data.Enum([
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
export const DistinctAssetStep01RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    source: DistinctAssetStep01SourceSchema,
    coordinate: DistinctAssetCoordinateSchema,
  }),
);
export const DistinctAssetStep02RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    trace_membership: rootMembershipProofSchema(
      EventKeySchema,
      ValidationTraceDescriptorSchema,
    ),
    pre: ValidationMachineStateSchema,
    trace_proof: ValidationTraceProofSchema,
    control: DistinctAssetValueAndMintControlSchema,
  }),
);
export const DistinctAssetStep03DatumSchema = faultProofStepDatumSchema(
  DistinctAssetFoldStateSchema,
);
export const DistinctAssetStep04DatumSchema = DistinctAssetStep03DatumSchema;
export const DistinctAssetStep05DatumSchema = DistinctAssetStep03DatumSchema;
export const DistinctAssetStep06DatumSchema = DistinctAssetStep03DatumSchema;
const ValueAssetMutationWitnessSchema = Data.Object({
  delta_was_present: Data.Boolean(),
  old_delta: Data.Integer(),
  delta_proof: ProofSchema,
});
const CommonLayoutSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
});
export const DistinctAssetStep03RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Enum([
    Data.Object({ Skip: CommonLayoutSchema }),
    Data.Object({
      Authenticate: Data.Object({
        input_index: Data.Integer(),
        output_index: Data.Integer(),
        evidence: Data.Object({
          source_kind: Data.Integer(),
          key: Data.Bytes(),
          next_schedule_hash: Data.Bytes(),
          descriptor_cbor: Data.Bytes(),
          asset_index: Data.Integer(),
          policy_id: Data.Bytes(),
          asset_name: Data.Bytes(),
          quantity: Data.Integer(),
          asset_peaks: FrontierSchema,
          asset_siblings: Data.Array(Data.Bytes()),
          mutation: ValueAssetMutationWitnessSchema,
        }),
      }),
    }),
  ]),
);
export const DistinctAssetStep04RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Enum([
    Data.Object({ Skip: CommonLayoutSchema }),
    Data.Object({
      Authenticate: Data.Object({
        input_index: Data.Integer(),
        output_index: Data.Integer(),
        evidence: Data.Object({
          output_index: Data.Integer(),
          descriptor_cbor: Data.Bytes(),
          asset_index: Data.Integer(),
          policy_id: Data.Bytes(),
          asset_name: Data.Bytes(),
          quantity: Data.Integer(),
          asset_peaks: FrontierSchema,
          asset_siblings: Data.Array(Data.Bytes()),
          mutation: ValueAssetMutationWitnessSchema,
        }),
      }),
    }),
  ]),
);
export const DistinctAssetStep05RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Enum([
    Data.Object({ Skip: CommonLayoutSchema }),
    Data.Object({
      Authenticate: Data.Object({
        input_index: Data.Integer(),
        output_index: Data.Integer(),
        evidence: Data.Object({
          mint_index: Data.Integer(),
          policy_id: Data.Bytes(),
          asset_name: Data.Bytes(),
          quantity: Data.Integer(),
          siblings: Data.Array(Data.Bytes()),
          mutation: ValueAssetMutationWitnessSchema,
        }),
      }),
    }),
  ]),
);
export const DistinctAssetStep06RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
export const DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS = Object.freeze([
  DistinctAssetStep02DatumSchema,
  DistinctAssetStep03DatumSchema,
  DistinctAssetStep04DatumSchema,
  DistinctAssetStep05DatumSchema,
  DistinctAssetStep06DatumSchema,
  DistinctAssetStep06DatumSchema,
] as const);
