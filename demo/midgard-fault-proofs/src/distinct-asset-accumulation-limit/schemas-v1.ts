import {
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  ForcedInclusionTxV1Schema,
  FrontierPeakV1Schema,
  HeaderV1Schema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  ProofSchema,
  RejectionReasonV1Schema,
  rootMembershipProofSchema,
  ValidationMachineStateV1Schema,
  ValidationTraceDescriptorV1Schema,
  ValidationTraceProofV1Schema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

const FrontierSchema = Data.Array(FrontierPeakV1Schema);
const NativeScriptsControlV1Schema = Data.Object({
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

export const DistinctAssetVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const DistinctAssetCoordinateV1Schema = Data.Object({
  fold: Data.Integer(),
  primary_index: Data.Integer(),
  asset_index: Data.Integer(),
});
export const DistinctAssetBoundV1Schema = Data.Object({
  subject: DistinctAssetVerdictSubjectV1Schema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  coordinate: DistinctAssetCoordinateV1Schema,
});
export const DistinctAssetValueAndMintControlV1Schema = Data.Object({
  native_control: NativeScriptsControlV1Schema,
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
export const DistinctAssetFoldStateV1Schema = Data.Object({
  bound: DistinctAssetBoundV1Schema,
  control: Data.Nullable(DistinctAssetValueAndMintControlV1Schema),
  stage: Data.Integer(),
  decisive_fault_holds: Data.Nullable(Data.Boolean()),
});
export const DistinctAssetStep02DatumV1Schema = faultProofStepDatumSchema(
  DistinctAssetBoundV1Schema,
);
export const DistinctAssetStep01SourceV1Schema = Data.Enum([
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
export const DistinctAssetStep01RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({
    source: DistinctAssetStep01SourceV1Schema,
    coordinate: DistinctAssetCoordinateV1Schema,
  }),
);
export const DistinctAssetStep02RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    trace_membership: rootMembershipProofSchema(
      EventKeySchema,
      ValidationTraceDescriptorV1Schema,
    ),
    pre: ValidationMachineStateV1Schema,
    trace_proof: ValidationTraceProofV1Schema,
    control: DistinctAssetValueAndMintControlV1Schema,
  }),
);
export const DistinctAssetStep03DatumV1Schema = faultProofStepDatumSchema(
  DistinctAssetFoldStateV1Schema,
);
export const DistinctAssetStep04DatumV1Schema =
  DistinctAssetStep03DatumV1Schema;
export const DistinctAssetStep05DatumV1Schema =
  DistinctAssetStep03DatumV1Schema;
export const DistinctAssetStep06DatumV1Schema =
  DistinctAssetStep03DatumV1Schema;
const ValueAssetMutationWitnessV1Schema = Data.Object({
  delta_was_present: Data.Boolean(),
  old_delta: Data.Integer(),
  delta_proof: ProofSchema,
});
const CommonLayoutV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
});
export const DistinctAssetStep03RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Enum([
    Data.Object({ Skip: CommonLayoutV1Schema }),
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
          mutation: ValueAssetMutationWitnessV1Schema,
        }),
      }),
    }),
  ]),
);
export const DistinctAssetStep04RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Enum([
    Data.Object({ Skip: CommonLayoutV1Schema }),
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
          mutation: ValueAssetMutationWitnessV1Schema,
        }),
      }),
    }),
  ]),
);
export const DistinctAssetStep05RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Enum([
    Data.Object({ Skip: CommonLayoutV1Schema }),
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
          mutation: ValueAssetMutationWitnessV1Schema,
        }),
      }),
    }),
  ]),
);
export const DistinctAssetStep06RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
export const DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS_V1 = Object.freeze([
  DistinctAssetStep02DatumV1Schema,
  DistinctAssetStep03DatumV1Schema,
  DistinctAssetStep04DatumV1Schema,
  DistinctAssetStep05DatumV1Schema,
  DistinctAssetStep06DatumV1Schema,
  DistinctAssetStep06DatumV1Schema,
] as const);
