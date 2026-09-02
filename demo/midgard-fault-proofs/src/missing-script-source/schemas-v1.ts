import {
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  ForcedInclusionTxV1Schema,
  FrontierPeakV1Schema,
  HeaderV1Schema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonV1Schema,
  rootMembershipProofSchema,
  ValidationMachineStateV1Schema,
  ValidationTraceDescriptorV1Schema,
  ValidationTraceProofV1Schema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ExecutionSourceVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const ExecutionSourceBoundV1Schema = Data.Object({
  subject: ExecutionSourceVerdictSubjectV1Schema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  witness_set_hash: Data.Bytes(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
});
export const AuthenticatedExecutionSourceV1Schema = Data.Object({
  bound: ExecutionSourceBoundV1Schema,
  prior_ledger_root: Data.Bytes(),
  required_script_hash: Data.Bytes(),
  source_count: Data.Integer(),
  scan_limit: Data.Integer(),
  source_peaks: Data.Array(FrontierPeakV1Schema),
});
export const AuthenticatedExecutionSourceTraceV1Schema = Data.Object({
  bound: ExecutionSourceBoundV1Schema,
  machine_state: ValidationMachineStateV1Schema,
});
export const AuthenticatedTransactionSourcesV1Schema = Data.Object({
  purpose: AuthenticatedExecutionSourceV1Schema,
  transaction_source_count: Data.Integer(),
});
export const AuthenticatedResolvedSourcesV1Schema = Data.Object({
  purpose: AuthenticatedExecutionSourceV1Schema,
  transaction_source_count: Data.Integer(),
  resolved_reference_source_count: Data.Integer(),
  source_identity_hash: Data.Bytes(),
});
export const SourceDescriptorV1Schema = Data.Object({
  source_index: Data.Integer(),
  location_kind: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const ExecutionSourceScanStateV1Schema = Data.Object({
  authenticated: AuthenticatedResolvedSourcesV1Schema,
  cursor: Data.Integer(),
  found: Data.Boolean(),
  next_expected_script_hash: Data.Bytes(),
  checkpoint_hash: Data.Bytes(),
});

export const ExecutionSourceStep01SourceV1Schema = Data.Enum([
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
export const ExecutionSourceStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: ExecutionSourceStep01SourceV1Schema,
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
  );
export const ExecutionSourceStep02DatumV1Schema = faultProofStepDatumSchema(
  ExecutionSourceBoundV1Schema,
);

const FrontierSchema = Data.Array(FrontierPeakV1Schema);
const ScriptSourcesReceiveControlV1Schema = Data.Object({
  source_count: Data.Integer(),
  source_peaks: FrontierSchema,
  receive_count: Data.Integer(),
  previous_hash: Data.Bytes(),
  candidate_hash: Data.Bytes(),
  descriptor_peaks: FrontierSchema,
});
const ScriptSourcesObserverControlV1Schema = Data.Object({
  total_count: Data.Integer(),
  seen: Data.Integer(),
  previous_hash: Data.Bytes(),
});
const ScriptSourcesDiscoveryControlV1Schema = Data.Object({
  purpose_cursor: Data.Integer(),
  source_cursor: Data.Integer(),
  redeemer_cursor: Data.Integer(),
  current_purpose_kind: Data.Integer(),
  current_purpose_index: Data.Integer(),
  current_script_hash: Data.Bytes(),
  current_subject: Data.Bytes(),
  matched_source_index: Data.Integer(),
  matched_language_tag: Data.Integer(),
  matched_source_leaf: Data.Bytes(),
  used_inline_bitmap: Data.Integer(),
  used_redeemer_bitmap: Data.Integer(),
  redeemer_item_control_hash: Data.Bytes(),
  execution_count: Data.Integer(),
  execution_peaks: FrontierSchema,
});
const ScriptSourcesMintFoldControlV1Schema = Data.Object({
  policy_count: Data.Integer(),
  policy_cursor: Data.Integer(),
  previous_policy: Data.Bytes(),
  active_policy: Data.Bytes(),
  item_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  item_cursor: Data.Integer(),
  assets_remaining: Data.Integer(),
  policy_asset_cursor: Data.Integer(),
  previous_asset: Data.Bytes(),
  asset_count: Data.Integer(),
  asset_peaks: FrontierSchema,
});
export const ScriptSourcesControlV1Schema = Data.Object({
  compact_cbor: Data.Bytes(),
  witness_set_compact_cbor: Data.Bytes(),
  field_preimage_lengths_cbor: Data.Bytes(),
  context_cbor: Data.Bytes(),
  resolved_input_count: Data.Integer(),
  resolved_inputs_accumulator: Data.Bytes(),
  signer_count: Data.Integer(),
  signer_frontier_commitment: Data.Bytes(),
  resolved_item_peaks: FrontierSchema,
  stage: Data.Integer(),
  source_count: Data.Integer(),
  source_peaks: FrontierSchema,
  redeemer_count: Data.Integer(),
  redeemer_peaks: FrontierSchema,
  replay_cursor: Data.Integer(),
  replay_accumulator: Data.Bytes(),
  replay_remaining_schedule_hash: Data.Bytes(),
  spend_index: Data.Integer(),
  purpose_count: Data.Integer(),
  purpose_peaks: FrontierSchema,
  output_cursor: Data.Integer(),
  output_count: Data.Integer(),
  output_peaks: FrontierSchema,
  output_total_count: Data.Integer(),
  receive_scan: ScriptSourcesReceiveControlV1Schema,
  source_total_count: Data.Integer(),
  redeemer_total_count: Data.Integer(),
  observer_scan: ScriptSourcesObserverControlV1Schema,
  discovery: ScriptSourcesDiscoveryControlV1Schema,
  output_proof: Data.Nullable(Data.Any()),
  pending_source_cbor: Data.Bytes(),
  mint_fold: ScriptSourcesMintFoldControlV1Schema,
  resolution_schedule_hash: Data.Bytes(),
});
export const NativeScriptsControlV1Schema = Data.Object({
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
export const ExecutionSourceStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      trace_membership: rootMembershipProofSchema(
        EventKeySchema,
        ValidationTraceDescriptorV1Schema,
      ),
      machine_state: ValidationMachineStateV1Schema,
      trace_proof: ValidationTraceProofV1Schema,
    }),
  );
export const ExecutionSourceStep03DatumV1Schema = faultProofStepDatumSchema(
  AuthenticatedExecutionSourceTraceV1Schema,
);
export const ExecutionSourceStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      control: Data.Any(),
      absolute_purpose_index: Data.Integer(),
      required_script_hash: Data.Bytes(),
      purpose_subject: Data.Bytes(),
      purpose_siblings: Data.Array(Data.Bytes()),
      transaction_source_count: Data.Integer(),
    }),
  );
export const ExecutionSourceStep04DatumV1Schema = faultProofStepDatumSchema(
  AuthenticatedTransactionSourcesV1Schema,
);
export const ExecutionSourceStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      resolved_reference_source_count: Data.Integer(),
    }),
  );
export const ExecutionSourceStep05DatumV1Schema = faultProofStepDatumSchema(
  ExecutionSourceScanStateV1Schema,
);
export const ExecutionSourceStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      sources: Data.Array(SourceDescriptorV1Schema),
      item_budget: Data.Integer(),
    }),
  );
export const ExecutionSourceStep06DatumV1Schema =
  ExecutionSourceStep05DatumV1Schema;
export const ExecutionSourceStep06RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
