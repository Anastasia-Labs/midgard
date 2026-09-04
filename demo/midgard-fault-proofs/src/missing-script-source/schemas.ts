import {
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  ForcedInclusionTxV1Schema,
  FrontierPeakSchema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonSchema,
  rootMembershipProofSchema,
  ValidationMachineStateSchema,
  ValidationTraceDescriptorSchema,
  ValidationTraceProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ExecutionSourceVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const ExecutionSourceBoundSchema = Data.Object({
  subject: ExecutionSourceVerdictSubjectSchema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  witness_set_hash: Data.Bytes(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
});
export const AuthenticatedExecutionSourceSchema = Data.Object({
  bound: ExecutionSourceBoundSchema,
  prior_ledger_root: Data.Bytes(),
  required_script_hash: Data.Bytes(),
  source_count: Data.Integer(),
  scan_limit: Data.Integer(),
  source_peaks: Data.Array(FrontierPeakSchema),
});
export const AuthenticatedExecutionSourceTraceSchema = Data.Object({
  bound: ExecutionSourceBoundSchema,
  machine_state: ValidationMachineStateSchema,
});
export const AuthenticatedTransactionSourcesSchema = Data.Object({
  purpose: AuthenticatedExecutionSourceSchema,
  transaction_source_count: Data.Integer(),
});
export const AuthenticatedResolvedSourcesSchema = Data.Object({
  purpose: AuthenticatedExecutionSourceSchema,
  transaction_source_count: Data.Integer(),
  resolved_reference_source_count: Data.Integer(),
  source_identity_hash: Data.Bytes(),
});
export const SourceDescriptorSchema = Data.Object({
  source_index: Data.Integer(),
  location_kind: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const ExecutionSourceScanStateSchema = Data.Object({
  authenticated: AuthenticatedResolvedSourcesSchema,
  cursor: Data.Integer(),
  found: Data.Boolean(),
  next_expected_script_hash: Data.Bytes(),
  checkpoint_hash: Data.Bytes(),
});

export const ExecutionSourceStep01SourceSchema = Data.Enum([
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
export const ExecutionSourceStep01RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    source: ExecutionSourceStep01SourceSchema,
    purpose_kind: Data.Integer(),
    purpose_index: Data.Integer(),
  }),
);
export const ExecutionSourceStep02DatumSchema = faultProofStepDatumSchema(
  ExecutionSourceBoundSchema,
);

const FrontierSchema = Data.Array(FrontierPeakSchema);
const ScriptSourcesReceiveControlSchema = Data.Object({
  source_count: Data.Integer(),
  source_peaks: FrontierSchema,
  receive_count: Data.Integer(),
  previous_hash: Data.Bytes(),
  candidate_hash: Data.Bytes(),
  descriptor_peaks: FrontierSchema,
});
const ScriptSourcesObserverControlSchema = Data.Object({
  total_count: Data.Integer(),
  seen: Data.Integer(),
  previous_hash: Data.Bytes(),
});
const ScriptSourcesDiscoveryControlSchema = Data.Object({
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
const ScriptSourcesMintFoldControlSchema = Data.Object({
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
export const ScriptSourcesControlSchema = Data.Object({
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
  receive_scan: ScriptSourcesReceiveControlSchema,
  source_total_count: Data.Integer(),
  redeemer_total_count: Data.Integer(),
  observer_scan: ScriptSourcesObserverControlSchema,
  discovery: ScriptSourcesDiscoveryControlSchema,
  output_proof: Data.Nullable(Data.Any()),
  pending_source_cbor: Data.Bytes(),
  mint_fold: ScriptSourcesMintFoldControlSchema,
  resolution_schedule_hash: Data.Bytes(),
});
export const NativeScriptsControlSchema = Data.Object({
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
export const ExecutionSourceStep02RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    trace_membership: rootMembershipProofSchema(
      EventKeySchema,
      ValidationTraceDescriptorSchema,
    ),
    machine_state: ValidationMachineStateSchema,
    trace_proof: ValidationTraceProofSchema,
  }),
);
export const ExecutionSourceStep03DatumSchema = faultProofStepDatumSchema(
  AuthenticatedExecutionSourceTraceSchema,
);
export const ExecutionSourceStep03RedeemerSchema = faultProofStepRedeemerSchema(
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
export const ExecutionSourceStep04DatumSchema = faultProofStepDatumSchema(
  AuthenticatedTransactionSourcesSchema,
);
export const ExecutionSourceStep04RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    resolved_reference_source_count: Data.Integer(),
  }),
);
export const ExecutionSourceStep05DatumSchema = faultProofStepDatumSchema(
  ExecutionSourceScanStateSchema,
);
export const ExecutionSourceStep05RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    sources: Data.Array(SourceDescriptorSchema),
    item_budget: Data.Integer(),
  }),
);
export const ExecutionSourceStep06DatumSchema =
  ExecutionSourceStep05DatumSchema;
export const ExecutionSourceStep06RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
