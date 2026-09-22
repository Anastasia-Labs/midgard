import {
  BoundedItemChunkProofSchema,
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  ForcedInclusionTxV1Schema,
  FrontierPeakSchema,
  HeaderSchema,
  NativeScriptFrameSchema,
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
  execution_index: Data.Integer(),
  accused_class: Data.Integer(),
});
export const AuthenticatedExecutionSourceSchema = Data.Object({
  bound: ExecutionSourceBoundSchema,
  prior_ledger_root: Data.Bytes(),
  source_index: Data.Integer(),
  origin_kind: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
});
export const ExecutionSourceScanStateSchema = Data.Object({
  source: AuthenticatedExecutionSourceSchema,
  control_cbor: Data.Bytes(),
  next_expected_script_hash: Data.Bytes(),
  checkpoint_hash: Data.Bytes(),
  result_class: Data.Integer(),
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
    execution_index: Data.Integer(),
  }),
);
export const ExecutionSourceStep02DatumSchema = faultProofStepDatumSchema(
  ExecutionSourceBoundSchema,
);

const FrontierSchema = Data.Array(FrontierPeakSchema);
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
    control: NativeScriptsControlSchema,
    purpose_kind: Data.Integer(),
    purpose_index: Data.Integer(),
    script_hash: Data.Bytes(),
    purpose_subject: Data.Bytes(),
    purpose_siblings: Data.Array(Data.Bytes()),
    source_index: Data.Integer(),
    origin_kind: Data.Integer(),
    source_key: Data.Bytes(),
    language_tag: Data.Integer(),
    total_length: Data.Integer(),
    item_commitment: Data.Bytes(),
    source_siblings: Data.Array(Data.Bytes()),
    redeemer_leaf: Data.Bytes(),
    execution_siblings: Data.Array(Data.Bytes()),
  }),
);
export const ExecutionSourceStep03DatumSchema = faultProofStepDatumSchema(
  AuthenticatedExecutionSourceSchema,
);
export const ExecutionSourceStep03RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    first_chunk: BoundedItemChunkProofSchema,
  }),
);
export const ExecutionSourceStep04DatumSchema = faultProofStepDatumSchema(
  ExecutionSourceScanStateSchema,
);
export const ExecutionSourceStep04RedeemerSchema = faultProofStepRedeemerSchema(
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
export const ExecutionSourceStep05DatumSchema =
  ExecutionSourceStep04DatumSchema;
export const ExecutionSourceStep05RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
