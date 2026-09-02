import {
  BoundedItemChunkProofV1Schema,
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  ForcedInclusionTxV1Schema,
  FrontierPeakV1Schema,
  HeaderV1Schema,
  NativeScriptFrameV1Schema,
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
  execution_index: Data.Integer(),
  accused_class: Data.Integer(),
});
export const AuthenticatedExecutionSourceV1Schema = Data.Object({
  bound: ExecutionSourceBoundV1Schema,
  prior_ledger_root: Data.Bytes(),
  source_index: Data.Integer(),
  origin_kind: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
});
export const ExecutionSourceScanStateV1Schema = Data.Object({
  source: AuthenticatedExecutionSourceV1Schema,
  control_cbor: Data.Bytes(),
  next_expected_script_hash: Data.Bytes(),
  checkpoint_hash: Data.Bytes(),
  result_class: Data.Integer(),
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
      execution_index: Data.Integer(),
    }),
  );
export const ExecutionSourceStep02DatumV1Schema = faultProofStepDatumSchema(
  ExecutionSourceBoundV1Schema,
);

const FrontierSchema = Data.Array(FrontierPeakV1Schema);
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
      control: NativeScriptsControlV1Schema,
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
export const ExecutionSourceStep03DatumV1Schema = faultProofStepDatumSchema(
  AuthenticatedExecutionSourceV1Schema,
);
export const ExecutionSourceStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      first_chunk: BoundedItemChunkProofV1Schema,
    }),
  );
export const ExecutionSourceStep04DatumV1Schema = faultProofStepDatumSchema(
  ExecutionSourceScanStateV1Schema,
);
export const ExecutionSourceStep04RedeemerV1Schema =
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
export const ExecutionSourceStep05DatumV1Schema =
  ExecutionSourceStep04DatumV1Schema;
export const ExecutionSourceStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
