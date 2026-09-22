import {
  BoundedItemChunkProofSchema,
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

const UnusedRedeemerFrontierSchema = Data.Array(
  Data.Object({ height: Data.Integer(), hash: Data.Bytes() }),
);
const UnusedRedeemerReceiveControlSchema = Data.Object({
  source_count: Data.Integer(),
  source_peaks: Data.Array(FrontierPeakSchema),
  receive_count: Data.Integer(),
  previous_hash: Data.Bytes(),
  candidate_hash: Data.Bytes(),
  descriptor_peaks: Data.Array(FrontierPeakSchema),
});
const UnusedRedeemerObserverControlSchema = Data.Object({
  total_count: Data.Integer(),
  seen: Data.Integer(),
  previous_hash: Data.Bytes(),
});
const UnusedRedeemerDiscoveryControlSchema = Data.Object({
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
  execution_peaks: Data.Array(FrontierPeakSchema),
});
const UnusedRedeemerMintFoldControlSchema = Data.Object({
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
  asset_peaks: Data.Array(FrontierPeakSchema),
});
export const UnusedRedeemerScriptSourcesControlSchema = Data.Object({
  compact_cbor: Data.Bytes(),
  witness_set_compact_cbor: Data.Bytes(),
  field_preimage_lengths_cbor: Data.Bytes(),
  context_cbor: Data.Bytes(),
  resolved_input_count: Data.Integer(),
  resolved_inputs_accumulator: Data.Bytes(),
  signer_count: Data.Integer(),
  signer_frontier_commitment: Data.Bytes(),
  resolved_item_peaks: Data.Array(FrontierPeakSchema),
  stage: Data.Integer(),
  source_count: Data.Integer(),
  source_peaks: Data.Array(FrontierPeakSchema),
  redeemer_count: Data.Integer(),
  redeemer_peaks: Data.Array(FrontierPeakSchema),
  replay_cursor: Data.Integer(),
  replay_accumulator: Data.Bytes(),
  replay_remaining_schedule_hash: Data.Bytes(),
  spend_index: Data.Integer(),
  purpose_count: Data.Integer(),
  purpose_peaks: Data.Array(FrontierPeakSchema),
  output_cursor: Data.Integer(),
  output_count: Data.Integer(),
  output_peaks: Data.Array(FrontierPeakSchema),
  output_total_count: Data.Integer(),
  receive_scan: UnusedRedeemerReceiveControlSchema,
  source_total_count: Data.Integer(),
  redeemer_total_count: Data.Integer(),
  observer_scan: UnusedRedeemerObserverControlSchema,
  discovery: UnusedRedeemerDiscoveryControlSchema,
  output_proof: Data.Nullable(Data.Any()),
  pending_source_cbor: Data.Bytes(),
  mint_fold: UnusedRedeemerMintFoldControlSchema,
  resolution_schedule_hash: Data.Bytes(),
});
export const UnusedRedeemerItemControlSchema = Data.Object({
  version: Data.Integer(),
  mode: Data.Integer(),
  stage: Data.Integer(),
  item_index: Data.Integer(),
  item_count: Data.Integer(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  expected_purpose_tag: Data.Integer(),
  expected_pointer_index: Data.Integer(),
  purpose_tag: Data.Integer(),
  pointer_index: Data.Integer(),
  data_offset: Data.Integer(),
  data_length: Data.Integer(),
  execution_memory: Data.Integer(),
  execution_steps: Data.Integer(),
  traversal: Data.Nullable(Data.Any()),
});
export const UnusedRedeemerSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const UnusedRedeemerBoundSchema = Data.Object({
  subject: UnusedRedeemerSubjectSchema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  redeemer_index: Data.Integer(),
});
export const UnusedRedeemerAuthenticatedDescriptorSchema = Data.Object({
  bound: UnusedRedeemerBoundSchema,
  event_key_hash: Data.Bytes(),
  descriptor: ValidationTraceDescriptorSchema,
});
export const UnusedRedeemerAuthenticatedControlSchema = Data.Object({
  bound: UnusedRedeemerBoundSchema,
  program_counter: Data.Integer(),
  stage: Data.Integer(),
  expected_item_control_hash: Data.Bytes(),
  used_redeemer_bitmap: Data.Integer(),
  current_purpose_kind: Data.Integer(),
  current_purpose_index: Data.Integer(),
  redeemer_count: Data.Integer(),
  purpose_count: Data.Integer(),
  purpose_peaks: UnusedRedeemerFrontierSchema,
  execution_count: Data.Integer(),
  execution_peaks: UnusedRedeemerFrontierSchema,
});
export const UnusedRedeemerAuthenticatedItemHeaderSchema = Data.Object({
  authenticated: UnusedRedeemerAuthenticatedControlSchema,
  item_index: Data.Integer(),
  item_count: Data.Integer(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  purpose_tag: Data.Integer(),
  pointer_index: Data.Integer(),
  data_offset: Data.Integer(),
  data_length: Data.Integer(),
});
export const UnusedRedeemerAuthenticatedSchema = Data.Object({
  bound: UnusedRedeemerBoundSchema,
  purpose_tag: Data.Integer(),
  pointer_index: Data.Integer(),
  item_count: Data.Integer(),
  item_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  redeemer_leaf: Data.Bytes(),
  purpose_count: Data.Integer(),
  purpose_peaks: UnusedRedeemerFrontierSchema,
  execution_count: Data.Integer(),
  execution_peaks: UnusedRedeemerFrontierSchema,
});
export const UnusedRedeemerReverseScanSchema = Data.Object({
  authenticated: UnusedRedeemerAuthenticatedSchema,
  cursor: Data.Integer(),
  used: Data.Boolean(),
  checkpoint_hash: Data.Bytes(),
});
export const UnusedRedeemerDecisionSchema = Data.Object({
  subject: UnusedRedeemerSubjectSchema,
  redeemer_index: Data.Integer(),
  unused: Data.Boolean(),
});
export const UnusedRedeemerStep02DatumSchema = faultProofStepDatumSchema(
  UnusedRedeemerBoundSchema,
);
export const UnusedRedeemerStep02aDatumSchema = faultProofStepDatumSchema(
  UnusedRedeemerAuthenticatedDescriptorSchema,
);
export const UnusedRedeemerStep02bDatumSchema = faultProofStepDatumSchema(
  UnusedRedeemerAuthenticatedControlSchema,
);
export const UnusedRedeemerStep02cDatumSchema = faultProofStepDatumSchema(
  UnusedRedeemerAuthenticatedItemHeaderSchema,
);
export const UnusedRedeemerStep03DatumSchema = faultProofStepDatumSchema(
  UnusedRedeemerAuthenticatedSchema,
);
export const UnusedRedeemerStep04DatumSchema = faultProofStepDatumSchema(
  UnusedRedeemerReverseScanSchema,
);
export const UnusedRedeemerStep05DatumSchema = UnusedRedeemerStep04DatumSchema;
export const UnusedRedeemerStep06DatumSchema = faultProofStepDatumSchema(
  UnusedRedeemerDecisionSchema,
);
export const UnusedRedeemerStep02RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    trace_membership: rootMembershipProofSchema(
      EventKeySchema,
      ValidationTraceDescriptorSchema,
    ),
  }),
);
export const UnusedRedeemerStep02aRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    machine_state: ValidationMachineStateSchema,
    trace_proof: ValidationTraceProofSchema,
    control: UnusedRedeemerScriptSourcesControlSchema,
  }),
);
export const UnusedRedeemerStep02bRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    item_control: UnusedRedeemerItemControlSchema,
    chunk_proof: BoundedItemChunkProofSchema,
    next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
  }),
);
export const UnusedRedeemerStep02cRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    chunk_proof: BoundedItemChunkProofSchema,
    next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
  }),
);
export const UnusedRedeemerStep03RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({ input_index: Data.Integer(), output_index: Data.Integer() }),
);
export const UnusedRedeemerSelectionOpeningSchema = Data.Object({
  frontier_index: Data.Integer(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
  script_hash: Data.Bytes(),
  purpose_subject: Data.Bytes(),
  purpose_siblings: Data.Array(Data.Bytes()),
  language_tag: Data.Integer(),
  source_leaf: Data.Bytes(),
  redeemer_leaf: Data.Bytes(),
  execution_siblings: Data.Array(Data.Bytes()),
});
export const UnusedRedeemerStep04RedeemerSchema =
  UnusedRedeemerStep03RedeemerSchema;
export const UnusedRedeemerStep05RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    openings: Data.Array(UnusedRedeemerSelectionOpeningSchema),
    item_budget: Data.Integer(),
  }),
);
export const UnusedRedeemerStep06RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);

export const LegacyUnusedRedeemerVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const LegacyUnusedRedeemerBoundWitnessSchema = Data.Object({
  subject: LegacyUnusedRedeemerVerdictSubjectSchema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  redeemer_index: Data.Integer(),
});
const LegacyUnusedRedeemerFrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes(),
});
export const LegacyUnusedRedeemerAuthenticatedWitnessSchema = Data.Object({
  bound: LegacyUnusedRedeemerBoundWitnessSchema,
  prior_ledger_root: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  script_total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  source_count: Data.Integer(),
  source_peaks: Data.Array(LegacyUnusedRedeemerFrontierPeakSchema),
  purpose_count: Data.Integer(),
  purpose_peaks: Data.Array(LegacyUnusedRedeemerFrontierPeakSchema),
});
export const LegacyUnusedRedeemerReverseScanSchema = Data.Object({
  witness: LegacyUnusedRedeemerAuthenticatedWitnessSchema,
  alternate_cursor: Data.Integer(),
  purpose_cursor: Data.Integer(),
  shadowed: Data.Boolean(),
  used: Data.Boolean(),
  checkpoint_hash: Data.Bytes(),
});
export const LegacyUnusedRedeemerDecisionSchema = Data.Object({
  subject: LegacyUnusedRedeemerVerdictSubjectSchema,
  redeemer_index: Data.Integer(),
  unused: Data.Boolean(),
});
export const LegacyUnusedRedeemerSourceOpeningSchema = Data.Object({
  source_index: Data.Integer(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const LegacyUnusedRedeemerPurposeOpeningSchema = Data.Object({
  frontier_index: Data.Integer(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
  script_hash: Data.Bytes(),
  purpose_subject: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const LegacyUnusedRedeemerTerminalScriptSourcesWitnessSchema =
  Data.Object({
    witness_cbor: Data.Bytes(),
  });
export const LegacyUnusedRedeemerStep01SourceSchema = Data.Enum([
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
export const LegacyUnusedRedeemerStep01RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: LegacyUnusedRedeemerStep01SourceSchema,
      redeemer_index: Data.Integer(),
    }),
  );
export const LegacyUnusedRedeemerStep02DatumSchema = faultProofStepDatumSchema(
  LegacyUnusedRedeemerBoundWitnessSchema,
);
export const LegacyUnusedRedeemerStep02RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      trace_membership: rootMembershipProofSchema(
        EventKeySchema,
        ValidationTraceDescriptorSchema,
      ),
      machine_state: ValidationMachineStateSchema,
      trace_proof: ValidationTraceProofSchema,
      control: LegacyUnusedRedeemerTerminalScriptSourcesWitnessSchema,
      language_tag: Data.Integer(),
      script_hash: Data.Bytes(),
      total_length: Data.Integer(),
      item_commitment: Data.Bytes(),
      source_siblings: Data.Array(Data.Bytes()),
    }),
  );
export const LegacyUnusedRedeemerStep03DatumSchema = faultProofStepDatumSchema(
  LegacyUnusedRedeemerAuthenticatedWitnessSchema,
);
export const LegacyUnusedRedeemerStep03RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({ input_index: Data.Integer(), output_index: Data.Integer() }),
  );
export const LegacyUnusedRedeemerStep04DatumSchema = faultProofStepDatumSchema(
  LegacyUnusedRedeemerReverseScanSchema,
);
export const LegacyUnusedRedeemerStep04RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      openings: Data.Array(LegacyUnusedRedeemerSourceOpeningSchema),
    }),
  );
export const LegacyUnusedRedeemerStep05DatumSchema =
  LegacyUnusedRedeemerStep04DatumSchema;
export const LegacyUnusedRedeemerStep05RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      openings: Data.Array(LegacyUnusedRedeemerPurposeOpeningSchema),
      item_budget: Data.Integer(),
    }),
  );
export const LegacyUnusedRedeemerStep06DatumSchema = faultProofStepDatumSchema(
  LegacyUnusedRedeemerDecisionSchema,
);
export const LegacyUnusedRedeemerStep06RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
