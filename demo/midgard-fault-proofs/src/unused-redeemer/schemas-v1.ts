import {
  BoundedItemChunkProofV1Schema,
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

const UnusedRedeemerFrontierV1Schema = Data.Array(
  Data.Object({ height: Data.Integer(), hash: Data.Bytes() }),
);
const UnusedRedeemerReceiveControlV1Schema = Data.Object({
  source_count: Data.Integer(),
  source_peaks: Data.Array(FrontierPeakV1Schema),
  receive_count: Data.Integer(),
  previous_hash: Data.Bytes(),
  candidate_hash: Data.Bytes(),
  descriptor_peaks: Data.Array(FrontierPeakV1Schema),
});
const UnusedRedeemerObserverControlV1Schema = Data.Object({
  total_count: Data.Integer(),
  seen: Data.Integer(),
  previous_hash: Data.Bytes(),
});
const UnusedRedeemerDiscoveryControlV1Schema = Data.Object({
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
  execution_peaks: Data.Array(FrontierPeakV1Schema),
});
const UnusedRedeemerMintFoldControlV1Schema = Data.Object({
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
  asset_peaks: Data.Array(FrontierPeakV1Schema),
});
export const UnusedRedeemerScriptSourcesControlV1Schema = Data.Object({
  compact_cbor: Data.Bytes(),
  witness_set_compact_cbor: Data.Bytes(),
  field_preimage_lengths_cbor: Data.Bytes(),
  context_cbor: Data.Bytes(),
  resolved_input_count: Data.Integer(),
  resolved_inputs_accumulator: Data.Bytes(),
  signer_count: Data.Integer(),
  signer_frontier_commitment: Data.Bytes(),
  resolved_item_peaks: Data.Array(FrontierPeakV1Schema),
  stage: Data.Integer(),
  source_count: Data.Integer(),
  source_peaks: Data.Array(FrontierPeakV1Schema),
  redeemer_count: Data.Integer(),
  redeemer_peaks: Data.Array(FrontierPeakV1Schema),
  replay_cursor: Data.Integer(),
  replay_accumulator: Data.Bytes(),
  replay_remaining_schedule_hash: Data.Bytes(),
  spend_index: Data.Integer(),
  purpose_count: Data.Integer(),
  purpose_peaks: Data.Array(FrontierPeakV1Schema),
  output_cursor: Data.Integer(),
  output_count: Data.Integer(),
  output_peaks: Data.Array(FrontierPeakV1Schema),
  output_total_count: Data.Integer(),
  receive_scan: UnusedRedeemerReceiveControlV1Schema,
  source_total_count: Data.Integer(),
  redeemer_total_count: Data.Integer(),
  observer_scan: UnusedRedeemerObserverControlV1Schema,
  discovery: UnusedRedeemerDiscoveryControlV1Schema,
  output_proof: Data.Nullable(Data.Any()),
  pending_source_cbor: Data.Bytes(),
  mint_fold: UnusedRedeemerMintFoldControlV1Schema,
  resolution_schedule_hash: Data.Bytes(),
});
export const UnusedRedeemerItemControlV1Schema = Data.Object({
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
export const UnusedRedeemerSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const UnusedRedeemerBoundV1Schema = Data.Object({
  subject: UnusedRedeemerSubjectV1Schema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  redeemer_index: Data.Integer(),
});
export const UnusedRedeemerAuthenticatedDescriptorV1Schema = Data.Object({
  bound: UnusedRedeemerBoundV1Schema,
  event_key_hash: Data.Bytes(),
  descriptor: ValidationTraceDescriptorV1Schema,
});
export const UnusedRedeemerAuthenticatedControlV1Schema = Data.Object({
  bound: UnusedRedeemerBoundV1Schema,
  program_counter: Data.Integer(),
  stage: Data.Integer(),
  expected_item_control_hash: Data.Bytes(),
  used_redeemer_bitmap: Data.Integer(),
  current_purpose_kind: Data.Integer(),
  current_purpose_index: Data.Integer(),
  redeemer_count: Data.Integer(),
  purpose_count: Data.Integer(),
  purpose_peaks: UnusedRedeemerFrontierV1Schema,
  execution_count: Data.Integer(),
  execution_peaks: UnusedRedeemerFrontierV1Schema,
});
export const UnusedRedeemerAuthenticatedItemHeaderV1Schema = Data.Object({
  authenticated: UnusedRedeemerAuthenticatedControlV1Schema,
  item_index: Data.Integer(),
  item_count: Data.Integer(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  purpose_tag: Data.Integer(),
  pointer_index: Data.Integer(),
  data_offset: Data.Integer(),
  data_length: Data.Integer(),
});
export const UnusedRedeemerAuthenticatedV1Schema = Data.Object({
  bound: UnusedRedeemerBoundV1Schema,
  purpose_tag: Data.Integer(),
  pointer_index: Data.Integer(),
  item_count: Data.Integer(),
  item_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  redeemer_leaf: Data.Bytes(),
  purpose_count: Data.Integer(),
  purpose_peaks: UnusedRedeemerFrontierV1Schema,
  execution_count: Data.Integer(),
  execution_peaks: UnusedRedeemerFrontierV1Schema,
});
export const UnusedRedeemerReverseScanV1Schema = Data.Object({
  authenticated: UnusedRedeemerAuthenticatedV1Schema,
  cursor: Data.Integer(),
  used: Data.Boolean(),
  checkpoint_hash: Data.Bytes(),
});
export const UnusedRedeemerDecisionV1Schema = Data.Object({
  subject: UnusedRedeemerSubjectV1Schema,
  redeemer_index: Data.Integer(),
  unused: Data.Boolean(),
});
export const UnusedRedeemerStep02DatumV1Schema = faultProofStepDatumSchema(
  UnusedRedeemerBoundV1Schema,
);
export const UnusedRedeemerStep02aDatumV1Schema = faultProofStepDatumSchema(
  UnusedRedeemerAuthenticatedDescriptorV1Schema,
);
export const UnusedRedeemerStep02bDatumV1Schema = faultProofStepDatumSchema(
  UnusedRedeemerAuthenticatedControlV1Schema,
);
export const UnusedRedeemerStep02cDatumV1Schema = faultProofStepDatumSchema(
  UnusedRedeemerAuthenticatedItemHeaderV1Schema,
);
export const UnusedRedeemerStep03DatumV1Schema = faultProofStepDatumSchema(
  UnusedRedeemerAuthenticatedV1Schema,
);
export const UnusedRedeemerStep04DatumV1Schema = faultProofStepDatumSchema(
  UnusedRedeemerReverseScanV1Schema,
);
export const UnusedRedeemerStep05DatumV1Schema =
  UnusedRedeemerStep04DatumV1Schema;
export const UnusedRedeemerStep06DatumV1Schema = faultProofStepDatumSchema(
  UnusedRedeemerDecisionV1Schema,
);
export const UnusedRedeemerStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      trace_membership: rootMembershipProofSchema(
        EventKeySchema,
        ValidationTraceDescriptorV1Schema,
      ),
    }),
  );
export const UnusedRedeemerStep02aRedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      machine_state: ValidationMachineStateV1Schema,
      trace_proof: ValidationTraceProofV1Schema,
      control: UnusedRedeemerScriptSourcesControlV1Schema,
    }),
  );
export const UnusedRedeemerStep02bRedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      item_control: UnusedRedeemerItemControlV1Schema,
      chunk_proof: BoundedItemChunkProofV1Schema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
    }),
  );
export const UnusedRedeemerStep02cRedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      chunk_proof: BoundedItemChunkProofV1Schema,
      next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
    }),
  );
export const UnusedRedeemerStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({ input_index: Data.Integer(), output_index: Data.Integer() }),
  );
export const UnusedRedeemerSelectionOpeningV1Schema = Data.Object({
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
export const UnusedRedeemerStep04RedeemerV1Schema =
  UnusedRedeemerStep03RedeemerV1Schema;
export const UnusedRedeemerStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      openings: Data.Array(UnusedRedeemerSelectionOpeningV1Schema),
      item_budget: Data.Integer(),
    }),
  );
export const UnusedRedeemerStep06RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );

export const LegacyUnusedRedeemerVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const LegacyUnusedRedeemerBoundWitnessV1Schema = Data.Object({
  subject: LegacyUnusedRedeemerVerdictSubjectV1Schema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  redeemer_index: Data.Integer(),
});
const LegacyUnusedRedeemerFrontierPeakV1Schema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes(),
});
export const LegacyUnusedRedeemerAuthenticatedWitnessV1Schema = Data.Object({
  bound: LegacyUnusedRedeemerBoundWitnessV1Schema,
  prior_ledger_root: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  script_total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  source_count: Data.Integer(),
  source_peaks: Data.Array(LegacyUnusedRedeemerFrontierPeakV1Schema),
  purpose_count: Data.Integer(),
  purpose_peaks: Data.Array(LegacyUnusedRedeemerFrontierPeakV1Schema),
});
export const LegacyUnusedRedeemerReverseScanV1Schema = Data.Object({
  witness: LegacyUnusedRedeemerAuthenticatedWitnessV1Schema,
  alternate_cursor: Data.Integer(),
  purpose_cursor: Data.Integer(),
  shadowed: Data.Boolean(),
  used: Data.Boolean(),
  checkpoint_hash: Data.Bytes(),
});
export const LegacyUnusedRedeemerDecisionV1Schema = Data.Object({
  subject: LegacyUnusedRedeemerVerdictSubjectV1Schema,
  redeemer_index: Data.Integer(),
  unused: Data.Boolean(),
});
export const LegacyUnusedRedeemerSourceOpeningV1Schema = Data.Object({
  source_index: Data.Integer(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const LegacyUnusedRedeemerPurposeOpeningV1Schema = Data.Object({
  frontier_index: Data.Integer(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
  script_hash: Data.Bytes(),
  purpose_subject: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const LegacyUnusedRedeemerTerminalScriptSourcesWitnessV1Schema =
  Data.Object({
    witness_cbor: Data.Bytes(),
  });
export const LegacyUnusedRedeemerStep01SourceV1Schema = Data.Enum([
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
export const LegacyUnusedRedeemerStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: LegacyUnusedRedeemerStep01SourceV1Schema,
      redeemer_index: Data.Integer(),
    }),
  );
export const LegacyUnusedRedeemerStep02DatumV1Schema =
  faultProofStepDatumSchema(LegacyUnusedRedeemerBoundWitnessV1Schema);
export const LegacyUnusedRedeemerStep02RedeemerV1Schema =
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
      control: LegacyUnusedRedeemerTerminalScriptSourcesWitnessV1Schema,
      language_tag: Data.Integer(),
      script_hash: Data.Bytes(),
      total_length: Data.Integer(),
      item_commitment: Data.Bytes(),
      source_siblings: Data.Array(Data.Bytes()),
    }),
  );
export const LegacyUnusedRedeemerStep03DatumV1Schema =
  faultProofStepDatumSchema(LegacyUnusedRedeemerAuthenticatedWitnessV1Schema);
export const LegacyUnusedRedeemerStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({ input_index: Data.Integer(), output_index: Data.Integer() }),
  );
export const LegacyUnusedRedeemerStep04DatumV1Schema =
  faultProofStepDatumSchema(LegacyUnusedRedeemerReverseScanV1Schema);
export const LegacyUnusedRedeemerStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      openings: Data.Array(LegacyUnusedRedeemerSourceOpeningV1Schema),
    }),
  );
export const LegacyUnusedRedeemerStep05DatumV1Schema =
  LegacyUnusedRedeemerStep04DatumV1Schema;
export const LegacyUnusedRedeemerStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      openings: Data.Array(LegacyUnusedRedeemerPurposeOpeningV1Schema),
      item_budget: Data.Integer(),
    }),
  );
export const LegacyUnusedRedeemerStep06DatumV1Schema =
  faultProofStepDatumSchema(LegacyUnusedRedeemerDecisionV1Schema);
export const LegacyUnusedRedeemerStep06RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
