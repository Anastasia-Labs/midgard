import {
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedInclusionTxSchema,
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

export const MissingRedeemerSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
const FrontierSchema = Data.Array(FrontierPeakSchema);
export const MissingRedeemerScriptDiscoverySchema = Data.Object({
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
export const MissingRedeemerReceiveScanSchema = Data.Object({
  source_count: Data.Integer(),
  source_peaks: FrontierSchema,
  receive_count: Data.Integer(),
  previous_hash: Data.Bytes(),
  candidate_hash: Data.Bytes(),
  descriptor_peaks: FrontierSchema,
});
export const MissingRedeemerObserverScanSchema = Data.Object({
  total_count: Data.Integer(),
  seen: Data.Integer(),
  previous_hash: Data.Bytes(),
});
export const MissingRedeemerMintFoldSchema = Data.Object({
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
export const MissingRedeemerScriptSourcesControlSchema = Data.Object({
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
  receive_scan: MissingRedeemerReceiveScanSchema,
  source_total_count: Data.Integer(),
  redeemer_total_count: Data.Integer(),
  observer_scan: MissingRedeemerObserverScanSchema,
  discovery: MissingRedeemerScriptDiscoverySchema,
  output_proof: Data.Nullable(Data.Any()),
  pending_source_cbor: Data.Bytes(),
  mint_fold: MissingRedeemerMintFoldSchema,
  resolution_schedule_hash: Data.Bytes(),
});
export const MissingRedeemerBoundPurposeSchema = Data.Object({
  subject: MissingRedeemerSubjectSchema,
  witness_set_hash: Data.Bytes(),
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
});
export const MissingRedeemerAuthenticatedPurposeSchema = Data.Object({
  bound: MissingRedeemerBoundPurposeSchema,
  purpose_count: Data.Integer(),
  redeemer_tag: Data.Integer(),
  required_script_hash: Data.Bytes(),
  source_index: Data.Integer(),
  source_language_tag: Data.Integer(),
  source_leaf: Data.Bytes(),
});
export const MissingRedeemerAuthenticatedStageTenSchema = Data.Object({
  bound: MissingRedeemerBoundPurposeSchema,
  source_count: Data.Integer(),
  source_peaks: FrontierSchema,
  purpose_count: Data.Integer(),
  purpose_peaks: FrontierSchema,
  discovery: MissingRedeemerScriptDiscoverySchema,
});
export const MissingRedeemerAuthenticatedDescriptorSchema = Data.Object({
  bound: MissingRedeemerBoundPurposeSchema,
  event_key_hash: Data.Bytes(),
  descriptor: ValidationTraceDescriptorSchema,
});
export const MissingRedeemerScanSchema = Data.Object({
  authenticated: MissingRedeemerAuthenticatedPurposeSchema,
  checkpoint_hash: Data.Bytes(),
  cursor: Data.Integer(),
  item_count: Data.Integer(),
  found: Data.Boolean(),
});
export const MissingRedeemerDecisionSchema = Data.Object({
  bound: MissingRedeemerBoundPurposeSchema,
  redeemer_missing: Data.Boolean(),
});
const Source = Data.Enum([
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
        ForcedInclusionTxSchema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);
export const MissingRedeemerStep01RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    source: Source,
    purpose_kind: Data.Integer(),
    purpose_index: Data.Integer(),
  }),
);
export const MissingRedeemerStep02DatumSchema = faultProofStepDatumSchema(
  MissingRedeemerBoundPurposeSchema,
);
export const MissingRedeemerStep02RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    trace_membership: rootMembershipProofSchema(
      EventKeySchema,
      ValidationTraceDescriptorSchema,
    ),
  }),
);
export const MissingRedeemerStep02aDatumSchema = faultProofStepDatumSchema(
  MissingRedeemerAuthenticatedDescriptorSchema,
);
export const MissingRedeemerStep02aRedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      machine_state: ValidationMachineStateSchema,
      trace_proof: ValidationTraceProofSchema,
      control: MissingRedeemerScriptSourcesControlSchema,
    }),
  );
export const MissingRedeemerStep02bDatumSchema = faultProofStepDatumSchema(
  MissingRedeemerAuthenticatedStageTenSchema,
);
export const MissingRedeemerStep02bRedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      absolute_purpose_index: Data.Integer(),
      purpose_siblings: Data.Array(Data.Bytes()),
      source_origin_kind: Data.Integer(),
      source_key: Data.Bytes(),
      source_language_tag: Data.Integer(),
      source_script_hash: Data.Bytes(),
      source_total_length: Data.Integer(),
      source_item_commitment: Data.Bytes(),
      source_siblings: Data.Array(Data.Bytes()),
    }),
  );
export const MissingRedeemerAuthenticationStateSchema = Data.Enum([
  Data.Object({
    Ready: Data.Object({
      authenticated: MissingRedeemerAuthenticatedPurposeSchema,
    }),
  }),
  Data.Object({
    Grammar: Data.Object({
      authenticated: MissingRedeemerAuthenticatedPurposeSchema,
      checkpoint_hash: Data.Bytes(),
    }),
  }),
]);
export const MissingRedeemerStep03DatumSchema = faultProofStepDatumSchema(
  MissingRedeemerAuthenticationStateSchema,
);
export const MissingRedeemerStep03RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Enum([
    Data.Object({
      AuthenticateDirect: Data.Object({
        input_index: Data.Integer(),
        output_index: Data.Integer(),
        opening: FieldOpeningSchema,
      }),
    }),
    Data.Object({
      StartGrammar: Data.Object({
        input_index: Data.Integer(),
        output_index: Data.Integer(),
        opening: FieldOpeningSchema,
        item_budget: Data.Integer(),
      }),
    }),
    Data.Object({
      ResumeGrammar: Data.Object({
        input_index: Data.Integer(),
        output_index: Data.Integer(),
        opening: FieldOpeningSchema,
        checkpoint_bytes: Data.Bytes(),
        item_budget: Data.Integer(),
      }),
    }),
    Data.Object({
      FinishGrammar: Data.Object({
        input_index: Data.Integer(),
        output_index: Data.Integer(),
        opening: FieldOpeningSchema,
        checkpoint_bytes: Data.Bytes(),
      }),
    }),
  ]),
);
export const MissingRedeemerStep04DatumSchema = faultProofStepDatumSchema(
  MissingRedeemerScanSchema,
);
export const MissingRedeemerStep04RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    opening: FieldOpeningSchema,
    checkpoint_bytes: Data.Bytes(),
    item_budget: Data.Integer(),
  }),
);
export const MissingRedeemerStep05DatumSchema = faultProofStepDatumSchema(
  MissingRedeemerDecisionSchema,
);
export const MissingRedeemerStep05RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
