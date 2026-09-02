import {
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningV1Schema,
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

export const MissingRedeemerSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
const FrontierSchema = Data.Array(FrontierPeakV1Schema);
export const MissingRedeemerScriptDiscoveryV1Schema = Data.Object({
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
export const MissingRedeemerReceiveScanV1Schema = Data.Object({
  source_count: Data.Integer(),
  source_peaks: FrontierSchema,
  receive_count: Data.Integer(),
  previous_hash: Data.Bytes(),
  candidate_hash: Data.Bytes(),
  descriptor_peaks: FrontierSchema,
});
export const MissingRedeemerObserverScanV1Schema = Data.Object({
  total_count: Data.Integer(),
  seen: Data.Integer(),
  previous_hash: Data.Bytes(),
});
export const MissingRedeemerMintFoldV1Schema = Data.Object({
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
export const MissingRedeemerScriptSourcesControlV1Schema = Data.Object({
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
  receive_scan: MissingRedeemerReceiveScanV1Schema,
  source_total_count: Data.Integer(),
  redeemer_total_count: Data.Integer(),
  observer_scan: MissingRedeemerObserverScanV1Schema,
  discovery: MissingRedeemerScriptDiscoveryV1Schema,
  output_proof: Data.Nullable(Data.Any()),
  pending_source_cbor: Data.Bytes(),
  mint_fold: MissingRedeemerMintFoldV1Schema,
  resolution_schedule_hash: Data.Bytes(),
});
export const MissingRedeemerBoundPurposeV1Schema = Data.Object({
  subject: MissingRedeemerSubjectV1Schema,
  witness_set_hash: Data.Bytes(),
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
});
export const MissingRedeemerAuthenticatedPurposeV1Schema = Data.Object({
  bound: MissingRedeemerBoundPurposeV1Schema,
  purpose_count: Data.Integer(),
  redeemer_tag: Data.Integer(),
  required_script_hash: Data.Bytes(),
  source_index: Data.Integer(),
  source_language_tag: Data.Integer(),
  source_leaf: Data.Bytes(),
});
export const MissingRedeemerAuthenticatedStageTenV1Schema = Data.Object({
  bound: MissingRedeemerBoundPurposeV1Schema,
  source_count: Data.Integer(),
  source_peaks: FrontierSchema,
  purpose_count: Data.Integer(),
  purpose_peaks: FrontierSchema,
  discovery: MissingRedeemerScriptDiscoveryV1Schema,
});
export const MissingRedeemerAuthenticatedDescriptorV1Schema = Data.Object({
  bound: MissingRedeemerBoundPurposeV1Schema,
  event_key_hash: Data.Bytes(),
  descriptor: ValidationTraceDescriptorV1Schema,
});
export const MissingRedeemerScanV1Schema = Data.Object({
  authenticated: MissingRedeemerAuthenticatedPurposeV1Schema,
  checkpoint_hash: Data.Bytes(),
  cursor: Data.Integer(),
  item_count: Data.Integer(),
  found: Data.Boolean(),
});
export const MissingRedeemerDecisionV1Schema = Data.Object({
  bound: MissingRedeemerBoundPurposeV1Schema,
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
      header: HeaderV1Schema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxV1Schema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);
export const MissingRedeemerStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: Source,
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
  );
export const MissingRedeemerStep02DatumV1Schema = faultProofStepDatumSchema(
  MissingRedeemerBoundPurposeV1Schema,
);
export const MissingRedeemerStep02RedeemerV1Schema =
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
export const MissingRedeemerStep02aDatumV1Schema = faultProofStepDatumSchema(
  MissingRedeemerAuthenticatedDescriptorV1Schema,
);
export const MissingRedeemerStep02aRedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      machine_state: ValidationMachineStateV1Schema,
      trace_proof: ValidationTraceProofV1Schema,
      control: MissingRedeemerScriptSourcesControlV1Schema,
    }),
  );
export const MissingRedeemerStep02bDatumV1Schema = faultProofStepDatumSchema(
  MissingRedeemerAuthenticatedStageTenV1Schema,
);
export const MissingRedeemerStep02bRedeemerV1Schema =
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
export const MissingRedeemerAuthenticationStateV1Schema = Data.Enum([
  Data.Object({
    Ready: Data.Object({
      authenticated: MissingRedeemerAuthenticatedPurposeV1Schema,
    }),
  }),
  Data.Object({
    Grammar: Data.Object({
      authenticated: MissingRedeemerAuthenticatedPurposeV1Schema,
      checkpoint_hash: Data.Bytes(),
    }),
  }),
]);
export const MissingRedeemerStep03DatumV1Schema = faultProofStepDatumSchema(
  MissingRedeemerAuthenticationStateV1Schema,
);
export const MissingRedeemerStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Enum([
      Data.Object({
        AuthenticateDirect: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          opening: FieldOpeningV1Schema,
        }),
      }),
      Data.Object({
        StartGrammar: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          opening: FieldOpeningV1Schema,
          item_budget: Data.Integer(),
        }),
      }),
      Data.Object({
        ResumeGrammar: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          opening: FieldOpeningV1Schema,
          checkpoint_bytes: Data.Bytes(),
          item_budget: Data.Integer(),
        }),
      }),
      Data.Object({
        FinishGrammar: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          opening: FieldOpeningV1Schema,
          checkpoint_bytes: Data.Bytes(),
        }),
      }),
    ]),
  );
export const MissingRedeemerStep04DatumV1Schema = faultProofStepDatumSchema(
  MissingRedeemerScanV1Schema,
);
export const MissingRedeemerStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  );
export const MissingRedeemerStep05DatumV1Schema = faultProofStepDatumSchema(
  MissingRedeemerDecisionV1Schema,
);
export const MissingRedeemerStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
