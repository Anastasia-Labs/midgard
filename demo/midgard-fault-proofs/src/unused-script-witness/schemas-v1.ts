import {
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  ForcedInclusionTxV1Schema,
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

export const UnusedScriptVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const UnusedScriptBoundWitnessV1Schema = Data.Object({
  subject: UnusedScriptVerdictSubjectV1Schema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  script_index: Data.Integer(),
});
const UnusedScriptFrontierPeakV1Schema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes(),
});
export const UnusedScriptAuthenticatedWitnessV1Schema = Data.Object({
  bound: UnusedScriptBoundWitnessV1Schema,
  prior_ledger_root: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  script_total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  source_count: Data.Integer(),
  source_peaks: Data.Array(UnusedScriptFrontierPeakV1Schema),
  purpose_count: Data.Integer(),
  purpose_peaks: Data.Array(UnusedScriptFrontierPeakV1Schema),
});
export const UnusedScriptReverseScanV1Schema = Data.Object({
  witness: UnusedScriptAuthenticatedWitnessV1Schema,
  alternate_cursor: Data.Integer(),
  purpose_cursor: Data.Integer(),
  shadowed: Data.Boolean(),
  used: Data.Boolean(),
  checkpoint_hash: Data.Bytes(),
});
export const UnusedScriptDecisionV1Schema = Data.Object({
  subject: UnusedScriptVerdictSubjectV1Schema,
  script_index: Data.Integer(),
  unused: Data.Boolean(),
});
export const UnusedScriptSourceOpeningV1Schema = Data.Object({
  source_index: Data.Integer(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const UnusedScriptPurposeOpeningV1Schema = Data.Object({
  frontier_index: Data.Integer(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
  script_hash: Data.Bytes(),
  purpose_subject: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const UnusedScriptTerminalScriptSourcesWitnessV1Schema = Data.Object({
  witness_cbor: Data.Bytes(),
});
export const UnusedScriptStep01SourceV1Schema = Data.Enum([
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
export const UnusedScriptStep01RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({
    source: UnusedScriptStep01SourceV1Schema,
    script_index: Data.Integer(),
  }),
);
export const UnusedScriptStep02DatumV1Schema = faultProofStepDatumSchema(
  UnusedScriptBoundWitnessV1Schema,
);
export const UnusedScriptStep02RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    trace_membership: rootMembershipProofSchema(
      EventKeySchema,
      ValidationTraceDescriptorV1Schema,
    ),
    machine_state: ValidationMachineStateV1Schema,
    trace_proof: ValidationTraceProofV1Schema,
    control: UnusedScriptTerminalScriptSourcesWitnessV1Schema,
    language_tag: Data.Integer(),
    script_hash: Data.Bytes(),
    total_length: Data.Integer(),
    item_commitment: Data.Bytes(),
    source_siblings: Data.Array(Data.Bytes()),
  }),
);
export const UnusedScriptStep03DatumV1Schema = faultProofStepDatumSchema(
  UnusedScriptAuthenticatedWitnessV1Schema,
);
export const UnusedScriptStep03RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({ input_index: Data.Integer(), output_index: Data.Integer() }),
);
export const UnusedScriptStep04DatumV1Schema = faultProofStepDatumSchema(
  UnusedScriptReverseScanV1Schema,
);
export const UnusedScriptStep04RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    openings: Data.Array(UnusedScriptSourceOpeningV1Schema),
  }),
);
export const UnusedScriptStep05DatumV1Schema = UnusedScriptStep04DatumV1Schema;
export const UnusedScriptStep05RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    openings: Data.Array(UnusedScriptPurposeOpeningV1Schema),
    item_budget: Data.Integer(),
  }),
);
export const UnusedScriptStep06DatumV1Schema = faultProofStepDatumSchema(
  UnusedScriptDecisionV1Schema,
);
export const UnusedScriptStep06RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
