import {
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  ForcedInclusionTxSchema,
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

export const UnusedScriptVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const UnusedScriptBoundWitnessSchema = Data.Object({
  subject: UnusedScriptVerdictSubjectSchema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  script_index: Data.Integer(),
});
const UnusedScriptFrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes(),
});
export const UnusedScriptAuthenticatedWitnessSchema = Data.Object({
  bound: UnusedScriptBoundWitnessSchema,
  prior_ledger_root: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  script_total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  source_count: Data.Integer(),
  source_peaks: Data.Array(UnusedScriptFrontierPeakSchema),
  purpose_count: Data.Integer(),
  purpose_peaks: Data.Array(UnusedScriptFrontierPeakSchema),
});
export const UnusedScriptReverseScanSchema = Data.Object({
  witness: UnusedScriptAuthenticatedWitnessSchema,
  alternate_cursor: Data.Integer(),
  purpose_cursor: Data.Integer(),
  shadowed: Data.Boolean(),
  used: Data.Boolean(),
  checkpoint_hash: Data.Bytes(),
});
export const UnusedScriptDecisionSchema = Data.Object({
  subject: UnusedScriptVerdictSubjectSchema,
  script_index: Data.Integer(),
  unused: Data.Boolean(),
});
export const UnusedScriptSourceOpeningSchema = Data.Object({
  source_index: Data.Integer(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const UnusedScriptPurposeOpeningSchema = Data.Object({
  frontier_index: Data.Integer(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
  script_hash: Data.Bytes(),
  purpose_subject: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export const UnusedScriptTerminalScriptSourcesWitnessSchema = Data.Object({
  witness_cbor: Data.Bytes(),
});
export const UnusedScriptStep01SourceSchema = Data.Enum([
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
export const UnusedScriptStep01RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    source: UnusedScriptStep01SourceSchema,
    script_index: Data.Integer(),
  }),
);
export const UnusedScriptStep02DatumSchema = faultProofStepDatumSchema(
  UnusedScriptBoundWitnessSchema,
);
export const UnusedScriptStep02RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    trace_membership: rootMembershipProofSchema(
      EventKeySchema,
      ValidationTraceDescriptorSchema,
    ),
    machine_state: ValidationMachineStateSchema,
    trace_proof: ValidationTraceProofSchema,
    control: UnusedScriptTerminalScriptSourcesWitnessSchema,
    language_tag: Data.Integer(),
    script_hash: Data.Bytes(),
    total_length: Data.Integer(),
    item_commitment: Data.Bytes(),
    source_siblings: Data.Array(Data.Bytes()),
  }),
);
export const UnusedScriptStep03DatumSchema = faultProofStepDatumSchema(
  UnusedScriptAuthenticatedWitnessSchema,
);
export const UnusedScriptStep03RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({ input_index: Data.Integer(), output_index: Data.Integer() }),
);
export const UnusedScriptStep04DatumSchema = faultProofStepDatumSchema(
  UnusedScriptReverseScanSchema,
);
export const UnusedScriptStep04RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    openings: Data.Array(UnusedScriptSourceOpeningSchema),
  }),
);
export const UnusedScriptStep05DatumSchema = UnusedScriptStep04DatumSchema;
export const UnusedScriptStep05RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    openings: Data.Array(UnusedScriptPurposeOpeningSchema),
    item_budget: Data.Integer(),
  }),
);
export const UnusedScriptStep06DatumSchema = faultProofStepDatumSchema(
  UnusedScriptDecisionSchema,
);
export const UnusedScriptStep06RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
