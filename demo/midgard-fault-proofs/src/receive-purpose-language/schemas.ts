import {
  EventKeySchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  ForcedInclusionTxV1Schema,
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

import { NativeScriptsControlSchema } from "../execution-source-script-decoding/schemas.js";

export const ReceivePurposeVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const ReceivePurposeBoundExecutionSchema = Data.Object({
  subject: ReceivePurposeVerdictSubjectSchema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  execution_index: Data.Integer(),
});
export const AuthenticatedReceiveLanguageSchema = Data.Object({
  bound: ReceivePurposeBoundExecutionSchema,
  prior_ledger_root: Data.Bytes(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
  source_index: Data.Integer(),
  origin_kind: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
});

export const ReceivePurposeStep01SourceSchema = Data.Enum([
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
export const ReceivePurposeStep01RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    source: ReceivePurposeStep01SourceSchema,
    execution_index: Data.Integer(),
  }),
);
export const ReceivePurposeStep02DatumSchema = faultProofStepDatumSchema(
  ReceivePurposeBoundExecutionSchema,
);
export const ReceivePurposeStep02RedeemerSchema = faultProofStepRedeemerSchema(
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
export const ReceivePurposeStep03DatumSchema = faultProofStepDatumSchema(
  AuthenticatedReceiveLanguageSchema,
);
export const ReceivePurposeStep03RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
