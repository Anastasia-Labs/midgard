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

import { NativeScriptsControlV1Schema } from "../execution-source-script-decoding/schemas-v1.js";

export const ReceivePurposeVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const ReceivePurposeBoundExecutionV1Schema = Data.Object({
  subject: ReceivePurposeVerdictSubjectV1Schema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  execution_index: Data.Integer(),
});
export const AuthenticatedReceiveLanguageV1Schema = Data.Object({
  bound: ReceivePurposeBoundExecutionV1Schema,
  prior_ledger_root: Data.Bytes(),
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
  source_index: Data.Integer(),
  origin_kind: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
});

export const ReceivePurposeStep01SourceV1Schema = Data.Enum([
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
export const ReceivePurposeStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: ReceivePurposeStep01SourceV1Schema,
      execution_index: Data.Integer(),
    }),
  );
export const ReceivePurposeStep02DatumV1Schema = faultProofStepDatumSchema(
  ReceivePurposeBoundExecutionV1Schema,
);
export const ReceivePurposeStep02RedeemerV1Schema =
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
export const ReceivePurposeStep03DatumV1Schema = faultProofStepDatumSchema(
  AuthenticatedReceiveLanguageV1Schema,
);
export const ReceivePurposeStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
