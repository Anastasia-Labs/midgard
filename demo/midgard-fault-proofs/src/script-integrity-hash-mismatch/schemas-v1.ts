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

import { NativeScriptsControlSchema } from "../execution-source-script-decoding/schemas-v1.js";

export const IntegrityVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const BoundIntegritySchema = Data.Object({
  subject: IntegrityVerdictSubjectSchema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  script_integrity_hash: Data.Bytes(),
});
export const AuthenticatedIntegritySchema = Data.Object({
  bound: BoundIntegritySchema,
  prior_ledger_root: Data.Bytes(),
  redeemer_witness_hash: Data.Bytes(),
  selected_language_bitmap: Data.Integer(),
  execution_count: Data.Integer(),
});
export const IntegrityLanguageFoldSchema = Data.Object({
  authenticated: AuthenticatedIntegritySchema,
  cursor: Data.Integer(),
  rebuilt_language_bitmap: Data.Integer(),
  selected_language_count: Data.Integer(),
});
export const IntegrityDecisionSchema = Data.Object({
  authenticated: AuthenticatedIntegritySchema,
  expected_hash: Data.Bytes(),
});
export const IntegrityStep01SourceSchema = Data.Enum([
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
export const IntegrityStep01RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({ source: IntegrityStep01SourceSchema }),
);
export const IntegrityStep02DatumSchema =
  faultProofStepDatumSchema(BoundIntegritySchema);
export const IntegrityStep02RedeemerSchema = faultProofStepRedeemerSchema(
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
    redeemer_witness_hash: Data.Bytes(),
  }),
);
const continuation = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
});
export const IntegrityStep03DatumSchema = faultProofStepDatumSchema(
  AuthenticatedIntegritySchema,
);
export const IntegrityStep03RedeemerSchema =
  faultProofStepRedeemerSchema(continuation);
export const IntegrityStep04DatumSchema = faultProofStepDatumSchema(
  IntegrityLanguageFoldSchema,
);
export const IntegrityStep04RedeemerSchema =
  faultProofStepRedeemerSchema(continuation);
export const IntegrityStep05DatumSchema = faultProofStepDatumSchema(
  IntegrityDecisionSchema,
);
export const IntegrityStep05RedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
