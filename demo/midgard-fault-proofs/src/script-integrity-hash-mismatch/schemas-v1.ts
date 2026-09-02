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

export const IntegrityVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const BoundIntegrityV1Schema = Data.Object({
  subject: IntegrityVerdictSubjectV1Schema,
  validation_traces_root: Data.Bytes(),
  validation_trace_count: Data.Integer(),
  script_integrity_hash: Data.Bytes(),
});
export const AuthenticatedIntegrityV1Schema = Data.Object({
  bound: BoundIntegrityV1Schema,
  prior_ledger_root: Data.Bytes(),
  redeemer_witness_hash: Data.Bytes(),
  selected_language_bitmap: Data.Integer(),
  execution_count: Data.Integer(),
});
export const IntegrityLanguageFoldV1Schema = Data.Object({
  authenticated: AuthenticatedIntegrityV1Schema,
  cursor: Data.Integer(),
  rebuilt_language_bitmap: Data.Integer(),
  selected_language_count: Data.Integer(),
});
export const IntegrityDecisionV1Schema = Data.Object({
  authenticated: AuthenticatedIntegrityV1Schema,
  expected_hash: Data.Bytes(),
});
export const IntegrityStep01SourceV1Schema = Data.Enum([
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
export const IntegrityStep01RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({ source: IntegrityStep01SourceV1Schema }),
);
export const IntegrityStep02DatumV1Schema = faultProofStepDatumSchema(
  BoundIntegrityV1Schema,
);
export const IntegrityStep02RedeemerV1Schema = faultProofStepRedeemerSchema(
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
    redeemer_witness_hash: Data.Bytes(),
  }),
);
const continuation = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
});
export const IntegrityStep03DatumV1Schema = faultProofStepDatumSchema(
  AuthenticatedIntegrityV1Schema,
);
export const IntegrityStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(continuation);
export const IntegrityStep04DatumV1Schema = faultProofStepDatumSchema(
  IntegrityLanguageFoldV1Schema,
);
export const IntegrityStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(continuation);
export const IntegrityStep05DatumV1Schema = faultProofStepDatumSchema(
  IntegrityDecisionV1Schema,
);
export const IntegrityStep05RedeemerV1Schema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
