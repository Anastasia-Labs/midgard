import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema } from "../common.js";
import {
  BoundedItemChunkProofSchema,
  ForcedInclusionTxSchema,
  HeaderSchema,
} from "../ledger-state.js";
import { RejectionReasonSchema } from "../rejection-reason-v1.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { FieldOpeningSchema } from "./field-opening-v1.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";
import { NativeScriptFrameSchema } from "./validation-auxiliary-witness-v1.js";

export const WITNESS_SCRIPT_DECODING_CATEGORY =
  "witnessScriptDecoding" as const;
export const WITNESS_SCRIPT_DECODING_PROPOSED_ID = "00000022" as const;

export const WITNESS_SCRIPT_DECODING_RESULT_PENDING = -1n;
export const WITNESS_SCRIPT_DECODING_RESULT_NO_FAULT = -2n;
export const WITNESS_SCRIPT_DECODING_RESULT_HEADER_MALFORMED = 0n;
export const WITNESS_SCRIPT_DECODING_RESULT_NATIVE_MALFORMED = 1n;
export const WITNESS_SCRIPT_DECODING_RESULT_NODE_LIMIT = 2n;
export const WITNESS_SCRIPT_DECODING_RESULT_DEPTH_LIMIT = 3n;

export const WitnessScriptDecodingVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: H32Schema,
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

export const WitnessScriptDecodingBoundSchema = Data.Object({
  subject: WitnessScriptDecodingVerdictSubjectSchema,
  witness_set_hash: H32Schema,
  script_index: Data.Integer(),
  accused_class: Data.Integer(),
});
export type WitnessScriptDecodingBound = Data.Static<
  typeof WitnessScriptDecodingBoundSchema
>;

export const WitnessScriptDecodingScanStateSchema = Data.Object({
  bound: WitnessScriptDecodingBoundSchema,
  total_length: Data.Integer(),
  item_commitment: H32Schema,
  control_cbor: Data.Bytes(),
  next_expected_script_hash: Data.Bytes({ minLength: 28, maxLength: 28 }),
  checkpoint_hash: H32Schema,
  result_class: Data.Integer(),
});
export type WitnessScriptDecodingScanState = Data.Static<
  typeof WitnessScriptDecodingScanStateSchema
>;

export const WitnessScriptDecodingStep01SourceSchema = Data.Enum([
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
export const WitnessScriptDecodingStep01ArgsSchema = Data.Object({
  source: WitnessScriptDecodingStep01SourceSchema,
  script_index: Data.Integer(),
});
export const WitnessScriptDecodingStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export const WitnessScriptDecodingStep01RedeemerSchema =
  faultProofStepRedeemerSchema(WitnessScriptDecodingStep01ArgsSchema);

export const WitnessScriptDecodingStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export const WitnessScriptDecodingStep02DatumSchema = faultProofStepDatumSchema(
  WitnessScriptDecodingBoundSchema,
);
export const WitnessScriptDecodingStep02RedeemerSchema =
  faultProofStepRedeemerSchema(WitnessScriptDecodingStep02ArgsSchema);

export const WitnessScriptDecodingStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  control_cbor: Data.Bytes(),
  chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
  next_chunk_proof: Data.Nullable(BoundedItemChunkProofSchema),
  frames: Data.Array(NativeScriptFrameSchema),
  step_budget: Data.Integer(),
});
export const WitnessScriptDecodingStep03DatumSchema = faultProofStepDatumSchema(
  WitnessScriptDecodingScanStateSchema,
);
export const WitnessScriptDecodingStep03RedeemerSchema =
  faultProofStepRedeemerSchema(WitnessScriptDecodingStep03ArgsSchema);

export const WitnessScriptDecodingStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const WitnessScriptDecodingStep04DatumSchema = faultProofStepDatumSchema(
  WitnessScriptDecodingScanStateSchema,
);
export const WitnessScriptDecodingStep04RedeemerSchema =
  faultProofStepRedeemerSchema(WitnessScriptDecodingStep04ArgsSchema);

export const WITNESS_SCRIPT_DECODING_PHYSICAL_SCRIPTS = Object.freeze([
  {
    role: "firstStep",
    title: "fraud_proofs/witness_script_decoding/step_01.main.spend",
    parameters: [
      "step_02_validator_script_hash",
      "computation_thread_token_policy_id",
      "hub_oracle",
    ],
  },
  {
    role: "itemAuthenticator",
    title: "fraud_proofs/witness_script_decoding/step_02.main.spend",
    parameters: [
      "step_03_validator_script_hash",
      "computation_thread_token_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  },
  {
    role: "resumableScan",
    title: "fraud_proofs/witness_script_decoding/step_03.main.spend",
    parameters: [
      "step_04_validator_script_hash",
      "computation_thread_token_policy_id",
    ],
  },
  {
    role: "terminal",
    title: "fraud_proofs/witness_script_decoding/step_04.main.spend",
    parameters: [
      "computation_thread_token_policy_id",
      "fraud_proof_token_policy_id",
      "fraud_proof_token_address",
    ],
  },
] as const);
