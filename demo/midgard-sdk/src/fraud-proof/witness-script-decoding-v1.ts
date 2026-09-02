import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema } from "@/common.js";
import {
  BoundedItemChunkProofV1Schema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
} from "@/ledger-state.js";
import { RejectionReasonV1Schema } from "@/rejection-reason-v1.js";
import { rootMembershipProofSchema } from "@/transition-trace.js";

import { FieldOpeningV1Schema } from "./field-opening-v1.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";
import { NativeScriptFrameV1Schema } from "./validation-auxiliary-witness-v1.js";

export const WITNESS_SCRIPT_DECODING_CATEGORY_V1 =
  "witnessScriptDecoding" as const;
export const WITNESS_SCRIPT_DECODING_PROPOSED_ID_V1 = "00000022" as const;

export const WITNESS_SCRIPT_DECODING_RESULT_PENDING_V1 = -1n;
export const WITNESS_SCRIPT_DECODING_RESULT_NO_FAULT_V1 = -2n;
export const WITNESS_SCRIPT_DECODING_RESULT_HEADER_MALFORMED_V1 = 0n;
export const WITNESS_SCRIPT_DECODING_RESULT_NATIVE_MALFORMED_V1 = 1n;
export const WITNESS_SCRIPT_DECODING_RESULT_NODE_LIMIT_V1 = 2n;
export const WITNESS_SCRIPT_DECODING_RESULT_DEPTH_LIMIT_V1 = 3n;

export const WitnessScriptDecodingVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: H32Schema,
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});

export const WitnessScriptDecodingBoundV1Schema = Data.Object({
  subject: WitnessScriptDecodingVerdictSubjectV1Schema,
  witness_set_hash: H32Schema,
  script_index: Data.Integer(),
  accused_class: Data.Integer(),
});
export type WitnessScriptDecodingBoundV1 = Data.Static<
  typeof WitnessScriptDecodingBoundV1Schema
>;

export const WitnessScriptDecodingScanStateV1Schema = Data.Object({
  bound: WitnessScriptDecodingBoundV1Schema,
  total_length: Data.Integer(),
  item_commitment: H32Schema,
  control_cbor: Data.Bytes(),
  next_expected_script_hash: Data.Bytes({ minLength: 28, maxLength: 28 }),
  checkpoint_hash: H32Schema,
  result_class: Data.Integer(),
});
export type WitnessScriptDecodingScanStateV1 = Data.Static<
  typeof WitnessScriptDecodingScanStateV1Schema
>;

export const WitnessScriptDecodingStep01SourceV1Schema = Data.Enum([
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
export const WitnessScriptDecodingStep01ArgsV1Schema = Data.Object({
  source: WitnessScriptDecodingStep01SourceV1Schema,
  script_index: Data.Integer(),
});
export const WitnessScriptDecodingStep01DatumV1Schema =
  faultProofStepDatumSchema(Data.Any());
export const WitnessScriptDecodingStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(WitnessScriptDecodingStep01ArgsV1Schema);

export const WitnessScriptDecodingStep02ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningV1Schema,
});
export const WitnessScriptDecodingStep02DatumV1Schema =
  faultProofStepDatumSchema(WitnessScriptDecodingBoundV1Schema);
export const WitnessScriptDecodingStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(WitnessScriptDecodingStep02ArgsV1Schema);

export const WitnessScriptDecodingStep03ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  control_cbor: Data.Bytes(),
  chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
  next_chunk_proof: Data.Nullable(BoundedItemChunkProofV1Schema),
  frames: Data.Array(NativeScriptFrameV1Schema),
  step_budget: Data.Integer(),
});
export const WitnessScriptDecodingStep03DatumV1Schema =
  faultProofStepDatumSchema(WitnessScriptDecodingScanStateV1Schema);
export const WitnessScriptDecodingStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(WitnessScriptDecodingStep03ArgsV1Schema);

export const WitnessScriptDecodingStep04ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const WitnessScriptDecodingStep04DatumV1Schema =
  faultProofStepDatumSchema(WitnessScriptDecodingScanStateV1Schema);
export const WitnessScriptDecodingStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(WitnessScriptDecodingStep04ArgsV1Schema);

export const WITNESS_SCRIPT_DECODING_PHYSICAL_SCRIPTS_V1 = Object.freeze([
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
