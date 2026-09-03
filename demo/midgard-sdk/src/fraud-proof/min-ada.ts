/** Q27 `min-ada` non-interactive wire types. */
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema } from "../common.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MembershipCarriageSchema,
  NativeTxInclusionCarriageSchema,
  NonMembershipCarriageSchema,
} from "./native.js";

export const MIN_ADA_VIOLATION_ID = "min-ada" as const;

export const MinAdaFaultSchema = Data.Enum([
  Data.Object({ MinAdaTx: Data.Object({ output_index: Data.Integer() }) }),
  Data.Literal("MinAdaUtxo"),
]);
export type MinAdaFault = Data.Static<typeof MinAdaFaultSchema>;
export const MinAdaFault = MinAdaFaultSchema as unknown as MinAdaFault;

export const MinAdaPostUtxoMembershipSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  out_ref: OutputReferenceSchema,
  descriptor_cbor: Data.Bytes(),
});
export type MinAdaPostUtxoMembership = Data.Static<
  typeof MinAdaPostUtxoMembershipSchema
>;
export const MinAdaPostUtxoMembership =
  MinAdaPostUtxoMembershipSchema as unknown as MinAdaPostUtxoMembership;

export const MinAdaStep01DatumSchema = faultProofStepDatumSchema(Data.Any());
export const MinAdaStep01ArgsSchema = Data.Object({
  tx_inclusion: Data.Nullable(NativeTxInclusionCarriageSchema),
  post_utxo_membership: Data.Nullable(MinAdaPostUtxoMembershipSchema),
  fault: MinAdaFaultSchema,
});
export const MinAdaStep01SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinAdaStep01ArgsSchema,
);

export const MinAdaPostUtxoStateSchema = Data.Object({
  out_ref: OutputReferenceSchema,
  descriptor_cbor: Data.Bytes(),
  post_utxos_root: H32Schema,
  prev_utxos_root: H32Schema,
});
export const MinAdaStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  fault: MinAdaFaultSchema,
  post_utxo: Data.Nullable(MinAdaPostUtxoStateSchema),
});
export type MinAdaStep02State = Data.Static<typeof MinAdaStep02StateSchema>;
export const MinAdaStep02State =
  MinAdaStep02StateSchema as unknown as MinAdaStep02State;

export const MinAdaStep02DatumSchema = faultProofStepDatumSchema(
  MinAdaStep02StateSchema,
);
export const MinAdaStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  yield_to_ref_input_index: Data.Integer(),
  outputs_opening: Data.Nullable(FieldOpeningSchema),
  post_membership: Data.Nullable(MembershipCarriageSchema),
});
export const MinAdaStep02SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinAdaStep02ArgsSchema,
);

export const MinAdaStep03StateSchema = Data.Enum([
  Data.Object({
    MinAdaTxDescriptor: Data.Object({
      total_length: Data.Integer(),
      lovelace: Data.Integer(),
    }),
  }),
  Data.Object({
    MinAdaUtxoDescriptor: Data.Object({
      descriptor_cbor: Data.Bytes(),
      out_ref_key: Data.Bytes(),
      prev_utxos_root: H32Schema,
    }),
  }),
]);
export const MinAdaStep03DatumSchema = faultProofStepDatumSchema(
  MinAdaStep03StateSchema,
);
export const MinAdaStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
});
export const MinAdaStep03SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinAdaStep03ArgsSchema,
);

export const MinAdaStep04StateSchema = Data.Object({
  out_ref_key: Data.Bytes(),
  prev_utxos_root: H32Schema,
});
export const MinAdaStep04DatumSchema = faultProofStepDatumSchema(
  MinAdaStep04StateSchema,
);
export const MinAdaStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  predecessor_non_membership: NonMembershipCarriageSchema,
});
export const MinAdaStep04SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinAdaStep04ArgsSchema,
);

export const MinAdaStep05StateSchema = Data.Enum([
  Data.Literal("PredicateAndCulpabilityAuthenticated"),
  // Lucid collapses a singleton enum to `void`, which cannot encode the
  // constructor name inside the surrounding nullable step datum. Keep the
  // canonical constructor at index zero; the second arm is never emitted and
  // is rejected by the on-chain step-05 datum decoder.
  Data.Literal("ReservedMinAdaStep05State"),
]);
export const MinAdaStep05DatumSchema = faultProofStepDatumSchema(
  MinAdaStep05StateSchema,
);
export const MinAdaStep05ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const MinAdaStep05SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinAdaStep05ArgsSchema,
);

export const MIN_ADA_STEP_NAMES = [
  "step_01",
  "step_02",
  "step_03",
  "step_04",
  "step_05",
] as const;
