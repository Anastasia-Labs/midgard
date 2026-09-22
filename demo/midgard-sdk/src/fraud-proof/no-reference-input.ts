/**
 * Re-derived onto the flat field commitments by #604 (the #575 off-chain builder
 * remediation): thread state carries the §2.5 anchor rather than a per-field
 * collection commitment, and a step redeemer carries a `FieldOpeningV1` rather
 * than a reproduced `..._preimage: List<…>`. The rebind is explained once in
 * `docs/fault-proofs/decisions/0001-reference-input-field-evidence.md`.
 */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema, ProofSchema } from "../common.js";
import {
  EventKeySchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
} from "../ledger-state.js";
import { RejectionReasonSchema } from "../rejection-reason.js";
import {
  EventToStepMembershipProofSchema,
  rootMembershipProofSchema,
  TransitionTraceMembershipProofSchema,
} from "../transition-trace.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MidgardTxInputSchema,
  NativeTxInclusionCarriage,
  NativeTxInclusionCarriageSchema,
  NonMembershipCarriageSchema,
} from "./native.js";

/**
 * No-reference-input fault proof — proves a Midgard block includes a native L2
 * transaction that references an input which never existed in the block's prev
 * ledger, and whose producing transaction is not in the block either.
 *
 * This mirrors the non-existent-input (`no_input`) family but operates on the
 * transaction's reference inputs rather than its spend inputs. It uses the same
 * native transaction-root inclusion path: the bad transaction is committed by
 * the block's `transactions_root` rather than as PlutusData, and schemas reuse
 * the shared `native.js` helpers so the step datum/redeemer envelope matches
 * every other fault-proof family.
 */

export const NoReferenceInputTxInclusionArgsSchema =
  NativeTxInclusionCarriageSchema;
export type NoReferenceInputTxInclusionArgs = NativeTxInclusionCarriage;
export const NoReferenceInputTxInclusionArgs =
  NativeTxInclusionCarriage as unknown as NoReferenceInputTxInclusionArgs;

export const NoReferenceInputStepCancelSchema = FaultProofStepCancelSchema;
export type NoReferenceInputStepCancel = FaultProofStepCancel;
export const NoReferenceInputStepCancel =
  FaultProofStepCancel as unknown as NoReferenceInputStepCancel;

// ## Step 01 — verify inclusion of the bad transaction
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so it
// is read with the generic computation-thread step datum. Spending it requires
// only the native-tx inclusion redeemer; the produced UTxO carries step-02.

export const NoReferenceInputVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const NoReferenceInputStep01SourceSchema = Data.Enum([
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
export const NoReferenceInputForcedSourcePayloadSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
  direction: Data.Integer(),
});

export const NoReferenceInputStep01ArgsSchema = Data.Object({
  source: NoReferenceInputStep01SourceSchema,
});

export const NoReferenceInputStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep01ArgsSchema);
export type NoReferenceInputStep01SpendRedeemer = Data.Static<
  typeof NoReferenceInputStep01SpendRedeemerSchema
>;
export const NoReferenceInputStep01SpendRedeemer =
  asDataType<NoReferenceInputStep01SpendRedeemer>(
    NoReferenceInputStep01SpendRedeemerSchema,
  );

// ## Step 02 — provide the reference-inputs preimage and select the bad input

/**
 * Mirrors `midgard/fraud_proofs/no_reference_input/step_02.State`. #604: the
 * retired `bad_tx_reference_inputs_hash` became the §2.5 anchor `bad_tx_id`.
 */
export const NoReferenceInputStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  blocks_prev_utxos_root: H32Schema,
  blocks_transactions_root: H32Schema,
});
export type NoReferenceInputStep02State = Data.Static<
  typeof NoReferenceInputStep02StateSchema
>;
export const NoReferenceInputStep02State =
  asDataType<NoReferenceInputStep02State>(NoReferenceInputStep02StateSchema);

export const NoReferenceInputStep02DatumSchema = faultProofStepDatumSchema(
  NoReferenceInputStep02StateSchema,
);
export type NoReferenceInputStep02Datum = Data.Static<
  typeof NoReferenceInputStep02DatumSchema
>;
export const NoReferenceInputStep02Datum =
  asDataType<NoReferenceInputStep02Datum>(NoReferenceInputStep02DatumSchema);

/**
 * Mirrors `midgard/fraud_proofs/no_reference_input/step_02.Args`. Field 1, not
 * field 0 — the index is a literal at the on-chain call site, and §4 removed the
 * domain separation that used to make the two distinguishable by hash alone.
 */
export const NoReferenceInputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  reference_inputs_opening: FieldOpeningSchema,
  bad_reference_input_index: Data.Integer(),
});
export type NoReferenceInputStep02Args = Data.Static<
  typeof NoReferenceInputStep02ArgsSchema
>;
export const NoReferenceInputStep02Args =
  asDataType<NoReferenceInputStep02Args>(NoReferenceInputStep02ArgsSchema);

export const NoReferenceInputStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep02ArgsSchema);
export type NoReferenceInputStep02SpendRedeemer = Data.Static<
  typeof NoReferenceInputStep02SpendRedeemerSchema
>;
export const NoReferenceInputStep02SpendRedeemer =
  asDataType<NoReferenceInputStep02SpendRedeemer>(
    NoReferenceInputStep02SpendRedeemerSchema,
  );

// ## Step 03 — prove the missing reference input is absent from the block's
// prev ledger
//
// `missing_reference_input` is committed as a `MidgardTxInput`
// (Constr 0 [tx_id, index]), which is byte-identical to the aiken
// `OutputReference` the validator threads through. The on-chain non-membership
// key, however, is the node's `CML.TransactionInput` CBOR, produced by
// `encode_midgard_tx_input` — not this datum encoding.

export const NoReferenceInputStep03StateSchema = Data.Object({
  missing_reference_input: MidgardTxInputSchema,
  blocks_prev_utxos_root: H32Schema,
  blocks_transactions_root: H32Schema,
});
export type NoReferenceInputStep03State = Data.Static<
  typeof NoReferenceInputStep03StateSchema
>;
export const NoReferenceInputStep03State =
  asDataType<NoReferenceInputStep03State>(NoReferenceInputStep03StateSchema);

export const NoReferenceInputStep03DatumSchema = faultProofStepDatumSchema(
  NoReferenceInputStep03StateSchema,
);
export type NoReferenceInputStep03Datum = Data.Static<
  typeof NoReferenceInputStep03DatumSchema
>;
export const NoReferenceInputStep03Datum =
  asDataType<NoReferenceInputStep03Datum>(NoReferenceInputStep03DatumSchema);

export const NoReferenceInputStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  non_membership_in_ledger: NonMembershipCarriageSchema,
});
export type NoReferenceInputStep03Args = Data.Static<
  typeof NoReferenceInputStep03ArgsSchema
>;
export const NoReferenceInputStep03Args =
  asDataType<NoReferenceInputStep03Args>(NoReferenceInputStep03ArgsSchema);

export const NoReferenceInputStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep03ArgsSchema);
export type NoReferenceInputStep03SpendRedeemer = Data.Static<
  typeof NoReferenceInputStep03SpendRedeemerSchema
>;
export const NoReferenceInputStep03SpendRedeemer =
  asDataType<NoReferenceInputStep03SpendRedeemer>(
    NoReferenceInputStep03SpendRedeemerSchema,
  );

// ## Step 04 — prove the missing reference input was not produced within the
// block

export const NoReferenceInputStep04StateSchema = Data.Object({
  missing_reference_input_tx_id: H32Schema,
  blocks_transactions_root: H32Schema,
});
export type NoReferenceInputStep04State = Data.Static<
  typeof NoReferenceInputStep04StateSchema
>;
export const NoReferenceInputStep04State =
  asDataType<NoReferenceInputStep04State>(NoReferenceInputStep04StateSchema);

export const NoReferenceInputStep04DatumSchema = faultProofStepDatumSchema(
  NoReferenceInputStep04StateSchema,
);
export type NoReferenceInputStep04Datum = Data.Static<
  typeof NoReferenceInputStep04DatumSchema
>;
export const NoReferenceInputStep04Datum =
  asDataType<NoReferenceInputStep04Datum>(NoReferenceInputStep04DatumSchema);

export const NoReferenceInputStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  non_membership_in_txs: NonMembershipCarriageSchema,
});
export type NoReferenceInputStep04Args = Data.Static<
  typeof NoReferenceInputStep04ArgsSchema
>;
export const NoReferenceInputStep04Args =
  asDataType<NoReferenceInputStep04Args>(NoReferenceInputStep04ArgsSchema);

export const NoReferenceInputStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep04ArgsSchema);
export type NoReferenceInputStep04SpendRedeemer = Data.Static<
  typeof NoReferenceInputStep04SpendRedeemerSchema
>;
export const NoReferenceInputStep04SpendRedeemer =
  asDataType<NoReferenceInputStep04SpendRedeemer>(
    NoReferenceInputStep04SpendRedeemerSchema,
  );

export const NoReferenceInputStep02ForcedStateSchema = Data.Object({
  subject: NoReferenceInputVerdictSubjectSchema,
  event_key: EventKeySchema,
  event_root: H32Schema,
  event_count: Data.Integer(),
  trace_root: H32Schema,
  trace_count: Data.Integer(),
});
export type NoReferenceInputStep02ForcedState = Data.Static<
  typeof NoReferenceInputStep02ForcedStateSchema
>;
export const NoReferenceInputStep02ForcedState =
  asDataType<NoReferenceInputStep02ForcedState>(
    NoReferenceInputStep02ForcedStateSchema,
  );

export const NoReferenceInputStep02ForcedArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  reference_inputs_opening: FieldOpeningSchema,
  event_membership: EventToStepMembershipProofSchema,
});
export type NoReferenceInputStep02ForcedArgs = Data.Static<
  typeof NoReferenceInputStep02ForcedArgsSchema
>;
export const NoReferenceInputStep02ForcedArgs =
  asDataType<NoReferenceInputStep02ForcedArgs>(
    NoReferenceInputStep02ForcedArgsSchema,
  );

export const NoReferenceInputStep02ThreadStateSchema = Data.Enum([
  Data.Object({ State: NoReferenceInputStep02StateSchema }),
  Data.Object({ ForcedState: NoReferenceInputStep02ForcedStateSchema }),
]);
export type NoReferenceInputStep02ThreadState = Data.Static<
  typeof NoReferenceInputStep02ThreadStateSchema
>;
export const NoReferenceInputStep02ThreadState =
  asDataType<NoReferenceInputStep02ThreadState>(
    NoReferenceInputStep02ThreadStateSchema,
  );

export const NoReferenceInputStep02ThreadDatumSchema =
  faultProofStepDatumSchema(NoReferenceInputStep02ThreadStateSchema);
export type NoReferenceInputStep02ThreadDatum = Data.Static<
  typeof NoReferenceInputStep02ThreadDatumSchema
>;
export const NoReferenceInputStep02ThreadDatum =
  asDataType<NoReferenceInputStep02ThreadDatum>(
    NoReferenceInputStep02ThreadDatumSchema,
  );

export const NoReferenceInputStep02ThreadArgsSchema = Data.Enum([
  Data.Object({ Args: NoReferenceInputStep02ArgsSchema }),
  Data.Object({ ForcedArgs: NoReferenceInputStep02ForcedArgsSchema }),
]);
export type NoReferenceInputStep02ThreadArgs = Data.Static<
  typeof NoReferenceInputStep02ThreadArgsSchema
>;
export const NoReferenceInputStep02ThreadArgs =
  asDataType<NoReferenceInputStep02ThreadArgs>(
    NoReferenceInputStep02ThreadArgsSchema,
  );

export const NoReferenceInputStep02ThreadSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep02ThreadArgsSchema);
export type NoReferenceInputStep02ThreadSpendRedeemer = Data.Static<
  typeof NoReferenceInputStep02ThreadSpendRedeemerSchema
>;
export const NoReferenceInputStep02ThreadSpendRedeemer =
  asDataType<NoReferenceInputStep02ThreadSpendRedeemer>(
    NoReferenceInputStep02ThreadSpendRedeemerSchema,
  );

export const NoReferenceInputStep03ForcedStateSchema = Data.Object({
  event_key: EventKeySchema,
  trace_root: H32Schema,
  trace_count: Data.Integer(),
  step_index: Data.Integer(),
  selected_input: Data.Nullable(MidgardTxInputSchema),
});
export type NoReferenceInputStep03ForcedState = Data.Static<
  typeof NoReferenceInputStep03ForcedStateSchema
>;
export const NoReferenceInputStep03ForcedState =
  asDataType<NoReferenceInputStep03ForcedState>(
    NoReferenceInputStep03ForcedStateSchema,
  );

export const NoReferenceInputStep03ForcedArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  transition_membership: TransitionTraceMembershipProofSchema,
});
export type NoReferenceInputStep03ForcedArgs = Data.Static<
  typeof NoReferenceInputStep03ForcedArgsSchema
>;
export const NoReferenceInputStep03ForcedArgs =
  asDataType<NoReferenceInputStep03ForcedArgs>(
    NoReferenceInputStep03ForcedArgsSchema,
  );

export const NoReferenceInputStep03ThreadStateSchema = Data.Enum([
  Data.Object({ State: NoReferenceInputStep03StateSchema }),
  Data.Object({ ForcedState: NoReferenceInputStep03ForcedStateSchema }),
]);
export type NoReferenceInputStep03ThreadState = Data.Static<
  typeof NoReferenceInputStep03ThreadStateSchema
>;
export const NoReferenceInputStep03ThreadState =
  asDataType<NoReferenceInputStep03ThreadState>(
    NoReferenceInputStep03ThreadStateSchema,
  );

export const NoReferenceInputStep03ThreadDatumSchema =
  faultProofStepDatumSchema(NoReferenceInputStep03ThreadStateSchema);
export type NoReferenceInputStep03ThreadDatum = Data.Static<
  typeof NoReferenceInputStep03ThreadDatumSchema
>;
export const NoReferenceInputStep03ThreadDatum =
  asDataType<NoReferenceInputStep03ThreadDatum>(
    NoReferenceInputStep03ThreadDatumSchema,
  );

export const NoReferenceInputStep03ThreadArgsSchema = Data.Enum([
  Data.Object({ Args: NoReferenceInputStep03ArgsSchema }),
  Data.Object({ ForcedArgs: NoReferenceInputStep03ForcedArgsSchema }),
]);
export type NoReferenceInputStep03ThreadArgs = Data.Static<
  typeof NoReferenceInputStep03ThreadArgsSchema
>;
export const NoReferenceInputStep03ThreadArgs =
  asDataType<NoReferenceInputStep03ThreadArgs>(
    NoReferenceInputStep03ThreadArgsSchema,
  );

export const NoReferenceInputStep03ThreadSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep03ThreadArgsSchema);
export type NoReferenceInputStep03ThreadSpendRedeemer = Data.Static<
  typeof NoReferenceInputStep03ThreadSpendRedeemerSchema
>;
export const NoReferenceInputStep03ThreadSpendRedeemer =
  asDataType<NoReferenceInputStep03ThreadSpendRedeemer>(
    NoReferenceInputStep03ThreadSpendRedeemerSchema,
  );

export const NoReferenceInputStep04ForcedStateSchema = Data.Object({
  selected_input: Data.Nullable(MidgardTxInputSchema),
  pre_utxos_root: H32Schema,
});
export type NoReferenceInputStep04ForcedState = Data.Static<
  typeof NoReferenceInputStep04ForcedStateSchema
>;
export const NoReferenceInputStep04ForcedState =
  asDataType<NoReferenceInputStep04ForcedState>(
    NoReferenceInputStep04ForcedStateSchema,
  );

export const NoReferenceInputStep04ForcedArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  membership: Data.Nullable(
    Data.Object({ value_hash: H32Schema, proof: ProofSchema }),
  ),
});
export type NoReferenceInputStep04ForcedArgs = Data.Static<
  typeof NoReferenceInputStep04ForcedArgsSchema
>;
export const NoReferenceInputStep04ForcedArgs =
  asDataType<NoReferenceInputStep04ForcedArgs>(
    NoReferenceInputStep04ForcedArgsSchema,
  );

export const NoReferenceInputStep04ThreadStateSchema = Data.Enum([
  Data.Object({ State: NoReferenceInputStep04StateSchema }),
  Data.Object({ ForcedState: NoReferenceInputStep04ForcedStateSchema }),
]);
export type NoReferenceInputStep04ThreadState = Data.Static<
  typeof NoReferenceInputStep04ThreadStateSchema
>;
export const NoReferenceInputStep04ThreadState =
  asDataType<NoReferenceInputStep04ThreadState>(
    NoReferenceInputStep04ThreadStateSchema,
  );

export const NoReferenceInputStep04ThreadDatumSchema =
  faultProofStepDatumSchema(NoReferenceInputStep04ThreadStateSchema);
export type NoReferenceInputStep04ThreadDatum = Data.Static<
  typeof NoReferenceInputStep04ThreadDatumSchema
>;
export const NoReferenceInputStep04ThreadDatum =
  asDataType<NoReferenceInputStep04ThreadDatum>(
    NoReferenceInputStep04ThreadDatumSchema,
  );

export const NoReferenceInputStep04ThreadArgsSchema = Data.Enum([
  Data.Object({ Args: NoReferenceInputStep04ArgsSchema }),
  Data.Object({ ForcedArgs: NoReferenceInputStep04ForcedArgsSchema }),
]);
export type NoReferenceInputStep04ThreadArgs = Data.Static<
  typeof NoReferenceInputStep04ThreadArgsSchema
>;
export const NoReferenceInputStep04ThreadArgs =
  asDataType<NoReferenceInputStep04ThreadArgs>(
    NoReferenceInputStep04ThreadArgsSchema,
  );

export const NoReferenceInputStep04ThreadSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep04ThreadArgsSchema);
export type NoReferenceInputStep04ThreadSpendRedeemer = Data.Static<
  typeof NoReferenceInputStep04ThreadSpendRedeemerSchema
>;
export const NoReferenceInputStep04ThreadSpendRedeemer =
  asDataType<NoReferenceInputStep04ThreadSpendRedeemer>(
    NoReferenceInputStep04ThreadSpendRedeemerSchema,
  );
