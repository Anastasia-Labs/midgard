/**
 * Re-derived onto the flat field commitments by #604 (the #575 off-chain builder
 * remediation): thread state carries the §2.5 anchor rather than a per-field
 * collection commitment, and a step redeemer carries a `FieldOpeningV1` rather
 * than a reproduced `..._preimage: List<…>`. The rebind is explained once in
 * `docs/fault-proofs/offchain-builder-staleness-575.md`.
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
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
  NativeTxInclusionCarriageSchema,
  NonMembershipCarriageSchema,
} from "./native.js";

/**
 * Non-existent-input (a.k.a. `no_input`) fault proof — proves a Midgard block
 * includes a native L2 transaction that spends an input which never existed in
 * the block's prev ledger, and whose producing transaction is not in the block
 * either.
 *
 * This is the native transaction-root inclusion path (the same one double-spend
 * uses): the bad transaction is committed by the block's `transactions_root`
 * rather than as PlutusData. Schemas reuse the shared `native.js` helpers so the
 * step datum/redeemer envelope matches every other fault-proof family.
 */

export const NonExistentInputTxInclusionArgsSchema =
  NativeTxInclusionArgsSchema;
export type NonExistentInputTxInclusionArgs = NativeTxInclusionArgs;
export const NonExistentInputTxInclusionArgs =
  NativeTxInclusionArgs as unknown as NonExistentInputTxInclusionArgs;

export const NonExistentInputStepCancelSchema = FaultProofStepCancelSchema;
export type NonExistentInputStepCancel = FaultProofStepCancel;
export const NonExistentInputStepCancel =
  FaultProofStepCancel as unknown as NonExistentInputStepCancel;

// ## Step 01 — verify inclusion of the bad transaction
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so it
// is read with the generic computation-thread step datum. Spending it requires
// only the native-tx inclusion redeemer; the produced UTxO carries step-02.

export const NonExistentInputVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const NonExistentInputStep01SourceSchema = Data.Enum([
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
export const NonExistentInputForcedSourcePayloadSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
  direction: Data.Integer(),
});

export const NonExistentInputStep01ArgsSchema = Data.Object({
  source: NonExistentInputStep01SourceSchema,
});

export const NonExistentInputStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NonExistentInputStep01ArgsSchema);
export type NonExistentInputStep01SpendRedeemer = Data.Static<
  typeof NonExistentInputStep01SpendRedeemerSchema
>;
export const NonExistentInputStep01SpendRedeemer =
  asDataType<NonExistentInputStep01SpendRedeemer>(
    NonExistentInputStep01SpendRedeemerSchema,
  );

// ## Step 02 — provide the spend-inputs preimage and select the bad input

/**
 * Mirrors `midgard/fraud_proofs/no_input/step_02.State`. #604: the retired
 * `bad_tx_inputs_hash` became the §2.5 anchor `bad_tx_id`; the two ledger roots
 * are unchanged.
 */
export const NonExistentInputStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  blocks_prev_utxos_root: H32Schema,
  blocks_transactions_root: H32Schema,
});
export type NonExistentInputStep02State = Data.Static<
  typeof NonExistentInputStep02StateSchema
>;
export const NonExistentInputStep02State =
  asDataType<NonExistentInputStep02State>(NonExistentInputStep02StateSchema);

export const NonExistentInputStep02DatumSchema = faultProofStepDatumSchema(
  NonExistentInputStep02StateSchema,
);
export type NonExistentInputStep02Datum = Data.Static<
  typeof NonExistentInputStep02DatumSchema
>;
export const NonExistentInputStep02Datum =
  asDataType<NonExistentInputStep02Datum>(NonExistentInputStep02DatumSchema);

/** Mirrors `midgard/fraud_proofs/no_input/step_02.Args`. */
export const NonExistentInputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningSchema,
  bad_input_index: Data.Integer(),
});
export type NonExistentInputStep02Args = Data.Static<
  typeof NonExistentInputStep02ArgsSchema
>;
export const NonExistentInputStep02Args =
  asDataType<NonExistentInputStep02Args>(NonExistentInputStep02ArgsSchema);

export const NonExistentInputStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NonExistentInputStep02ArgsSchema);
export type NonExistentInputStep02SpendRedeemer = Data.Static<
  typeof NonExistentInputStep02SpendRedeemerSchema
>;
export const NonExistentInputStep02SpendRedeemer =
  asDataType<NonExistentInputStep02SpendRedeemer>(
    NonExistentInputStep02SpendRedeemerSchema,
  );

// ## Step 03 — prove the missing input is absent from the block's prev ledger
//
// `missing_input` is committed as a `MidgardTxInput` (Constr 0 [tx_id, index]),
// which is byte-identical to the aiken `OutputReference` the validator threads
// through. The on-chain non-membership key, however, is the node's
// `CML.TransactionInput` CBOR, produced by `encode_midgard_tx_input` — not this
// datum encoding.

export const NonExistentInputStep03StateSchema = Data.Object({
  missing_input: MidgardTxInputSchema,
  blocks_prev_utxos_root: H32Schema,
  blocks_transactions_root: H32Schema,
});
export type NonExistentInputStep03State = Data.Static<
  typeof NonExistentInputStep03StateSchema
>;
export const NonExistentInputStep03State =
  asDataType<NonExistentInputStep03State>(NonExistentInputStep03StateSchema);

export const NonExistentInputStep03DatumSchema = faultProofStepDatumSchema(
  NonExistentInputStep03StateSchema,
);
export type NonExistentInputStep03Datum = Data.Static<
  typeof NonExistentInputStep03DatumSchema
>;
export const NonExistentInputStep03Datum =
  asDataType<NonExistentInputStep03Datum>(NonExistentInputStep03DatumSchema);

export const NonExistentInputStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  non_membership_in_ledger: NonMembershipCarriageSchema,
});
export type NonExistentInputStep03Args = Data.Static<
  typeof NonExistentInputStep03ArgsSchema
>;
export const NonExistentInputStep03Args =
  asDataType<NonExistentInputStep03Args>(NonExistentInputStep03ArgsSchema);

export const NonExistentInputStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NonExistentInputStep03ArgsSchema);
export type NonExistentInputStep03SpendRedeemer = Data.Static<
  typeof NonExistentInputStep03SpendRedeemerSchema
>;
export const NonExistentInputStep03SpendRedeemer =
  asDataType<NonExistentInputStep03SpendRedeemer>(
    NonExistentInputStep03SpendRedeemerSchema,
  );

// ## Step 04 — prove the missing input was not produced within the block

export const NonExistentInputStep04StateSchema = Data.Object({
  missing_input_tx_id: H32Schema,
  blocks_transactions_root: H32Schema,
});
export type NonExistentInputStep04State = Data.Static<
  typeof NonExistentInputStep04StateSchema
>;
export const NonExistentInputStep04State =
  asDataType<NonExistentInputStep04State>(NonExistentInputStep04StateSchema);

export const NonExistentInputStep04DatumSchema = faultProofStepDatumSchema(
  NonExistentInputStep04StateSchema,
);
export type NonExistentInputStep04Datum = Data.Static<
  typeof NonExistentInputStep04DatumSchema
>;
export const NonExistentInputStep04Datum =
  asDataType<NonExistentInputStep04Datum>(NonExistentInputStep04DatumSchema);

export const NonExistentInputStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  non_membership_in_txs: NonMembershipCarriageSchema,
});
export type NonExistentInputStep04Args = Data.Static<
  typeof NonExistentInputStep04ArgsSchema
>;
export const NonExistentInputStep04Args =
  asDataType<NonExistentInputStep04Args>(NonExistentInputStep04ArgsSchema);

export const NonExistentInputStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NonExistentInputStep04ArgsSchema);
export type NonExistentInputStep04SpendRedeemer = Data.Static<
  typeof NonExistentInputStep04SpendRedeemerSchema
>;
export const NonExistentInputStep04SpendRedeemer =
  asDataType<NonExistentInputStep04SpendRedeemer>(
    NonExistentInputStep04SpendRedeemerSchema,
  );

export const NonExistentInputStep02ForcedStateSchema = Data.Object({
  subject: NonExistentInputVerdictSubjectSchema,
  event_key: EventKeySchema,
  event_root: H32Schema,
  event_count: Data.Integer(),
  trace_root: H32Schema,
  trace_count: Data.Integer(),
});
export type NonExistentInputStep02ForcedState = Data.Static<
  typeof NonExistentInputStep02ForcedStateSchema
>;
export const NonExistentInputStep02ForcedState =
  asDataType<NonExistentInputStep02ForcedState>(
    NonExistentInputStep02ForcedStateSchema,
  );

export const NonExistentInputStep02ForcedArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningSchema,
  event_membership: EventToStepMembershipProofSchema,
});
export type NonExistentInputStep02ForcedArgs = Data.Static<
  typeof NonExistentInputStep02ForcedArgsSchema
>;
export const NonExistentInputStep02ForcedArgs =
  asDataType<NonExistentInputStep02ForcedArgs>(
    NonExistentInputStep02ForcedArgsSchema,
  );

export const NonExistentInputStep02ThreadStateSchema = Data.Enum([
  Data.Object({ State: NonExistentInputStep02StateSchema }),
  Data.Object({ ForcedState: NonExistentInputStep02ForcedStateSchema }),
]);
export type NonExistentInputStep02ThreadState = Data.Static<
  typeof NonExistentInputStep02ThreadStateSchema
>;
export const NonExistentInputStep02ThreadState =
  asDataType<NonExistentInputStep02ThreadState>(
    NonExistentInputStep02ThreadStateSchema,
  );

export const NonExistentInputStep02ThreadDatumSchema =
  faultProofStepDatumSchema(NonExistentInputStep02ThreadStateSchema);
export type NonExistentInputStep02ThreadDatum = Data.Static<
  typeof NonExistentInputStep02ThreadDatumSchema
>;
export const NonExistentInputStep02ThreadDatum =
  asDataType<NonExistentInputStep02ThreadDatum>(
    NonExistentInputStep02ThreadDatumSchema,
  );

export const NonExistentInputStep02ThreadArgsSchema = Data.Enum([
  Data.Object({ Args: NonExistentInputStep02ArgsSchema }),
  Data.Object({ ForcedArgs: NonExistentInputStep02ForcedArgsSchema }),
]);
export type NonExistentInputStep02ThreadArgs = Data.Static<
  typeof NonExistentInputStep02ThreadArgsSchema
>;
export const NonExistentInputStep02ThreadArgs =
  asDataType<NonExistentInputStep02ThreadArgs>(
    NonExistentInputStep02ThreadArgsSchema,
  );

export const NonExistentInputStep02ThreadSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NonExistentInputStep02ThreadArgsSchema);
export type NonExistentInputStep02ThreadSpendRedeemer = Data.Static<
  typeof NonExistentInputStep02ThreadSpendRedeemerSchema
>;
export const NonExistentInputStep02ThreadSpendRedeemer =
  asDataType<NonExistentInputStep02ThreadSpendRedeemer>(
    NonExistentInputStep02ThreadSpendRedeemerSchema,
  );

export const NonExistentInputStep03ForcedStateSchema = Data.Object({
  event_key: EventKeySchema,
  trace_root: H32Schema,
  trace_count: Data.Integer(),
  step_index: Data.Integer(),
  selected_input: Data.Nullable(MidgardTxInputSchema),
});
export type NonExistentInputStep03ForcedState = Data.Static<
  typeof NonExistentInputStep03ForcedStateSchema
>;
export const NonExistentInputStep03ForcedState =
  asDataType<NonExistentInputStep03ForcedState>(
    NonExistentInputStep03ForcedStateSchema,
  );

export const NonExistentInputStep03ForcedArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  transition_membership: TransitionTraceMembershipProofSchema,
});
export type NonExistentInputStep03ForcedArgs = Data.Static<
  typeof NonExistentInputStep03ForcedArgsSchema
>;
export const NonExistentInputStep03ForcedArgs =
  asDataType<NonExistentInputStep03ForcedArgs>(
    NonExistentInputStep03ForcedArgsSchema,
  );

export const NonExistentInputStep03ThreadStateSchema = Data.Enum([
  Data.Object({ State: NonExistentInputStep03StateSchema }),
  Data.Object({ ForcedState: NonExistentInputStep03ForcedStateSchema }),
]);
export type NonExistentInputStep03ThreadState = Data.Static<
  typeof NonExistentInputStep03ThreadStateSchema
>;
export const NonExistentInputStep03ThreadState =
  asDataType<NonExistentInputStep03ThreadState>(
    NonExistentInputStep03ThreadStateSchema,
  );

export const NonExistentInputStep03ThreadDatumSchema =
  faultProofStepDatumSchema(NonExistentInputStep03ThreadStateSchema);
export type NonExistentInputStep03ThreadDatum = Data.Static<
  typeof NonExistentInputStep03ThreadDatumSchema
>;
export const NonExistentInputStep03ThreadDatum =
  asDataType<NonExistentInputStep03ThreadDatum>(
    NonExistentInputStep03ThreadDatumSchema,
  );

export const NonExistentInputStep03ThreadArgsSchema = Data.Enum([
  Data.Object({ Args: NonExistentInputStep03ArgsSchema }),
  Data.Object({ ForcedArgs: NonExistentInputStep03ForcedArgsSchema }),
]);
export type NonExistentInputStep03ThreadArgs = Data.Static<
  typeof NonExistentInputStep03ThreadArgsSchema
>;
export const NonExistentInputStep03ThreadArgs =
  asDataType<NonExistentInputStep03ThreadArgs>(
    NonExistentInputStep03ThreadArgsSchema,
  );

export const NonExistentInputStep03ThreadSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NonExistentInputStep03ThreadArgsSchema);
export type NonExistentInputStep03ThreadSpendRedeemer = Data.Static<
  typeof NonExistentInputStep03ThreadSpendRedeemerSchema
>;
export const NonExistentInputStep03ThreadSpendRedeemer =
  asDataType<NonExistentInputStep03ThreadSpendRedeemer>(
    NonExistentInputStep03ThreadSpendRedeemerSchema,
  );

export const NonExistentInputStep04ForcedStateSchema = Data.Object({
  selected_input: Data.Nullable(MidgardTxInputSchema),
  pre_utxos_root: H32Schema,
});
export type NonExistentInputStep04ForcedState = Data.Static<
  typeof NonExistentInputStep04ForcedStateSchema
>;
export const NonExistentInputStep04ForcedState =
  asDataType<NonExistentInputStep04ForcedState>(
    NonExistentInputStep04ForcedStateSchema,
  );

export const NonExistentInputStep04ForcedArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  membership: Data.Nullable(
    Data.Object({ value_hash: H32Schema, proof: ProofSchema }),
  ),
});
export type NonExistentInputStep04ForcedArgs = Data.Static<
  typeof NonExistentInputStep04ForcedArgsSchema
>;
export const NonExistentInputStep04ForcedArgs =
  asDataType<NonExistentInputStep04ForcedArgs>(
    NonExistentInputStep04ForcedArgsSchema,
  );

export const NonExistentInputStep04ThreadStateSchema = Data.Enum([
  Data.Object({ State: NonExistentInputStep04StateSchema }),
  Data.Object({ ForcedState: NonExistentInputStep04ForcedStateSchema }),
]);
export type NonExistentInputStep04ThreadState = Data.Static<
  typeof NonExistentInputStep04ThreadStateSchema
>;
export const NonExistentInputStep04ThreadState =
  asDataType<NonExistentInputStep04ThreadState>(
    NonExistentInputStep04ThreadStateSchema,
  );

export const NonExistentInputStep04ThreadDatumSchema =
  faultProofStepDatumSchema(NonExistentInputStep04ThreadStateSchema);
export type NonExistentInputStep04ThreadDatum = Data.Static<
  typeof NonExistentInputStep04ThreadDatumSchema
>;
export const NonExistentInputStep04ThreadDatum =
  asDataType<NonExistentInputStep04ThreadDatum>(
    NonExistentInputStep04ThreadDatumSchema,
  );

export const NonExistentInputStep04ThreadArgsSchema = Data.Enum([
  Data.Object({ Args: NonExistentInputStep04ArgsSchema }),
  Data.Object({ ForcedArgs: NonExistentInputStep04ForcedArgsSchema }),
]);
export type NonExistentInputStep04ThreadArgs = Data.Static<
  typeof NonExistentInputStep04ThreadArgsSchema
>;
export const NonExistentInputStep04ThreadArgs =
  asDataType<NonExistentInputStep04ThreadArgs>(
    NonExistentInputStep04ThreadArgsSchema,
  );

export const NonExistentInputStep04ThreadSpendRedeemerSchema =
  faultProofStepRedeemerSchema(NonExistentInputStep04ThreadArgsSchema);
export type NonExistentInputStep04ThreadSpendRedeemer = Data.Static<
  typeof NonExistentInputStep04ThreadSpendRedeemerSchema
>;
export const NonExistentInputStep04ThreadSpendRedeemer =
  asDataType<NonExistentInputStep04ThreadSpendRedeemer>(
    NonExistentInputStep04ThreadSpendRedeemerSchema,
  );
