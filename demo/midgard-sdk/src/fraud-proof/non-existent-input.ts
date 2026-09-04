/**
 * Re-derived onto the flat field commitments by #604 (the #575 off-chain builder
 * remediation): thread state carries the §2.5 anchor rather than a per-field
 * collection commitment, and a step redeemer carries a `FieldOpeningV1` rather
 * than a reproduced `..._preimage: List<…>`. The rebind is explained once in
 * `docs/fault-proofs/offchain-builder-staleness-575.md`.
 */

import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
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

export const NonExistentInputStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeTxInclusionCarriageSchema);
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
