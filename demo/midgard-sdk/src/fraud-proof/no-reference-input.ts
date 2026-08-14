/**
 * Re-derived onto the flat field commitments by #604 (the #575 off-chain builder
 * remediation): thread state carries the §2.5 anchor rather than a per-field
 * collection commitment, and a step redeemer carries a `FieldOpeningV1` rather
 * than a reproduced `..._preimage: List<…>`. The rebind is explained once in
 * `docs/fault-proofs/offchain-builder-staleness-575.md`.
 */

import { Data } from "@lucid-evolution/lucid";

import { H32Schema, ProofSchema } from "@/common.js";

import { FieldOpeningV1Schema } from "./field-opening-v1.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MidgardTxInputSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
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
  NativeTxInclusionArgsSchema;
export type NoReferenceInputTxInclusionArgs = NativeTxInclusionArgs;
export const NoReferenceInputTxInclusionArgs =
  NativeTxInclusionArgs as unknown as NoReferenceInputTxInclusionArgs;

export const NoReferenceInputStepCancelSchema = FaultProofStepCancelSchema;
export type NoReferenceInputStepCancel = FaultProofStepCancel;
export const NoReferenceInputStepCancel =
  FaultProofStepCancel as unknown as NoReferenceInputStepCancel;

// ## Step 01 — verify inclusion of the bad transaction
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so it
// is read with the generic computation-thread step datum. Spending it requires
// only the native-tx inclusion redeemer; the produced UTxO carries step-02.

export const NoReferenceInputStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputTxInclusionArgsSchema);
export type NoReferenceInputStep01SpendRedeemer = Data.Static<
  typeof NoReferenceInputStep01SpendRedeemerSchema
>;
export const NoReferenceInputStep01SpendRedeemer =
  NoReferenceInputStep01SpendRedeemerSchema as unknown as NoReferenceInputStep01SpendRedeemer;

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
  NoReferenceInputStep02StateSchema as unknown as NoReferenceInputStep02State;

export const NoReferenceInputStep02DatumSchema = faultProofStepDatumSchema(
  NoReferenceInputStep02StateSchema,
);
export type NoReferenceInputStep02Datum = Data.Static<
  typeof NoReferenceInputStep02DatumSchema
>;
export const NoReferenceInputStep02Datum =
  NoReferenceInputStep02DatumSchema as unknown as NoReferenceInputStep02Datum;

/**
 * Mirrors `midgard/fraud_proofs/no_reference_input/step_02.Args`. Field 1, not
 * field 0 — the index is a literal at the on-chain call site, and §4 removed the
 * domain separation that used to make the two distinguishable by hash alone.
 */
export const NoReferenceInputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  reference_inputs_opening: FieldOpeningV1Schema,
  bad_reference_input_index: Data.Integer(),
});
export type NoReferenceInputStep02Args = Data.Static<
  typeof NoReferenceInputStep02ArgsSchema
>;
export const NoReferenceInputStep02Args =
  NoReferenceInputStep02ArgsSchema as unknown as NoReferenceInputStep02Args;

export const NoReferenceInputStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep02ArgsSchema);
export type NoReferenceInputStep02SpendRedeemer = Data.Static<
  typeof NoReferenceInputStep02SpendRedeemerSchema
>;
export const NoReferenceInputStep02SpendRedeemer =
  NoReferenceInputStep02SpendRedeemerSchema as unknown as NoReferenceInputStep02SpendRedeemer;

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
  NoReferenceInputStep03StateSchema as unknown as NoReferenceInputStep03State;

export const NoReferenceInputStep03DatumSchema = faultProofStepDatumSchema(
  NoReferenceInputStep03StateSchema,
);
export type NoReferenceInputStep03Datum = Data.Static<
  typeof NoReferenceInputStep03DatumSchema
>;
export const NoReferenceInputStep03Datum =
  NoReferenceInputStep03DatumSchema as unknown as NoReferenceInputStep03Datum;

export const NoReferenceInputStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  non_membership_proof_in_ledger: ProofSchema,
  non_membership_proof_script_redeemer_index: Data.Integer(),
});
export type NoReferenceInputStep03Args = Data.Static<
  typeof NoReferenceInputStep03ArgsSchema
>;
export const NoReferenceInputStep03Args =
  NoReferenceInputStep03ArgsSchema as unknown as NoReferenceInputStep03Args;

export const NoReferenceInputStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep03ArgsSchema);
export type NoReferenceInputStep03SpendRedeemer = Data.Static<
  typeof NoReferenceInputStep03SpendRedeemerSchema
>;
export const NoReferenceInputStep03SpendRedeemer =
  NoReferenceInputStep03SpendRedeemerSchema as unknown as NoReferenceInputStep03SpendRedeemer;

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
  NoReferenceInputStep04StateSchema as unknown as NoReferenceInputStep04State;

export const NoReferenceInputStep04DatumSchema = faultProofStepDatumSchema(
  NoReferenceInputStep04StateSchema,
);
export type NoReferenceInputStep04Datum = Data.Static<
  typeof NoReferenceInputStep04DatumSchema
>;
export const NoReferenceInputStep04Datum =
  NoReferenceInputStep04DatumSchema as unknown as NoReferenceInputStep04Datum;

export const NoReferenceInputStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  non_membership_proof_in_txs: ProofSchema,
  non_membership_proof_script_redeemer_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type NoReferenceInputStep04Args = Data.Static<
  typeof NoReferenceInputStep04ArgsSchema
>;
export const NoReferenceInputStep04Args =
  NoReferenceInputStep04ArgsSchema as unknown as NoReferenceInputStep04Args;

export const NoReferenceInputStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NoReferenceInputStep04ArgsSchema);
export type NoReferenceInputStep04SpendRedeemer = Data.Static<
  typeof NoReferenceInputStep04SpendRedeemerSchema
>;
export const NoReferenceInputStep04SpendRedeemer =
  NoReferenceInputStep04SpendRedeemerSchema as unknown as NoReferenceInputStep04SpendRedeemer;
