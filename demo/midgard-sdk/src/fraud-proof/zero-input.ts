/**
 * ⚠️ **STALE AS OF #575 — do not build a datum or redeemer from this module
 * and expect chain to accept it. Owner: #579.** The rebind, its three concrete
 * divergences, and why they are not re-derived in this lane are explained once
 * in `docs/fault-proofs/offchain-builder-staleness-575.md`.
 */

import {
  deriveMidgardNativeFieldCollectionV1,
  encodeCbor,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";

import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxBodyCompact,
  type NativeTxBodyCompact as NativeTxBodyCompactData,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";

export const ZeroInputStep01DatumSchema = faultProofStepDatumSchema(Data.Any());
export type ZeroInputStep01Datum = Data.Static<
  typeof ZeroInputStep01DatumSchema
>;
export const ZeroInputStep01Datum =
  ZeroInputStep01DatumSchema as unknown as ZeroInputStep01Datum;

export const ZeroInputStep01SpendRedeemerSchema = faultProofStepRedeemerSchema(
  NativeTxInclusionCarriageSchema,
);
export type ZeroInputStep01SpendRedeemer = Data.Static<
  typeof ZeroInputStep01SpendRedeemerSchema
>;
export const ZeroInputStep01SpendRedeemer =
  ZeroInputStep01SpendRedeemerSchema as unknown as ZeroInputStep01SpendRedeemer;

export const ZeroInputStep02StateSchema = Data.Object({
  bad_tx_spend_inputs_hash: H32Schema,
});
export type ZeroInputStep02State = Data.Static<
  typeof ZeroInputStep02StateSchema
>;
export const ZeroInputStep02State =
  ZeroInputStep02StateSchema as unknown as ZeroInputStep02State;

export const ZeroInputStep02DatumSchema = faultProofStepDatumSchema(
  ZeroInputStep02StateSchema,
);
export type ZeroInputStep02Datum = Data.Static<
  typeof ZeroInputStep02DatumSchema
>;
export const ZeroInputStep02Datum =
  ZeroInputStep02DatumSchema as unknown as ZeroInputStep02Datum;

export const ZeroInputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type ZeroInputStep02Args = Data.Static<typeof ZeroInputStep02ArgsSchema>;
export const ZeroInputStep02Args =
  ZeroInputStep02ArgsSchema as unknown as ZeroInputStep02Args;

export const ZeroInputStep02SpendRedeemerSchema = faultProofStepRedeemerSchema(
  ZeroInputStep02ArgsSchema,
);
export type ZeroInputStep02SpendRedeemer = Data.Static<
  typeof ZeroInputStep02SpendRedeemerSchema
>;
export const ZeroInputStep02SpendRedeemer =
  ZeroInputStep02SpendRedeemerSchema as unknown as ZeroInputStep02SpendRedeemer;

export {
  FaultProofStepCancel as ZeroInputStepCancel,
  FaultProofStepCancelSchema as ZeroInputStepCancelSchema,
  NativeTxInclusionArgs as ZeroInputTxInclusionArgs,
  NativeTxInclusionArgsSchema as ZeroInputTxInclusionArgsSchema,
};

/**
 * The `spend_inputs_hash` a native transaction body carries when it spends
 * nothing, **as the transaction codec computes it today**. Derived from the same
 * codec that materializes native V1 transactions, so it cannot drift from the
 * hashes the fixtures and builders in this repository actually produce.
 *
 * **Residual. Two owners, and they have to land in order.**
 * `docs/spec/midgard-tx.md` §4 re-defines every field commitment as a flat
 * `blake2b_256` over the §5.1 envelope, which makes the empty field
 * `blake2b_256(#"80")` — `EMPTY_FIELD_COMMITMENT_HEX_V1` in
 * `native-tx-field-access-v1.ts`.
 *
 * That is what the **source** of `fraud_proofs/zero_input/step_02` requires
 * (`empty_spend_inputs_hash = native_tx_field_access_v1.empty_field_commitment`);
 * it is not yet what this constant computes.
 *
 * **What #569 did and did not change.** #569 added the flat per-field producers
 * — `codec/native-tx-field-items-v1.ts`, whose
 * `midgardFieldCommitmentForFieldV1` *is* the §4 flat commitment — and pinned
 * all nine against the Aiken producers with cross-language vectors. It
 * deliberately did **not** re-point the nine-field *consumer* path:
 * `deriveNativeTxBodyCompact` / `deriveNativeTxWitnessSetCompact` and their
 * roughly twenty downstream callers (validation machine, watcher indexer, node,
 * fault-proof tooling) still route through the retired counted
 * `deriveMidgardNativeFieldCollectionV1`, which is what the expression below
 * calls and why it still yields `eb25ed4a…`.
 *
 * **Owner of that consumer path: #585.** It is not a family-rebind lane's to
 * make — the swap also retires the counted per-item publication chain and needs
 * the §5.3/§5.6 item grammar re-pointed in the producers first. The residual
 * note on `deriveNativeTxBodyCompact` (`@al-ft/midgard-core`'s
 * `codec/native-body.ts`) is the full one.
 *
 * So the re-pin still needs two halves that must land together:
 *
 *   1. the nine-field consumer path swaps to the flat commitment (#585),
 *      which is what makes this constant `45b0cfc2…`; and
 *   2. the blueprint is regenerated so the deployed script agrees (#579, the
 *      identity re-derivation batch).
 *
 * Moving this constant on its own breaks the fault-proof emulator in one
 * direction or the other, and which direction depends on an artifact no commit
 * pins: `onchain/aiken/plutus.json` is an untracked build output, so whichever
 * script the emulator executes is simply whatever was last built there. A
 * blueprint built before #567 tests the counted hash and agrees with the value
 * below; one built from current source tests the flat hash and does not. That
 * ambiguity is precisely why the re-pin belongs to the batch that regenerates
 * the blueprint rather than to any lane that can only move one side of it.
 * `native-tx-field-access-v1` carries the flat constant meanwhile, and
 * `tests/native-tx-field-access-v1.test.ts` asserts the gap so it cannot be
 * forgotten.
 */
export const EMPTY_SPEND_INPUTS_HASH: string =
  deriveMidgardNativeFieldCollectionV1({
    fieldIndex: 0,
    preimageCbor: encodeCbor([]),
  }).commitment.toString("hex");

/**
 * A transaction violates the "at least one input" ledger rule when its body
 * commits to an empty spend-inputs list.
 */
export const nativeTxBodyHasZeroInputViolation = ({
  txBody,
}: {
  readonly txBody: NativeTxBodyCompactData;
}): boolean => {
  const roundTripped = Data.from(
    Data.to(txBody, NativeTxBodyCompact),
    NativeTxBodyCompact,
  );
  return roundTripped.spend_inputs_hash === EMPTY_SPEND_INPUTS_HASH;
};
