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
 * That is what the **source** of `fraud_proofs/zero_input/step_02` requires;
 * it is not yet what any script requires in practice. `onchain/aiken/plutus.json`
 * is a build artifact, not a tracked file, and the blueprint currently on disk
 * predates the flat swap: it carries the retired counted-scheme
 * `eb25ed4a…` and no occurrence of `45b0cfc2…`. Every emulator-backed suite
 * in this repository — the fault-proof tests above all — executes *that*
 * blueprint, which is why they agree with the constant below rather than with
 * the Aiken source.
 *
 * So the re-pin needs both halves, in sequence:
 *
 *   1. the nine per-field producers in `@al-ft/midgard-core` swap to the flat
 *      commitment (#569), which is what this constant is derived from; and
 *   2. the blueprint is regenerated so the deployed script agrees (#579, the
 *      identity re-derivation batch, sequenced after the #575–#578 family
 *      rebind lanes).
 *
 * Re-pointing this constant at #569 alone would break the fault-proof emulator:
 * the predicate would expect the flat hash while the script in the blueprint
 * still tests the counted one. Re-pointing it before #569 would disagree with
 * every transaction the codec builds. `native-tx-field-access-v1` carries the
 * flat constant meanwhile, and `tests/native-tx-field-access-v1.test.ts`
 * asserts the gap so it cannot be forgotten.
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
