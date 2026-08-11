/**
 * ⚠️ **STALE AS OF #575 — do not build a datum or redeemer from this module
 * and expect chain to accept it. Owner: #579.** The rebind, its three concrete
 * divergences, and why they are not re-derived in this lane are explained once
 * in `docs/fault-proofs/offchain-builder-staleness-575.md`.
 */

import { MIDGARD_EMPTY_FIELD_COMMITMENT_V1 } from "@al-ft/midgard-core";
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
 * nothing: `docs/spec/midgard-tx.md` §4's flat commitment of the empty §5.1
 * field, i.e. `blake2b_256(#"80")`.
 *
 * Taken from `midgard-core`'s `MIDGARD_EMPTY_FIELD_COMMITMENT_V1` rather than
 * written out as a literal, so it cannot drift from the hashes the codec,
 * fixtures and builders in this repository actually produce. Its Aiken twin is
 * `native_tx_field_access_v1.empty_field_commitment`, which is what
 * `fraud_proofs/zero_input/step_02` compares against; the cross-language golden
 * vector proves the two equal.
 *
 * §4's plain hashing carries no field index, so this value is the empty
 * commitment of **all nine** fields, not of field 0 alone. Field identity is
 * positional, and the aliasing is safe because the §4 positional-identity
 * invariant makes every consumer take its expected hash from the committed
 * compact structure in view — here, `txBody.spend_inputs_hash`.
 */
export const EMPTY_SPEND_INPUTS_HASH: string =
  MIDGARD_EMPTY_FIELD_COMMITMENT_V1.toString("hex");

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
