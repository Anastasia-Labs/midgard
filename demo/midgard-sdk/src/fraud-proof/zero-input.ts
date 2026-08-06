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
 * nothing: the canonical V1 bounded-collection commitment for dynamic field
 * zero with no items. Derived from the same codec that materializes native V1
 * transactions, so it cannot drift from the `empty_spend_inputs_hash`
 * parameter the step-02 validator compares against.
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
