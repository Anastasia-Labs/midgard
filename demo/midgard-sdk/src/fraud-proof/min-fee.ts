/**
 * Wire and rule twin of the standalone `min-fee` fault-proof family (Q20).
 *
 * The family proves one public predicate over a block-committed native-V1
 * transaction and the disputed header's own fee schedule:
 *
 * `body.fee < min_fee_a * canonical_tx_size + min_fee_b`.
 *
 * Field order is consensus wire format. Keep every `Data.Object` below in the
 * declaration order of the corresponding Aiken record.
 */
import {
  computeMidgardNativeTxCanonicalSizeFromProofSource,
  type MidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
import { FieldCarriageSchema } from "../native-tx-field-access.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxCompactSchema,
  NativeTxInclusionCarriage,
  NativeTxInclusionCarriageSchema,
  NativeTxWitnessSetCompactSchema,
} from "./native.js";

export const MIN_FEE_VIOLATION_ID = "min-fee" as const;

/**
 * A pre-registration computation-thread asset name. The category id remains a
 * caller argument until the immutable catalogue registration wave allocates it.
 */
export const minFeeThreadTokenAssetName = (
  categoryId: string,
  challengedHeaderHash: string,
): string => {
  if (!/^[0-9a-f]{8}$/u.test(categoryId)) {
    throw new Error("min-fee category id must be 4 bytes of lowercase hex");
  }
  if (!/^[0-9a-f]{56}$/u.test(challengedHeaderHash)) {
    throw new Error("challenged header hash must be 28 bytes of lowercase hex");
  }
  return `${categoryId}${challengedHeaderHash}`;
};

// ## Step 01 — authenticate the compact transaction and header schedule

export const MinFeeStep01DatumSchema = faultProofStepDatumSchema(Data.Any());
export type MinFeeStep01Datum = Data.Static<typeof MinFeeStep01DatumSchema>;
export const MinFeeStep01Datum = asDataType<MinFeeStep01Datum>(
  MinFeeStep01DatumSchema,
);

export const MinFeeStep01SpendRedeemerSchema = faultProofStepRedeemerSchema(
  NativeTxInclusionCarriageSchema,
);
export type MinFeeStep01SpendRedeemer = Data.Static<
  typeof MinFeeStep01SpendRedeemerSchema
>;
export const MinFeeStep01SpendRedeemer = asDataType<MinFeeStep01SpendRedeemer>(
  MinFeeStep01SpendRedeemerSchema,
);

// ## Step 02 — authenticate all nine lengths and compare the exact boundary

export const MinFeeStep02StateSchema = Data.Object({
  bad_tx: NativeTxCompactSchema,
  bad_tx_body_fee: Data.Integer(),
  bad_tx_id: H32Schema,
  min_fee_a: Data.Integer(),
  min_fee_b: Data.Integer(),
});
export type MinFeeStep02State = Data.Static<typeof MinFeeStep02StateSchema>;
export const MinFeeStep02State = asDataType<MinFeeStep02State>(
  MinFeeStep02StateSchema,
);

export const MinFeeStep02DatumSchema = faultProofStepDatumSchema(
  MinFeeStep02StateSchema,
);
export type MinFeeStep02Datum = Data.Static<typeof MinFeeStep02DatumSchema>;
export const MinFeeStep02Datum = asDataType<MinFeeStep02Datum>(
  MinFeeStep02DatumSchema,
);

export const MinFeeStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  native_tx_compact_cbor: Data.Bytes(),
  witness_set: NativeTxWitnessSetCompactSchema,
  /** Exactly nine values in §2.5 wire order. */
  field_carriages: Data.Tuple([
    FieldCarriageSchema,
    FieldCarriageSchema,
    FieldCarriageSchema,
    FieldCarriageSchema,
    FieldCarriageSchema,
    FieldCarriageSchema,
    FieldCarriageSchema,
    FieldCarriageSchema,
    FieldCarriageSchema,
  ]),
});
export type MinFeeStep02Args = Data.Static<typeof MinFeeStep02ArgsSchema>;
export const MinFeeStep02Args = asDataType<MinFeeStep02Args>(
  MinFeeStep02ArgsSchema,
);

export const MinFeeStep02SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinFeeStep02ArgsSchema,
);
export type MinFeeStep02SpendRedeemer = Data.Static<
  typeof MinFeeStep02SpendRedeemerSchema
>;
export const MinFeeStep02SpendRedeemer = asDataType<MinFeeStep02SpendRedeemer>(
  MinFeeStep02SpendRedeemerSchema,
);

export {
  FaultProofStepCancel as MinFeeStepCancel,
  FaultProofStepCancelSchema as MinFeeStepCancelSchema,
  NativeTxInclusionCarriage as MinFeeTxInclusionArgs,
  NativeTxInclusionCarriageSchema as MinFeeTxInclusionArgsSchema,
};

const requireNonNegative = (value: bigint, label: string): bigint => {
  if (value < 0n) {
    throw new Error(`${label} must be non-negative`);
  }
  return value;
};

/** Exact twin of `compact.min_fee_lovelace_v1`. */
export const minFeeLovelace = ({
  minFeeA,
  minFeeB,
  canonicalTxSize,
}: {
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly canonicalTxSize: bigint;
}): bigint =>
  requireNonNegative(minFeeA, "min_fee_a") *
    requireNonNegative(canonicalTxSize, "canonical_tx_size") +
  requireNonNegative(minFeeB, "min_fee_b");

/**
 * Derives the exact full-transaction size through the canonical core codec and
 * applies the header's fee schedule without Number arithmetic.
 */
export const minimumFeeFromProofSource = ({
  source,
  minFeeA,
  minFeeB,
}: {
  readonly source: MidgardNativeTxProofSource;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
}): { readonly canonicalTxSize: bigint; readonly minimumFee: bigint } => {
  const canonicalTxSize = BigInt(
    computeMidgardNativeTxCanonicalSizeFromProofSource(source),
  );
  return {
    canonicalTxSize,
    minimumFee: minFeeLovelace({
      minFeeA,
      minFeeB,
      canonicalTxSize,
    }),
  };
};

export const hasMinFeeViolation = ({
  fee,
  minFeeA,
  minFeeB,
  canonicalTxSize,
}: {
  readonly fee: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly canonicalTxSize: bigint;
}): boolean =>
  requireNonNegative(fee, "fee") <
  minFeeLovelace({ minFeeA, minFeeB, canonicalTxSize });
