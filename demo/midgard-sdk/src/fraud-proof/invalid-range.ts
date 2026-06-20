import { MIDGARD_POSIX_TIME_NONE } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxBodyCompact,
  type NativeTxBodyCompact as NativeTxBodyCompactData,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
} from "./native.js";

export const InvalidRangeStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type InvalidRangeStep01Datum = Data.Static<
  typeof InvalidRangeStep01DatumSchema
>;
export const InvalidRangeStep01Datum =
  InvalidRangeStep01DatumSchema as unknown as InvalidRangeStep01Datum;

export const InvalidRangeStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeTxInclusionArgsSchema);
export type InvalidRangeStep01SpendRedeemer = Data.Static<
  typeof InvalidRangeStep01SpendRedeemerSchema
>;
export const InvalidRangeStep01SpendRedeemer =
  InvalidRangeStep01SpendRedeemerSchema as unknown as InvalidRangeStep01SpendRedeemer;

export const NormalizedTimeRangeSchema = Data.Enum([
  Data.Object({
    ClosedRange: Data.Object({
      lower: Data.Integer(),
      upper: Data.Integer(),
    }),
  }),
  Data.Object({ FromNegInf: Data.Object({ upper: Data.Integer() }) }),
  Data.Object({ ToPosInf: Data.Object({ lower: Data.Integer() }) }),
  Data.Literal("Always"),
  Data.Literal("InvalidRange"),
]);
export type NormalizedTimeRange = Data.Static<typeof NormalizedTimeRangeSchema>;
export const NormalizedTimeRange =
  NormalizedTimeRangeSchema as unknown as NormalizedTimeRange;

export const InvalidRangeStep02StateSchema = Data.Object({
  block_valid_from: Data.Integer(),
  block_valid_to: Data.Integer(),
  bad_tx_normalized_validity_range: NormalizedTimeRangeSchema,
});
export type InvalidRangeStep02State = Data.Static<
  typeof InvalidRangeStep02StateSchema
>;
export const InvalidRangeStep02State =
  InvalidRangeStep02StateSchema as unknown as InvalidRangeStep02State;

export const InvalidRangeStep02DatumSchema = faultProofStepDatumSchema(
  InvalidRangeStep02StateSchema,
);
export type InvalidRangeStep02Datum = Data.Static<
  typeof InvalidRangeStep02DatumSchema
>;
export const InvalidRangeStep02Datum =
  InvalidRangeStep02DatumSchema as unknown as InvalidRangeStep02Datum;

export const InvalidRangeStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type InvalidRangeStep02Args = Data.Static<
  typeof InvalidRangeStep02ArgsSchema
>;
export const InvalidRangeStep02Args =
  InvalidRangeStep02ArgsSchema as unknown as InvalidRangeStep02Args;

export const InvalidRangeStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(InvalidRangeStep02ArgsSchema);
export type InvalidRangeStep02SpendRedeemer = Data.Static<
  typeof InvalidRangeStep02SpendRedeemerSchema
>;
export const InvalidRangeStep02SpendRedeemer =
  InvalidRangeStep02SpendRedeemerSchema as unknown as InvalidRangeStep02SpendRedeemer;

export {
  FaultProofStepCancel as InvalidRangeStepCancel,
  FaultProofStepCancelSchema as InvalidRangeStepCancelSchema,
  NativeTxInclusionArgs as InvalidRangeTxInclusionArgs,
  NativeTxInclusionArgsSchema as InvalidRangeTxInclusionArgsSchema,
};

export const normalizeNativeTxValidityRange = (
  txBody: NativeTxBodyCompactData,
): NormalizedTimeRange => {
  const roundTripped = Data.from(
    Data.to(txBody, NativeTxBodyCompact),
    NativeTxBodyCompact,
  );
  const lower = roundTripped.validity_interval_start;
  const exclusiveUpper = roundTripped.validity_interval_end;
  const hasLower = lower !== MIDGARD_POSIX_TIME_NONE;
  const hasUpper = exclusiveUpper !== MIDGARD_POSIX_TIME_NONE;

  if (!hasLower && !hasUpper) {
    return "Always";
  }
  if (!hasLower) {
    return { FromNegInf: { upper: exclusiveUpper - 1n } };
  }
  if (!hasUpper) {
    return { ToPosInf: { lower } };
  }

  const upper = exclusiveUpper - 1n;
  if (lower > upper) {
    return "InvalidRange";
  }
  return { ClosedRange: { lower, upper } };
};

export const invalidRangeViolationReason = ({
  blockValidFrom,
  blockValidTo,
  normalizedRange,
}: {
  readonly blockValidFrom: bigint;
  readonly blockValidTo: bigint;
  readonly normalizedRange: NormalizedTimeRange;
}):
  | "lower-before-block"
  | "upper-at-or-after-block"
  | "invalid-range"
  | null => {
  if (typeof normalizedRange === "string") {
    return normalizedRange === "InvalidRange" ? "invalid-range" : null;
  }
  if ("ClosedRange" in normalizedRange) {
    if (normalizedRange.ClosedRange.lower < blockValidFrom) {
      return "lower-before-block";
    }
    if (normalizedRange.ClosedRange.upper >= blockValidTo) {
      return "upper-at-or-after-block";
    }
    return null;
  }
  if ("FromNegInf" in normalizedRange) {
    return normalizedRange.FromNegInf.upper >= blockValidTo
      ? "upper-at-or-after-block"
      : null;
  }
  return normalizedRange.ToPosInf.lower < blockValidFrom
    ? "lower-before-block"
    : null;
};

export const nativeTxBodyHasInvalidRangeViolation = ({
  blockValidFrom,
  blockValidTo,
  txBody,
}: {
  readonly blockValidFrom: bigint;
  readonly blockValidTo: bigint;
  readonly txBody: NativeTxBodyCompactData;
}): boolean =>
  invalidRangeViolationReason({
    blockValidFrom,
    blockValidTo,
    normalizedRange: normalizeNativeTxValidityRange(txBody),
  }) !== null;
