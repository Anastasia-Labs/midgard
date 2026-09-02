import { MIDGARD_POSIX_TIME_NONE } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { OutputReferenceSchema } from "../common.js";
import { ForcedInclusionTxV1Schema, HeaderV1Schema } from "../ledger-state.js";
import { RejectionReasonV1Schema } from "../rejection-reason-v1.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
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

export const InvalidRangeStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type InvalidRangeStep01Datum = Data.Static<
  typeof InvalidRangeStep01DatumSchema
>;
export const InvalidRangeStep01Datum =
  InvalidRangeStep01DatumSchema as unknown as InvalidRangeStep01Datum;

export const InvalidRangeVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const InvalidRangeStep01SourceV1Schema = Data.Enum([
  Data.Object({
    AcceptedSource: Data.Object({ inclusion: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    ForcedSource: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderV1Schema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxV1Schema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);
export const InvalidRangeForcedSourcePayloadV1Schema = Data.Object({
  header: HeaderV1Schema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
  direction: Data.Integer(),
});
export const InvalidRangeStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({ source: InvalidRangeStep01SourceV1Schema }),
  );
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
  subject: InvalidRangeVerdictSubjectV1Schema,
  block_slot: Data.Integer(),
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
  blockSlot,
  normalizedRange,
}: {
  readonly blockSlot: bigint;
  readonly normalizedRange: NormalizedTimeRange;
}):
  | "starts-after-block-slot"
  | "ends-before-block-slot"
  | "invalid-range"
  | null => {
  if (typeof normalizedRange === "string") {
    return normalizedRange === "InvalidRange" ? "invalid-range" : null;
  }
  if ("ClosedRange" in normalizedRange) {
    if (normalizedRange.ClosedRange.lower > blockSlot) {
      return "starts-after-block-slot";
    }
    if (normalizedRange.ClosedRange.upper < blockSlot) {
      return "ends-before-block-slot";
    }
    return null;
  }
  if ("FromNegInf" in normalizedRange) {
    return normalizedRange.FromNegInf.upper < blockSlot
      ? "ends-before-block-slot"
      : null;
  }
  return normalizedRange.ToPosInf.lower > blockSlot
    ? "starts-after-block-slot"
    : null;
};

export const nativeTxBodyHasInvalidRangeViolation = ({
  blockSlot,
  txBody,
}: {
  readonly blockSlot: bigint;
  readonly txBody: NativeTxBodyCompactData;
}): boolean =>
  invalidRangeViolationReason({
    blockSlot,
    normalizedRange: normalizeNativeTxValidityRange(txBody),
  }) !== null;
