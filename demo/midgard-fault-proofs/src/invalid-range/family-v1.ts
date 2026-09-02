import {
  type NormalizedTimeRange,
  normalizeNativeTxValidityRange,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

export const INVALID_RANGE_CATEGORY_V1 = "invalidRange" as const;
export const INVALID_RANGE_CATEGORY_ID_V1 = "00000003" as const;
export type InvalidRangeReasonV1 =
  | "ValidityIntervalMalformed"
  | "ValidityIntervalExcludesBlockSlot";

export type InvalidRangeEvidenceV1 = Readonly<{
  subject: VerdictSubjectV1;
  blockSlot: bigint;
  normalizedRange: NormalizedTimeRange;
}>;

export const invalidRangeFaultForReasonV1 = (
  reason: InvalidRangeReasonV1,
  range: NormalizedTimeRange,
  slot: bigint,
): boolean => {
  if (reason === "ValidityIntervalMalformed") return range === "InvalidRange";
  if (range === "InvalidRange" || range === "Always") return false;
  if ("ClosedRange" in range)
    return range.ClosedRange.lower > slot || range.ClosedRange.upper < slot;
  if ("FromNegInf" in range) return range.FromNegInf.upper < slot;
  return range.ToPosInf.lower > slot;
};

export const prepareInvalidRangeEvidenceV1 = ({
  subject,
  blockSlot,
  txBody,
}: Readonly<{
  subject: VerdictSubjectV1;
  blockSlot: bigint;
  txBody: Parameters<typeof normalizeNativeTxValidityRange>[0];
}>): InvalidRangeEvidenceV1 => {
  if (!verdictSubjectIsCanonicalV1(subject))
    throw new Error("invalidRange: verdict subject is not canonical");
  if (subject.direction === 1n) {
    if (
      subject.rejection_reason !== "ValidityIntervalMalformed" &&
      subject.rejection_reason !== "ValidityIntervalExcludesBlockSlot"
    )
      throw new Error("invalidRange: typed rejection reason changed");
  } else if (subject.direction !== 0n || subject.rejection_reason !== null) {
    throw new Error("invalidRange: direction/reason polarity changed");
  }
  return Object.freeze({
    subject,
    blockSlot,
    normalizedRange: normalizeNativeTxValidityRange(txBody),
  });
};

export const invalidRangeEvidenceClosesV1 = (
  evidence: InvalidRangeEvidenceV1,
): boolean => {
  const reason = evidence.subject.rejection_reason;
  const fault =
    reason === null
      ? evidence.normalizedRange === "InvalidRange" ||
        invalidRangeFaultForReasonV1(
          "ValidityIntervalExcludesBlockSlot",
          evidence.normalizedRange,
          evidence.blockSlot,
        )
      : invalidRangeFaultForReasonV1(
          reason as InvalidRangeReasonV1,
          evidence.normalizedRange,
          evidence.blockSlot,
        );
  return evidence.subject.direction === 0n ? fault : !fault;
};
