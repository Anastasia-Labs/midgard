import {
  type NormalizedTimeRange,
  normalizeNativeTxValidityRange,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

export const INVALID_RANGE_CATEGORY = "invalidRange" as const;
export const INVALID_RANGE_CATEGORY_ID = "00000003" as const;
export type InvalidRangeReason =
  | "ValidityIntervalMalformed"
  | "ValidityIntervalExcludesBlockSlot";

export type InvalidRangeEvidence = Readonly<{
  subject: VerdictSubject;
  blockSlot: bigint;
  normalizedRange: NormalizedTimeRange;
}>;

export const invalidRangeFaultForReason = (
  reason: InvalidRangeReason,
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

export const prepareInvalidRangeEvidence = ({
  subject,
  blockSlot,
  txBody,
}: Readonly<{
  subject: VerdictSubject;
  blockSlot: bigint;
  txBody: Parameters<typeof normalizeNativeTxValidityRange>[0];
}>): InvalidRangeEvidence => {
  if (!verdictSubjectIsCanonical(subject))
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

export const invalidRangeEvidenceCloses = (
  evidence: InvalidRangeEvidence,
): boolean => {
  const reason = evidence.subject.rejection_reason;
  const fault =
    reason === null
      ? evidence.normalizedRange === "InvalidRange" ||
        invalidRangeFaultForReason(
          "ValidityIntervalExcludesBlockSlot",
          evidence.normalizedRange,
          evidence.blockSlot,
        )
      : invalidRangeFaultForReason(
          reason as InvalidRangeReason,
          evidence.normalizedRange,
          evidence.blockSlot,
        );
  return evidence.subject.direction === 0n ? fault : !fault;
};
