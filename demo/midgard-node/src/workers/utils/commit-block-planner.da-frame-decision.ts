import type {
  CommitDaFrameMeasurement,
  CommitDaFrameStepDown,
} from "./commit-block-planner.commit-scheduler-evidence-key.js";

export const commitDaFrameStepDownPassBound = (txCount: number): number => {
  let passes = 2;
  for (let reach = 1; reach < txCount; reach *= 2) passes += 1;
  return txCount <= 0 ? 1 : passes;
};

export const planCommitDaFrameStepDown = ({
  measurement,
  maxInnerBytes,
}: {
  readonly measurement: CommitDaFrameMeasurement;
  readonly maxInnerBytes: number;
}): CommitDaFrameStepDown => {
  const n = measurement.acceptedTxCount;
  if (
    !Number.isSafeInteger(n) ||
    n < 0 ||
    measurement.acceptedTxIds.length !== n ||
    new Set(measurement.acceptedTxIds.map((id) => id.toString("hex"))).size !==
      n ||
    measurement.prefixes.length !== n + 1 ||
    measurement.prefixes.some(
      (prefix) =>
        !Number.isSafeInteger(prefix.innerBytesUpperBound) ||
        prefix.innerBytesUpperBound < 0 ||
        !/^[0-9a-f]{64}$/.test(prefix.materialDigest),
    ) ||
    measurement.prefixes[n]?.innerBytesUpperBound !==
      measurement.innerBytesUpperBound
  )
    return { status: "incomplete" };
  const first = measurement.hasMandatoryWork ? 0 : 1;
  if (first > n) return { status: "exact_check_required" };
  let minimum = first;
  let fitting: number | undefined;
  for (let prefix = first; prefix <= n; prefix += 1) {
    const bytes = measurement.prefixes[prefix]!.innerBytesUpperBound;
    if (bytes <= maxInnerBytes) fitting = prefix;
    if (bytes <= measurement.prefixes[minimum]!.innerBytesUpperBound)
      minimum = prefix;
  }
  const chosen = fitting ?? minimum;
  return chosen === n
    ? { status: fitting === undefined ? "exact_check_required" : "fits" }
    : { status: "step_down", nextTxCount: chosen };
};
