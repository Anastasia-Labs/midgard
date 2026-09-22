/**
 * Nearest-rank percentile helpers.
 *
 * Both pick the `ceil(n * quantile)`-th sample (1-based), clamped into range.
 * They differ only in what the caller already guarantees and what an empty
 * sample set means: `percentileOfSorted` trusts a pre-sorted input and reports
 * `null`, while `percentileOfUnsorted` sorts a copy and reports `0`.
 */
export const percentileOfSorted = (
  sortedValues: readonly number[],
  quantile: number,
): number | null => {
  if (sortedValues.length === 0) {
    return null;
  }
  const index = Math.min(
    sortedValues.length - 1,
    Math.max(0, Math.ceil(sortedValues.length * quantile) - 1),
  );
  return sortedValues[index]!;
};

export const percentileOfUnsorted = (
  values: readonly number[],
  quantile: number,
): number => {
  if (values.length === 0) {
    return 0;
  }
  const sorted = [...values].sort((left, right) => left - right);
  const index = Math.min(
    sorted.length - 1,
    Math.max(0, Math.ceil(sorted.length * quantile) - 1),
  );
  return sorted[index]!;
};
