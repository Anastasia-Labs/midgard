export const countWatcherReadinessReasons = (
  reasons: readonly Readonly<{ reason: string }>[],
): Readonly<Record<string, string>> => {
  const counts = new Map<string, number>();
  for (const { reason } of reasons)
    counts.set(reason, (counts.get(reason) ?? 0) + 1);
  return Object.freeze(
    Object.fromEntries(
      [...counts].map(([reason, count]) => [reason, count.toString()]),
    ),
  );
};
