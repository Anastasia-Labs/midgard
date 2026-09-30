import { Effect } from "effect";

export const hexOf = (value: Buffer): string => value.toString("hex");

export const logCommitMpfPhaseTiming = (
  phase: string,
  startedAtMs: number,
  counts: Record<string, number>,
): Effect.Effect<void, never> => {
  const countSummary = Object.entries(counts)
    .map(([key, value]) => `${key}=${value.toString()}`)
    .join(",");
  const suffix = countSummary.length > 0 ? `,${countSummary}` : "";
  return Effect.logInfo(
    `🔹 Commit MPF phase ${phase} completed duration_ms=${Math.max(
      0,
      Date.now() - startedAtMs,
    ).toString()}${suffix}`,
  );
};
