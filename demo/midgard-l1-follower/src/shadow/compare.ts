import type {
  ShadowComparator,
  ShadowContext,
  ShadowReading,
  ShadowRole,
} from "./comparator.js";
import { type DiffEntry, diffValues } from "./diff.js";

/** One comparator's outcome at one block. */
export type ShadowResult =
  | Readonly<{ role: ShadowRole; name: string; outcome: "equal" }>
  | Readonly<{
      role: ShadowRole;
      name: string;
      outcome: "differs";
      diff: readonly DiffEntry[];
    }>
  | Readonly<{
      role: ShadowRole;
      name: string;
      outcome: "skipped";
      reason: string;
    }>
  | Readonly<{
      role: ShadowRole;
      name: string;
      outcome: "error";
      error: string;
    }>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const side = async (
  read: () => Promise<ShadowReading>,
): Promise<ShadowReading | Readonly<{ kind: "error"; error: string }>> => {
  try {
    return await read();
  } catch (error) {
    return { kind: "error", error: message(error) };
  }
};

/**
 * Runs every comparator at one block. A comparator that throws is an
 * `error` result, never an exception: one broken comparator must not stop
 * the others or the follower.
 */
export const compareAll = async (
  comparators: readonly ShadowComparator[],
  context: ShadowContext,
): Promise<ShadowResult[]> => {
  const results: ShadowResult[] = [];
  for (const comparator of comparators) {
    const { role, name } = comparator;
    if (comparator.observe !== undefined)
      try {
        await comparator.observe(context);
      } catch (error) {
        results.push({ role, name, outcome: "error", error: message(error) });
        continue;
      }
    const projected = await side(() => comparator.projected(context));
    const current = await side(() => comparator.current(context));
    if (projected.kind === "error" || current.kind === "error") {
      const failed = projected.kind === "error" ? projected : current;
      results.push({
        role,
        name,
        outcome: "error",
        error: `${projected.kind === "error" ? "projected" : "current"}: ${failed.kind === "error" ? failed.error : ""}`,
      });
      continue;
    }
    if (projected.kind === "unavailable" || current.kind === "unavailable") {
      results.push({
        role,
        name,
        outcome: "skipped",
        reason:
          projected.kind === "unavailable"
            ? `projected: ${projected.reason}`
            : `current: ${current.kind === "unavailable" ? current.reason : ""}`,
      });
      continue;
    }
    const diff = diffValues(projected.value, current.value);
    results.push(
      diff.length === 0
        ? { role, name, outcome: "equal" }
        : { role, name, outcome: "differs", diff },
    );
  }
  return results;
};

/** The first non-equal, non-skipped result, as text; null when all agree. */
export const firstDisagreement = (
  results: readonly ShadowResult[],
): string | null => {
  for (const result of results) {
    if (result.outcome === "differs")
      return `${result.role}/${result.name} differs: ${JSON.stringify(result.diff.slice(0, 3))}`;
    if (result.outcome === "error")
      return `${result.role}/${result.name} failed: ${result.error}`;
  }
  return null;
};
