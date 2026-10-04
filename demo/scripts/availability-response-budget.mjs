import { minimumDaResponseBudgetMs } from "./deployment-profiles.mjs";

export const requiredAvailabilityWaits = Object.freeze([
  "A1",
  "A5",
  "A7",
  "B6",
]);

/** Analyze the owner-requested serial envelope, not an unproven runtime path.
 * Unknown waits remain unknown: a lower bound inside a window is not acceptance.
 * Callers must supply an enforced duration and source for every required term.
 */
export const analyzeAvailabilityResponseBudget = (profile, waits) => {
  const byItem = new Map();
  for (const wait of waits) {
    if (!requiredAvailabilityWaits.includes(wait.item) || byItem.has(wait.item))
      throw new Error(
        `Unexpected or duplicate availability wait: ${wait.item}`,
      );
    if (
      wait.ms !== undefined &&
      (!Number.isSafeInteger(wait.ms) || wait.ms < 0 || !wait.source)
    )
      throw new Error(
        `Availability wait ${wait.item} needs an enforced duration and source`,
      );
    byItem.set(wait.item, wait);
  }
  const missing = requiredAvailabilityWaits.filter(
    (item) => byItem.get(item)?.ms === undefined,
  );
  const baseMs = minimumDaResponseBudgetMs(profile);
  const knownWaitMs = waits.reduce(
    (sum, wait) => sum + (wait.ms === undefined ? 0 : wait.ms),
    0,
  );
  const lowerBoundMs = baseMs + knownWaitMs;
  const limits = [
    {
      name: "small response",
      ms: profile.timing.da_small_response_window_ms,
      strict: false,
    },
    {
      name: "full response",
      ms: profile.timing.da_full_response_window_ms,
      strict: false,
    },
    {
      name: "block maturity",
      ms: profile.timing.block_maturity_ms,
      strict: true,
    },
  ];
  const exceeded = limits.filter(({ ms, strict }) =>
    strict ? lowerBoundMs >= ms : lowerBoundMs > ms,
  );
  return {
    profile: profile.name,
    baseMs,
    knownWaitMs,
    lowerBoundMs,
    totalMs: missing.length === 0 ? lowerBoundMs : undefined,
    missing,
    exceeded,
    accepted: missing.length === 0 && exceeded.length === 0,
    remainingMs: Math.min(...limits.map(({ ms }) => ms - lowerBoundMs)),
  };
};
