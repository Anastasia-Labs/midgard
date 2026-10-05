import { setTimeout as delay } from "node:timers/promises";

import { historyAdmissionExpired } from "./history-admission-expired.js";
import { historyProofDeadline } from "./history-proof-deadline.js";
import { DEFAULT_POLICY } from "./supervisor.js";

/** Retry only a completed, expired attempt on the same immutable owner. */
export const retryHistoryAdmission = async <T>(
  owner: Readonly<{ admit: (deadline: number) => Promise<T> }>,
  signal: AbortSignal,
): Promise<T | undefined> => {
  while (!signal.aborted) {
    const deadline = historyProofDeadline(DEFAULT_POLICY.probeTimeoutMs);
    if (deadline === null)
      throw new Error("history child proof budget is invalid");
    try {
      // This await is deliberately uncancelled: shutdown joins the real loader.
      const admission = await owner.admit(deadline);
      return signal.aborted ? undefined : admission;
    } catch (error) {
      if (!historyAdmissionExpired(error, deadline)) throw error;
      if (signal.aborted) return undefined;
    }
    try {
      await delay(DEFAULT_POLICY.initialBackoffMs, undefined, { signal });
    } catch (error) {
      if (!signal.aborted) throw error;
    }
  }
  return undefined;
};
