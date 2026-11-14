import { expect } from "vitest";

import type { JourneyRunPolls } from "./journey-runner.journey-mode.js";

/** The watcher's `/v1/status` fields a healthy verdict reads. */
export type JourneyHealthyWatcherStatus = {
  liveness: string;
  readiness: string;
  readinessReasons: unknown[];
  activeAlerts: unknown[];
  launchScope: { complete: boolean };
};

/**
 * Healthy processing after the correction: `observe` finds the watcher's
 * healthy decision, then the watcher's own status must be live and ready with
 * no reasons and no active alerts. A ready watcher whose alerts are still
 * active (a rewind's alert clears on its next decided tip) is read again
 * within the same allowance; if they never clear, the wait fails naming them.
 * Returns that decision with the status read that proved it, which the result
 * records.
 */
export const awaitHealthyWatcher = async <T>({
  name,
  observe,
  status,
  polls,
  progress,
  stallTimeoutMs,
}: {
  name: string;
  observe: () => Promise<T | undefined>;
  status: () => Promise<JourneyHealthyWatcherStatus>;
  polls: Pick<JourneyRunPolls, "pollWhileReplaying">;
  progress: () => Promise<number>;
  stallTimeoutMs: number;
}): Promise<{ decision: T; status: JourneyHealthyWatcherStatus }> => {
  let activeAlerts: unknown[] = [];
  try {
    return await polls.pollWhileReplaying(
      name,
      async () => {
        const decision = await observe();
        if (decision === undefined) return undefined;
        const current = await status();
        activeAlerts = [];
        if (current.readiness !== "ready") return undefined;
        if (current.activeAlerts.length > 0) {
          activeAlerts = current.activeAlerts;
          return undefined;
        }
        expect(current).toMatchObject({
          liveness: "live",
          readiness: "ready",
          readinessReasons: [],
          activeAlerts: [],
          launchScope: { complete: true },
        });
        return { decision, status: current };
      },
      progress,
      stallTimeoutMs,
    );
  } catch (cause) {
    if (activeAlerts.length === 0) throw cause;
    throw new Error(
      `The watcher stayed ready with alerts still active: ${JSON.stringify(activeAlerts)}`,
      { cause },
    );
  }
};
