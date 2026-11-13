import { setTimeout as pause } from "node:timers/promises";

import type { JourneyRunPolls } from "./journey-runner.journey-mode.js";
import { measureJourneyStage } from "./stage-timing.js";
import { isTransientTransportError } from "./transport-failure.js";

/**
 * A family's bounded waits: each stage measured in the family's directory,
 * and `onStage` told which stage is active.
 */
export const createJourneyRunPolls = ({
  directory,
  onStage,
}: Readonly<{
  directory: string;
  onStage(name: string): void;
}>): JourneyRunPolls => {
  const stage = async <T>(
    name: string,
    action: () => Promise<T>,
  ): Promise<T> => {
    onStage(name);
    return await measureJourneyStage(directory, name, action);
  };
  // A read inside a wait that got no answer (`isTransientTransportError`)
  // is "not yet": the wait tries again within its own bound, and at the bound
  // fails by name with the last such failure as its cause.
  const attempt = async <T>(
    action: () => Promise<T | undefined>,
    failure: { last?: unknown },
  ): Promise<T | undefined> => {
    try {
      return await action();
    } catch (cause) {
      if (!isTransientTransportError(cause)) throw cause;
      failure.last = cause;
      return undefined;
    }
  };
  const timedOut = (name: string, failure: { last?: unknown }) =>
    new Error(
      `Timed out waiting for ${name}`,
      failure.last === undefined ? undefined : { cause: failure.last },
    );
  const poll = async <T>(
    name: string,
    action: () => Promise<T | undefined>,
    timeoutMs = 300_000,
  ): Promise<T> =>
    stage(name, async () => {
      const deadline = performance.now() + timeoutMs;
      const failure: { last?: unknown } = {};
      for (;;) {
        const result = await attempt(action, failure);
        if (result !== undefined) return result;
        if (performance.now() >= deadline) throw timedOut(name, failure);
        await pause(1000);
      }
    });
  // A stage that waits on the watcher's L1 catch-up is budgeted by progress,
  // not by wall clock. The watcher's follower walks every L1 block it missed
  // before it can classify a header, so a long L1 gap (a node outage, a frozen
  // devnet) legitimately takes hours. The stage fails only when the watcher's
  // progress stops for the whole allowance; the journey timeout still bounds
  // the total.
  const pollWhileReplaying = async <T>(
    name: string,
    action: () => Promise<T | undefined>,
    progress: () => Promise<string | null>,
    stallTimeoutMs: number,
  ): Promise<T> =>
    stage(name, async () => {
      let lastProgress = await progress();
      let deadline = performance.now() + stallTimeoutMs;
      const failure: { last?: unknown } = {};
      for (;;) {
        const result = await attempt(action, failure);
        if (result !== undefined) return result;
        const current = await progress();
        if (current !== lastProgress) {
          lastProgress = current;
          deadline = performance.now() + stallTimeoutMs;
        }
        if (performance.now() >= deadline) throw timedOut(name, failure);
        await pause(1000);
      }
    });
  return Object.freeze({ stage, poll, pollWhileReplaying });
};
