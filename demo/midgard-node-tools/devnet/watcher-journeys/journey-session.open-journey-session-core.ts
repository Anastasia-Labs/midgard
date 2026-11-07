import { mkdir, mkdtemp } from "node:fs/promises";
import { join } from "node:path";

import { measureJourneyStage } from "./stage-timing.js";

/**
 * What every journey session shares, whoever runs its watcher: its own
 * directory under the run's sessions, measured stages, and one close that
 * stops what the session started in reverse order, once.
 */
export const openJourneySessionCore = async (runDirectory: string) => {
  const sessionsDirectory = join(runDirectory, "work/journeys/sessions");
  await mkdir(sessionsDirectory, { recursive: true, mode: 0o700 });
  const directory = await mkdtemp(join(sessionsDirectory, "session-"));
  const cleanup: (() => Promise<void>)[] = [];
  let closed = false;
  let closePromise: Promise<void> | undefined;
  const close = () =>
    (closePromise ??= (async () => {
      closed = true;
      const failures: unknown[] = [];
      for (const stop of cleanup.reverse()) {
        try {
          await stop();
        } catch (cause) {
          failures.push(cause);
        }
      }
      if (failures.length > 0)
        throw new AggregateError(
          failures,
          "Could not close watcher journey session",
        );
    })());
  const stage = <T>(name: string, action: () => Promise<T>) =>
    measureJourneyStage(directory, name, action);
  return {
    directory,
    /** Stops `stop` when the session closes, after everything added later. */
    onClose: (stop: () => Promise<void>) => {
      cleanup.push(stop);
    },
    isClosed: () => closed,
    close,
    stage,
  };
};
