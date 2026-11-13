import { mkdir, mkdtemp } from "node:fs/promises";
import { join } from "node:path";

import { measureJourneyStage } from "./stage-timing.js";

const sessionClosed = () => new Error("Watcher journey session is closed");

type RunningFamily = Readonly<{
  controller: AbortController;
  /** The family's own work, settled once it has stopped. */
  work: Promise<unknown>;
  /** Settled once the family's run has returned, closing the session first. */
  unwound: Promise<void>;
}>;

/**
 * What every journey session shares, whoever runs its watcher: its own
 * directory under the run's sessions, measured stages, one family run at a
 * time, and one close that stops what the session started in reverse order,
 * once.
 *
 * Every family runs through `runFamily`, which hands its work one abort
 * signal: aborted when the family's time limit passes or the session
 * closes. A family that fails, or reaches its limit, closes the session
 * (fail-fast), and its run returns only once the session has closed. A close
 * first aborts the running family and waits for its work to stop; only then
 * does it stop what the session started, so nothing the session started
 * (operator-2's hold, the replay host, the watcher) ends under a family
 * still at work.
 */
export const openJourneySessionCore = async (runDirectory: string) => {
  const sessionsDirectory = join(runDirectory, "work/journeys/sessions");
  await mkdir(sessionsDirectory, { recursive: true, mode: 0o700 });
  const directory = await mkdtemp(join(sessionsDirectory, "session-"));
  const cleanup: (() => Promise<void>)[] = [];
  const closing = new AbortController();
  let closed = false;
  let closePromise: Promise<void> | undefined;
  let family: RunningFamily | undefined;
  const markClosed = () => {
    closed = true;
    family?.controller.abort(sessionClosed());
    closing.abort(sessionClosed());
  };
  const close = () => {
    markClosed();
    const work = family?.work;
    return (closePromise ??= (async () => {
      await work?.catch(() => undefined);
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
  };
  /**
   * A close from inside the running family's own work, which cannot wait for
   * that work to stop: it marks the session closed and aborts the family,
   * whose run then closes the session. With no family running it is
   * `close()`.
   */
  const requestClose = (): Promise<void> => {
    if (family === undefined) return close();
    markClosed();
    return Promise.resolve();
  };
  const runFamily = async <T>(
    work: (signal: AbortSignal) => Promise<T>,
    limit?: AbortSignal,
  ): Promise<T> => {
    if (family !== undefined)
      throw new Error(
        "An earlier family's run in this watcher journey session has not unwound; refusing to start another",
      );
    if (closed) throw sessionClosed();
    const controller = new AbortController();
    const onLimit = () => {
      controller.abort(
        new Error("The family reached its time limit", {
          cause: limit?.reason,
        }),
      );
      // The run's own close below reports a close that fails.
      close().catch(() => undefined);
    };
    let unwound!: () => void;
    const running = (async () => await work(controller.signal))();
    family = {
      controller,
      work: running,
      unwound: new Promise<void>((resolve) => {
        unwound = resolve;
      }),
    };
    if (limit?.aborted === true) onLimit();
    else limit?.addEventListener("abort", onLimit, { once: true });
    try {
      const result = await running;
      // Work that ended well after its abort still ran past its limit.
      controller.signal.throwIfAborted();
      return result;
    } catch (cause) {
      await close();
      throw cause;
    } finally {
      limit?.removeEventListener("abort", onLimit);
      family = undefined;
      unwound();
    }
  };
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
    requestClose,
    /**
     * Runs one family's `work` with the family's abort signal, aborted when
     * `limit` aborts (the family's time limit) or the session closes.
     * Refuses while an earlier family's run has not unwound, and in a closed
     * session.
     */
    runFamily,
    /** Settles once no family's run is in flight. */
    familyUnwound: (): Promise<void> => family?.unwound ?? Promise.resolve(),
    /** Aborted once the session starts to close. */
    signal: closing.signal,
    stage,
  };
};
