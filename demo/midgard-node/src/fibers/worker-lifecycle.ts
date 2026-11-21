import { Worker, type WorkerOptions } from "worker_threads";

/**
 * How long a parent waits for a worker's termination before it stops blocking
 * on it. A thread inside a long synchronous native call cannot stop until the
 * call returns, and an unbounded wait there holds every permit the parent
 * holds (the L1 control plane above all) for as long as the call runs.
 */
export const WORKER_TERMINATION_WAIT_MS = 30_000;

/** The part of a worker thread its runners use; tests substitute a stub. */
export type SpawnedWorker = Pick<Worker, "terminate" | "on" | "off">;

export type SpawnWorker = (
  entry: string | URL,
  options: WorkerOptions,
) => SpawnedWorker;

export const spawnWorkerThread: SpawnWorker = (entry, options) =>
  new Worker(entry, options);

/**
 * The bounded wait for a worker's termination expired. The worker may still be
 * running: nothing that it holds has been released.
 */
export class WorkerTerminationUnconfirmedError extends Error {
  constructor(readonly waitTimeoutMs: number) {
    super(
      `Worker termination was not confirmed within ${waitTimeoutMs.toString()} ms; its post-termination cleanup stays deferred until the worker stops.`,
    );
    this.name = "WorkerTerminationUnconfirmedError";
  }
}

/**
 * Worker termination is asynchronous. Reusing one promise makes every exit
 * path (normal completion, failure, and Effect interruption) await the same
 * child shutdown instead of releasing parent-held resources early.
 *
 * With `waitTimeoutMs` a caller stops waiting after that long and gets a
 * `WorkerTerminationUnconfirmedError`, while the termination itself carries on:
 * `afterTermination` still runs only once the worker has actually stopped, so
 * a lease it releases stays withheld until then (or until its bounded TTL).
 */
export const makeAwaitedWorkerTerminator = (
  worker: Pick<Worker, "terminate">,
  afterTermination: () => Promise<void> = () => Promise.resolve(),
  options: { readonly waitTimeoutMs?: number } = {},
): (() => Promise<number>) => {
  let termination: Promise<number> | undefined;
  const terminateOnce = () => {
    if (termination === undefined) {
      termination = (async () => {
        // A rejected termination does not prove that the worker stopped. Never
        // release its logical MPF lease in that case: the bounded lease TTL is
        // the only safe fallback while a live worker may still hold store
        // handles.
        const workerResult = await worker.terminate();
        await afterTermination();
        return workerResult;
      })();
      // A caller that stopped waiting no longer observes this promise.
      termination.catch(() => undefined);
    }
    return termination;
  };
  const waitTimeoutMs = options.waitTimeoutMs;
  if (waitTimeoutMs === undefined) return terminateOnce;
  return () => {
    const pending = terminateOnce();
    let timer: ReturnType<typeof setTimeout> | undefined;
    const expired = new Promise<never>((_, reject) => {
      timer = setTimeout(
        () => reject(new WorkerTerminationUnconfirmedError(waitTimeoutMs)),
        waitTimeoutMs,
      );
      timer.unref?.();
    });
    return Promise.race([pending, expired]).finally(() => clearTimeout(timer));
  };
};
