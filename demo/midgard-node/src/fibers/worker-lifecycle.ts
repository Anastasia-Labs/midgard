import type { Worker } from "node:worker_threads";

/**
 * Worker termination is asynchronous. Reusing one promise makes every exit
 * path (normal completion, failure, and Effect interruption) await the same
 * child shutdown instead of releasing parent-held resources early.
 */
export const makeAwaitedWorkerTerminator = (
  worker: Pick<Worker, "terminate">,
  afterTermination: () => Promise<void> = () => Promise.resolve(),
): (() => Promise<number>) => {
  let termination: Promise<number> | undefined;
  return () =>
    (termination ??= (async () => {
      // A rejected termination does not prove that the worker stopped. Never
      // release its logical MPF lease in that case: the bounded lease TTL is
      // the only safe fallback while a live worker may still hold store
      // handles.
      const workerResult = await worker.terminate();
      await afterTermination();
      return workerResult;
    })());
};
