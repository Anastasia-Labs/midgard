import { Effect } from "effect";
import type { WorkerOptions } from "worker_threads";

import { WorkerError } from "../workers/utils/common.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";
import {
  makeAwaitedWorkerTerminator,
  type SpawnWorker,
  spawnWorkerThread,
  WORKER_TERMINATION_WAIT_MS,
} from "./worker-lifecycle.js";

const commitWorkerError = (message: string, cause: unknown) =>
  new WorkerError({ worker: "commit-block-header", message, cause });

/**
 * Runs one commitment worker until `takeOutput` accepts one of its messages,
 * then terminates it.
 *
 * `releaseLedgerLease` runs only once the worker has actually stopped: a
 * worker that has not stopped may still hold MPF store handles, so its lease
 * is never released early, and the lease's bounded TTL is the fallback. Every
 * exit path waits for the termination at most `terminationWaitMs`. A job whose
 * worker does not stop in time fails, as one whose termination is rejected
 * does; an interrupted job logs it and returns. Either way the caller (and
 * the L1 control-plane permit it holds) is never blocked on a worker that
 * does not stop.
 */
export const runCommitWorkerInThread = <Message, Output>({
  workerEntry,
  workerOptions,
  takeOutput,
  releaseLedgerLease,
  terminationWaitMs = WORKER_TERMINATION_WAIT_MS,
  spawnWorker = spawnWorkerThread,
}: {
  readonly workerEntry?: string | URL;
  readonly workerOptions: WorkerOptions;
  readonly takeOutput: (message: Message) => Output | undefined;
  readonly releaseLedgerLease: () => Promise<void>;
  readonly terminationWaitMs?: number;
  readonly spawnWorker?: SpawnWorker;
}): Effect.Effect<Output, WorkerError> =>
  Effect.async<Output, WorkerError, never>((resume) => {
    Effect.runSync(Effect.logInfo(`👷 Starting block commitment worker...`));
    const worker = spawnWorker(
      workerEntry ??
        resolveWorkerEntry(import.meta.url, "commit-block-header.js"),
      workerOptions,
    );
    const terminate = makeAwaitedWorkerTerminator(worker, releaseLedgerLease, {
      waitTimeoutMs: terminationWaitMs,
    });
    let settled = false;
    const cleanup = () => {
      worker.off("message", onMessage);
      worker.off("error", onError);
      worker.off("exit", onExit);
    };
    const settle = (result: Effect.Effect<Output, WorkerError>) => {
      if (settled) return;
      settled = true;
      cleanup();
      void terminate().then(
        () => resume(result),
        (cause) =>
          resume(
            Effect.fail(
              commitWorkerError(
                "Failed to terminate commitment worker.",
                cause,
              ),
            ),
          ),
      );
    };
    const onMessage = (message: Message) => {
      const output = takeOutput(message);
      if (output !== undefined) settle(Effect.succeed(output));
    };
    const onError = (e: Error) => {
      settle(
        Effect.fail(commitWorkerError(`Error in commitment worker: ${e}`, e)),
      );
    };
    const onExit = (code: number) => {
      settle(
        Effect.fail(
          commitWorkerError(
            `Commitment worker exited before producing output with code: ${code}`,
            `exit code ${code}`,
          ),
        ),
      );
    };
    worker.on("message", onMessage);
    worker.on("error", onError);
    worker.on("exit", onExit);
    return Effect.tryPromise(() => {
      if (!settled) {
        settled = true;
        cleanup();
      }
      return terminate();
    }).pipe(
      Effect.catchAll((error) =>
        Effect.logError(
          `Commitment worker termination after interruption was not confirmed; its ledger MPF lease stays held until the worker stops or the lease TTL expires: ${String(error.cause)}`,
        ),
      ),
      Effect.asVoid,
    );
  });
