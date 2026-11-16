/**
 * The verification's roots session in a worker thread of its own
 * (`workers/confirmed-ledger-root.ts`), started for a run's scope and
 * terminated with it: each command is posted and its one reply awaited
 * (`confirmed-ledger-root-message.ts`).
 */
import { Worker } from "node:worker_threads";

import { Effect } from "effect";

import { resolveWorkerEntry } from "../fibers/resolve-worker-entry.js";
import { BUNDLED_WORKER_EXEC_ARGV } from "../services/validation-pool.bundled-worker-exec-argv.js";
import {
  type ConfirmedLedgerRootsCommand,
  leavesCommand,
  packConfirmedLedgerRootsRequest,
  parseConfirmedLedgerFedResponse,
  parseConfirmedLedgerLeavesResponse,
  parseConfirmedLedgerRootsResponse,
} from "./confirmed-ledger-root-message.js";
import type {
  ConfirmedLedgerRootsSession,
  DeriveConfirmedLedgerRoots,
} from "./confirmed-ledger-roots.js";

/**
 * The old-generation heap of the roots' worker thread, in MB: 2048. A run
 * holds there the processed deltas, the page of leaves it is given and
 * the open branches of its two tries, whatever the size of
 * `confirmed_ledger` (`confirmed-ledger-roots.ts`); 1,000,000 entries ran
 * with the whole process under 740 MB resident
 * (`tests/confirmed-ledger-root.bounded.compiled.test.ts`). Node
 * ignores the limit when the process itself runs with
 * `--max-old-space-size`; the thread then shares the process's heap bound.
 */
export const CONFIRMED_LEDGER_ROOT_WORKER_HEAP_MB = 2048;

const asError = (cause: unknown) =>
  cause instanceof Error ? cause : new Error(String(cause));

/** Posts each command to `worker` and awaits its reply; once the thread
 * fails or exits, every command fails with why. */
const commandChannel = (worker: Worker) => {
  let awaiting: ((reply: Effect.Effect<unknown, Error>) => void) | undefined;
  let failure: Error | undefined;
  const settle = (reply: Effect.Effect<unknown, Error>) => {
    const resume = awaiting;
    awaiting = undefined;
    resume?.(reply);
  };
  const fail = (error: Error) => {
    failure ??= error;
    settle(Effect.fail(failure));
  };
  worker.on("message", (value: unknown) => settle(Effect.succeed(value)));
  worker.once("error", fail);
  worker.once("exit", (code) =>
    fail(
      new Error(
        `confirmed_ledger root worker exited with code ${code.toString()} before the run ended`,
      ),
    ),
  );
  return <A>(
    command: ConfirmedLedgerRootsCommand,
    transfer: ArrayBuffer[],
    parse: (reply: unknown) => A,
  ): Effect.Effect<A, Error> =>
    Effect.async<unknown, Error>((resume) => {
      if (failure !== undefined) return resume(Effect.fail(failure));
      awaiting = resume;
      worker.postMessage(command, transfer);
    }).pipe(
      Effect.flatMap((reply) =>
        Effect.try({ try: () => parse(reply), catch: asError }),
      ),
    );
};

/** A session in a worker thread started for the scope. */
export const confirmedLedgerRootsInWorkerThread: DeriveConfirmedLedgerRoots = (
  delta,
) =>
  Effect.acquireRelease(
    Effect.sync(() => {
      const { request, transfer } = packConfirmedLedgerRootsRequest(delta);
      return new Worker(
        resolveWorkerEntry(import.meta.url, "confirmed-ledger-root.js"),
        {
          workerData: request,
          transferList: transfer,
          execArgv: [...BUNDLED_WORKER_EXEC_ARGV],
          resourceLimits: {
            maxOldGenerationSizeMb: CONFIRMED_LEDGER_ROOT_WORKER_HEAP_MB,
          },
        },
      );
    }),
    (worker) => Effect.promise(() => worker.terminate()),
  ).pipe(
    Effect.map((worker): ConfirmedLedgerRootsSession => {
      const command = commandChannel(worker);
      return {
        leaves: (entries) => {
          const { command: leaves, transfer } = leavesCommand(entries);
          return command(leaves, transfer, parseConfirmedLedgerLeavesResponse);
        },
        feed: (leaves) => {
          const owned = leaves.slice();
          return command(
            { kind: "feed", leaves: owned.buffer },
            [owned.buffer],
            parseConfirmedLedgerFedResponse,
          );
        },
        roots: command(
          { kind: "roots" },
          [],
          parseConfirmedLedgerRootsResponse,
        ),
      };
    }),
  );
