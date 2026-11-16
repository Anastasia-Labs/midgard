/**
 * Derives the landed ledger's two roots from the `confirmed_ledger` entries
 * and processed deltas in its `workerData`
 * (`confirmed-ledger-root-message.ts`) and posts them back, once, off the
 * main thread: the full verification of the landed ledger runs here
 * (`confirmed-ledger-verification.ts`). The thread is started per run and
 * ends with it, so the trie it builds is freed with the thread.
 */
import { createRequire } from "node:module";
import { parentPort, workerData } from "node:worker_threads";

import { Effect } from "effect";

import type { ConfirmedLedgerRootsResponse } from "../mpf/confirmed-ledger-root-message.js";

if (parentPort === null)
  throw new Error("confirmed-ledger-root must run inside a worker thread");
const port = parentPort;

const require = createRequire(import.meta.url);

// blake2b swaps its CommonJS export to its WebAssembly build once ready;
// the ledger roots' modules are imported only after that, so the trie
// they build hashes with it (as in `mpf-root-builder.ts`).
const runtime = new Promise<{
  readonly roots: typeof import("../mpf/confirmed-ledger-roots.js");
  readonly message: typeof import("../mpf/confirmed-ledger-root-message.js");
}>((resolve, reject) => {
  const blake2b = require("blake2b") as {
    readonly ready: (callback: (error?: unknown) => void) => void;
  };
  blake2b.ready((error?: unknown) => {
    if (error !== undefined) {
      reject(
        error instanceof Error
          ? error
          : new Error("blake2b initialization failed", { cause: error }),
      );
      return;
    }
    setImmediate(() => {
      void Promise.all([
        import("../mpf/confirmed-ledger-roots.js"),
        import("../mpf/confirmed-ledger-root-message.js"),
      ]).then(([roots, message]) => resolve({ roots, message }), reject);
    });
  });
});

void runtime
  .then(({ roots, message }) => {
    const input = message.unpackConfirmedLedgerRootsRequest(workerData);
    if (typeof input === "string") throw new Error(input);
    return Effect.runPromise(roots.deriveConfirmedLedgerRoots(input));
  })
  .then(
    ({ frontierRoot, tailRoot }) =>
      port.postMessage({
        frontierRoot,
        tailRoot,
      } satisfies ConfirmedLedgerRootsResponse),
    (error: unknown) =>
      port.postMessage({
        error: error instanceof Error ? error.message : String(error),
      } satisfies ConfirmedLedgerRootsResponse),
  );
