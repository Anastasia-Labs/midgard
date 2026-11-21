/**
 * Derives the landed ledger's two roots off the main thread, for one full
 * verification of the landed ledger (`confirmed-ledger-verification.ts`):
 * it starts with the processed deltas in its `workerData` and answers the
 * main thread's commands one at a time (`confirmed-ledger-root-message.ts`)
 * with a `ConfirmedLedgerRootsBuilder`: the leaves of a page of
 * `confirmed_ledger` entries, a feed of leaves in path order, and the
 * roots. It holds the deltas, one page and the open branches of the two
 * tries, however large `confirmed_ledger` is. The thread is started per run
 * and ends with it.
 */
import { createRequire } from "node:module";
import { parentPort, workerData } from "node:worker_threads";

import type { ConfirmedLedgerRootsResponse } from "../mpf/confirmed-ledger-root-message.js";

if (parentPort === null)
  throw new Error("confirmed-ledger-root must run inside a worker thread");
const port = parentPort;

const require = createRequire(import.meta.url);

// blake2b swaps its CommonJS export to its WebAssembly build once ready;
// the ledger roots' modules are imported only after that, so what they
// hash with it uses it (as in `mpf-root-builder.ts`). The streaming
// builder (`mpf-sorted-root.ts`) hashes with `@noble/hashes` instead.
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

const failure = (error: unknown): ConfirmedLedgerRootsResponse => ({
  error: error instanceof Error ? error.message : String(error),
});

void runtime.then(
  ({ roots, message }) => {
    let builder: InstanceType<typeof roots.ConfirmedLedgerRootsBuilder>;
    try {
      const delta = message.unpackConfirmedLedgerRootsRequest(workerData);
      if (typeof delta === "string") throw new Error(delta);
      builder = new roots.ConfirmedLedgerRootsBuilder(delta);
    } catch (error) {
      // Every command is refused with why the thread could not start.
      port.on("message", () => port.postMessage(failure(error)));
      return;
    }
    port.on("message", (value: unknown) => {
      try {
        const command = message.unpackConfirmedLedgerRootsCommand(value);
        if (typeof command === "string") throw new Error(command);
        switch (command.kind) {
          case "leaves": {
            const leaves = roots.ledgerLeaves(command.entries);
            port.postMessage(
              {
                leaves: leaves.buffer as ArrayBuffer,
              } satisfies ConfirmedLedgerRootsResponse,
              [leaves.buffer as ArrayBuffer],
            );
            return;
          }
          case "feed":
            builder.feed(command.leaves);
            port.postMessage({
              fed: true,
            } satisfies ConfirmedLedgerRootsResponse);
            return;
          case "roots":
            port.postMessage(
              builder.roots() satisfies ConfirmedLedgerRootsResponse,
            );
        }
      } catch (error) {
        port.postMessage(failure(error));
      }
    });
  },
  (error: unknown) =>
    port.on("message", () => port.postMessage(failure(error))),
);
