/**
 * The built root worker (`dist/confirmed-ledger-root.js`), started as the
 * verification starts it (its packed deltas as `workerData`, the bundled
 * worker flags and its fixed heap limit) and driven through its message
 * protocol (`confirmed-ledger-root-message.ts`), derives the roots of a
 * 100,000-entry `confirmed_ledger` with a delta of 2,000 changes: the
 * entries go to it a page at a time, as the verification reads them, and
 * their leaves come back to be fed in path order. At eight checkpoints
 * the worker's heap is read after a full collection (a heap snapshot
 * forces one): its heap in use, with the array buffers it holds, rises
 * by less than 4 MB from the first checkpoint to the peak of the later
 * ones, so a worker that kept the entries, the leaves or the trie fails.
 * Its roots are the ones the same pass in this thread derives.
 * `confirmed-ledger-root.bounded.bench.ts` runs the worker at 1,000,000
 * entries.
 */
import type { HeapInfo } from "node:v8";
import { Worker } from "node:worker_threads";

import { describe, expect, it } from "vitest";

import {
  CONFIRMED_LEDGER_ENTRY_PAGE,
  CONFIRMED_LEDGER_LEAF_PAGE,
} from "../src/landed-blocks/ledger-leaves.js";
import {
  type ConfirmedLedgerDeltaOp,
  type ConfirmedLedgerRoots,
  type ConfirmedLedgerRootsCommand,
  leavesCommand,
  packConfirmedLedgerRootsRequest,
  parseConfirmedLedgerFedResponse,
  parseConfirmedLedgerLeavesResponse,
  parseConfirmedLedgerRootsResponse,
} from "../src/mpf/confirmed-ledger-root-message.js";
import { CONFIRMED_LEDGER_ROOT_WORKER_HEAP_MB } from "../src/mpf/confirmed-ledger-root-worker.js";
import {
  ConfirmedLedgerRootsBuilder,
  LEAF_BYTES,
  sortedLeaves,
} from "../src/mpf/confirmed-ledger-roots.js";
import { BUNDLED_WORKER_EXEC_ARGV } from "../src/services/validation-pool.bundled-worker-exec-argv.js";

const WORKER = new URL("../dist/confirmed-ledger-root.js", import.meta.url);

const ENTRIES = 100_000;
const CHANGES = 1_000;
/** The heap the later checkpoints' peak may exceed the first by. */
const BOUND_MB = 4;
const MB = 1024 * 1024;

/** A Midgard output holding `lovelace` (from 2^16, below 2^32). */
const output = (lovelace: number) =>
  Buffer.from(
    `a200583900899d9f267b0acf680fe51468ad46dca83b5e4d28e0dad6c68dbf0e2ea04755ad14d178a134575bba463b566827d339fdbdf0ec961814491c01821a${lovelace
      .toString(16)
      .padStart(8, "0")}a0`,
    "hex",
  );
/** The outref labelled `label`: its transaction id holds the label. */
const outRef = (label: number) => {
  const txId = Buffer.alloc(32);
  txId.writeUInt32BE(label, 0);
  return Buffer.concat([
    Buffer.from("825820", "hex"),
    txId,
    Buffer.from("190000", "hex"),
  ]);
};
const entry = (label: number) => ({
  outref: outRef(label),
  output: output(1_000_000 + label),
});

/** Labels 0 to `ENTRIES` are held; the delta spends the first `CHANGES`
 * and produces as many new ones. */
const DELTA: ConfirmedLedgerDeltaOp[] = [
  ...Array.from({ length: CHANGES }, (_, label) => ({
    kind: "spent" as const,
    outref: outRef(label),
  })),
  ...Array.from({ length: CHANGES }, (_, index) => ({
    kind: "produced" as const,
    ...entry(ENTRIES + index),
  })),
];

/** The worker started on `DELTA` as the verification starts it. */
const startWorker = () => {
  const { request, transfer } = packConfirmedLedgerRootsRequest(DELTA);
  return new Worker(WORKER, {
    workerData: request,
    transferList: transfer,
    execArgv: [...BUNDLED_WORKER_EXEC_ARGV],
    resourceLimits: {
      maxOldGenerationSizeMb: CONFIRMED_LEDGER_ROOT_WORKER_HEAP_MB,
    },
  });
};

/** Posts `command` to `worker` and parses its one reply. */
const ask = <A>(
  worker: Worker,
  command: ConfirmedLedgerRootsCommand,
  transfer: ArrayBuffer[],
  parse: (reply: unknown) => A,
) =>
  new Promise<A>((resolve, reject) => {
    const onError = (error: Error) => {
      worker.off("message", onMessage);
      reject(error);
    };
    const onMessage = (reply: unknown) => {
      worker.off("error", onError);
      try {
        resolve(parse(reply));
      } catch (error) {
        reject(error as Error);
      }
    };
    worker.once("message", onMessage);
    worker.once("error", onError);
    worker.postMessage(command, transfer);
  });

/** The worker's heap in use, with the array buffers it holds outside it,
 * after a full collection: taking a heap snapshot collects first. */
const heapAfterCollection = async (worker: Worker) => {
  for await (const chunk of await worker.getHeapSnapshot()) void chunk;
  const heap: HeapInfo = await worker.getHeapStatistics();
  return heap.used_heap_size + (heap.external_memory ?? 0);
};

describe("the built root worker's heap", () => {
  it(`stays flat over the pages of ${ENTRIES.toString()} entries`, async () => {
    const startedAt = Date.now();
    const worker = startWorker();
    const heaps: number[] = [];
    const leaves = new Uint8Array(ENTRIES * LEAF_BYTES);
    let sorted = new Uint8Array(0);
    let roots: ConfirmedLedgerRoots | undefined;
    try {
      // Checkpoints after entry pages 5, 15, 25, 35 and 45 of 50.
      for (
        let from = 0, page = 0;
        from < ENTRIES;
        from += CONFIRMED_LEDGER_ENTRY_PAGE, page += 1
      ) {
        const { command, transfer } = leavesCommand(
          Array.from(
            { length: Math.min(CONFIRMED_LEDGER_ENTRY_PAGE, ENTRIES - from) },
            (_, index) => entry(from + index),
          ),
        );
        leaves.set(
          await ask(
            worker,
            command,
            transfer,
            parseConfirmedLedgerLeavesResponse,
          ),
          from * LEAF_BYTES,
        );
        if (page % 10 === 4) heaps.push(await heapAfterCollection(worker));
      }
      // Checkpoints after feeds 4, 7 and 10 of 10.
      sorted = sortedLeaves(leaves);
      const size = CONFIRMED_LEDGER_LEAF_PAGE * LEAF_BYTES;
      for (let at = 0, feed = 1; at < sorted.length; at += size, feed += 1) {
        const owned = sorted.slice(at, at + size);
        await ask(
          worker,
          { kind: "feed", leaves: owned.buffer },
          [owned.buffer],
          parseConfirmedLedgerFedResponse,
        );
        if (feed % 3 === 1 && feed > 1)
          heaps.push(await heapAfterCollection(worker));
      }
      roots = await ask(
        worker,
        { kind: "roots" },
        [],
        parseConfirmedLedgerRootsResponse,
      );
    } finally {
      await worker.terminate();
    }
    const [early, ...later] = heaps;
    const late = Math.max(...later);
    console.log(
      `${ENTRIES.toString()} entries in ${(Date.now() - startedAt).toString()} ms: worker heap after collection ${heaps.map((each) => (each / MB).toFixed(1)).join(", ")} MB`,
    );
    expect(heaps).toHaveLength(8);
    expect(late - early!).toBeLessThan(BOUND_MB * MB);
    const here = new ConfirmedLedgerRootsBuilder(DELTA);
    here.feed(sorted);
    expect(roots).toEqual(here.roots());
    expect(roots?.tailRoot).not.toBe(roots?.frontierRoot);
  }, 300_000);
});
