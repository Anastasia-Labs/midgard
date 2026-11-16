/**
 * The built root worker (`dist/confirmed-ledger-root.js`) as the
 * verification starts it (`confirmedLedgerRootsInWorkerThread`), fed
 * `confirmed_ledger`'s leaves in path order: it gives the root of
 * `confirmed_ledger` and, from the same pass, the root with the deltas
 * applied, each derived on its own. A `confirmed_ledger` entry the delta
 * spends changes the frontier's root and leaves the tail's as it was; a
 * delta that does not apply to `confirmed_ledger` fails the pass by name,
 * and a request of another shape is refused by name.
 */
import { Worker } from "node:worker_threads";

import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import type { ConfirmedLedgerRootsInput } from "../src/mpf/confirmed-ledger-root-message.js";
import { confirmedLedgerRootsInWorkerThread } from "../src/mpf/confirmed-ledger-root-worker.js";
import { confirmedLedgerRootsOf } from "../src/mpf/confirmed-ledger-roots.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { BUNDLED_WORKER_EXEC_ARGV } from "../src/services/validation-pool.bundled-worker-exec-argv.js";

const WORKER = new URL("../dist/confirmed-ledger-root.js", import.meta.url);

/** A Midgard output holding `lovelace` (below 2^32). */
const output = (lovelace: number) =>
  Buffer.from(
    `a200583900899d9f267b0acf680fe51468ad46dca83b5e4d28e0dad6c68dbf0e2ea04755ad14d178a134575bba463b566827d339fdbdf0ec961814491c01821a${lovelace
      .toString(16)
      .padStart(8, "0")}a0`,
    "hex",
  );
const outRef = (label: number) =>
  Buffer.from(
    `825820${label.toString(16).padStart(2, "0").repeat(32)}19${label
      .toString(16)
      .padStart(4, "0")}`,
    "hex",
  );
const entry = (label: number, lovelace = 1_000_000 + label) => ({
  outref: outRef(label),
  output: output(lovelace),
});
const rootOf = (entries: readonly { outref: Buffer; output: Buffer }[]) =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([...entries]));

/** The reply of a worker started on `workerData` to a roots command. */
const replyOn = async (workerData: unknown) => {
  const worker = new Worker(WORKER, {
    workerData,
    execArgv: [...BUNDLED_WORKER_EXEC_ARGV],
  });
  try {
    return await new Promise<unknown>((resolve, reject) => {
      worker.once("message", resolve);
      worker.once("error", reject);
      worker.postMessage({ kind: "roots" });
    });
  } finally {
    await worker.terminate();
  }
};

const roots = (input: ConfirmedLedgerRootsInput) =>
  Effect.runPromise(
    Effect.either(
      confirmedLedgerRootsOf(input, confirmedLedgerRootsInWorkerThread),
    ),
  ).then((outcome) =>
    Either.isRight(outcome)
      ? outcome.right
      : { error: (outcome.left as Error).message },
  );

const A = entry(1);
const B = entry(2);
const C = entry(3);
const D = entry(4);

describe("the built root worker", () => {
  it("gives the frontier's and the tail's roots from one pass", async () => {
    // B is produced and spent again after the frontier.
    expect(
      await roots({
        confirmed: [A, B],
        delta: [
          { kind: "spent", outref: A.outref },
          { kind: "produced", ...C },
          { kind: "spent", outref: B.outref },
          { kind: "produced", ...D },
        ],
      }),
    ).toEqual({
      frontierRoot: await rootOf([A, B]),
      tailRoot: await rootOf([C, D]),
    });
  }, 60_000);

  it("derives each root on its own: a changed entry the delta spends moves only the frontier's", async () => {
    const changed = { ...A, output: output(9_000_000) };
    const delta = [
      { kind: "spent" as const, outref: A.outref },
      { kind: "produced" as const, ...C },
    ];
    const response = await roots({ confirmed: [changed, B], delta });
    expect(response).toEqual({
      frontierRoot: await rootOf([changed, B]),
      tailRoot: await rootOf([B, C]),
    });
    expect(response).not.toMatchObject({ frontierRoot: await rootOf([A, B]) });
  }, 60_000);

  it("gives the empty root for an empty ledger", async () => {
    const empty = await rootOf([]);
    expect(await roots({ confirmed: [], delta: [] })).toEqual({
      frontierRoot: empty,
      tailRoot: empty,
    });
  }, 60_000);

  it("fails a pass whose delta does not apply to confirmed_ledger", async () => {
    expect(
      await roots({
        confirmed: [A],
        delta: [{ kind: "produced", ...A }],
      }),
    ).toEqual({
      error: `confirmed_ledger root worker failed: the delta produces ${A.outref.toString("hex")}, which confirmed_ledger already holds`,
    });
    expect(
      await roots({
        confirmed: [A],
        delta: [{ kind: "spent", outref: B.outref }],
      }),
    ).toEqual({
      error: `confirmed_ledger root worker failed: the delta spends ${B.outref.toString("hex")}, which confirmed_ledger does not hold`,
    });
  }, 60_000);

  it("refuses a request of another shape by name", async () => {
    expect(await replyOn({ outRefs: new ArrayBuffer(0) })).toEqual({
      error: "the request is not a delta and its kinds",
    });
  }, 60_000);
});
