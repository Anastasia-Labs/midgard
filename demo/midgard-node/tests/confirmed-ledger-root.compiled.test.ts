/**
 * The built root worker (`dist/confirmed-ledger-root.js`) as the
 * verification starts it, on a request the source codec packs: it posts
 * the root of `confirmed_ledger` and, from the same trie, the root with the
 * deltas applied, each derived on its own. A `confirmed_ledger` entry the
 * delta spends changes the frontier's root and leaves the tail's as it
 * was; a request of another shape is refused by name.
 */
import { Worker } from "node:worker_threads";

import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type ConfirmedLedgerRootsInput,
  packConfirmedLedgerRootsRequest,
} from "../src/mpf/confirmed-ledger-root-message.js";
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

const posted = async (
  workerData: unknown,
  transferList: ArrayBuffer[] = [],
) => {
  const worker = new Worker(WORKER, {
    workerData,
    transferList,
    execArgv: [...BUNDLED_WORKER_EXEC_ARGV],
  });
  try {
    return await new Promise<unknown>((resolve, reject) => {
      worker.once("message", resolve);
      worker.once("error", reject);
    });
  } finally {
    await worker.terminate();
  }
};

const roots = (input: ConfirmedLedgerRootsInput) => {
  const { request, transfer } = packConfirmedLedgerRootsRequest(input);
  return posted(request, transfer);
};

const A = entry(1);
const B = entry(2);
const C = entry(3);
const D = entry(4);

describe("the built root worker", () => {
  it("posts the frontier's and the tail's roots from one input", async () => {
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

  it("posts the empty root for an empty input", async () => {
    const empty = await rootOf([]);
    expect(await roots({ confirmed: [], delta: [] })).toEqual({
      frontierRoot: empty,
      tailRoot: empty,
    });
  }, 60_000);

  it("posts a failure for a delta that does not apply to the trie", async () => {
    expect(
      await roots({
        confirmed: [A],
        delta: [{ kind: "produced", ...A }],
      }),
    ).toEqual({
      error: expect.stringContaining(
        "inserting a new entry in confirmed-ledger-roots MPF",
      ) as unknown as string,
    });
  }, 60_000);

  it("refuses a request of another shape by name", async () => {
    expect(await posted({ outRefs: new ArrayBuffer(0) })).toEqual({
      error: "the request is not confirmed_ledger, a delta and its kinds",
    });
  }, 60_000);
});
