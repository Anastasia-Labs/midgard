/**
 * The built root worker (`dist/confirmed-ledger-root.js`), started as the
 * verification starts it (`confirmedLedgerRootsInWorkerThread`, under its
 * fixed heap limit), derives the roots of a 1,000,000-entry
 * `confirmed_ledger` with a delta of 2,000 changes: the entries go to it a
 * page at a time, as the verification reads them, and their leaves come
 * back to be fed in path order. Its roots are the ones the same pass in
 * this thread derives.
 */
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  CONFIRMED_LEDGER_ENTRY_PAGE,
  CONFIRMED_LEDGER_LEAF_PAGE,
} from "../src/landed-blocks/ledger-leaves.js";
import type { ConfirmedLedgerDeltaOp } from "../src/mpf/confirmed-ledger-root-message.js";
import { confirmedLedgerRootsInWorkerThread } from "../src/mpf/confirmed-ledger-root-worker.js";
import {
  ConfirmedLedgerRootsBuilder,
  LEAF_BYTES,
  sortedLeaves,
} from "../src/mpf/confirmed-ledger-roots.js";

const ENTRIES = 1_000_000;
const CHANGES = 1_000;

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

describe("the built root worker at size", () => {
  it(`derives the roots of ${ENTRIES.toString()} entries a page at a time`, async () => {
    const startedAt = Date.now();
    const { roots, leaves } = await Effect.runPromise(
      Effect.scoped(
        Effect.gen(function* () {
          const session = yield* confirmedLedgerRootsInWorkerThread(DELTA);
          const leaves = new Uint8Array(ENTRIES * LEAF_BYTES);
          for (
            let from = 0;
            from < ENTRIES;
            from += CONFIRMED_LEDGER_ENTRY_PAGE
          ) {
            const page = Array.from(
              { length: Math.min(CONFIRMED_LEDGER_ENTRY_PAGE, ENTRIES - from) },
              (_, index) => entry(from + index),
            );
            leaves.set(yield* session.leaves(page), from * LEAF_BYTES);
          }
          const sorted = sortedLeaves(leaves);
          const page = CONFIRMED_LEDGER_LEAF_PAGE * LEAF_BYTES;
          for (let at = 0; at < sorted.length; at += page)
            yield* session.feed(sorted.subarray(at, at + page));
          return { roots: yield* session.roots, leaves: sorted };
        }),
      ),
    );
    console.log(
      `${ENTRIES.toString()} entries: roots in the worker after ${(Date.now() - startedAt).toString()} ms`,
    );
    const here = new ConfirmedLedgerRootsBuilder(DELTA);
    here.feed(leaves);
    expect(roots).toEqual(here.roots());
    expect(roots.tailRoot).not.toBe(roots.frontierRoot);
  }, 1_800_000);
});
