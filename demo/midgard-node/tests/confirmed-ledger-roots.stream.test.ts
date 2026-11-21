/**
 * The landed ledger's roots from one pass over its leaves in path order
 * (`confirmed-ledger-roots.ts`, `mpf-sorted-root.ts`): byte for byte the
 * roots the node's Merkle Patricia Forestry trie derives
 * (`computeLedgerMpfRootFromLedgerEntries`), for random ledgers and random
 * delta sequences, in any number of feeds; for the empty ledger, one leaf,
 * and leaves whose paths share long prefixes. Leaves out of path order are
 * refused, and so is a delta whose changes do not apply.
 */
import { Effect } from "effect";
import * as fc from "fast-check";
import { describe, expect, it } from "vitest";

import type { ConfirmedLedgerDeltaOp } from "../src/mpf/confirmed-ledger-root-message.js";
import {
  ConfirmedLedgerRootsBuilder,
  confirmedLedgerRootsOf,
  LEAF_BYTES,
  ledgerLeaves,
  sortedLeaves,
} from "../src/mpf/confirmed-ledger-roots.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { mpfPath, SortedMpfRoot } from "../src/mpf/mpf-sorted-root.js";

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
const entry = (label: number, lovelace = 1_000_000 + label) => ({
  outref: outRef(label),
  output: output(lovelace),
});
type Entry = ReturnType<typeof entry>;

const rootOf = (entries: readonly Entry[]) =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([...entries]));
const rootsOf = (
  confirmed: readonly Entry[],
  delta: ConfirmedLedgerDeltaOp[],
) => Effect.runPromise(confirmedLedgerRootsOf({ confirmed, delta }));

/** The roots of `confirmed`'s leaves fed in path order, `page` leaves at
 * a time. */
const fedInPages = (
  confirmed: readonly Entry[],
  delta: ConfirmedLedgerDeltaOp[],
  page: number,
) => {
  const builder = new ConfirmedLedgerRootsBuilder(delta);
  const leaves = sortedLeaves(ledgerLeaves(confirmed));
  for (let at = 0; at < leaves.length; at += page * LEAF_BYTES)
    builder.feed(leaves.subarray(at, at + page * LEAF_BYTES));
  return builder.roots();
};

/** A ledger of distinct labels, and a delta sequence over it that applies:
 * each step spends a held outref, or produces one not held (a new label,
 * or one spent earlier, again). */
const ledgerAndDelta = fc
  .record({
    labels: fc.uniqueArray(fc.integer({ min: 0, max: 5_000 }), {
      maxLength: 60,
    }),
    steps: fc.array(
      fc.record({
        spend: fc.boolean(),
        pick: fc.nat(),
        lovelace: fc.integer({ min: 2 ** 16, max: 2 ** 32 - 1 }),
      }),
      { maxLength: 40 },
    ),
  })
  .map(({ labels, steps }) => {
    const confirmed = labels.map((label) => entry(label));
    const held = new Map(
      confirmed.map((each) => [each.outref.toString("hex"), each]),
    );
    const spent: Entry[] = [];
    const delta: ConfirmedLedgerDeltaOp[] = [];
    let fresh = 10_000;
    for (const { spend, pick, lovelace } of steps) {
      const holding = [...held.values()];
      if (spend && holding.length > 0) {
        const gone = holding[pick % holding.length]!;
        held.delete(gone.outref.toString("hex"));
        spent.push(gone);
        delta.push({ kind: "spent", outref: gone.outref });
        continue;
      }
      const again = spent.length > 0 && pick % 3 === 0;
      const made = again
        ? {
            outref: spent.splice(pick % spent.length, 1)[0]!.outref,
            output: output(lovelace),
          }
        : entry(fresh++, lovelace);
      held.set(made.outref.toString("hex"), made);
      delta.push({ kind: "produced", ...made });
    }
    return { confirmed, delta, tail: [...held.values()] };
  });

describe("the landed ledger's roots in one pass", () => {
  it("derives the trie's roots of a random ledger and delta, in any number of feeds", async () => {
    await fc.assert(
      fc.asyncProperty(
        ledgerAndDelta,
        fc.integer({ min: 1, max: 7 }),
        async ({ confirmed, delta, tail }, page) => {
          const expected = {
            frontierRoot: await rootOf(confirmed),
            tailRoot: await rootOf(tail),
          };
          expect(await rootsOf(confirmed, delta)).toEqual(expected);
          expect(fedInPages(confirmed, delta, page)).toEqual(expected);
        },
      ),
      { numRuns: 150 },
    );
  }, 120_000);

  it("derives the empty trie's root, and one leaf's", async () => {
    const empty = await rootOf([]);
    expect(await rootsOf([], [])).toEqual({
      frontierRoot: empty,
      tailRoot: empty,
    });
    expect(new SortedMpfRoot().root()).toBe(empty);
    const one = entry(7);
    expect(await rootsOf([one], [])).toEqual({
      frontierRoot: await rootOf([one]),
      tailRoot: await rootOf([one]),
    });
    expect(await rootsOf([], [{ kind: "produced", ...one }])).toEqual({
      frontierRoot: empty,
      tailRoot: await rootOf([one]),
    });
    expect(
      await rootsOf([one], [{ kind: "spent", outref: one.outref }]),
    ).toEqual({ frontierRoot: await rootOf([one]), tailRoot: empty });
  }, 60_000);

  it("derives the trie's root of leaves whose paths share long prefixes", async () => {
    // Of 60,000 labels, the pairs whose paths share the most leading
    // nibbles (about eight, past both an odd and an even count).
    const labels = Array.from({ length: 60_000 }, (_, label) => label);
    const paths = labels
      .map((label) => ({
        label,
        path: Buffer.from(mpfPath(outRef(label))).toString("hex"),
      }))
      .sort((left, right) => (left.path < right.path ? -1 : 1));
    const shared = (left: string, right: string) => {
      let nibbles = 0;
      while (nibbles < 64 && left[nibbles] === right[nibbles]) nibbles += 1;
      return nibbles;
    };
    const pairs = paths
      .slice(1)
      .map((each, index) => ({
        labels: [paths[index]!.label, each.label],
        nibbles: shared(paths[index]!.path, each.path),
      }))
      .sort((left, right) => right.nibbles - left.nibbles)
      .slice(0, 12);
    expect(pairs[0]!.nibbles).toBeGreaterThanOrEqual(7);
    expect(new Set(pairs.map(({ nibbles }) => nibbles % 2)).size).toBe(2);
    const close = [...new Set(pairs.flatMap(({ labels: each }) => each))].map(
      (label) => entry(label),
    );
    // Each pair alone, all pairs together, and with a spread of others.
    for (const { labels: each } of pairs) {
      const pair = each.map((label) => entry(label));
      expect((await rootsOf(pair, [])).frontierRoot).toBe(await rootOf(pair));
    }
    const spread = [
      ...close,
      ...labels.slice(0, 40).map((label) => entry(label)),
    ];
    const ledger = [
      ...new Map(
        spread.map((each) => [each.outref.toString("hex"), each]),
      ).values(),
    ];
    const gone = close.slice(0, 6);
    const delta: ConfirmedLedgerDeltaOp[] = gone.map((each) => ({
      kind: "spent",
      outref: each.outref,
    }));
    expect(await rootsOf(ledger, delta)).toEqual({
      frontierRoot: await rootOf(ledger),
      tailRoot: await rootOf(ledger.filter((each) => !gone.includes(each))),
    });
  }, 120_000);

  it("refuses leaves out of path order", () => {
    const leaves = sortedLeaves(ledgerLeaves([entry(1), entry(2)]));
    const first = leaves.subarray(0, LEAF_BYTES);
    const second = leaves.subarray(LEAF_BYTES);
    const twice = new ConfirmedLedgerRootsBuilder([]);
    twice.feed(first);
    expect(() => twice.feed(first)).toThrow("trie leaves out of path order");
    const backwards = new ConfirmedLedgerRootsBuilder([]);
    backwards.feed(second);
    expect(() => backwards.feed(first)).toThrow(
      "trie leaves out of path order",
    );
    expect(() =>
      new ConfirmedLedgerRootsBuilder([]).feed(first.subarray(1)),
    ).toThrow("packed leaves are not whole 64-byte leaves");
  });

  it("refuses a delta whose changes do not apply", async () => {
    const [a, b] = [entry(1), entry(2)];
    const failure = (confirmed: Entry[], delta: ConfirmedLedgerDeltaOp[]) =>
      Effect.runPromise(
        Effect.flip(confirmedLedgerRootsOf({ confirmed, delta })),
      ).then((error) => (error as Error).message);
    const hex = (each: Entry) => each.outref.toString("hex");
    expect(await failure([a], [{ kind: "spent", outref: b.outref }])).toContain(
      `the delta spends ${hex(b)}, which confirmed_ledger does not hold`,
    );
    expect(await failure([a], [{ kind: "produced", ...a }])).toContain(
      `the delta produces ${hex(a)}, which confirmed_ledger already holds`,
    );
    expect(
      await failure(
        [],
        [
          { kind: "produced", ...b },
          { kind: "produced", ...b },
        ],
      ),
    ).toContain(`the delta produces ${hex(b)} again before spending it`);
    expect(
      await failure(
        [a],
        [
          { kind: "spent", outref: a.outref },
          { kind: "spent", outref: a.outref },
        ],
      ),
    ).toContain(`the delta spends ${hex(a)} again before producing it`);
  });
});
