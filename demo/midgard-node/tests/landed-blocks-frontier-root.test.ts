/**
 * The ledger store holds the confirmed-ledger frontier's root once a block
 * folds into it, even when a `retain` dropped that block's root while it
 * waited to merge (it and its child left the retention window, which names
 * only the frontier's root and the newest processed one). An L1 rollback
 * that then removes every row after the frontier leaves a new block whose
 * parent is the frontier, with no processed row to rebuild its ledger
 * from: it is processed with no local fault. The same holds for a frontier
 * startup's repair folded before the store was open. When the store lost
 * the old frontier's root too, so the new one cannot be rebuilt, the fold
 * still moves the frontier.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import { foldMerge } from "../src/landed-blocks/confirmed-merges.js";
import { LANDED_BLOCK_REBASE_PENDING } from "../src/landed-blocks/holds.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import {
  Frontier,
  markApplied,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  A1,
  block,
  confirmedAt,
  entry,
  fixture,
  G0,
  G1,
  GENESIS,
  harness,
  inNode,
  localFaultAlarms,
  opening,
  ports,
  queueOf,
  rootOf,
} from "./landed-blocks-own.fixture.js";

const B1 = entry("b1", 5_000_000n);
const C1 = entry("c1", 6_000_000n);

/** Own blocks `a`, then `b` and `c` each on `a`, their journals in the
 * harness; `merged` is the queue root at `a`. */
const setup = async () => {
  const { a, genesisRoot, genesisState, journalA } = await fixture();
  const onA = { hash: a.hash, root: a.header.utxosRoot, endTime: 1_000n };
  const b = block(3, onA, await rootOf([G1, B1]));
  const c = block(4, onA, await rootOf([A1, C1]));
  const state = harness();
  state.journals.set(a.hash, journalA);
  const onAJournal = (
    spent: Buffer,
    produced: typeof B1,
    expectedUtxosRoot: string,
    tx: string,
  ) => ({
    baseTailHeaderHash: a.hash,
    baseUtxosRoot: a.header.utxosRoot,
    expectedUtxosRoot,
    spent: [spent],
    produced: [produced],
    depositIds: [],
    withdrawals: [],
    forcedIds: [],
    txIds: [simDigest(tx)],
  });
  state.journals.set(
    b.hash,
    onAJournal(A1.outref, B1, b.header.utxosRoot, "own:tx-b"),
  );
  state.journals.set(
    c.hash,
    onAJournal(G1.outref, C1, c.header.utxosRoot, "own:tx-c"),
  );
  const merged = confirmedAt(a, a.header.utxosRoot);
  /** Processes `a` and `b`, marks both applied, and has the store retain
   * `roots` alone. */
  const processedThenRetained = (roots: readonly string[]) =>
    Effect.gen(function* () {
      yield* processLandedQueue(ports(state), queueOf(genesisState, [a, b]));
      // What the rebase marks once it applied both.
      yield* markApplied([a.hash, b.hash]);
      const ledgers = yield* Effect.promise(() => state.ledgers);
      yield* Effect.promise(() => ledgers.retain(new Set(roots)));
      expect(yield* opening(state, a.header.utxosRoot)).toBe(
        "LedgerNotRetained",
      );
      return ledgers;
    });
  return { a, b, c, genesisRoot, state, merged, processedThenRetained };
};

it("processes a block on the frontier after a rollback removed every later row, when a retain dropped the frontier block's root before it folded", async () => {
  const { a, b, c, genesisRoot, state, merged, processedThenRetained } =
    await setup();
  await inNode(
    Effect.gen(function* () {
      // Both left the window unmerged: retention names the frontier's root
      // and the newest processed one.
      const ledgers = yield* processedThenRetained([
        genesisRoot,
        b.header.utxosRoot,
      ]);

      // `a` merges and folds; retention then names the new frontier's root.
      yield* processLandedQueue(ports(state), queueOf(merged, [b]));
      yield* Effect.promise(() =>
        ledgers.retain(new Set([a.header.utxosRoot, b.header.utxosRoot])),
      );
      // A rollback removes `b`, the only row after the frontier.
      yield* processLandedQueue(ports(state), queueOf(merged, []));
      expect((yield* retrieveRows).map((row) => row.state)).toEqual([
        "removed",
      ]);

      const alarms = yield* localFaultAlarms();
      const held = yield* processLandedQueue(
        ports(state),
        queueOf(merged, [c]),
      );
      expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
      expect(yield* retrieveRows).toContainEqual(
        expect.objectContaining({ headerHash: c.hash, state: "processed" }),
      );
      expect((yield* localFaultAlarms()) - alarms).toBe(0n);
    }),
  );
}, 120_000);

it("processes a block on the frontier the startup fold of an own journal chain set, when the store retains only the chain's base", async () => {
  const { a, c, genesisRoot, state, merged } = await setup();
  await inNode(
    Effect.gen(function* () {
      // What startup's repair of an unanchored confirmed ledger writes:
      // the frontier at the chain's base, then the stored-delta fold of
      // `a`'s journal. The store retains the base ledger, not `a`'s.
      yield* ConfirmedLedgerDB.insertMultiple([
        ...(yield* ledgerRows(GENESIS, new Map())),
      ]);
      yield* Frontier.upsert({
        headerHash: SDK.GENESIS_HEADER_HASH,
        utxosRoot: genesisRoot,
      });
      yield* foldMerge(
        {
          headerHash: a.hash,
          parentHeaderHash: SDK.GENESIS_HEADER_HASH,
          parentUtxosRoot: genesisRoot,
          utxosRoot: a.header.utxosRoot,
          kind: "own",
          state: "processed",
          applied: true,
          spent: [G0.outref],
          produced: [A1],
          depositIds: [],
          withdrawals: [],
          forcedIds: [],
          txIds: [simDigest("own:tx-a")],
        },
        null,
      );
      expect(yield* opening(state, a.header.utxosRoot)).toBe(
        "LedgerNotRetained",
      );

      const alarms = yield* localFaultAlarms();
      const held = yield* processLandedQueue(
        ports(state),
        queueOf(merged, [c]),
      );
      expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
      expect(yield* retrieveRows).toContainEqual(
        expect.objectContaining({ headerHash: c.hash, state: "processed" }),
      );
      expect((yield* localFaultAlarms()) - alarms).toBe(0n);
    }),
  );
}, 120_000);

it("moves the frontier to a block whose root the store neither retains nor can rebuild", async () => {
  const { a, b, state, merged, processedThenRetained } = await setup();
  await inNode(
    Effect.gen(function* () {
      // The store keeps neither `a`'s root nor the frontier's before it.
      yield* processedThenRetained([b.header.utxosRoot]);
      yield* processLandedQueue(ports(state), queueOf(merged, [b]));
      expect(yield* Frontier.retrieve).toEqual({
        headerHash: a.hash,
        utxosRoot: a.header.utxosRoot,
      });
      expect(yield* opening(state, a.header.utxosRoot)).toBe(
        "LedgerNotRetained",
      );
    }),
  );
}, 120_000);
