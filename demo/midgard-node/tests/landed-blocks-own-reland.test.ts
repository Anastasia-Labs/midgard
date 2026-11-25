/**
 * An own block on a parent that relands in the same run after a `retain`
 * dropped the parent's ledger: the relanded row is kept, not processed
 * again, so nothing persisted its root this run, and the child's check
 * rebuilds the parent's ledger from the newest retained root on the
 * processed chain by the rows' deltas. Both blocks are processed in that
 * run with no local fault.
 */
import { Effect } from "effect";
import { expect, it } from "vitest";

import { LANDED_BLOCK_REBASE_PENDING } from "../src/landed-blocks/holds.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { markApplied, retrieveRows } from "../src/landed-blocks/store.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  A1,
  type Block,
  block,
  entry,
  fixture,
  G1,
  harness,
  inInstalledNode,
  localFaultAlarms,
  opening,
  ports,
  queueOf,
  rootOf,
} from "./landed-blocks-own.fixture.js";

const B1 = entry("b1", 5_000_000n);

it("processes an own block and its relanded parent in one run after a retain dropped the parent's ledger, with no local fault", async () => {
  const { a, genesisRoot, genesisState, journalA } = await fixture();
  const b = block(
    3,
    { hash: a.hash, root: a.header.utxosRoot, endTime: a.header.endTime },
    await rootOf([G1, B1]),
  );
  const state = harness();
  state.journals.set(a.hash, journalA);
  state.journals.set(b.hash, {
    baseTailHeaderHash: a.hash,
    baseUtxosRoot: a.header.utxosRoot,
    expectedUtxosRoot: b.header.utxosRoot,
    spent: [A1.outref],
    produced: [B1],
    depositIds: [],
    withdrawals: [],
    forcedIds: [],
    txIds: [simDigest("own:tx-b")],
  });
  await inInstalledNode(state)(
    Effect.gen(function* () {
      const process = (nodes: readonly Block[]) =>
        processLandedQueue(ports(state), queueOf(genesisState, nodes));
      yield* process([a]);
      // What the rebase marks once it applied `a`: leaving the queue then
      // makes its row `removed`, kept for it to reland.
      yield* markApplied([a.hash]);
      yield* process([]);
      expect((yield* retrieveRows).map((row) => row.state)).toEqual([
        "removed",
      ]);
      const ledgers = yield* Effect.promise(() => state.ledgers);
      yield* Effect.promise(() => ledgers.retain(new Set([genesisRoot])));
      expect(yield* opening(state, a.header.utxosRoot)).toBe(
        "LedgerNotRetained",
      );

      const alarms = yield* localFaultAlarms();
      const held = yield* process([a, b]);
      expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
      const rows = yield* retrieveRows;
      expect(rows).toHaveLength(2);
      expect(rows).toContainEqual(
        expect.objectContaining({ headerHash: a.hash, state: "processed" }),
      );
      expect(rows).toContainEqual(
        expect.objectContaining({
          headerHash: b.hash,
          kind: "own",
          state: "processed",
        }),
      );
      expect((yield* localFaultAlarms()) - alarms).toBe(0n);
      expect(yield* opening(state, b.header.utxosRoot)).toBe("opened");
    }),
  );
}, 120_000);
