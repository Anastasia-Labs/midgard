/**
 * The temporal `confirmed_ledger` on this node's own merged block (plan
 * §10.5, §15 N5), deterministic, on the node database: a merge a rollback
 * undid is unfolded back to the header the root returned to (its spent
 * outputs, its landed row and its consumed deposit come back); a fold on a
 * wrong base is refused by name and writes nothing; and a fork onto another
 * header with the same UTxO root is bound by header identity, never
 * re-anchored by the equal root. A fold is released (`releaseFinalFolds`)
 * only once the prune boundary reaches its merge, never when unfolded: the
 * pending-table rows a folded own or foreign block marked stay marked until
 * that release deletes them, and an unfold before it loses none of them.
 */
import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";
import { describe, expect, it } from "vitest";

import { fullScanCounter } from "../src/database/confirmedLedger.js";
import { DepositsDB } from "../src/database/index.js";
import type { LandedStateQueueElement } from "../src/l1-state-queue/index.js";
import {
  pruneMerges,
  retrieveMergeLinks,
} from "../src/landed-blocks/confirmed-merges.js";
import { CONFIRMED_LEDGER_BASE_MISMATCH } from "../src/landed-blocks/holds.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import {
  Frontier,
  markApplied,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import { makeDepositEntry } from "./database.test/fixtures.make-deposit-submission-attempt.js";
import { insertDeposits } from "./helpers/event-rows.js";
import { admitPending } from "./helpers/landed-blocks-sim.mempool.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  AFTER_A,
  block,
  confirmedAt,
  confirmedKeys,
  fixture,
  GENESIS,
  harness,
  hex,
  inNode,
  ports,
  queueOf,
  sortedKeys,
} from "./landed-blocks-own.fixture.js";

/** `effect`'s result and the whole-`confirmed_ledger` reads it made. */
const scansOf = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const before = (yield* Metric.value(fullScanCounter)).count;
    const result = yield* effect;
    const after = (yield* Metric.value(fullScanCounter)).count;
    return { result, scans: after - before };
  });

const depositStatus = (id: Buffer) =>
  DepositsDB.retrieveByEventId(id).pipe(
    Effect.map((found) =>
      found._tag === "Some" ? found.value[DepositsDB.Columns.STATUS] : null,
    ),
  );

/** Every mempool row (with its delta) as `id@marking header`, or `id@-` when pending. */
const mempoolMarks = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ tx_id: Buffer; included_by: Buffer | null }>`
    SELECT m.tx_id, m.included_by FROM mempool m
    JOIN mempool_tx_deltas d ON d.tx_id = m.tx_id
    ORDER BY m.tx_id`;
  const deltas = yield* sql<{ count: string }>`
    SELECT COUNT(*) AS count FROM mempool_tx_deltas`;
  expect(Number(deltas[0]!.count)).toBe(rows.length);
  return rows.map(
    (row) =>
      `${hex(row.tx_id)}@${row.included_by === null ? "-" : hex(row.included_by)}`,
  );
});

/** A queue output as the history retains it, created at `slot`. */
const createdAt = (
  element: LandedStateQueueElement,
  slot: number,
): LandedStateQueueElement => ({ ...element, created: { slot, txIndex: 0 } });

describe("temporal confirmed_ledger on an own merged block", () => {
  it("unfolds a merge a rollback undid back to the root's header, then folds it again", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, { ...journalA, status: "locally_applied" });
    const deposit = makeDepositEntry({
      [DepositsDB.Columns.PROJECTED_HEADER_HASH]: Buffer.from(a.hash, "hex"),
      [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
    });
    const id = deposit[DepositsDB.Columns.ID];
    const unmerged = queueOf(genesisState, [a]);
    const merged = queueOf(confirmedAt(a, ""), []);
    await inNode(
      Effect.gen(function* () {
        yield* insertDeposits([deposit]);
        yield* processLandedQueue(ports(state), unmerged);
        // The fold and the unfold below read no whole ledger: O(delta).
        expect(
          yield* scansOf(processLandedQueue(ports(state), merged)),
        ).toEqual({ result: undefined, scans: 0 });
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
        expect(yield* depositStatus(id)).toBe(DepositsDB.Status.Consumed);

        // The merge rolls back: the root is genesis again, `a` a node.
        const unfold = yield* scansOf(
          processLandedQueue(ports(state), unmerged),
        );
        expect(unfold.scans).toBe(0);
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: SDK.GENESIS_HEADER_HASH,
          utxosRoot: genesisState.utxoRoot,
        });
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));
        expect(yield* depositStatus(id)).toBe(DepositsDB.Status.Projected);
        expect((yield* retrieveMergeLinks).size).toBe(0);
        const rows = yield* retrieveRows;
        expect(
          rows.map((row) => [row.headerHash, row.kind, row.state, row.applied]),
        ).toEqual([[a.hash, "own", "processed", true]]);

        // Merged again: folded again, exactly once.
        expect(yield* processLandedQueue(ports(state), merged)).toBeUndefined();
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
        expect(yield* depositStatus(id)).toBe(DepositsDB.Status.Consumed);
        expect([...(yield* retrieveMergeLinks).keys()]).toEqual([a.hash]);
        expect(state.replays).toHaveLength(0);
      }),
    );
  }, 120_000);

  it("refuses a fold on a wrong base by name, writing nothing", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, { ...journalA, status: "locally_applied" });
    const deposit = makeDepositEntry({
      [DepositsDB.Columns.PROJECTED_HEADER_HASH]: Buffer.from(a.hash, "hex"),
      [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
    });
    const id = deposit[DepositsDB.Columns.ID];
    const merged = queueOf(confirmedAt(a, ""), []);
    await inNode(
      Effect.gen(function* () {
        yield* insertDeposits([deposit]);
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        const unchanged = Effect.gen(function* () {
          expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));
          expect(yield* depositStatus(id)).toBe(DepositsDB.Status.Projected);
          expect((yield* retrieveMergeLinks).size).toBe(0);
          expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
            a.hash,
          ]);
        });

        // The frontier names `a`'s parent with another root.
        yield* Frontier.upsert({
          headerHash: SDK.GENESIS_HEADER_HASH,
          utxosRoot: a.header.utxosRoot,
        });
        const wrongRoot = yield* processLandedQueue(ports(state), merged);
        expect(wrongRoot?.reason).toBe(CONFIRMED_LEDGER_BASE_MISMATCH);
        expect(wrongRoot?.detail).toContain(a.hash);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );
        yield* unchanged;

        // The frontier is right, but confirmed_ledger lacks an output `a`
        // spends: refused after the merge row and the event marks were
        // written, which the refusal rolls back.
        yield* Frontier.upsert({
          headerHash: SDK.GENESIS_HEADER_HASH,
          utxosRoot: genesisState.utxoRoot,
        });
        const [spent] = journalA.spent;
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM confirmed_ledger WHERE outref = ${spent!}`;
        const missing = yield* processLandedQueue(ports(state), merged);
        expect(missing?.reason).toBe(CONFIRMED_LEDGER_BASE_MISMATCH);
        expect(missing?.detail).toContain("spends");
        expect(yield* depositStatus(id)).toBe(DepositsDB.Status.Projected);
        expect((yield* retrieveMergeLinks).size).toBe(0);
        expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
          a.hash,
        ]);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );
      }),
    );
  }, 120_000);

  it("binds a fork onto an equal-root header by header identity, never re-anchoring on the root", async () => {
    const { a, genesisRoot, genesisState, journalA } = await fixture();
    // `a2` is another own block on genesis with `a`'s delta: the same root.
    const a2 = block(
      9,
      { hash: SDK.GENESIS_HEADER_HASH, root: genesisRoot, endTime: 0n },
      a.header.utxosRoot,
    );
    expect(a2.hash).not.toBe(a.hash);
    const state = harness();
    state.journals.set(a.hash, { ...journalA, status: "locally_applied" });
    state.journals.set(a2.hash, { ...journalA, status: "locally_applied" });
    const deposit = makeDepositEntry({
      [DepositsDB.Columns.PROJECTED_HEADER_HASH]: Buffer.from(a.hash, "hex"),
      [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
    });
    const id = deposit[DepositsDB.Columns.ID];
    const mergedA2 = queueOf(confirmedAt(a2, ""), []);
    await inNode(
      Effect.gen(function* () {
        yield* insertDeposits([deposit]);
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        yield* processLandedQueue(
          ports(state),
          queueOf(confirmedAt(a, ""), []),
        );
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        expect(yield* depositStatus(id)).toBe(DepositsDB.Status.Consumed);

        // A rollback undid `a`'s commit and merge; `a2` landed and merged.
        const genesisQueue = queueOf(genesisState, [a2]);
        state.history = [
          createdAt(genesisQueue.root!, 1),
          createdAt(genesisQueue.nodes[0]!, 2),
          createdAt(mergedA2.root!, 3),
        ];
        yield* processLandedQueue(ports(state), mergedA2);
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: a2.hash,
          utxosRoot: a.header.utxosRoot,
        });
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
        // `a`'s fold is gone and its deposit reopened; `a2`'s is retained.
        expect([...(yield* retrieveMergeLinks).keys()]).toEqual([a2.hash]);
        expect(yield* depositStatus(id)).toBe(DepositsDB.Status.Projected);
        expect(
          (yield* retrieveRows).some(
            (row) => row.headerHash === a.hash && row.state === "processed",
          ),
        ).toBe(false);
      }),
    );
  }, 120_000);
  it("releases a fold only once the prune boundary reaches its merge, never one a rollback unfolded", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, { ...journalA, status: "locally_applied" });
    const unmerged = queueOf(genesisState, [a]);
    const merged = queueOf(confirmedAt(a, ""), []);
    const mergedAt7 = { ...merged, root: createdAt(merged.root!, 7) };
    const sql = (effect: ReturnType<typeof pruneMerges>) =>
      Effect.flatMap(SqlClient.SqlClient, (client) =>
        client.withTransaction(effect),
      );
    await inNode(
      Effect.gen(function* () {
        yield* processLandedQueue(ports(state), unmerged);
        yield* processLandedQueue(ports(state), mergedAt7);
        expect((yield* retrieveMergeLinks).get(a.hash)?.merge?.slot).toBe(7);
        // Below the merge: not final, not released.
        expect(yield* sql(pruneMerges(6))).toEqual([]);
        expect([...(yield* retrieveMergeLinks).keys()]).toEqual([a.hash]);

        // The merge rolls back: unfolded, so never released.
        yield* processLandedQueue(ports(state), unmerged);
        expect((yield* retrieveMergeLinks).size).toBe(0);
        expect(yield* sql(pruneMerges(1_000))).toEqual([]);

        // Merged again; the boundary reaches it: released once, still folded.
        yield* processLandedQueue(ports(state), mergedAt7);
        expect(yield* sql(pruneMerges(7))).toEqual([a.hash]);
        expect((yield* retrieveMergeLinks).size).toBe(0);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
        expect(yield* sql(pruneMerges(1_000))).toEqual([]);
        // Processing at a later boundary releases nothing further.
        expect(
          yield* processLandedQueue(ports(state), mergedAt7, {
            prunedThroughSlot: 1_000,
          }),
        ).toBeUndefined();
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
      }),
    );
  }, 120_000);
  it("keeps a folded block's rows marked until its fold is final, and loses none when a rollback unfolds it first", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, { ...journalA, status: "locally_applied" });
    // `a` (own) includes txA; the foreign `f` on it includes txF.
    const [txA] = journalA.txIds;
    const txF = simDigest("foreign:tx-f");
    const base = ports(state);
    const withIncludes: LandedBlockPorts<never> = {
      ...base,
      replay: (input) =>
        base
          .replay(input)
          .pipe(
            Effect.map((replayed) =>
              replayed.kind === "replayed"
                ? { ...replayed, txIds: [txF] }
                : replayed,
            ),
          ),
    };
    const at = (slot: number, queue: ReturnType<typeof queueOf>) => ({
      ...queue,
      root: createdAt(queue.root!, slot),
    });
    const fCommitted = queueOf(genesisState, [a, f]);
    const aMerged = at(7, queueOf(confirmedAt(a, ""), [f]));
    const fMerged = at(9, queueOf(confirmedAt(f, ""), []));
    const aMergedAlone = at(7, queueOf(confirmedAt(a, ""), []));
    const prune = (slot: number) =>
      Effect.flatMap(SqlClient.SqlClient, (client) =>
        client.withTransaction(pruneMerges(slot)),
      );
    const marked = (headerHash: string) => (id: Buffer) =>
      `${hex(id)}@${headerHash}`;
    await inNode(
      Effect.gen(function* () {
        yield* admitPending(
          [txA!, txF].map((id, index) => ({
            id,
            spent: [],
            produced: [],
            at: new Date(Date.parse("2026-10-01T00:00:00.000Z") + index),
          })),
        );
        yield* processLandedQueue(withIncludes, fCommitted);
        // The rebase applied `f` (its own suites cover the rebase).
        yield* markApplied([f.hash]);
        const both = [marked(a.hash)(txA!), marked(f.hash)(txF)].sort();
        expect(yield* mempoolMarks).toEqual(both);

        // Both merges land: both blocks fold, and their rows stay marked.
        yield* processLandedQueue(withIncludes, aMerged);
        yield* processLandedQueue(withIncludes, fMerged);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(f.hash);
        expect(
          [...(yield* retrieveMergeLinks).values()]
            .map((link) => [link.headerHash, link.merge?.slot])
            .sort(),
        ).toEqual(
          [
            [a.hash, 7],
            [f.hash, 9],
          ].sort(),
        );
        expect(yield* mempoolMarks).toEqual(both);

        // Below both merges: nothing final, nothing deleted.
        expect(yield* prune(6)).toEqual([]);
        expect(yield* mempoolMarks).toEqual(both);

        // `a`'s merge is final: its rows (and deltas) are deleted, `f`'s stay.
        expect(yield* prune(7)).toEqual([a.hash]);
        expect(yield* mempoolMarks).toEqual([marked(f.hash)(txF)]);

        // A rollback undoes `f`'s merge before its release: the unfold
        // leaves its row marked by `f`, which is a processed node again.
        yield* processLandedQueue(withIncludes, aMerged);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        expect((yield* retrieveMergeLinks).size).toBe(0);
        expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
          f.hash,
        ]);
        expect(yield* mempoolMarks).toEqual([marked(f.hash)(txF)]);

        // `f` leaves the landed chain: its mark clears, its row is pending.
        yield* processLandedQueue(withIncludes, aMergedAlone);
        expect(yield* mempoolMarks).toEqual([`${hex(txF)}@-`]);
        expect(yield* prune(1_000)).toEqual([]);
        expect(yield* mempoolMarks).toEqual([`${hex(txF)}@-`]);
      }),
    );
  }, 120_000);
});
