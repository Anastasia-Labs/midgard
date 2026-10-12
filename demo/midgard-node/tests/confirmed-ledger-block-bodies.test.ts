/**
 * An own block's bodies (its `blocks` rows over the `immutable` payloads)
 * live until its fold is final (plan §10.5, N5-R1 item 2). The merge fiber's
 * finalization folds the block but releases nothing: a rollback of the merge
 * unfolds it and the block has to merge again, and that merge reads the
 * bodies. They are released (`releaseFinalFolds`) in the prune transaction
 * that makes the fold final, and nothing of the block is left behind then.
 */
import "./utils.js";

import fs from "node:fs";
import path from "node:path";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  BlocksDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import type { LandedStateQueueElement } from "../src/l1-state-queue/index.js";
import {
  pruneMerges,
  retrieveMergeLinks,
} from "../src/landed-blocks/confirmed-merges.js";
import type { OwnJournal } from "../src/landed-blocks/ports.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import {
  diagnoseMissingBlockTxs,
  finalizeConfirmedMergeTransaction,
  preflightDecodeBlockTxs,
} from "../src/transactions/state-queue/merge-to-confirmed-state.js";
import { fetchFirstBlockTxs } from "../src/transactions/utils.js";
import {
  AFTER_A,
  type Block,
  confirmedAt,
  confirmedKeys,
  fixture,
  GENESIS,
  harness,
  hex,
  inNode,
  ports,
  queueOf,
  rootOf,
  sortedKeys,
} from "./landed-blocks-own.fixture.js";

const Journals = PendingBlockFinalizationsDB;

/** A Midgard-native transaction the merge's preflight decodes. */
const nativeTx = () => {
  const [fixtureTx] = JSON.parse(
    fs.readFileSync(path.resolve(__dirname, "./txs/txs_0.json"), "utf8"),
  ) as { readonly cborHex: string }[];
  const tx = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
    Buffer.from(fixtureTx!.cborHex, "hex"),
  );
  return {
    txId: computeMidgardNativeTxId(
      decodeMidgardNativeTxFullFromCanonicalCbor(tx),
    ),
    tx,
  };
};

/** The pending-finalization record of the own block `a`, as the merge fiber reads it. */
const recordOf = (a: Block, journal: OwnJournal) =>
  ({
    [Journals.Columns.HEADER_HASH]: Buffer.from(a.hash, "hex"),
    [Journals.Columns.STATUS]: Journals.Status.LocallyApplied,
    [Journals.Columns.BASE_TAIL_HEADER_HASH]: Buffer.from(
      journal.baseTailHeaderHash,
      "hex",
    ),
    [Journals.Columns.BASE_UTXOS_ROOT]: journal.baseUtxosRoot,
    [Journals.Columns.EXPECTED_UTXOS_ROOT]: journal.expectedUtxosRoot,
    depositEventIds: journal.depositIds,
    forcedTransactionEventIds: journal.forcedIds,
    withdrawalEventIds: [],
    withdrawalMembers: [],
    mempoolTxIds: journal.txIds,
    ledgerDelta: { spent: journal.spent, produced: journal.produced },
  }) as unknown as PendingBlockFinalizationsDB.Record;

/** A queue output as the history retains it, created at `slot`. */
const createdAt = (
  element: LandedStateQueueElement,
  slot: number,
): LandedStateQueueElement => ({ ...element, created: { slot, txIndex: 0 } });

/** The tx ids `header`'s `blocks` rows name. */
const bodies = (header: string) =>
  BlocksDB.retrieveTxHashesByHeaderHash(Buffer.from(header, "hex")).pipe(
    Effect.map((ids) => ids.map(hex)),
  );

const pruneIn = (slot: number) =>
  Effect.flatMap(SqlClient.SqlClient, (sql) =>
    sql.withTransaction(pruneMerges(slot)),
  );

/** `a`, its journal and its committed bodies, over the own-block fixture. */
const setUp = async () => {
  const { a, genesisState, journalA } = await fixture();
  const tx = nativeTx();
  const journal: OwnJournal = {
    ...journalA,
    status: "locally_applied",
    txIds: [tx.txId],
  };
  const state = harness();
  state.journals.set(a.hash, journal);
  const unmerged = queueOf(genesisState, [a]);
  const merged = queueOf(confirmedAt(a, ""), []);
  return {
    a,
    tx,
    state,
    record: recordOf(a, journal),
    unmerged,
    // The merge created the root at slot 7.
    mergedAt7: { ...merged, root: createdAt(merged.root!, 7) },
    // What commit-time local finalization stored for the block.
    commitBodies: Effect.gen(function* () {
      yield* ImmutableDB.insertTxsValidatedNative([
        { tx_id: tx.txId, tx: tx.tx },
      ]);
      yield* BlocksDB.insert(Buffer.from(a.hash, "hex"), [tx.txId]);
    }),
  };
};

describe("an own block's bodies live until its fold is final", () => {
  it("merges again after a rollback of its finalized merge, reading the bodies, onto the replayed ledger", async () => {
    const s = await setUp();
    await inNode(
      Effect.gen(function* () {
        yield* s.commitBodies;
        yield* processLandedQueue(ports(s.state), s.unmerged);

        // The merge lands; the merge fiber finalizes it, then processing
        // records its merge point.
        expect(
          yield* finalizeConfirmedMergeTransaction({
            journal: s.record,
          }),
        ).toBe("folded");
        yield* processLandedQueue(ports(s.state), s.mergedAt7);
        expect((yield* retrieveMergeLinks).get(s.a.hash)?.merge?.slot).toBe(7);

        // The merge rolls back (below k): the fold unfolds, `a` is queued.
        yield* processLandedQueue(ports(s.state), s.unmerged);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));

        // The merge again reads the oldest block's bodies and decodes them.
        const read = yield* fetchFirstBlockTxs(s.unmerged.nodes[0]!.element);
        expect(hex(read.headerHash)).toBe(s.a.hash);
        expect(read.txHashes.map(hex)).toEqual([hex(s.tx.txId)]);
        expect(
          diagnoseMissingBlockTxs(read.txHashes.length, read.txs.length),
        ).toBeUndefined();
        const decoded = yield* preflightDecodeBlockTxs(read.txs);
        expect(decoded.map((entry) => hex(entry.txId))).toEqual([
          hex(s.tx.txId),
        ]);

        // It lands and finalizes: the ledger is the fresh replay's.
        expect(
          yield* finalizeConfirmedMergeTransaction({
            journal: s.record,
          }),
        ).toBe("folded");
        yield* processLandedQueue(ports(s.state), s.mergedAt7);
        const entries = yield* ConfirmedLedgerDB.retrieve;
        expect(sortedKeys(entries)).toEqual(sortedKeys(AFTER_A));
        expect(yield* computeLedgerMpfRootFromLedgerEntries(entries)).toBe(
          yield* Effect.promise(() => rootOf(AFTER_A)),
        );
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: s.a.hash,
          utxosRoot: s.a.header.utxosRoot,
        });
      }),
    );
  }, 120_000);

  it("releases the bodies in the prune transaction that makes the fold final, leaving nothing of the block", async () => {
    const s = await setUp();
    const other = "0b".repeat(28);
    const otherTx = Buffer.alloc(32, 0x0b);
    await inNode(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* s.commitBodies;
        // Another block's rows, which no release touches.
        yield* BlocksDB.insert(Buffer.from(other, "hex"), [otherTx]);
        yield* processLandedQueue(ports(s.state), s.unmerged);
        yield* finalizeConfirmedMergeTransaction({
          journal: s.record,
        });
        yield* processLandedQueue(ports(s.state), s.mergedAt7);

        // Below the merge: not final, nothing released.
        expect(yield* pruneIn(6)).toEqual([]);
        expect(yield* bodies(s.a.hash)).toEqual([hex(s.tx.txId)]);

        // A prune transaction that fails after the release keeps it all.
        const aborted = yield* Effect.either(
          sql.withTransaction(
            pruneMerges(7).pipe(Effect.zipRight(Effect.fail("abort"))),
          ),
        );
        expect(Either.isLeft(aborted)).toBe(true);
        expect([...(yield* retrieveMergeLinks).keys()]).toEqual([s.a.hash]);
        expect(yield* bodies(s.a.hash)).toEqual([hex(s.tx.txId)]);

        // The fold is final: its bodies go with its stored delta.
        expect(yield* pruneIn(7)).toEqual([s.a.hash]);
        expect(yield* bodies(s.a.hash)).toEqual([]);
        expect((yield* retrieveMergeLinks).size).toBe(0);
        const spent = yield* sql<{ count: string }>`
          SELECT COUNT(*) AS count FROM node_confirmed_ledger_spent`;
        expect(Number(spent[0]!.count)).toBe(0);
        expect(yield* bodies(other)).toEqual([hex(otherTx)]);
        // Still folded: the release changes no ledger state.
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
        expect(yield* pruneIn(1_000)).toEqual([]);
      }),
    );
  }, 120_000);
});
