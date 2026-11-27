/**
 * An own block's merge is folded by landed-block processing alone:
 * the merge confirming on L1 writes no local ledger, so a driver
 * run at the pre-merge view that follows it never moves the ledger back.
 *
 * Its bodies (its `blocks` rows over the `immutable` payloads) live until
 * its fold is final. The fold releases nothing: a
 * rollback of the merge unfolds it and the block has to merge again, and
 * that merge reads the bodies. They are released (`releaseFinalFolds`) in
 * the prune transaction that makes the fold final, and nothing of the block
 * is left behind then.
 */

import fs from "node:fs";
import path from "node:path";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk/core";
import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  BlocksDB,
  ConfirmedLedgerDB,
  DepositsDB,
  ImmutableDB,
  WithdrawalsDB,
} from "../src/database/index.js";
import type { LandedStateQueueElement } from "../src/l1-state-queue/index.js";
import {
  pruneMerges,
  retrieveMergeLinks,
} from "../src/landed-blocks/confirmed-merges.js";
import type { OwnJournal } from "../src/landed-blocks/ports.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { Frontier, markApplied } from "../src/landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { preflightDecodeBlockTxs } from "../src/transactions/state-queue/merge-to-confirmed-state.fetch-canonical-merge-candidate-readiness.js";
import { diagnoseMissingBlockTxs } from "../src/transactions/state-queue/merge-to-confirmed-state.merge-options.js";
import { fetchFirstBlockTxs } from "../src/transactions/utils.fetch-first-block-txs.js";
import { databaseFixtureBytes } from "./database.test/fixtures.bytes.js";
import {
  makeDepositEntry,
  makeHistoryWithdrawalEntry,
} from "./database.test/fixtures.events.js";
import { insertDeposits, insertWithdrawals } from "./helpers/event-rows.js";
import { insertOwnJournal } from "./helpers/landed-blocks-sim.own.js";
import {
  AFTER_A,
  type Block,
  confirmedAt,
  confirmedKeys,
  fixture,
  GENESIS,
  harness,
  hex,
  inInstalledNode,
  ports,
  queueOf,
  rootOf,
  sortedKeys,
} from "./landed-blocks-own.fixture.js";

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

/**
 * Where `a`'s merge stands in the confirmed ledger's frontier and keys and in
 * its deposit's and withdrawal's statuses: 0 before its fold, 1 after it, -1
 * for anything else.
 */
const mergeProgress = (a: Block, deposit: Buffer, withdrawal: Buffer) =>
  Effect.gen(function* () {
    const frontier = (yield* Frontier.retrieve)?.headerHash;
    const keys = (yield* confirmedKeys).join(",");
    const depositRow = yield* DepositsDB.retrieveByEventId(deposit);
    const withdrawalRow = yield* WithdrawalsDB.retrieveByEventId(withdrawal);
    const depositStatus =
      depositRow._tag === "Some"
        ? depositRow.value[DepositsDB.Columns.STATUS]
        : null;
    const withdrawalStatus =
      withdrawalRow._tag === "Some"
        ? withdrawalRow.value[WithdrawalsDB.Columns.STATUS]
        : null;
    const stage = (value: unknown, before: unknown, after: unknown) =>
      value === after ? 1 : value === before ? 0 : -1;
    return {
      frontier: stage(frontier, SDK.GENESIS_HEADER_HASH, a.hash),
      ledger: stage(
        keys,
        sortedKeys(GENESIS).join(","),
        sortedKeys(AFTER_A).join(","),
      ),
      deposit: stage(
        depositStatus,
        DepositsDB.Status.Projected,
        DepositsDB.Status.Consumed,
      ),
      withdrawal: stage(
        withdrawalStatus,
        WithdrawalsDB.Status.Projected,
        WithdrawalsDB.Status.Finalized,
      ),
    };
  });

type MergeProgress = Effect.Effect.Success<ReturnType<typeof mergeProgress>>;

/** Every step at which a measure of `steps` moved back, or was unknown. */
const regressions = (
  steps: readonly (readonly [string, MergeProgress])[],
): string[] =>
  steps.flatMap(([step, progress], index) =>
    Object.entries(progress).flatMap(([measure, value]) => {
      const before =
        index === 0 ? 0 : steps[index - 1]![1][measure as keyof MergeProgress];
      return value < 0 || value < before
        ? [`${step}: ${measure} ${before.toString()} -> ${value.toString()}`]
        : [];
    }),
  );

const pruneIn = (slot: number) =>
  Effect.flatMap(SqlClient.SqlClient, (sql) =>
    sql.withTransaction(pruneMerges(slot)),
  );

/** `a`, its journal and its committed bodies, over the own-block fixture. */
const setUp = async () => {
  const { a, genesisState, journalA } = await fixture();
  const tx = nativeTx();
  const journal: OwnJournal = { ...journalA, txIds: [tx.txId] };
  const state = harness();
  state.journals.set(a.hash, journal);
  const unmerged = queueOf(genesisState, [a]);
  const merged = queueOf(confirmedAt(a, ""), []);
  return {
    a,
    tx,
    state,
    unmerged,
    // The merge created the root at slot 7.
    mergedAt7: { ...merged, root: createdAt(merged.root!, 7) },
    // Its journal, and what applying the block when it landed stored for
    // it.
    commitBodies: Effect.gen(function* () {
      yield* insertOwnJournal({
        headerHash: a.hash,
        baseHeaderHash: journal.baseTailHeaderHash,
        baseUtxosRoot: journal.baseUtxosRoot,
        expectedUtxosRoot: journal.expectedUtxosRoot,
        spent: journal.spent,
        produced: journal.produced,
        txIds: journal.txIds,
        at: new Date("2026-10-01T00:00:00.000Z"),
      });
      yield* ImmutableDB.insertTxsValidatedNative([
        { tx_id: tx.txId, tx: tx.tx },
      ]);
      yield* BlocksDB.insert(Buffer.from(a.hash, "hex"), [tx.txId]);
    }),
  };
};

describe("an own block's merge folds forward only", () => {
  it("never moves the confirmed ledger or the block's events back when a run at the pre-merge view follows the merge", async () => {
    const s = await setUp();
    const deposit = makeDepositEntry({
      [DepositsDB.Columns.PROJECTED_HEADER_HASH]: Buffer.from(s.a.hash, "hex"),
      [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
    });
    const withdrawal = {
      ...makeHistoryWithdrawalEntry(),
      [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: databaseFixtureBytes(
        "block-bodies-withdrawal-settlement",
        64,
      ),
      [WithdrawalsDB.Columns.VALIDITY]:
        WithdrawalsDB.Validity.WithdrawalIsValid,
      [WithdrawalsDB.Columns.VALIDITY_DETAIL]: { checked: true },
      [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]: Buffer.from(
        s.a.hash,
        "hex",
      ),
      [WithdrawalsDB.Columns.STATUS]: WithdrawalsDB.Status.Projected,
    };
    await inInstalledNode(s.state)(
      Effect.gen(function* () {
        const progress = mergeProgress(
          s.a,
          deposit[DepositsDB.Columns.ID],
          withdrawal[WithdrawalsDB.Columns.ID],
        );
        const steps: [string, MergeProgress][] = [];
        yield* s.commitBodies;
        yield* insertDeposits([deposit]);
        yield* insertWithdrawals([withdrawal]);
        yield* processLandedQueue(ports(s.state), s.unmerged);
        // The rebase applied `a` (its own suites cover the rebase).
        yield* markApplied([s.a.hash]);
        steps.push(["applied", yield* progress]);

        // Guards against a reintroduced fiber-side fold: on current code the
        // merge fiber writes no local ledger, so this step is a no-op.
        steps.push(["merge confirmed", yield* progress]);

        // A driver run at the pre-merge view, whose queue root is still
        // genesis (a retried hold, or a run already in flight).
        yield* processLandedQueue(ports(s.state), s.unmerged);
        steps.push(["run at the pre-merge view", yield* progress]);

        // The follower's view shows the merge.
        yield* processLandedQueue(ports(s.state), s.mergedAt7);
        steps.push(["merge landed", yield* progress]);

        expect(regressions(steps)).toEqual([]);
        expect(steps[steps.length - 1]![1]).toEqual({
          frontier: 1,
          ledger: 1,
          deposit: 1,
          withdrawal: 1,
        });
      }),
    );
  }, 120_000);
});

describe("an own block's bodies live until its fold is final", () => {
  it("merges again after a rollback of its folded merge, reading the bodies, onto the replayed ledger", async () => {
    const s = await setUp();
    await inInstalledNode(s.state)(
      Effect.gen(function* () {
        yield* s.commitBodies;
        yield* processLandedQueue(ports(s.state), s.unmerged);
        // The rebase applied `a` (its own suites cover the rebase).
        yield* markApplied([s.a.hash]);

        // The merge lands; processing folds it at its merge point.
        yield* processLandedQueue(ports(s.state), s.mergedAt7);
        expect((yield* retrieveMergeLinks).get(s.a.hash)?.merge?.slot).toBe(7);

        // The merge rolls back (below k): the fold unfolds, `a` is queued.
        yield* processLandedQueue(ports(s.state), s.unmerged);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));
        // The rebase applies `a` again.
        yield* markApplied([s.a.hash]);

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

        // It lands and folds: the ledger is the fresh replay's.
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
    await inInstalledNode(s.state)(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* s.commitBodies;
        // Another block's rows, which no release touches.
        yield* BlocksDB.insert(Buffer.from(other, "hex"), [otherTx]);
        yield* processLandedQueue(ports(s.state), s.unmerged);
        // The rebase applied `a` (its own suites cover the rebase).
        yield* markApplied([s.a.hash]);
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
