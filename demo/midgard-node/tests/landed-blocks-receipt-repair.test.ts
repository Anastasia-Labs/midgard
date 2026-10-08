/**
 * The repair of acceptance receipts that record a rejected member (plan
 * §7.3, N3; migration 0016), on the node database through the production
 * working-ledger rebase (`prepareLandedBlockRebase`, with the modelled
 * native MPF owner of `landed-blocks-rebase.fixture.ts`).
 *
 * The state is the one the earlier commit-stage rejection wrote
 * (`src/mpf/commit-rejection.ts` at 368263e5d, lines 408-440): the
 * rejected transaction leaves both pending tables, a `tx_rejections` row is
 * inserted, its ledger effects are reverted and its CEK ownership is
 * released, while its acceptance receipt stays unreversed and its batch
 * co-members stay pending. Migration 0016 records that member on the
 * receipt.
 *
 * - a receipt whose members are all decided is repaired by the next
 *   rebase: its pending members are rejected as batch members and it is
 *   reversed; a rebase rejection of a co-member reverses it too;
 * - without the record, that rejection reaches an undecided member: the
 *   rebase holds `landed_block_batch_undecided`, as it does for a member
 *   whose rejection row was pruned before the migration;
 * - a receipt with an undecided member is left as it is, and so is a repair
 *   whose batch rejection reaches one; the rebase runs;
 * - a mutant rebuild that skips the repair fails the repaired expectations.
 */

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as CekProgramMaterialDB from "../src/database/cekProgramMaterial.js";
import {
  MempoolDB,
  ProcessedMempoolDB,
  TxRejectionsDB,
} from "../src/database/index.js";
import { migrationByVersion } from "../src/database/migrations/index.js";
import { LANDED_BLOCK_BATCH_UNDECIDED } from "../src/landed-blocks/holds.js";
import { REBASE_REJECTIONS } from "../src/landed-blocks/rebase.js";
import * as RejectClosure from "../src/services/working-ledger-recompute.reject-closure.js";
import {
  admitPending,
  type SimPendingTx,
} from "./helpers/landed-blocks-sim.mempool.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  attempt,
  E0,
  expectHeld,
  expectRebased,
  freshNative,
  hex,
  pendingTx,
  processOf,
  receipt,
  rejections,
  run,
  seed,
  sqlRun,
  unreversedReceipts,
} from "./landed-blocks-rebase.fixture.js";

type Globals = Awaited<ReturnType<typeof processOf>>;

const LEGACY_CODE = "E_COMMIT_WITHDRAWN_REFERENCE_INPUT";

/** The earlier commit-stage rejection of `tx`, write for write. */
const legacyCommitStageRejection = (globals: Globals, tx: SimPendingTx) =>
  sqlRun(globals, (sql) =>
    Effect.gen(function* () {
      yield* MempoolDB.clearTxs([tx.id]);
      yield* ProcessedMempoolDB.clearTxs([tx.id]);
      yield* TxRejectionsDB.insertMany([
        {
          [TxRejectionsDB.Columns.TX_ID]: tx.id,
          [TxRejectionsDB.Columns.REJECT_CODE]: LEGACY_CODE,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: "rejected at commit",
        },
      ]);
      for (const output of tx.produced)
        yield* sql`DELETE FROM mempool_ledger WHERE outref = ${output.outref}`;
      yield* CekProgramMaterialDB.releaseAdmissionOwnership([tx.id]);
    }),
  );

/** The retention sweep's prune of the rejection row of `tx`. */
const pruneRejection = (globals: Globals, tx: SimPendingTx) =>
  sqlRun(
    globals,
    (sql) => sql`DELETE FROM tx_rejections WHERE tx_id = ${tx.id}`,
  );

/** Migration 0016's record of rejected receipt members, run on this state. */
const migrate = (globals: Globals) => {
  const text = migrationByVersion.get(16)!.sql;
  return sqlRun(globals, (sql) =>
    sql.unsafe(text.slice(text.indexOf("INSERT INTO"))),
  );
};

const recordedMembers = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ tx_id: Buffer }>`SELECT tx_id
        FROM event_history_l2_ledger_receipt_rejections ORDER BY tx_id`,
    ),
  ).then((rows) => rows.map((row) => hex(row.tx_id)));

const pendingIds = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ tx_id: Buffer }>`SELECT tx_id FROM mempool
        ORDER BY tx_id`,
    ),
  ).then((rows) => rows.map((row) => hex(row.tx_id)));

const sorted = (pairs: (readonly string[])[]) =>
  [...pairs].sort((left, right) =>
    left[0]! < right[0]! ? -1 : left[0]! > right[0]! ? 1 : 0,
  );

/**
 * A legacy receipt `[a, b]`: `a` rejected by the earlier commit stage, `b`
 * pending and spending `bSpends`, recorded by the migration unless
 * `recorded` is false.
 */
const legacyBatch = async (
  bSpends: readonly Buffer[],
  options: { recorded?: boolean; pruned?: boolean } = {},
) => {
  const globals = await processOf(freshNative());
  await seed(globals);
  const a = pendingTx("a", [], 1);
  const b = pendingTx("b", bSpends, 2);
  await run(globals, admitPending([a, b]));
  await receipt(globals, [a.id, b.id]);
  await legacyCommitStageRejection(globals, a);
  if (options.pruned === true) await pruneRejection(globals, a);
  if (options.recorded !== false) await migrate(globals);
  return { globals, a, b };
};

const observe = async (globals: Globals) => ({
  shown: await attempt(globals),
  rejections: sorted(await rejections(globals)),
  unreversed: await unreversedReceipts(globals),
  recorded: await recordedMembers(globals),
  pending: await pendingIds(globals),
});

/** A repaired batch: `b` rejected as a batch member, the receipt reversed. */
const expectRepaired = (
  seen: Awaited<ReturnType<typeof observe>>,
  a: SimPendingTx,
  b: SimPendingTx,
) => {
  expectRebased(seen.shown);
  expect(seen.rejections).toEqual(
    sorted([
      [hex(a.id), LEGACY_CODE],
      [hex(b.id), REBASE_REJECTIONS.batch.code],
    ]),
  );
  expect(seen.unreversed).toBe(0);
  expect(seen.recorded).toEqual([]);
  expect(seen.pending).toEqual([]);
  expect(seen.shown.working).not.toContain(hex(b.produced[0]!.outref));
};

describe(
  "acceptance receipts that record a rejected member",
  { concurrent: false },
  () => {
    it("records the earlier rejected member on its unreversed receipt", async () => {
      const { globals, a } = await legacyBatch([]);
      expect(await recordedMembers(globals)).toEqual([hex(a.id)]);
      expect(await unreversedReceipts(globals)).toBe(1);
    });

    it("rejects the pending co-member as a batch member and reverses the receipt on the next rebase", async () => {
      const { globals, a, b } = await legacyBatch([]);
      expectRepaired(await observe(globals), a, b);
      // The repair is applied once: the next rebase finds nothing to do.
      expectRebased(await attempt(globals));
      expect(await rejections(globals)).toHaveLength(2);
    });

    it("mutant: a rebuild that skips the repair fails the repaired expectations", async () => {
      const { globals, a, b } = await legacyBatch([]);
      const close = RejectClosure.closeRejections;
      const spy = vi
        .spyOn(RejectClosure, "closeRejections")
        .mockImplementation((input) =>
          close({ ...input, repairRecordedRejections: false }),
        );
      const seen = await observe(globals);
      spy.mockRestore();
      expect(() => expectRepaired(seen, a, b)).toThrow();
      expect(seen.pending).toEqual([hex(b.id)]);
      expect(seen.unreversed).toBe(1);
    });

    it("reverses the receipt when the rebase rejects the co-member directly", async () => {
      const { globals, a, b } = await legacyBatch([E0.outref]);
      const seen = await observe(globals);
      expectRebased(seen.shown);
      expect(seen.rejections).toEqual(
        sorted([
          [hex(a.id), LEGACY_CODE],
          [hex(b.id), REBASE_REJECTIONS.direct.code],
        ]),
      );
      expect(seen.unreversed).toBe(0);
      expect(seen.recorded).toEqual([]);
    });

    it("holds landed_block_batch_undecided for that rejection when the member is not recorded", async () => {
      const { globals, b } = await legacyBatch([E0.outref], {
        recorded: false,
      });
      expectHeld(
        await attempt(globals),
        /batch's acceptance cannot be reversed/,
        LANDED_BLOCK_BATCH_UNDECIDED,
      );
      expect(await pendingIds(globals)).toEqual([hex(b.id)]);
    });

    it("holds landed_block_batch_undecided while a member's rejection row was pruned before the migration", async () => {
      const { globals, b } = await legacyBatch([E0.outref], { pruned: true });
      expect(await recordedMembers(globals)).toEqual([]);
      for (let retry = 0; retry < 2; retry += 1)
        expectHeld(
          await attempt(globals),
          /batch's acceptance cannot be reversed/,
          LANDED_BLOCK_BATCH_UNDECIDED,
        );
      expect(await unreversedReceipts(globals)).toBe(1);
      expect(await pendingIds(globals)).toEqual([hex(b.id)]);
    });

    it("leaves a recorded receipt with an undecided member as it is, and the rebase runs", async () => {
      const globals = await processOf(freshNative());
      await seed(globals);
      const a = pendingTx("a", [], 1);
      const b = pendingTx("b", [], 2);
      await run(globals, admitPending([a, b]));
      await receipt(globals, [a.id, b.id, simDigest("rebase:tx:gone")]);
      await legacyCommitStageRejection(globals, a);
      await migrate(globals);
      const seen = await observe(globals);
      expectRebased(seen.shown);
      expect(seen.rejections).toEqual([[hex(a.id), LEGACY_CODE]]);
      expect(seen.unreversed).toBe(1);
      expect(seen.recorded).toEqual([hex(a.id)]);
      expect(seen.pending).toEqual([hex(b.id)]);
    });

    it("does not apply a repair whose batch rejection reaches an undecided member, and the rebase runs", async () => {
      const globals = await processOf(freshNative());
      await seed(globals);
      const a = pendingTx("a", [], 1);
      const b = pendingTx("b", [], 2);
      await run(globals, admitPending([a, b]));
      await receipt(globals, [a.id, b.id]);
      await receipt(globals, [b.id, simDigest("rebase:tx:gone")]);
      await legacyCommitStageRejection(globals, a);
      await migrate(globals);
      const seen = await observe(globals);
      expectRebased(seen.shown);
      expect(seen.rejections).toEqual([[hex(a.id), LEGACY_CODE]]);
      expect(seen.unreversed).toBe(2);
      expect(seen.pending).toEqual([hex(b.id)]);
    });
  },
);
