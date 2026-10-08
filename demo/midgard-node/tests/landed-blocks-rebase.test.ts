/**
 * The working-ledger rebase run from the history owner's preparation
 * (`prepareLandedBlockRebase`, plan §7.3, N3) on the node database, with a
 * modelled native MPF owner:
 *
 * - every failure the rebase can meet past the transient ones (the native
 *   move, the event check, the batch closure, the receipt reversal, the
 *   ledger encoding) is caught: the preparation returns, the failure is
 *   recorded and raised as `landed_block_rebase_failed`, the reconciliation
 *   stays pending, and the next attempt after the cause is gone runs the
 *   rebase and clears it; a fresh process meets the same hold;
 * - a batch co-member a base block includes is settled by it, while the
 *   member whose input the base spent is rejected `direct`;
 * - a pending transaction this node's own landed block includes is not
 *   re-applied on top of that block.
 */

import type { View } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect, Exit, Fiber } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { reportHistorySyncReasons } from "../src/commands/listen-startup.report-history-sync.js";
import { foldToRoot } from "../src/landed-blocks/fold.js";
import { LANDED_BLOCK_REBASE_FAILED } from "../src/landed-blocks/holds.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { REBASE_REJECTIONS } from "../src/landed-blocks/rebase.js";
import { rollBackRows } from "../src/landed-blocks/settlements.js";
import { insertRow, retrieveRows } from "../src/landed-blocks/store.js";
import { withHistoryWrite } from "../src/services/event-history-producer.js";
import { Globals } from "../src/services/globals.js";
import { failure as recomputeFailure } from "../src/services/working-ledger-recompute.pending-txs.js";
import * as RejectClosure from "../src/services/working-ledger-recompute.reject-closure.js";
import { admitPending } from "./helpers/landed-blocks-sim.mempool.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  attempt,
  BLOCK,
  E0,
  E1,
  entry,
  expectHeld,
  expectRebased,
  freshNative,
  FRONTIER,
  hex,
  land,
  type Native,
  pendingTx,
  processOf,
  R0,
  R1,
  receipt,
  rejections,
  root,
  run,
  seed,
  settlements,
  sqlRun,
  unreversedReceipts,
} from "./landed-blocks-rebase.fixture.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

beforeEach(() => {
  vi.restoreAllMocks();
});
afterEach(() => {
  vi.restoreAllMocks();
});

describe(
  "a failed landed-block rebase holds by name and retries",
  { concurrent: false },
  () => {
    it("catches a native MPF that retains no root of the processed chain", async () => {
      const native: Native = {
        ...freshNative(),
        durableRoot: root(0x99),
        retainsNothing: true,
      };
      const globals = await processOf(native);
      await seed(globals);
      expectHeld(
        await attempt(globals),
        /retains no root of the processed landed chain/,
      );
      native.retainsNothing = false;
      expectRebased(await attempt(globals));
    });

    it("catches a native delta that reaches another root", async () => {
      const native: Native = { ...freshNative(), reaches: root(0x77) };
      const globals = await processOf(native);
      await seed(globals);
      expectHeld(await attempt(globals), /delta reaches native root/);
      native.reaches = undefined;
      native.durableRoot = R0;
      expectRebased(await attempt(globals));
    });

    it("catches a processed block naming an event the node's tables lack", async () => {
      const globals = await processOf(freshNative());
      await seed(globals, { depositIds: [Buffer.alloc(36, 0x5d)] });
      expectHeld(await attempt(globals), /names an event deposits_utxos lacks/);
      await sqlRun(
        globals,
        (sql) => sql`UPDATE node_landed_blocks SET deposit_ids = '{}'::bytea[]`,
      );
      expectRebased(await attempt(globals));
    });

    it("catches a working-ledger output that does not encode", async () => {
      const globals = await processOf(freshNative());
      await seed(globals);
      const odd = makeOutRefCbor(simDigest("rebase:odd"), 0);
      await sqlRun(
        globals,
        (
          sql,
        ) => sql`INSERT INTO confirmed_ledger (tx_id, outref, output, address)
      VALUES (${simDigest("rebase:odd")}, ${odd}, ${Buffer.from("ff", "hex")}, 'addr_test_odd')`,
      );
      expectHeld(
        await attempt(globals),
        /recomputed ledger output is not canonical/,
      );
      await sqlRun(
        globals,
        (sql) => sql`DELETE FROM confirmed_ledger WHERE outref = ${odd}`,
      );
      expectRebased(await attempt(globals));
    });

    it("catches a batch whose acceptance cannot be reversed", async () => {
      const globals = await processOf(freshNative());
      await seed(globals);
      const a = pendingTx("a", [E0.outref], 1);
      await run(globals, admitPending([a]));
      // Its co-member is neither pending nor in a base block.
      await receipt(globals, [a.id, simDigest("rebase:tx:gone")]);
      expectHeld(
        await attempt(globals),
        /batch's acceptance cannot be reversed/,
      );
      await sqlRun(
        globals,
        (sql) => sql`DELETE FROM event_history_l2_ledger_receipts`,
      );
      expectRebased(await attempt(globals));
      expect(await rejections(globals)).toEqual([
        [hex(a.id), REBASE_REJECTIONS.direct.code],
      ]);
    });

    it("catches a receipt the rejection could not reverse", async () => {
      const globals = await processOf(freshNative());
      await seed(globals);
      const spy = vi
        .spyOn(RejectClosure, "recordRejections")
        .mockReturnValue(
          Effect.fail(
            recomputeFailure(
              "A rejected transaction's acceptance receipt could not be reversed",
              "1",
            ),
          ) as never,
        );
      expectHeld(await attempt(globals), /receipt could not be reversed/);
      spy.mockRestore();
      expectRebased(await attempt(globals));
    });

    it("starts a fresh process on a rebase that still fails: the hold is shown during startup", async () => {
      const native: Native = {
        ...freshNative(),
        durableRoot: root(0x99),
        retainsNothing: true,
      };
      await seed(await processOf(native));
      expectHeld(await attempt(await processOf(native)), /retains no root/);
      // A restart: new process globals, the same database and native store.
      const restarted = await processOf(native);
      const shown = await attempt(restarted);
      expectHeld(shown, /retains no root/);
      const reported: (readonly string[])[] = [];
      const reporter = Effect.runFork(
        reportHistorySyncReasons(
          (reasons) => Effect.sync(() => void reported.push(reasons)),
          1,
        ).pipe(Effect.provideService(Globals, restarted)),
      );
      await new Promise((resolve) => setTimeout(resolve, 20));
      native.retainsNothing = false;
      expectRebased(await attempt(restarted));
      await new Promise((resolve) => setTimeout(resolve, 20));
      await Effect.runPromise(Fiber.interrupt(reporter));
      expect(reported[0]).toContain(LANDED_BLOCK_REBASE_FAILED);
      expect(reported.at(-1)).not.toContain(LANDED_BLOCK_REBASE_FAILED);
    });
  },
);

describe(
  "pending transactions a base block includes",
  { concurrent: false },
  () => {
    it("settles a batch co-member a foreign block includes and rejects the member whose input it spent", async () => {
      const b = pendingTx("b", [], 2);
      const globals = await processOf(freshNative());
      await seed(globals, { txIds: [b.id] });
      const a = pendingTx("a", [E0.outref], 1);
      await run(globals, admitPending([a, b]));
      await receipt(globals, [a.id, b.id]);
      expectRebased(await attempt(globals));
      expect(await rejections(globals)).toEqual([
        [hex(a.id), REBASE_REJECTIONS.direct.code],
      ]);
      const unreversed = await run(
        globals,
        Effect.flatMap(
          SqlClient.SqlClient,
          (sql) => sql`SELECT 1 FROM event_history_l2_ledger_receipts
          WHERE reversed_at_revision IS NULL`,
        ),
      );
      expect(unreversed).toHaveLength(0);
    });

    it("does not re-apply a pending transaction this node's own landed block includes", async () => {
      // frontier -> own block O (E0 -> E1, includes T) -> foreign block X (E1 -> E2).
      const E2 = entry("e2", 4_000_000n);
      const R2 = root(0x12);
      const OWN = "0a".repeat(28);
      const t = pendingTx("own", [E0.outref], 1);
      const native: Native = { ...freshNative(), durableRoot: R1, reaches: R2 };
      const globals = await processOf(native);
      await seed(globals, {
        parentHeaderHash: OWN,
        parentUtxosRoot: R1,
        utxosRoot: R2,
        spent: [E1.outref],
        produced: [E2],
      });
      await run(
        globals,
        withHistoryWrite(
          insertRow({
            headerHash: OWN,
            parentHeaderHash: FRONTIER,
            parentUtxosRoot: R0,
            utxosRoot: R1,
            kind: "own",
            state: "processed",
            applied: false,
            spent: [E0.outref],
            produced: [E1],
            depositIds: [],
            withdrawals: [],
            forcedIds: [],
            txIds: [t.id],
          }),
        ),
      );
      await run(globals, admitPending([t]));
      const shown = await attempt(globals);
      expect(Exit.isSuccess(shown.exit)).toBe(true);
      expect(shown.failure).toBeUndefined();
      expect(shown.disposition).toBeUndefined();
      expect(shown.working).toEqual([hex(E2.outref)]);
      // T is settled by O: neither re-applied nor rejected.
      expect(await rejections(globals)).toEqual([]);
    });

    it("keeps a batch co-member settled by a base block folded before a later rebuild rejects another member", async () => {
      // frontier -> X (E0 -> E1, includes b); a spends E1. The first rebuild
      // settles b by X; X folds; Y (E1 -> E2) on X then rejects a.
      const E2 = entry("e2", 4_000_000n);
      const R2 = root(0x12);
      const b = pendingTx("b", [], 2);
      const a = pendingTx("a", [E1.outref], 1);
      const native = freshNative();
      const globals = await processOf(native);
      await seed(globals, { txIds: [b.id] });
      await run(globals, admitPending([a, b]));
      await receipt(globals, [a.id, b.id]);
      const first = await attempt(globals);
      expect(first.failure).toBeUndefined();
      expect(first.applied).toEqual([true]);
      expect(await rejections(globals)).toEqual([]);
      expect(await settlements(globals)).toEqual([[hex(b.id), BLOCK]]);

      // The production fold of X up to the merged root, with the view held.
      const foldPorts = {
        confirmView: () => Effect.succeed(true),
        write: <A, E, R>(work: Effect.Effect<A, E, R>) =>
          withHistoryWrite(work),
      } as unknown as LandedBlockPorts<never>;
      await sqlRun(globals, () =>
        foldToRoot(
          foldPorts,
          {} as View,
          { headerHash: BLOCK, utxosRoot: R1 },
          () => Effect.succeed(null),
        ),
      );
      expect(await run(globals, retrieveRows)).toEqual([]);
      await land(globals, {
        headerHash: "c2".repeat(28),
        parentHeaderHash: BLOCK,
        parentUtxosRoot: R1,
        utxosRoot: R2,
        spent: [E1.outref],
        produced: [E2],
      });
      native.reaches = R2;
      const shown = await attempt(globals);
      expect(Exit.isSuccess(shown.exit)).toBe(true);
      expect(shown.failure).toBeUndefined();
      expect(shown.reasons).not.toContain(LANDED_BLOCK_REBASE_FAILED);
      expect(shown.disposition).toBeUndefined();
      expect(shown.applied).toEqual([true]);
      expect(shown.working).toEqual([hex(E2.outref)]);
      expect(await rejections(globals)).toEqual([
        [hex(a.id), REBASE_REJECTIONS.direct.code],
      ]);
      expect(await unreversedReceipts(globals)).toBe(0);
    });

    it("rewinds the settlement of a rolled-back base block, so the co-member is pending again", async () => {
      // frontier -> own block O (E0 -> E1, includes b) -> foreign X (E1 ->
      // E2); a spends E1. O and X are rolled back; foreign Z (E0 -> E3)
      // lands on the frontier instead.
      const E2 = entry("e2", 4_000_000n);
      const E3 = entry("e3", 5_000_000n);
      const R2 = root(0x12);
      const R3 = root(0x13);
      const b = pendingTx("b", [], 2);
      const a = pendingTx("a", [E1.outref], 1);
      const native: Native = {
        ...freshNative(),
        durableRoot: R1,
        reaches: R2,
      };
      const globals = await processOf(native);
      await seed(globals, { kind: "own", txIds: [b.id] });
      await land(globals, {
        headerHash: "c2".repeat(28),
        parentHeaderHash: BLOCK,
        parentUtxosRoot: R1,
        utxosRoot: R2,
        spent: [],
        produced: [E2],
      });
      await run(globals, admitPending([a, b]));
      await receipt(globals, [a.id, b.id]);
      const first = await attempt(globals);
      expect(first.failure).toBeUndefined();
      expect(await rejections(globals)).toEqual([]);
      expect(await settlements(globals)).toEqual([[hex(b.id), BLOCK]]);

      // The production rollback of O and X: their settlements rewind.
      const left = await run(globals, retrieveRows);
      await sqlRun(globals, () => rollBackRows(left, []));
      expect(await settlements(globals)).toEqual([]);

      await land(globals, {
        headerHash: "c3".repeat(28),
        utxosRoot: R3,
        produced: [E3],
      });
      native.reaches = R3;
      const shown = await attempt(globals);
      expect(Exit.isSuccess(shown.exit)).toBe(true);
      expect(shown.failure).toBeUndefined();
      expect(shown.disposition).toBeUndefined();
      expect(shown.applied).toEqual([true]);
      expect(shown.working).toEqual([hex(E3.outref)]);
      // b is pending again: the batch rejection takes it with a.
      expect(await rejections(globals)).toEqual(
        [
          [hex(a.id), REBASE_REJECTIONS.direct.code],
          [hex(b.id), REBASE_REJECTIONS.batch.code],
        ].sort(([x], [y]) => (x! < y! ? -1 : 1)),
      );
      expect(await unreversedReceipts(globals)).toBe(0);
    });

    it("records the settlement again when a rolled-back base block relands before a rebase reverts it", async () => {
      const b = pendingTx("b", [], 2);
      const globals = await processOf(freshNative());
      await seed(globals, { txIds: [b.id] });
      await run(globals, admitPending([b]));
      await receipt(globals, [b.id, pendingTx("a", [E1.outref], 1).id]);
      expectRebased(await attempt(globals));
      expect(await settlements(globals)).toEqual([[hex(b.id), BLOCK]]);
      const [applied] = await run(globals, retrieveRows);
      await sqlRun(globals, () => rollBackRows([applied!], []));
      expect(await settlements(globals)).toEqual([]);
      // The production reland: the removed row is processed again.
      const [removed] = await run(globals, retrieveRows);
      expect(removed?.state).toBe("removed");
      await sqlRun(globals, () => rollBackRows([], [removed!]));
      expect(
        (await run(globals, retrieveRows)).map((row) => row.state),
      ).toEqual(["processed"]);
      expect(await settlements(globals)).toEqual([[hex(b.id), BLOCK]]);
    });
  },
);
