/**
 * The working-ledger rebase run by the follower-change driver (its
 * recompute's `rebaseIfDue`, plan §7.3, N1, N3) on the node database, with
 * a modelled native MPF owner and no history owner:
 *
 * - every failure the rebase can meet past the transient ones (the native
 *   move, the event check, the rejection record, the ledger encoding) is
 *   caught: the driver run returns it as its hold, raised as
 *   `landed_block_rebase_failed` (a store that lost the chain's root as
 *   `mpf_closure_missing`), the rebase stays due, and the next run
 *   after the cause is gone rebases and clears it; a fresh process meets
 *   the same hold;
 * - a pending transaction a base block includes stays out of the rebuild,
 *   while one whose input the base spent is rejected `direct`;
 * - a pending transaction this node's own landed block includes is not
 *   re-applied on top of that block.
 */

import type { View } from "@al-ft/midgard-l1-follower";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { foldToRoot } from "../src/landed-blocks/fold.js";
import { LANDED_BLOCK_REBASE_FAILED } from "../src/landed-blocks/holds.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { REBASE_REJECTIONS } from "../src/landed-blocks/rebase.js";
import { rollBackRows } from "../src/landed-blocks/settlements.js";
import { insertRow, retrieveRows } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";
import { MPF_CLOSURE_MISSING } from "../src/services/liveness-halt.js";
import { failure as recomputeFailure } from "../src/services/working-ledger-recompute.pending-txs.js";
import * as RejectClosure from "../src/services/working-ledger-recompute.reject-closure.js";
import { testWrite } from "./helpers/driver-recompute.js";
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
  rejections,
  root,
  run,
  seed,
  sqlRun,
} from "./landed-blocks-rebase.fixture.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

beforeEach(() => {
  vi.restoreAllMocks();
});

/**
 * The production fold's ports with the view held; an own block's journal
 * reads as locally applied.
 */
const foldPorts = {
  confirmView: () => Effect.succeed(true),
  ownJournal: () => Effect.succeed({ status: "locally_applied" }),
  write: <A, E, R>(work: Effect.Effect<A, E, R>) => withFollowerWrite(work),
} as unknown as LandedBlockPorts<never>;
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
        MPF_CLOSURE_MISSING,
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

    it("catches a rejection record that fails", async () => {
      const globals = await processOf(freshNative());
      await seed(globals);
      const spy = vi
        .spyOn(RejectClosure, "recordRejections")
        .mockReturnValue(
          Effect.fail(
            recomputeFailure(
              "A rejected transaction's rejection could not be recorded",
              "1",
            ),
          ) as never,
        );
      expectHeld(await attempt(globals), /rejection could not be recorded/);
      spy.mockRestore();
      expectRebased(await attempt(globals));
    });

    it("starts a fresh process on a rebase that still fails: its driver shows the same hold", async () => {
      const native: Native = {
        ...freshNative(),
        durableRoot: root(0x99),
        retainsNothing: true,
      };
      await seed(await processOf(native));
      expectHeld(
        await attempt(await processOf(native)),
        /retains no root/,
        MPF_CLOSURE_MISSING,
      );
      // A restart: new process globals, the same database and native store.
      const restarted = await processOf(native);
      const shown = await attempt(restarted);
      expectHeld(shown, /retains no root/, MPF_CLOSURE_MISSING);
      native.retainsNothing = false;
      expectRebased(await attempt(restarted));
      expect(
        await Effect.runPromise(currentLivenessReasons(restarted)),
      ).not.toContain(MPF_CLOSURE_MISSING);
    });
  },
);

describe(
  "pending transactions a base block includes",
  { concurrent: false },
  () => {
    it("keeps a transaction a foreign block includes out of the rebuild and rejects the one whose input it spent", async () => {
      const b = pendingTx("b", [], 2);
      const globals = await processOf(freshNative());
      await seed(globals, { txIds: [b.id] });
      const a = pendingTx("a", [E0.outref], 1);
      await run(globals, admitPending([a, b]));
      const shown = await attempt(globals);
      expectRebased(shown);
      expect(shown.working).toEqual([hex(E1.outref)]);
      expect(await rejections(globals)).toEqual([
        [hex(a.id), REBASE_REJECTIONS.direct.code],
      ]);
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
        testWrite(
          insertRow({
            headerHash: OWN,
            parentHeaderHash: FRONTIER,
            parentUtxosRoot: R0,
            utxosRoot: R1,
            kind: "own",
            state: "processed",
            // An own block is applied when processed (its journal live).
            applied: true,
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
      expect(shown.hold).toBeUndefined();
      expect(shown.due).toBe("none");
      expect(shown.working).toEqual([hex(E2.outref)]);
      // T is settled by O: neither re-applied nor rejected.
      expect(await rejections(globals)).toEqual([]);
    });

    it("keeps a transaction a base block included out of the rebuild after that block folds and a later rebuild rejects another", async () => {
      // frontier -> X (E0 -> E1, includes b); a spends E1. The first rebuild
      // leaves b out; X folds; Y (E1 -> E2) on X then rejects a.
      const E2 = entry("e2", 4_000_000n);
      const R2 = root(0x12);
      const b = pendingTx("b", [], 2);
      const a = pendingTx("a", [E1.outref], 1);
      const native = freshNative();
      const globals = await processOf(native);
      await seed(globals, { txIds: [b.id] });
      await run(globals, admitPending([a, b]));
      const first = await attempt(globals);
      expect(first.failure).toBeUndefined();
      expect(first.applied).toEqual([true]);
      expect(await rejections(globals)).toEqual([]);

      // The production fold of X up to the merged root, with the view held.
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
      expect(shown.hold).toBeUndefined();
      expect(shown.reasons).not.toContain(LANDED_BLOCK_REBASE_FAILED);
      expect(shown.due).toBe("none");
      expect(shown.applied).toEqual([true]);
      expect(shown.working).toEqual([hex(E2.outref)]);
      expect(await rejections(globals)).toEqual([
        [hex(a.id), REBASE_REJECTIONS.direct.code],
      ]);
    });

    it("keeps a transaction this node's own block included out of the rebuild after that block folds and a later rebuild rejects another", async () => {
      // frontier -> own O (E0 -> E1, includes b) -> foreign X (E2 out of
      // nothing); a spends E1. The first rebuild leaves b out; O folds once
      // its journal is locally applied; Y (E1 -> E3) on X then rejects a,
      // and b stays out.
      const E2 = entry("e2", 4_000_000n);
      const E3 = entry("e3", 5_000_000n);
      const R2 = root(0x12);
      const R3 = root(0x13);
      const X = "c2".repeat(28);
      const b = pendingTx("b", [], 2);
      const a = pendingTx("a", [E1.outref], 1);
      const native: Native = { ...freshNative(), durableRoot: R1, reaches: R2 };
      const globals = await processOf(native);
      await seed(globals, { kind: "own", applied: true, txIds: [b.id] });
      await land(globals, {
        headerHash: X,
        parentHeaderHash: BLOCK,
        parentUtxosRoot: R1,
        utxosRoot: R2,
        spent: [],
        produced: [E2],
      });
      await run(globals, admitPending([a, b]));
      const first = await attempt(globals);
      expect(first.failure).toBeUndefined();
      expect(await rejections(globals)).toEqual([]);

      await sqlRun(globals, () =>
        foldToRoot(
          foldPorts,
          {} as View,
          { headerHash: BLOCK, utxosRoot: R1 },
          () => Effect.succeed(null),
        ),
      );
      expect(
        (await run(globals, retrieveRows)).map((row) => row.headerHash),
      ).toEqual([X]);
      await land(globals, {
        headerHash: "c3".repeat(28),
        parentHeaderHash: X,
        parentUtxosRoot: R2,
        utxosRoot: R3,
        spent: [E1.outref],
        produced: [E3],
      });
      native.reaches = R3;
      const shown = await attempt(globals);
      expect(shown.hold).toBeUndefined();
      expect(shown.due).toBe("none");
      expect(shown.applied).toEqual([true, true]);
      expect(shown.working.sort()).toEqual(
        [hex(E2.outref), hex(E3.outref)].sort(),
      );
      expect(await rejections(globals)).toEqual([
        [hex(a.id), REBASE_REJECTIONS.direct.code],
      ]);
    });

    it("returns a transaction a rolled-back base block included to the rebuild", async () => {
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
      await seed(globals, { kind: "own", applied: true, txIds: [b.id] });
      await land(globals, {
        headerHash: "c2".repeat(28),
        parentHeaderHash: BLOCK,
        parentUtxosRoot: R1,
        utxosRoot: R2,
        spent: [],
        produced: [E2],
      });
      await run(globals, admitPending([a, b]));
      const first = await attempt(globals);
      expect(first.failure).toBeUndefined();
      expect(await rejections(globals)).toEqual([]);

      // The production rollback of O and X.
      const left = await run(globals, retrieveRows);
      await sqlRun(globals, () => rollBackRows(left, []));

      await land(globals, {
        headerHash: "c3".repeat(28),
        utxosRoot: R3,
        produced: [E3],
      });
      native.reaches = R3;
      const shown = await attempt(globals);
      expect(shown.hold).toBeUndefined();
      expect(shown.due).toBe("none");
      expect(shown.applied).toEqual([true]);
      // b is pending again and rebuilt on Z; a's input is gone.
      expect(shown.working.sort()).toEqual(
        [hex(E3.outref), hex(b.produced[0]!.outref)].sort(),
      );
      expect(await rejections(globals)).toEqual([
        [hex(a.id), REBASE_REJECTIONS.direct.code],
      ]);
    });
  },
);
