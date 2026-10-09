/**
 * The order the working-ledger rebuild replays pending transactions in
 * (`loadPendingTxs`, through the follower-change driver's landed-block
 * rebase), and what a rebuild keeps of a kept `mempool_ledger` row.
 *
 * - A transaction is never replayed before a pending transaction whose
 *   output it spends, whatever their time stamps and ids: a child whose id
 *   sorts before its parent's, admitted at the same time stamp, is kept with
 *   its parent, and is rejected as dependent (with its parent as the cause)
 *   when the parent is rejected.
 * - Beyond that, a time-stamp tie follows admission order
 *   (`tx_admissions.arrival_seq`), not tx id.
 * - A row the rebuild keeps keeps its `time_stamp_tz`: the commit worker
 *   selects the mempool up to its start time, so a kept row re-stamped later
 *   would move out of the next block's selection.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type * as Tx from "../src/database/utils/tx.js";
import { REBASE_REJECTIONS } from "../src/landed-blocks/rebase.js";
import { replayOrder } from "../src/services/working-ledger-recompute.pending-txs.js";
import { admitPending } from "./helpers/landed-blocks-sim.mempool.js";
import {
  attempt,
  E0,
  E1,
  expectRebased,
  freshNative,
  hex,
  pendingTx,
  processOf,
  rejectionCauses,
  rejections,
  run,
  seed,
  sqlRun,
} from "./landed-blocks-rebase.fixture.js";

/** `child` spends `parent`'s only output; both are admitted at `at`. */
const chain = (parentSpends: readonly Buffer[], at: number) => {
  const parent = pendingTx("parent", parentSpends, at);
  const child = pendingTx("child", [parent.produced[0]!.outref], at);
  // The ids order the child first: a tx-id tie-break replays it before the
  // transaction whose output it spends.
  expect(Buffer.compare(child.id, parent.id)).toBeLessThan(0);
  return { parent, child };
};

/** Records `txIds` as accepted admissions, in arrival order. */
const admitted =
  (globals: Awaited<ReturnType<typeof processOf>>) =>
  async (txIds: readonly Buffer[]) => {
    for (const [index, txId] of txIds.entries())
      await sqlRun(
        globals,
        (sql) =>
          sql`INSERT INTO tx_admissions ${sql.insert({
            tx_id: txId,
            arrival_seq: index + 1,
            status: "accepted",
            terminal_at: new Date(),
            submit_source: "native",
          })}`,
      );
  };

/** The rebase ran and cleared, whatever the pending transactions spent. */
const expectRebuilt = (shown: Awaited<ReturnType<typeof attempt>>) => {
  expect(shown.hold).toBeUndefined();
  expect(shown.due).toBe("none");
  expect(shown.applied).toEqual([true]);
};

describe("the rebuild's replay order", { concurrent: false }, () => {
  it("keeps a child admitted at its parent's time stamp whose id sorts first", async () => {
    // The foreign block spends E0 for E1; the parent spends E1.
    const { parent, child } = chain([E1.outref], 1);
    const globals = await processOf(freshNative());
    await seed(globals);
    await run(globals, admitPending([child, parent]));
    const shown = await attempt(globals);
    expectRebuilt(shown);
    expect(await rejections(globals)).toEqual([]);
    expect(shown.working.sort()).toEqual([hex(child.produced[0]!.outref)]);
  });

  it("rejects that child as dependent on its rejected parent", async () => {
    // The parent spends E0, which the foreign block spent.
    const { parent, child } = chain([E0.outref], 1);
    const globals = await processOf(freshNative());
    await seed(globals);
    await run(globals, admitPending([child, parent]));
    const shown = await attempt(globals);
    expectRebuilt(shown);
    expect(Object.fromEntries(await rejections(globals))).toEqual({
      [hex(parent.id)]: REBASE_REJECTIONS.direct.code,
      [hex(child.id)]: REBASE_REJECTIONS.dependent.code,
    });
    expect(await rejectionCauses(globals)).toEqual([
      [hex(child.id), hex(parent.id)],
    ]);
    expect(shown.working).toEqual([hex(E1.outref)]);
  });

  it("breaks a time-stamp tie between two spenders of one output by admission order", async () => {
    const early = pendingTx("early", [E1.outref], 1);
    const late = pendingTx("late", [E1.outref], 1);
    // The ids order the later admission first.
    expect(Buffer.compare(late.id, early.id)).toBeLessThan(0);
    const globals = await processOf(freshNative());
    await seed(globals);
    await run(globals, admitPending([late, early]));
    await admitted(globals)([early.id, late.id]);
    const shown = await attempt(globals);
    expectRebuilt(shown);
    expect(await rejections(globals)).toEqual([
      [hex(late.id), REBASE_REJECTIONS.direct.code],
    ]);
    expect(shown.working).toContain(hex(early.produced[0]!.outref));
  });
});

describe("replayOrder", () => {
  const pending = (label: string, spent: readonly Buffer[], at: number) => {
    const tx = pendingTx(label, spent, at);
    return {
      entry: {
        tx_id: tx.id,
        tx: Buffer.alloc(0),
        time_stamp_tz: tx.at,
      } as unknown as Tx.EntryWithTimeStamp,
      source: "mempool" as const,
      spent,
      produced: tx.produced.map((output) => ({
        tx_id: tx.id,
        outref: output.outref,
        output: output.output,
        address: "",
        source_event_id: null,
      })),
    };
  };
  const ids = (txs: readonly { entry: Tx.EntryWithTimeStamp }[]) =>
    txs.map(({ entry }) => hex(entry.tx_id));

  it("orders by time stamp, then admission order with none last, then tx id", () => {
    const a = pending("x", [], 1);
    const b = pending("y", [], 1);
    const c = pending("kept", [], 1);
    const d = pending("first", [], 0);
    const arrival = new Map([
      [hex(b.entry.tx_id), 1n],
      [hex(a.entry.tx_id), 2n],
    ]);
    expect(ids(replayOrder([a, b, c, d], arrival))).toEqual(ids([d, b, a, c]));
  });

  it("places every pending producer before its spender, across time stamps and admission order", () => {
    const parent = pending("parent", [], 2);
    const child = pending("child", [parent.produced[0]!.outref], 1);
    const grandchild = pending("dependent", [child.produced[0]!.outref], 0);
    const other = pending("second", [], 1);
    const arrival = new Map([
      [hex(grandchild.entry.tx_id), 1n],
      [hex(child.entry.tx_id), 2n],
      [hex(parent.entry.tx_id), 3n],
    ]);
    expect(
      ids(replayOrder([grandchild, other, child, parent], arrival)),
    ).toEqual(ids([parent, child, grandchild, other]));
  });
});

describe("a rebuild's kept rows", { concurrent: false }, () => {
  it("keeps the time stamp of a mempool_ledger row it keeps", async () => {
    const kept = pendingTx("kept", [], 1);
    const globals = await processOf(freshNative());
    await seed(globals);
    await run(globals, admitPending([kept]));
    const stamped = new Date("2026-01-01T00:00:00.000Z");
    const outref = kept.produced[0]!.outref;
    await sqlRun(
      globals,
      (sql) => sql`UPDATE mempool_ledger SET time_stamp_tz = ${stamped}
        WHERE outref = ${outref}`,
    );
    const shown = await attempt(globals);
    expectRebased(shown);
    expect(shown.working).toContain(hex(outref));
    const rows = await run(
      globals,
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql<{ time_stamp_tz: Date }>`
          SELECT time_stamp_tz FROM mempool_ledger WHERE outref = ${outref}`,
      ),
    );
    expect(rows.map((row) => row.time_stamp_tz.toISOString())).toEqual([
      stamped.toISOString(),
    ]);
  });
});
