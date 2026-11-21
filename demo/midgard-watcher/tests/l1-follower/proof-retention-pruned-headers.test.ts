/**
 * The pruned-header record: a prune step that deletes closed queue history
 * rows of a header records the header, and a later read or pin of that
 * header is not fresh. Once the header is committed again with the same key
 * (new open rows), its unpinned raw read refuses `beyond_retention` and its
 * pin and hold report `already_pruned`. A header a pin kept is never
 * recorded and reads whole.
 */
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { L1_PROOF_HISTORY_PRUNED } from "../../src/l1-follower/proof-retention.js";
import {
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_PRUNED_HEADERS_TABLE,
} from "../../src/l1-follower/tables.js";
import { okValue, reasonOf } from "../support/l1-follower-raw-reads-fixture.js";
import { postgresSchemas } from "../support/postgres-schemas.js";
import {
  closeRemovedHeaders,
  historyRows,
  nodeUnit,
  type Removed,
  removedHeader,
} from "../support/proof-retention-removed-header.js";

afterEach(closeRemovedHeaders);
const schemas = postgresSchemas();
afterAll(schemas.dropAll);

const headerRecords = (r: Removed) =>
  r.count(WATCHER_PRUNED_HEADERS_TABLE, "header_hash", r.header);
const headerPins = (r: Removed) =>
  r.count(WATCHER_PROOF_PINS_TABLE, "header_hash", r.header);

describe.each(["sqlite", "postgres"] as const)(
  "the pruned-header record (%s)",
  (dialect) => {
    const removed = async (pin: boolean) => {
      const r = await removedHeader({
        pin,
        resolveAtIngest: true,
        ...(dialect === "postgres" ? { open: await schemas.open() } : {}),
      });
      expect(r.h.store.dialect.name).toBe(dialect);
      return r;
    };

    it("records a header once a prune step deletes its closed queue history rows", async () => {
      const r = await removed(false);
      expect(await headerRecords(r)).toBe(0);
      await r.passKOneStep();
      expect(await headerRecords(r)).toBe(1);
      await r.h.pruneAll();
      expect(await historyRows(r)).toBe(0);
      expect(await headerRecords(r)).toBe(1);
    });

    it("refuses beyond_retention an unpinned read of a header committed again after pruning deleted its queue history, and answers its pin and hold already_pruned", async () => {
      const r = await removed(false);
      await r.passK();
      expect(await historyRows(r)).toBe(0);
      await r.recommit();
      expect(await historyRows(r)).toBeGreaterThan(0);
      expect(
        reasonOf(
          await r.h
            .reads()
            .unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
        ),
      ).toBe("beyond_retention");
      expect(await r.retention.pin(r.target)).toEqual({
        kind: "already_pruned",
      });
      expect(await headerPins(r)).toBe(0);
      expect(
        await r.retention.holdUnits(r.header, [nodeUnit(r.header)]),
      ).toEqual({ kind: "already_pruned", units: [nodeUnit(r.header)] });
      expect(r.retention.degradations()).toMatchObject([
        { reason: L1_PROOF_HISTORY_PRUNED, count: 1 },
      ]);
      expect(await headerRecords(r)).toBe(1);
    });

    it("reads an empty queue history for a header pinned before it had any, past pruning, where an unpinned one refuses", async () => {
      const r = await removed(false);
      const pinned = "c4".repeat(28);
      const unpinned = "c5".repeat(28);
      expect(
        await r.retention.pin({ category: "doubleSpend", headerHash: pinned }),
      ).toEqual({ kind: "pinned" });
      await r.passK();
      expect(await headerRecords(r)).toBe(1);
      const read = (header: string) =>
        r.h.reads().unitHistoryAtPoint(nodeUnit(header), r.h.tipPoint());
      expect(okValue(await read(pinned)).transactions).toEqual([]);
      expect(reasonOf(await read(unpinned))).toBe("beyond_retention");
    });

    it("records nothing for a pinned header and reads it whole past pruning and its next commit", async () => {
      const r = await removed(true);
      await r.passK();
      const recommitted = await r.recommit();
      expect(await headerRecords(r)).toBe(0);
      expect(
        okValue(
          await r.h
            .reads()
            .unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
        ).transactions.map(({ txHash }) => txHash),
      ).toEqual([r.commitHash, r.removalHash, recommitted]);
      expect(
        await r.retention.holdUnits(r.header, [nodeUnit(r.header)]),
      ).toEqual({ kind: "held" });
    });
  },
);

// A prune step holds the cursor row for its whole transaction: its record
// hooks read the pins, then later statements delete what no pin holds. A
// release that commits between the two would let the step delete a
// header's history without recording it. On Postgres write transactions are
// plain READ COMMITTED, so only the cursor lock keeps a release out of a
// step in flight. The holder below stands in for that step; the test waits
// until the release has either finished or is blocked behind the holder.
describe("releasing a pin while a prune step is in flight (postgres)", () => {
  it("waits for the step to commit before deleting the pin", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      open: await schemas.open(),
    });
    let locked!: (pid: number) => void;
    const holderPid = new Promise<number>((resolve) => {
      locked = resolve;
    });
    let finishStep!: () => void;
    const stepDone = new Promise<void>((resolve) => {
      finishStep = resolve;
    });
    const step = r.h.store.transaction("write", async (tx) => {
      const [row] = await tx.query("SELECT pg_backend_pid() AS pid");
      await tx.query("SELECT 1 AS one FROM l1_follower_cursor FOR UPDATE");
      locked(Number(row!.pid));
      await stepDone;
    });
    // A failed assertion must still end the step, or closing the store
    // waits on its connection forever.
    let released = false;
    let release: Promise<void> = Promise.resolve();
    try {
      const pid = await holderPid;
      release = r.retention.release(r.target).then(() => {
        released = true;
      });
      const blockedBehindStep = async () =>
        (
          await r.h.store.transaction("read", (tx) =>
            tx.query(
              "SELECT 1 AS one FROM pg_stat_activity WHERE ? = ANY(pg_blocking_pids(pid))",
              [pid],
            ),
          )
        ).length > 0;
      while (!released && !(await blockedBehindStep()));
      expect(released, "the release committed inside the prune step").toBe(
        false,
      );
      expect(await headerPins(r)).toBe(1);
    } finally {
      finishStep();
      await step;
    }
    await release;
    expect(await headerPins(r)).toBe(0);
  });
});
