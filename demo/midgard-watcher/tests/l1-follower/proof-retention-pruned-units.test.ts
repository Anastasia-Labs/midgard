/**
 * The pruned-unit record: a prune step that deletes closed history rows of a
 * followed unit records the unit, and a later read or hold of that unit is
 * not fresh. Its unpinned raw read refuses `beyond_retention` and its hold
 * reports `already_pruned`, even once new rows of the unit open. A unit a
 * hold kept is never recorded. Also: which units a header's pin holds when
 * they are neither followed nor the header's own state-queue node unit.
 */
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { L1_PROOF_HISTORY_PRUNED } from "../../src/l1-follower/proof-retention.js";
import {
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_PRUNED_UNITS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "../../src/l1-follower/tables.js";
import { okValue, reasonOf } from "../support/l1-follower-raw-reads-fixture.js";
import { postgresSchemas } from "../support/postgres-schemas.js";
import {
  closeRemovedHeaders,
  FOLLOWED_UNIT,
  nodeUnit,
  type Removed,
  removedHeader,
} from "../support/proof-retention-removed-header.js";

afterEach(closeRemovedHeaders);
const schemas = postgresSchemas();
afterAll(schemas.dropAll);

const unitHolds = (r: Removed, unit: string) =>
  r.count(WATCHER_PROOF_PIN_UNITS_TABLE, "unit", unit);
const unitRecords = (r: Removed) =>
  r.count(WATCHER_PRUNED_UNITS_TABLE, "unit", FOLLOWED_UNIT);
const unitRows = (r: Removed) =>
  r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT);

describe.each(["sqlite", "postgres"] as const)(
  "the pruned-unit record (%s)",
  (dialect) => {
    const following = async (pin: boolean) => {
      const r = await removedHeader({
        pin,
        resolveAtIngest: true,
        followUnit: true,
        ...(dialect === "postgres" ? { open: await schemas.open() } : {}),
      });
      expect(r.h.store.dialect.name).toBe(dialect);
      return r;
    };

    it("records a followed unit once a prune step deletes its closed history rows", async () => {
      const r = await following(false);
      expect(await unitRecords(r)).toBe(0);
      await r.passKOneStep();
      expect(await unitRecords(r)).toBe(1);
      await r.h.pruneAll();
      expect(await unitRows(r)).toBe(0);
      expect(await unitRecords(r)).toBe(1);
    });

    it("refuses beyond_retention an unpinned read of a unit minted again after pruning deleted its earlier rows, and reports its hold", async () => {
      const r = await following(true);
      await r.passK();
      await r.remint();
      expect(await unitRows(r)).toBe(1);
      expect(
        reasonOf(
          await r.h.reads().unitHistoryAtPoint(FOLLOWED_UNIT, r.h.tipPoint()),
        ),
      ).toBe("beyond_retention");
      expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
        kind: "already_pruned",
        units: [FOLLOWED_UNIT],
      });
      expect(await unitHolds(r, FOLLOWED_UNIT)).toBe(0);
      expect(r.retention.degradations()).toMatchObject([
        { reason: L1_PROOF_HISTORY_PRUNED, count: 1 },
      ]);
    });

    it("records nothing for a held unit and reads it whole past pruning and its next mint", async () => {
      const r = await following(true);
      expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
        kind: "held",
      });
      await r.passK();
      const reminted = await r.remint();
      expect(await unitRecords(r)).toBe(0);
      expect(
        okValue(
          await r.h.reads().unitHistoryAtPoint(FOLLOWED_UNIT, r.h.tipPoint()),
        ).transactions.map(({ txHash }) => txHash),
      ).toEqual([...r.unitTxs, reminted]);
    });
  },
);

describe("proof retention: a hold over no rows after pruning", () => {
  it("keeps a unit's degradation through a hold that finds no rows after pruning and no record of the unit", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    await r.passKOneStep();
    expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
      kind: "already_pruned",
      units: [FOLLOWED_UNIT],
    });
    await r.h.pruneAll();
    // A store pruned before it kept the record: no rows and no record.
    await r.h.store.transaction("write", (tx) =>
      tx.query(`DELETE FROM ${WATCHER_PRUNED_UNITS_TABLE}`),
    );
    expect(await unitRows(r)).toBe(0);
    expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
      kind: "held",
    });
    expect(r.retention.degradations()).toMatchObject([
      { reason: L1_PROOF_HISTORY_PRUNED, count: 1 },
    ]);
  });
});

describe("proof retention: units neither followed nor the header's own node unit", () => {
  const OTHER_HEADER = "c3".repeat(28);
  const UNFOLLOWED_UNIT = `${"71".repeat(28)}aa`;

  it("answers not_pinned for another header's node unit and for an unfollowed unit, and held for the header's own node unit", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    for (const unit of [nodeUnit(OTHER_HEADER), UNFOLLOWED_UNIT])
      expect(await r.retention.holdUnits(r.header, [unit])).toEqual({
        kind: "not_pinned",
      });
    expect(await r.retention.holdUnits(r.header, [nodeUnit(r.header)])).toEqual(
      { kind: "held" },
    );
    expect(
      await r.retention.holdUnits(r.header, [
        nodeUnit(r.header),
        nodeUnit(OTHER_HEADER),
      ]),
    ).toEqual({ kind: "not_pinned" });
    for (const unit of [nodeUnit(OTHER_HEADER), nodeUnit(r.header)])
      expect(await unitHolds(r, unit)).toBe(0);
  });

  it("holds the followed units of a request that also names another header's node unit, and answers not_pinned", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    expect(
      await r.retention.holdUnits(r.header, [
        FOLLOWED_UNIT,
        nodeUnit(OTHER_HEADER),
      ]),
    ).toEqual({ kind: "not_pinned" });
    expect(await unitHolds(r, FOLLOWED_UNIT)).toBe(1);
    expect(await unitHolds(r, nodeUnit(OTHER_HEADER))).toBe(0);
  });
});
