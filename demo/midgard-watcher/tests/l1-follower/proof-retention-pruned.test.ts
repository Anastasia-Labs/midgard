/**
 * Proof retention when pruning wins (E1 ruling): the result is not dropped.
 * A pin or unit hold pruning beat is the named degradation
 * `l1_proof_history_pruned` until the objective is released, or until a
 * later hold of the same unit lands. A header no pin holds (a read for no
 * open objective) writes no hold. A unit with no history rows is held and
 * the raw read decides; the header's own state-queue node unit is held by
 * the header pin, not by a unit hold.
 */
import { afterEach, describe, expect, it } from "vitest";

import { L1_PROOF_HISTORY_PRUNED } from "../../src/l1-follower/proof-retention.js";
import {
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "../../src/l1-follower/tables.js";
import { okValue, reasonOf } from "../support/l1-follower-raw-reads-fixture.js";
import {
  closeRemovedHeaders,
  FOLLOWED_UNIT,
  historyRows,
  nodeUnit,
  type Removed,
  removedHeader,
} from "../support/proof-retention-removed-header.js";

afterEach(closeRemovedHeaders);

describe("proof retention: pins and holds pruning beat", () => {
  it("reports already_pruned for a pin that lands after pruning removed the history, and holds nothing", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: true });
    await r.passK();
    expect(await historyRows(r)).toBe(0);
    expect((await r.retention.pin(r.target)).kind).toBe("already_pruned");
    expect(await r.retention.pinned()).toEqual([]);
  });

  it("names an objective whose pin pruning beat as a degradation until it is released", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: true });
    await r.passK();
    expect(r.retention.degradations()).toEqual([]);
    await r.retention.pin(r.target);
    const degradations = r.retention.degradations();
    expect(degradations).toMatchObject([
      { reason: L1_PROOF_HISTORY_PRUNED, count: 1 },
    ]);
    expect(degradations[0]!.detail).toContain(r.header);
    await r.retention.release(r.target);
    expect(r.retention.degradations()).toEqual([]);
  });

  it("reports every unit of a header whose own pin pruning beat", async () => {
    const r = await removedHeader({
      pin: false,
      resolveAtIngest: true,
      followUnit: true,
    });
    await r.passK();
    await r.retention.pin(r.target);
    expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
      kind: "already_pruned",
      units: [FOLLOWED_UNIT],
    });
  });

  it("reports the units pruning removed before a hold landed, degraded until the header's last pin releases", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    await r.passKOneStep();
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(1);
    expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
      kind: "already_pruned",
      units: [FOLLOWED_UNIT],
    });
    expect(r.retention.degradations()).toMatchObject([
      { reason: L1_PROOF_HISTORY_PRUNED, count: 1 },
    ]);
    const second = { category: "otherCategory", headerHash: r.header };
    expect(await r.retention.pin(second)).toEqual({ kind: "pinned" });
    await r.retention.release(r.target);
    expect(r.retention.degradations()).toHaveLength(1);
    await r.retention.release(second);
    expect(r.retention.degradations()).toEqual([]);
  });
});

describe("proof retention: a header no pin holds", () => {
  it("writes no unit hold for an unpinned header", async () => {
    const r = await removedHeader({
      pin: false,
      resolveAtIngest: true,
      followUnit: true,
    });
    expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
      kind: "not_pinned",
    });
    await r.passK();
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(0);
  });
});

describe("proof retention: a history the store holds none of", () => {
  it("pins it before any pruning (empty, not gone), and refuses it once pruning has passed the origin", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: true });
    const unseen = (byte: string) => ({
      category: "doubleSpend",
      headerHash: byte.repeat(28),
    });
    expect(await r.retention.pin(unseen("c1"))).toEqual({ kind: "pinned" });
    await r.passK();
    expect(await r.retention.pin(unseen("c2"))).toEqual({
      kind: "already_pruned",
    });
  });
});

const unitHolds = (r: Removed, unit: string) =>
  r.count(WATCHER_PROOF_PIN_UNITS_TABLE, "unit", unit);

describe("proof retention: units held by the header pin, or with no history rows", () => {
  it("holds a pinned header's own state-queue node unit through the header pin after pruning, with no unit hold and no degradation", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK();
    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await r.retention.holdUnits(r.header, [nodeUnit(r.header)])).toEqual(
      { kind: "held" },
    );
    expect(await unitHolds(r, nodeUnit(r.header))).toBe(0);
    expect(r.retention.degradations()).toEqual([]);
    expect(
      okValue(
        await r.h
          .reads()
          .unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
      ).transactions.map(({ txHash }) => txHash),
    ).toEqual([r.commitHash, r.removalHash]);
  });

  it("reports a header no pin holds for its own state-queue node unit", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: true });
    expect(await r.retention.holdUnits(r.header, [nodeUnit(r.header)])).toEqual(
      { kind: "not_pinned" },
    );
  });

  it("writes the hold for a unit with no history rows after pruning, names no degradation, and leaves the refusal to the raw read", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    await r.passK();
    // FOLLOWED_UNIT's rows were pruned; `unminted` never had any.
    const unminted = `${FOLLOWED_UNIT.slice(0, 56)}bb`;
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(0);
    expect(
      await r.retention.holdUnits(r.header, [FOLLOWED_UNIT, unminted]),
    ).toEqual({ kind: "held" });
    expect(await unitHolds(r, FOLLOWED_UNIT)).toBe(1);
    expect(await unitHolds(r, unminted)).toBe(1);
    expect(r.retention.degradations()).toEqual([]);
    for (const unit of [FOLLOWED_UNIT, unminted])
      expect(
        reasonOf(await r.h.reads().unitHistoryAtPoint(unit, r.h.tipPoint())),
      ).toBe("beyond_retention");
  });

  it("clears a unit's degradation once a later hold of that unit lands", async () => {
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
    expect(r.retention.degradations()).toMatchObject([
      { reason: L1_PROOF_HISTORY_PRUNED, count: 1 },
    ]);
    await r.h.pruneAll();
    expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
      kind: "held",
    });
    expect(r.retention.degradations()).toEqual([]);
  });
});

describe("proof retention: a budget-cut prune step never yields a false pinned", () => {
  it("refuses a header pin while a prune step has deleted part of the header's history", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: true });
    expect(await historyRows(r)).toBe(2);
    await r.passKOneStep();
    expect(await historyRows(r)).toBe(1);
    expect(await r.retention.pin(r.target)).toEqual({
      kind: "already_pruned",
    });
    expect(await r.retention.pinned()).toEqual([]);
  });

  it("refuses a unit hold while a prune step has deleted part of the unit's history", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(2);
    await r.passKOneStep();
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(1);
    expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
      kind: "already_pruned",
      units: [FOLLOWED_UNIT],
    });
    expect(await unitHolds(r, FOLLOWED_UNIT)).toBe(0);
  });
});

describe("raw unit history reads inside a budget-cut prune step", () => {
  it("refuses beyond_retention an unpinned history a prune step has deleted part of", async () => {
    const r = await removedHeader({
      pin: false,
      resolveAtIngest: true,
      followUnit: true,
    });
    await r.passKOneStep();
    expect(await historyRows(r)).toBe(1);
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(1);
    const reads = r.h.reads();
    for (const unit of [nodeUnit(r.header), FOLLOWED_UNIT])
      expect(
        reasonOf(await reads.unitHistoryAtPoint(unit, r.h.tipPoint())),
      ).toBe("beyond_retention");
  });

  it("reads a held history whole past the prune step", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    expect(await r.retention.holdUnits(r.header, [FOLLOWED_UNIT])).toEqual({
      kind: "held",
    });
    await r.passKOneStep();
    const reads = r.h.reads();
    expect(
      okValue(
        await reads.unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
      ).transactions.map(({ txHash }) => txHash),
    ).toEqual([r.commitHash, r.removalHash]);
    expect(
      okValue(
        await reads.unitHistoryAtPoint(FOLLOWED_UNIT, r.h.tipPoint()),
      ).transactions.map(({ txHash }) => txHash),
    ).toEqual(r.unitTxs);
  });
});
