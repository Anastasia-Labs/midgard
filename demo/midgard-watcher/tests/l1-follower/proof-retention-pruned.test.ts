/**
 * Proof retention when pruning wins (E1 ruling): the result is not dropped.
 * A pin or unit hold pruning beat is the named degradation
 * `l1_proof_history_pruned` until the objective is released. A header no
 * pin holds (a read for no open objective) writes no hold.
 */
import { afterEach, describe, expect, it } from "vitest";

import { L1_PROOF_HISTORY_PRUNED } from "../../src/l1-follower/proof-retention.js";
import { WATCHER_UNIT_HISTORY_TABLE } from "../../src/l1-follower/tables.js";
import {
  closeRemovedHeaders,
  FOLLOWED_UNIT,
  historyRows,
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
    await r.passK();
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(0);
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
