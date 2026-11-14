import { afterEach, describe, expect, it } from "vitest";

import { COMMITTEE_PINNED_BLOCKS_TABLE } from "../src/l1/follower/queue-table.js";
import {
  COMMITTEE_RETENTION_PIN_FAILED,
  COMMITTEE_RETENTION_PIN_PRUNED,
  type CommitteePinTargets,
  NO_PIN_TARGETS,
  writeCommitteePins,
} from "../src/l1/follower/retention-pins.js";
import {
  emulatorFollower,
  emulatorWallet,
  noChainIndexCommitteeConfig,
} from "./helpers/emulator-follower.js";

/**
 * The committee's retention pins on its follower, written through its one
 * pin write: a pinned block outlives the follower's k-deep pruning while
 * its holder's source names it, a pin on history the follower already
 * pruned is named and never inserted, and a pin sync that fails holds the
 * prune back with a named reason.
 */

const closers: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});

const fixture = async () => {
  const wallet = await emulatorWallet();
  const config = await noChainIndexCommitteeConfig(wallet.account.seedPhrase);
  const follower = await emulatorFollower(config);
  closers.push(follower.close);
  const rows = async (sql: string, params: readonly (number | string)[]) =>
    follower.store.transaction("read", (tx) => tx.query(sql, params));
  return {
    follower,
    /** Whether the follower still stores a block at `slot`. */
    stored: async (slot: number) =>
      (await rows("SELECT 1 FROM l1_blocks WHERE slot = ?", [slot])).length > 0,
    /** The holders pinning the block at `slot`. */
    pinnedBy: async (slot: number) =>
      (
        await rows(
          `SELECT holder FROM ${COMMITTEE_PINNED_BLOCKS_TABLE} WHERE slot = ?`,
          [slot],
        )
      ).map((row) => String(row.holder)),
  };
};

const blocks = (...slots: number[]): CommitteePinTargets => ({
  ...NO_PIN_TARGETS,
  blocks: slots,
});

describe("committee retention pins on its follower", () => {
  it("keeps a pinned block past the pruned window, and lets it go once its source no longer names it", async () => {
    const f = await fixture();
    const pinned = await f.follower.forward();
    let targets = blocks(pinned.slot);
    f.follower.retention.bind("records", () => targets);
    await f.follower.empty(f.follower.securityParameter + 3);
    expect(await f.follower.prunedThroughSlot()).toBeGreaterThan(pinned.slot);
    expect(await f.stored(pinned.slot)).toBe(true);
    expect(await f.pinnedBy(pinned.slot)).toEqual(["records"]);
    expect(f.follower.retention.reasons()).toEqual([]);

    targets = NO_PIN_TARGETS;
    await f.follower.forward();
    expect(await f.pinnedBy(pinned.slot)).toEqual([]);
    expect(await f.stored(pinned.slot)).toBe(false);
    expect(f.follower.pruneErrors).toEqual([]);
  });

  it("names a pin on a block the follower already pruned, and never inserts it", async () => {
    const f = await fixture();
    const pruned = await f.follower.forward();
    const kept = await f.follower.empty(f.follower.securityParameter + 3);
    expect(await f.stored(pruned.slot)).toBe(false);

    await expect(
      writeCommitteePins(f.follower.store, {
        holder: "intents",
        mode: "add",
        targets: blocks(pruned.slot, kept.slot),
      }),
    ).resolves.toEqual({
      alreadyPruned: [`block at slot ${pruned.slot.toString()}`],
    });
    expect(await f.pinnedBy(pruned.slot)).toEqual([]);
    expect(await f.pinnedBy(kept.slot)).toEqual(["intents"]);

    await f.follower.retention.add("records", blocks(pruned.slot));
    expect(f.follower.retention.reasons()).toEqual([
      `${COMMITTEE_RETENTION_PIN_PRUNED}: records need 1 pruned item(s): block at slot ${pruned.slot.toString()}`,
    ]);
    expect(await f.pinnedBy(pruned.slot)).toEqual([]);
  });

  it("holds the prune back with a named reason while its pins cannot be written", async () => {
    const f = await fixture();
    const block = await f.follower.forward();
    const before = await f.follower.prunedThroughSlot();
    f.follower.retention.bind("records", () => {
      throw new Error("committee store unavailable");
    });
    await f.follower.empty(f.follower.securityParameter + 3);
    expect(await f.follower.prunedThroughSlot()).toBe(before);
    expect(await f.stored(block.slot)).toBe(true);
    expect(f.follower.pruneErrors).toContain("committee store unavailable");
    expect(f.follower.retention.reasons()).toEqual([
      `${COMMITTEE_RETENTION_PIN_FAILED}: committee store unavailable`,
    ]);

    // Readable again: the pins are written, the prune resumes, the reason
    // clears.
    f.follower.retention.bind("records", () => NO_PIN_TARGETS);
    await f.follower.forward();
    expect(await f.follower.prunedThroughSlot()).toBeGreaterThan(block.slot);
    expect(f.follower.retention.reasons()).toEqual([]);
  });
});
