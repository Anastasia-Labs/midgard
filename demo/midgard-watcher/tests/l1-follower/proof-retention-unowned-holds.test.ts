/**
 * The proof roster's prune floor and the release of holds no objective owns,
 * on the fork simulator with pruning on: pruning holds at the store's origin
 * until the first pass at tip in the process, and a hold nothing owns goes
 * only in a pass at tip, once its header has left the queue and every unit
 * held under it has settled, each beyond completion depth (k + 1).
 */
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import { afterEach, describe, expect, it } from "vitest";

import { pruneFloorDegradations } from "../../src/l1-follower/follower-runtime.js";
import {
  L1_PROOF_HOLDS_UNOWNED,
  L1_PROOF_HOLDS_UNOWNED_LIVE,
} from "../../src/l1-follower/proof-retention.js";
import {
  createWatcherProofRosterRetention,
  WATCHER_PROOF_ROSTER_FLOOR,
} from "../../src/l1-follower/proof-roster-retention.js";
import { WATCHER_UNIT_HISTORY_TABLE } from "../../src/l1-follower/tables.js";
import { D, K, X } from "../support/l1-follower-raw-reads-fixture.js";
import {
  closeRemovedHeaders,
  departedRows,
  FOLLOWED_UNIT,
  historyRows,
  type Removed,
  removedHeader,
} from "../support/proof-retention-removed-header.js";
import {
  advance,
  floorLagsOf,
  followStatus,
  passAtTip,
  reportAtTip,
} from "../support/proof-roster.js";

afterEach(closeRemovedHeaders);

const FOLLOWED = FOLLOWED_UNIT.slice(0, 56);
const scriptAddress = (hash: string): Buffer =>
  Buffer.concat([Buffer.of(0x70), Buffer.from(hash, "hex")]);
const one = (name: string, quantity = 1n) =>
  new Map([[FOLLOWED, new Map([[name, quantity]])]]);

/** Mints `FOLLOWED + name` into an output at `script`; returns the tx hash. */
const mintAt = async (
  r: Removed,
  name: string,
  script = FOLLOWED,
): Promise<string> =>
  (
    await r.h.forward([
      {
        inputs: [r.h.chain.outsideInput()],
        outputs: [
          {
            address: scriptAddress(script),
            lovelace: 2_000_000n,
            assets: one(name),
          },
        ],
        mint: one(name),
        nonce: r.h.chain.nonce(),
      },
    ])
  ).hashes[0]!;

/** Spends the output `txHash#0` carrying `FOLLOWED + name`, burning it. */
const burn = async (r: Removed, txHash: string, name: string) => {
  const tx: SimTx = {
    inputs: [{ txHash: Buffer.from(txHash, "hex"), index: 0 }],
    outputs: [{ address: X, lovelace: 2_000_000n }],
    mint: one(name, -1n),
    nonce: r.h.chain.nonce(),
  };
  return (await r.h.forward([tx])).hashes[0]!;
};

const unitRows = (r: Removed, unit: string) =>
  r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", unit);

const nobody = { owned: () => false, queued: new Set<string>() };

describe("proof roster floor: pruning holds at the origin until the first pass at tip", () => {
  it("in a fresh process before the first pass: prunes nothing above the origin and reports the floor", async () => {
    const roster = createWatcherProofRosterRetention();
    const r = await removedHeader({
      pin: false,
      resolveAtIngest: true,
      roster,
    });
    await r.passK(2 * K + 4);
    // Unpinned and far deeper than the retained depth, yet kept.
    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await departedRows(r)).toBe(1);
    const lags = await floorLagsOf(r);
    expect(lags.map(({ floor }) => floor)).toEqual([
      WATCHER_PROOF_ROSTER_FLOOR,
    ]);
    expect(lags[0]!.lagSlots).toBeGreaterThan(0);
    const status = followStatus(r.h.chain.tip.height, true);
    expect(
      pruneFloorDegradations({
        ...status,
        prune: { ...status.prune, floorLags: lags },
      }),
    ).toEqual([
      {
        reason: `l1_prune_floor_${WATCHER_PROOF_ROSTER_FLOOR}`,
        count: lags[0]!.lagSlots,
        detail: expect.stringContaining(WATCHER_PROOF_ROSTER_FLOOR) as unknown,
      },
    ]);

    expect(await passAtTip(r)).toBe(true);
    expect(roster.lifted()).toBe(true);
    expect(await floorLagsOf(r)).toEqual([]);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBe(0);
    expect(await departedRows(r)).toBe(0);
  });

  it("is lifted only by a pass at tip: not before the follower reports atTip, nor over a block below its first tip, nor off the chain", async () => {
    const r = await removedHeader({
      pin: false,
      resolveAtIngest: true,
      roster: createWatcherProofRosterRetention(),
    });
    const early = r.h.tipPoint();
    // Replaying: the follower has not reported atTip.
    r.roster.observe(followStatus(r.h.chain.tip.height, false));
    expect(await r.retention.passAtTip(early)).toBe(false);
    await r.passK(2 * K + 4);
    expect(await historyRows(r)).toBeGreaterThan(0);
    // At tip now, but the observation predates the first tip height.
    reportAtTip(r);
    expect(await r.retention.passAtTip(early)).toBe(false);
    // A block the chain does not hold.
    expect(
      await r.retention.passAtTip({
        slot: r.h.tipPoint().slot,
        blockHash: "ee".repeat(32),
      }),
    ).toBe(false);
    // The follower fell behind again.
    r.roster.observe(followStatus(r.h.chain.tip.height, false));
    expect(await r.retention.passAtTip(r.h.tipPoint())).toBe(false);
    expect(r.roster.lifted()).toBe(false);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBeGreaterThan(0);

    expect(await passAtTip(r)).toBe(true);
    // Never drops again: a later pass short of the tip keeps it lifted.
    r.roster.observe(followStatus(r.h.chain.tip.height, false));
    expect(await r.retention.passAtTip(r.h.tipPoint())).toBe(false);
    expect(r.roster.lifted()).toBe(true);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBe(0);
  });

  it("over a lost follower store: the replay holds every fact until the first pass at tip pins the roster", async () => {
    const r = await removedHeader({
      pin: false,
      resolveAtIngest: true,
      roster: createWatcherProofRosterRetention(),
    });
    // The replay from the origin reaches far past the header's removal.
    r.roster.observe(followStatus(r.h.chain.tip.height, false));
    await r.passK(2 * K + 4);
    expect(await historyRows(r)).toBeGreaterThan(0);
    // The first pass re-pins the open objective before any prune.
    reportAtTip(r);
    expect(await r.retention.pin(r.target)).toEqual({ kind: "pinned" });
    expect(await r.retention.passAtTip(r.h.tipPoint())).toBe(true);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await departedRows(r)).toBe(1);
  });
});

describe("holds no objective owns: released only in a pass at tip, once nothing can need them", () => {
  it("keeps nothing it is not asked to release, and releases nothing before the floor is lifted", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      roster: createWatcherProofRosterRetention(),
    });
    await advance(r, K + 4);
    expect(await r.retention.releaseUnowned(nobody)).toEqual([]);
    expect(await r.retention.pinned()).toEqual([r.target]);
    expect(await passAtTip(r)).toBe(true);
    expect(
      await r.retention.releaseUnowned({ ...nobody, owned: () => true }),
    ).toEqual([]);
    expect(await r.retention.pinned()).toEqual([r.target]);
    expect(r.retention.degradations()).toEqual([]);
  });

  it("keeps a hold whose header is still queued, and releases it once the header has left the queue beyond completion depth", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      roster: createWatcherProofRosterRetention(),
    });
    expect(await passAtTip(r)).toBe(true);
    // The observation's finalized queue names the header.
    const queued = { ...nobody, queued: new Set([r.header]) };
    await advance(r, K + 4);
    expect(await r.retention.releaseUnowned(queued)).toEqual([]);
    expect(r.retention.degradations()).toContainEqual(
      expect.objectContaining({ reason: L1_PROOF_HOLDS_UNOWNED, count: 1 }),
    );
    await r.h.pruneAll();
    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await r.retention.releaseUnowned(nobody)).toEqual([r.target]);
    expect(r.retention.degradations()).toEqual([]);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBe(0);
  });

  it("keeps a departed header's hold until its departure is beyond completion depth (k + 1)", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      roster: createWatcherProofRosterRetention(),
    });
    expect(await passAtTip(r)).toBe(true);
    // The removal is the tip: K more blocks put it K + 1 deep.
    await advance(r, K);
    expect(await r.retention.releaseUnowned(nobody)).toEqual([]);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBeGreaterThan(0);
    await advance(r, 1);
    expect(await r.retention.releaseUnowned(nobody)).toEqual([r.target]);
  });

  it("keeps the hold of a header a rollback returns to the queue", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      roster: createWatcherProofRosterRetention(),
    });
    expect(await passAtTip(r)).toBe(true);
    // The removal rolls back: the header's node is live again.
    await r.h.backward(1);
    await advance(r, K + 4);
    expect(await passAtTip(r)).toBe(true);
    expect(await r.retention.releaseUnowned(nobody)).toEqual([]);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await r.retention.pinned()).toEqual([r.target]);
  });

  it("keeps a departed header's hold while a held thread unit is live, through passes and a forced prune; releases it once the unit is spent beyond completion depth", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
      roster: createWatcherProofRosterRetention(),
    });
    const thread = `${FOLLOWED}cc`;
    const minted = await mintAt(r, "cc");
    expect(await r.retention.holdUnits(r.header, [thread])).toEqual({
      kind: "held",
    });
    expect(await passAtTip(r)).toBe(true);
    for (let pass = 0; pass < 3; pass += 1) {
      await advance(r, K + 4);
      expect(await r.retention.releaseUnowned(nobody)).toEqual([]);
      await r.h.pruneAll();
    }
    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await unitRows(r, thread)).toBe(1);
    expect(r.retention.degradations()).toEqual([
      expect.objectContaining({ reason: L1_PROOF_HOLDS_UNOWNED, count: 1 }),
      expect.objectContaining({
        reason: L1_PROOF_HOLDS_UNOWNED_LIVE,
        count: 1,
      }),
    ]);

    await burn(r, minted, "cc");
    // Spent, but only one block deep: still kept, no longer by a live unit.
    expect(await r.retention.releaseUnowned(nobody)).toEqual([]);
    expect(r.retention.degradations()).toEqual([
      expect.objectContaining({ reason: L1_PROOF_HOLDS_UNOWNED, count: 1 }),
    ]);
    await advance(r, K);
    expect(await r.retention.releaseUnowned(nobody)).toEqual([]);
    await advance(r, 1);
    expect(await r.retention.releaseUnowned(nobody)).toEqual([r.target]);
    expect(r.retention.degradations()).toEqual([]);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBe(0);
    expect(await unitRows(r, thread)).toBe(0);
  });

  it("does not count a held unit live at the fraud-proof spend address, whose open history no hold keeps", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
      roster: createWatcherProofRosterRetention(),
    });
    const token = `${FOLLOWED}dd`;
    await mintAt(r, "dd", D.fraudProofSpend);
    expect(await r.retention.holdUnits(r.header, [token])).toEqual({
      kind: "held",
    });
    expect(await passAtTip(r)).toBe(true);
    await advance(r, K);
    expect(await r.retention.releaseUnowned(nobody)).toEqual([]);
    await advance(r, 1);
    expect(await r.retention.releaseUnowned(nobody)).toEqual([r.target]);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBe(0);
    // The unit is still live there: its open history stays.
    expect(await unitRows(r, token)).toBe(1);
  });
});
