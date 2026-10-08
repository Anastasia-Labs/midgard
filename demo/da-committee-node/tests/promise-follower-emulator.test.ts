import { CML, type UTxO } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { promiseCapacityEvidenceKey } from "../src/availability/promise-capacity-evidence.js";
import {
  type PromiseCapacityLiability,
  retiredPromiseCutoffs,
} from "../src/availability/promise-cutoff-source.js";
import {
  type FollowerPoint,
  sameBoundary,
} from "../src/l1/follower/availability-reads.js";
import type { CommitteeStore } from "../src/store.js";
import {
  openTestCommitteeStore,
  saveHealthyL1SourceState,
  testStoreDatabase,
} from "./helpers/committee-store.js";
import {
  type EmulatorFollower,
  emulatorFollower,
  emulatorWallet,
  noChainIndexCommitteeConfig,
  transactionId,
} from "./helpers/emulator-follower.js";

/**
 * A promise's capacity released at its terminal Close, proved on the
 * committee follower's facts from a config with no Kupo or Ogmios key: the
 * Close is the emulator's signed bytes landed in a follower block, and the
 * terminal point is proved by the production canonical read under the
 * follower's boundary, as the promise admission source reads it.
 *
 * The follower prunes after every block at a small k, as its loop does at
 * the tip; the terminal point stays readable through the committee's
 * retention pins on its records.
 */

const closers: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});

const ACTOR = "ef".repeat(28);
const SLOT_MS = 1_000;

const fixture = async () => {
  const wallet = await emulatorWallet();
  const config = await noChainIndexCommitteeConfig(wallet.account.seedPhrase);
  const follower = await emulatorFollower(config);
  closers.push(follower.close);
  const database = await testStoreDatabase();
  const store: CommitteeStore = await saveHealthyL1SourceState(
    await openTestCommitteeStore(database),
  );
  const split = await wallet.split(2);
  await follower.forward([{ cbor: split.cbor }]);
  // The committee's terminal Close of the promised header.
  const close = await wallet.spend({
    inputs: [split.coins[0]!],
    lovelace: 10_000_000n,
  });
  const contractManifestId = String(config.contractDeploymentInfo.manifestId);
  const liability: PromiseCapacityLiability = {
    headerHash: "12".repeat(28),
    commitmentDigest: "34".repeat(32),
    // Far in the future: only the terminal point can release the capacity.
    cutoffTimeMs: 1e12,
  };
  const key = promiseCapacityEvidenceKey({
    deploymentFingerprint: config.deploymentFingerprint,
    contractManifestId,
    actorId: ACTOR,
    commitmentDigest: liability.commitmentDigest,
  });
  /** The admission source's capacity read at the follower's boundary. */
  const retired = async (
    terminalPoint: FollowerPoint | undefined,
    boundary?: Awaited<ReturnType<EmulatorFollower["reads"]["readBoundary"]>>,
    promise: Partial<PromiseCapacityLiability> = {},
  ) => {
    const before = boundary ?? (await follower.reads.readBoundary());
    return retiredPromiseCutoffs({
      store,
      deploymentFingerprint: config.deploymentFingerprint,
      contractManifestId,
      actorId: ACTOR,
      recoveryDepth: config.automaticRecoveryMaxDepth,
      boundary: {
        slot: before.slot,
        blockHash: before.blockHash,
        blockNo: before.blockNo,
      },
      canonicalTimeMs: before.slot * SLOT_MS,
      slotTimeMs: (slot) => slot * SLOT_MS,
      liabilities: [{ ...liability, ...promise, terminalPoint }],
      readCanonicalPoint: (point) =>
        follower.reads.canonicalPoint(point, before),
      assertCurrent: async () => {
        if (!sameBoundary(before, await follower.reads.readBoundary()))
          throw new Error(
            "Canonical generation changed during capacity retirement proof",
          );
      },
    });
  };
  /**
   * Spends the Close's outputs in the next block: the Close's block then
   * holds nothing the follower tracks, so only a pin keeps it.
   */
  const spendClose = async () => {
    const outputs = CML.Transaction.from_cbor_hex(close).body().outputs();
    const inputs: UTxO[] = [];
    for (let index = 0; index < outputs.len(); index += 1)
      inputs.push({
        txHash: transactionId(close),
        outputIndex: index,
        address: wallet.address,
        assets: { lovelace: outputs.get(index).amount().coin() },
      });
    return follower.forward([
      { cbor: await wallet.spend({ inputs, lovelace: 5_000_000n }) },
    ]);
  };
  return {
    config,
    follower,
    store,
    close,
    spendClose,
    liability,
    retired,
    evidence: () => store.getPromiseCapacityEvidence(key),
  };
};

const REFUSED =
  "Capacity ancestry proof changed its point, height or read boundary";

describe("promise capacity released at a terminal Close on the follower's facts, with no chain index configured", () => {
  it("releases the capacity once the Close's block is on the follower's chain", async () => {
    const f = await fixture();
    const landed = await f.follower.forward([{ cbor: f.close }]);
    await f.follower.empty(f.config.finalityDepth);
    await expect(f.retired(landed)).resolves.toEqual(
      new Set([f.liability.commitmentDigest]),
    );
    expect(await f.evidence()).toMatchObject({
      retirementKind: "terminal",
      point: landed,
    });
  });

  it("refuses a terminal point on a branch the follower abandoned, recording nothing", async () => {
    const f = await fixture();
    const before = f.follower.tip();
    const abandoned = await f.follower.forward([{ cbor: f.close }]);
    await f.follower.rollBackTo(before);
    // The sibling at the same height carries no Close.
    await f.follower.forward([], abandoned.slot);
    await f.follower.empty(f.config.finalityDepth);
    await expect(f.retired(abandoned)).rejects.toThrow(REFUSED);
    expect(await f.evidence()).toBeUndefined();
  });

  it("refuses a terminal point whose height differs from the block's", async () => {
    const f = await fixture();
    const landed = await f.follower.forward([{ cbor: f.close }]);
    await f.follower.empty(f.config.finalityDepth);
    await expect(
      f.retired({ ...landed, blockNo: landed.blockNo + 1 }),
    ).rejects.toThrow(REFUSED);
    expect(await f.evidence()).toBeUndefined();
  });

  it("refuses a proof read under a boundary the follower has moved past", async () => {
    const f = await fixture();
    const landed = await f.follower.forward([{ cbor: f.close }]);
    const stale = await f.follower.reads.readBoundary();
    await f.follower.forward();
    await expect(f.retired(landed, stale)).rejects.toThrow(
      "The follower's view moved during a canonical read",
    );
    expect(await f.evidence()).toBeUndefined();
  });

  it("charges the capacity again when the Close's block is rolled back before certification", async () => {
    const f = await fixture();
    const before = f.follower.tip();
    const landed = await f.follower.forward([{ cbor: f.close }]);
    await expect(f.retired(landed)).resolves.toEqual(
      new Set([f.liability.commitmentDigest]),
    );
    await f.follower.rollBackTo(before);
    await f.follower.empty(2);
    await expect(f.retired(landed)).resolves.toEqual(new Set());
  });

  it("awaits the follower while it holds the committee", async () => {
    const f = await fixture();
    const landed = await f.follower.forward([{ cbor: f.close }]);
    f.follower.hold([{ reason: "follower_catching_up", detail: "behind" }]);
    await expect(f.retired(landed)).rejects.toThrow(
      "follower_catching_up: behind",
    );
    expect(await f.evidence()).toBeUndefined();
  });
  it(
    "certifies and admits the released capacity more than k + 2 blocks after the follower pruned the Close's block, and names the read without the pin",
    { timeout: 120_000 },
    async () => {
      for (const pinned of [true, false]) {
        const f = await fixture();
        if (pinned)
          f.follower.retention.bind("records", () =>
            f.store.readL1PinTargets(),
          );
        const landed = await f.follower.forward([{ cbor: f.close }]);
        await expect(f.retired(landed)).resolves.toEqual(
          new Set([f.liability.commitmentDigest]),
        );
        await f.spendClose();
        // Past the recovery depth: the release is certified.
        await f.follower.empty(f.config.automaticRecoveryMaxDepth + 1);
        expect(await f.follower.prunedThroughSlot()).toBeGreaterThan(
          landed.slot,
        );
        expect(f.follower.pruneErrors).toEqual([]);
        if (!pinned) {
          await expect(f.retired(landed)).rejects.toThrow(
            /^point_beyond_retention: /u,
          );
          continue;
        }
        await expect(f.retired(landed)).resolves.toEqual(
          new Set([f.liability.commitmentDigest]),
        );
        expect((await f.evidence())?.certifiedAt).toBeDefined();
        // Admission reads the certified point again, k + 2 blocks on.
        await f.follower.empty(f.follower.securityParameter + 3);
        await expect(f.retired(landed)).resolves.toEqual(
          new Set([f.liability.commitmentDigest]),
        );
      }
    },
  );

  it(
    "certifies an expired promise's release exactly once, at final, through a rollback at depth cd + 1",
    { timeout: 120_000 },
    async () => {
      const f = await fixture();
      f.follower.retention.bind("records", () => f.store.readL1PinTargets());
      const cd = f.config.finalityDepth;
      const k = f.config.automaticRecoveryMaxDepth;
      const before = f.follower.tip();
      // The promise's open cutoff falls in the next block's slot.
      const expiring = { cutoffTimeMs: (before.slot + 1) * SLOT_MS };
      const released = new Set([f.liability.commitmentDigest]);
      const expiry = await f.follower.forward();
      await expect(f.retired(undefined, undefined, expiring)).resolves.toEqual(
        released,
      );
      expect(await f.evidence()).toMatchObject({
        retirementKind: "open_cutoff",
        point: expiry,
      });
      // The expiry at depth cd + 1: safe, not final, so not certified.
      await f.follower.empty(cd);
      await expect(f.retired(undefined, undefined, expiring)).resolves.toEqual(
        released,
      );
      expect((await f.evidence())?.certifiedAt).toBeUndefined();

      // The rollback at depth cd + 1 takes the chain back before the
      // cutoff: the promise is charged again, nothing was certified.
      await f.follower.rollBackTo(before);
      await expect(f.retired(undefined, undefined, expiring)).resolves.toEqual(
        new Set(),
      );
      expect((await f.evidence())?.certifiedAt).toBeUndefined();

      // The cutoff passes again on the new branch; certification waits
      // for final (depth > k), then happens once.
      const reExpiry = await f.follower.forward();
      expect(reExpiry.blockHash).not.toBe(expiry.blockHash);
      await expect(f.retired(undefined, undefined, expiring)).resolves.toEqual(
        released,
      );
      await f.follower.empty(k - 1);
      await expect(f.retired(undefined, undefined, expiring)).resolves.toEqual(
        released,
      );
      expect(await f.evidence()).toMatchObject({ point: reExpiry });
      expect((await f.evidence())?.certifiedAt).toBeUndefined();
      const finalTip = await f.follower.empty(1);
      await expect(f.retired(undefined, undefined, expiring)).resolves.toEqual(
        released,
      );
      expect((await f.evidence())?.certifiedAt).toEqual(finalTip);
      await f.follower.empty(3);
      await expect(f.retired(undefined, undefined, expiring)).resolves.toEqual(
        released,
      );
      expect((await f.evidence())?.certifiedAt).toEqual(finalTip);
      expect(f.follower.pruneErrors).toEqual([]);
    },
  );
});
