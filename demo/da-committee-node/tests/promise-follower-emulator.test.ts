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
} from "./helpers/emulator-follower.js";

/**
 * A promise's capacity released at its terminal Close, proved on the
 * committee follower's facts from a config with no Kupo or Ogmios key: the
 * Close is the emulator's signed bytes landed in a follower block, and the
 * terminal point is proved by the production canonical read under the
 * follower's boundary, as the promise admission source reads it.
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
    terminalPoint: FollowerPoint,
    boundary?: Awaited<ReturnType<EmulatorFollower["reads"]["readBoundary"]>>,
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
      liabilities: [{ ...liability, terminalPoint }],
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
  return {
    config,
    follower,
    store,
    close,
    liability,
    retired,
    evidence: () => store.getPromiseCapacityEvidence(key),
  };
};

const REFUSED =
  "Capacity ancestry proof changed its point, height or selected tip";

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
});
