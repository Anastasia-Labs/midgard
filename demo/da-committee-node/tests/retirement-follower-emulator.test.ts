import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import type { FollowerPoint } from "../src/l1/follower/availability-reads.js";
import { type CommitteeStore } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { committeeRetirementSource } from "../src/store/retirement-source.js";
import { horizonMs, retentionFixture } from "./helpers/committee-retirement.js";
import {
  emulatorFollower,
  emulatorWallet,
  noChainIndexCommitteeConfig,
  transactionId,
} from "./helpers/emulator-follower.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";

/**
 * Store retirement of a header whose rows name the committee's own L1
 * submission, proved on the committee follower's facts from a config with
 * no Kupo or Ogmios key. The submission is the emulator's signed bytes
 * landed in a follower block; its block, the expiry checkpoint and the
 * certified point are proved by the production canonical and submission
 * reads under the follower's boundary.
 *
 * Retirement needs the rows' block, its certified point and the boundary
 * each more than 2,160 blocks apart, so the follower's chain runs to about
 * 4,330 blocks.
 */

const databases = postgresTestDatabases("c1b_retirement_follower");
const closers: (() => Promise<void> | void)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});
afterAll(async () => databases.dropAll());

const SLOT_MS = 1_000;
const RECOVERY_DEPTH = 2_160;
/** The rows' block: at 100 s, as the fixture's rows were. */
const OBSERVED_SLOT = 100;
/** The expiry checkpoint: past the rows' retention horizon. */
const EXPIRY_SLOT = (horizonMs + 1_000_000) / SLOT_MS;

type Landing = "valid" | "phase_two_failed" | "abandoned";

const fixture = async (landing: Landing) => {
  const wallet = await emulatorWallet();
  const config = await noChainIndexCommitteeConfig(wallet.account.seedPhrase);
  const follower = await emulatorFollower(config);
  closers.push(follower.close);
  const db = await databases.create();
  const store: CommitteeStore = await PostgresCommitteeStore.open(db.url);
  closers.push(() => store.close?.());

  const split = await wallet.split(2);
  await follower.forward([{ cbor: split.cbor }]);
  // The committee's L1 submission, with collateral the follower tracks: a
  // phase-2 failure consumes it, so the follower stores the failed tx too.
  const submission = await wallet.spend({
    inputs: [split.coins[0]!],
    lovelace: 10_000_000n,
    collateral: split.coins[1]!,
  });
  const beforeSubmission = follower.tip();
  const observed = await follower.forward(
    [{ cbor: submission, valid: landing !== "phase_two_failed" }],
    OBSERVED_SLOT,
  );
  if (landing === "abandoned") {
    await follower.rollBackTo(beforeSubmission);
    await follower.forward([], OBSERVED_SLOT);
  }
  await follower.empty(RECOVERY_DEPTH + 1);
  const expiry = await follower.forward([], EXPIRY_SLOT);

  const f = await retentionFixture(store, {
    observed,
    certified: expiry,
    submissionTxHash: transactionId(submission),
  });
  closers.push(() => f.journal.close());
  const boundary = async (): Promise<FollowerPoint> => {
    const { slot, blockHash, blockNo } = await follower.reads.readBoundary();
    return { slot, blockHash, blockNo };
  };
  const source = committeeRetirementSource({
    ...f.deps,
    readBoundary: boundary,
    slotTimeMs: (slot) => slot * SLOT_MS,
    readCanonicalPoint: (point, at, scope) =>
      scope.read(() => follower.reads.canonicalPoint(point, at)),
    readSubmissionPoint: (txHash, at, scope) =>
      scope.read(() => follower.reads.submissionPoint(txHash, at)),
  });
  const compact = async () => {
    const scope = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 100_000,
    });
    try {
      return await source.compact(scope);
    } finally {
      scope.close();
    }
  };
  // Initialize the floor, seed the header's rows, and take the expiry
  // checkpoint at the boundary past their retention horizon.
  await compact();
  const seeded = await f.seed(1);
  await compact();
  expect((await store.getRetirementFloor())?.checkpoint?.point).toEqual(expiry);
  // The boundary: past the checkpoint's recovery depth.
  await follower.empty(RECOVERY_DEPTH + 1);
  return {
    store,
    follower,
    compact,
    expiry,
    headerHash: seeded.header.headerHash,
  };
};

describe("store retirement proved on the follower's facts, with no chain index configured", () => {
  it(
    "retires the header once its rows' block, its submission and its checkpoint are canonical and deep",
    { timeout: 60_000 },
    async () => {
      const f = await fixture("valid");
      await expect(f.compact()).resolves.toEqual([f.headerHash]);
      expect(await f.store.getDaPayload(f.headerHash)).toBeUndefined();
      expect((await f.store.getRetirementFloor())?.point).toEqual(f.expiry);
    },
  );

  it(
    "holds the header when its submission failed phase 2",
    { timeout: 60_000 },
    async () => {
      const f = await fixture("phase_two_failed");
      await expect(f.compact()).resolves.toEqual([]);
      expect(await f.store.getDaPayload(f.headerHash)).toBeDefined();
      expect((await f.store.getRetirementFloor())?.point).toBeUndefined();
    },
  );

  it(
    "refuses retirement when its rows name a block the follower abandoned",
    { timeout: 60_000 },
    async () => {
      const f = await fixture("abandoned");
      await expect(f.compact()).rejects.toThrow(
        "Retirement checkpoint lacks exact same-boundary ancestry",
      );
      expect(await f.store.getDaPayload(f.headerHash)).toBeDefined();
      expect((await f.store.getRetirementFloor())?.point).toBeUndefined();
    },
  );
});
