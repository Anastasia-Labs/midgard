import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { committeeFollowerRetirementReads } from "../src/availability/promise-retirement-runtime.js";
import { COMMITTEE_QUEUE_TABLE } from "../src/l1/follower/queue-table.js";
import { NO_PIN_TARGETS } from "../src/l1/follower/retention-pins.js";
import { type CommitteeStore } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { committeeRetirementSource } from "../src/store/retirement-source.js";
import {
  horizonMs,
  oldPoint,
  retentionFixture,
} from "./helpers/committee-retirement.js";
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
 * landed in a follower block, next to the header's state-queue node; its
 * block, the header's landing, the expiry checkpoint and the certified
 * point are proved by the production canonical, landing and submission
 * reads under the follower's boundary.
 *
 * The follower prunes after every block, as its loop does at the tip, at a
 * small k. Retirement needs the rows' block, the checkpoint and the boundary
 * each more than its recovery depth (2,160 blocks, the signed profile's)
 * apart, so the chain runs to about 4,350 blocks and every point retirement
 * proves lies far below the follower's pruned window: only the committee's
 * retention pins keep it.
 */

const databases = postgresTestDatabases("c1c_retirement_follower");
const closers: (() => Promise<void> | void)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});
afterAll(async () => databases.dropAll());

const SLOT_MS = 1_000;
/** The follower's k. */
const K = 16;
/** The retirement's recovery depth, fixed by the signed profile. */
const RECOVERY_DEPTH = 2_160;
/** The rows' block: at 100 s, as the fixture's rows were. */
const OBSERVED_SLOT = 100;
/** The expiry checkpoint: past the rows' retention horizon. */
const EXPIRY_SLOT = (horizonMs + 1_000_000) / SLOT_MS;

type Landing = "valid" | "phase_two_failed" | "abandoned";

const fixture = async (
  landing: Landing,
  options: Readonly<{ pins: boolean }> = { pins: true },
) => {
  const wallet = await emulatorWallet();
  const config = await noChainIndexCommitteeConfig(wallet.account.seedPhrase);
  const follower = await emulatorFollower(config, K);
  closers.push(follower.close);
  const db = await databases.create();
  const store: CommitteeStore = await PostgresCommitteeStore.open(db.url);
  closers.push(() => store.close?.());
  // The production pin source: every point and submission the committee
  // store names, read again before every prune step.
  if (options.pins)
    follower.retention.bind("records", () => store.readL1PinTargets());

  // The seeded rows name the chain once it exists.
  const chain = {
    observed: oldPoint,
    certified: oldPoint,
    submissionTxHash: "88".repeat(32),
  };
  const f = await retentionFixture(store, chain);
  closers.push(() => f.journal.close());
  const headerHash = f.header(1).headerHash;

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
  // The header's node as the committee projection records it: created by
  // the block that carries the header's commit.
  await follower.store.transaction("write", (tx) =>
    tx.query(
      `INSERT INTO ${COMMITTEE_QUEUE_TABLE} (tx_hash, output_index, kind, header_hash, problems, created_slot, created_height, created_tx_index) VALUES (?, 0, 'node', ?, '[]', ?, ?, 0)`,
      [
        Buffer.from(transactionId(submission), "hex"),
        headerHash,
        observed.slot,
        observed.blockNo,
      ],
    ),
  );
  if (landing === "abandoned") {
    // The fork the follower switches to carries another spend of the same
    // coin at that slot; its unspent output keeps that block stored.
    await follower.rollBackTo(beforeSubmission);
    const rival = await wallet.spend({
      inputs: [split.coins[0]!],
      lovelace: 9_000_000n,
    });
    await follower.forward([{ cbor: rival }], OBSERVED_SLOT);
  }
  chain.observed = observed;
  chain.submissionTxHash = transactionId(submission);

  const source = committeeRetirementSource({
    ...f.deps,
    ...committeeFollowerRetirementReads(follower.reads, () =>
      follower.reads.readBoundary(),
    ),
    slotTimeMs: (slot) => slot * SLOT_MS,
  });
  const inScope = async <T>(
    run: (scope: SDK.DaAvailabilityReadScope) => Promise<T>,
  ): Promise<T> => {
    const scope = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 100_000,
    });
    try {
      return await run(scope);
    } finally {
      scope.close();
    }
  };
  const compact = () => inScope((scope) => source.compact(scope));
  // Initialize the floor, run past the recovery depth to the expiry block,
  // seed the header's rows (their capacity certified there), and take the
  // expiry checkpoint at the boundary past the rows' horizon.
  await compact();
  await follower.empty(RECOVERY_DEPTH + 1);
  const expiry = await follower.forward([], EXPIRY_SLOT);
  chain.certified = expiry;
  await f.seed(1);
  await compact();
  expect((await store.getRetirementFloor())?.checkpoint?.point).toEqual(expiry);
  // The boundary: past the checkpoint's recovery depth.
  await follower.empty(RECOVERY_DEPTH + 1);
  // Every point retirement proves now lies below the pruned window.
  expect(await follower.prunedThroughSlot()).toBeGreaterThanOrEqual(
    expiry.slot,
  );
  expect(follower.pruneErrors).toEqual([]);
  return {
    store,
    follower,
    compact,
    capture: () => inScope((scope) => source.capture(scope)),
    holds: source.holds,
    observed,
    expiry,
    headerHash,
  };
};

type Fixture = Awaited<ReturnType<typeof fixture>>;

const expectRetained = async (f: Fixture): Promise<void> => {
  expect(await f.store.getDaPayload(f.headerHash)).toBeDefined();
  expect((await f.store.getRetirementFloor())?.point).toBeUndefined();
};

const held = (f: Fixture, cause: string, detail = "") =>
  expect.stringMatching(
    new RegExp(
      `^committee_retirement_held: header ${f.headerHash}: ${cause}: ${detail}`,
      "u",
    ),
  );

describe("store retirement proved on the follower's facts, with no chain index configured", () => {
  it(
    "retires the header once its rows' block, its landing, its submission and its checkpoint are canonical and deep, below the pruned window",
    { timeout: 180_000 },
    async () => {
      const f = await fixture("valid");
      await expect(f.compact()).resolves.toEqual([f.headerHash]);
      expect(f.holds()).toEqual([]);
      expect(await f.store.getDaPayload(f.headerHash)).toBeUndefined();
      expect((await f.store.getRetirementFloor())?.point).toEqual(f.expiry);
    },
  );

  it(
    "without the committee's pins the follower prunes the checkpoint: retirement takes it again and retires nothing",
    { timeout: 180_000 },
    async () => {
      const f = await fixture("valid", { pins: false });
      await expect(f.compact()).resolves.toEqual([]);
      await expectRetained(f);
      // The checkpoint the follower no longer holds is taken again at the
      // boundary: the expiry waits, it never wedges.
      expect((await f.store.getRetirementFloor())?.checkpoint?.point).toEqual(
        f.follower.tip(),
      );
    },
  );

  it(
    "holds the header with a named reason when its submission failed phase 2",
    { timeout: 180_000 },
    async () => {
      const f = await fixture("phase_two_failed");
      await expect(f.compact()).resolves.toEqual([]);
      await expectRetained(f);
      expect(f.holds()).toEqual([held(f, "submission_without_valid_landing")]);
    },
  );

  it(
    "holds the header with a named reason when its rows name a block the follower abandoned",
    { timeout: 180_000 },
    async () => {
      const f = await fixture("abandoned");
      await expect(f.compact()).resolves.toEqual([]);
      await expectRetained(f);
      expect(f.holds()).toEqual([
        held(
          f,
          "point_not_canonical",
          `its observed chain point at ${f.observed.slot.toString()}:`,
        ),
      ]);
    },
  );

  it(
    "reads the retirement floor on capture more than k + 2 blocks later, and names the read once its pin is gone",
    { timeout: 180_000 },
    async () => {
      const f = await fixture("valid");
      await expect(f.compact()).resolves.toEqual([f.headerHash]);
      // The floor is now the expiry point; the follower runs on and prunes.
      await f.follower.empty(K + 3);
      expect(await f.follower.prunedThroughSlot()).toBeGreaterThan(
        f.expiry.slot,
      );
      await expect(f.capture()).resolves.toBeDefined();
      // With no pin left, the next prune deletes the floor's block: the
      // capture refuses with the follower's named reason.
      f.follower.retention.bind("records", () => NO_PIN_TARGETS);
      await f.follower.forward();
      await expect(f.capture()).rejects.toThrow(/^point_beyond_retention: /u);
    },
  );
});
