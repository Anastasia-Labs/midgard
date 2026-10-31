import { supersededAttemptReadSchedule } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { fundWallet } from "./orphaned-reservation-fixture.js";
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import type { Fixture } from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** The original attempt expired at the tip with its workflow gone. */
const expiredWithJournalsLost = async () => {
  const fixture = await setupFundingRecoveryFixture(false, true);
  fixture.workflowJournal.forgetObjective(fixture.objective);
  supersededAttemptReadSchedule.forget(fixture.transactionHash);
  fixture.useUnspentPendingInputs();
  fundWallet(fixture);
  fixture.chain.mockResolvedValue({ kind: "not_found" });
  return fixture;
};

const orphanOf = async (fixture: Fixture) =>
  (await fixture.records()).find(
    ({ reservationId }) => reservationId === fixture.plan.reservationId,
  )!;

/** The store abandons the orphan's pending attempt, unretired. */
const abandonPending = async (fixture: Fixture) => {
  const orphan = await orphanOf(fixture);
  await fixture.store.abandonPendingTransition({
    journal: [],
    plan: fixture.plan,
    expectedRevision: orphan.revision,
    transitionDigest: orphan.pendingTransition!.transitionDigest,
  });
};

describe("the store settles signed funding no workflow drives", () => {
  it("only on a retirement of each of its attempts, each of the attempt it names", async () => {
    const fixture = await expiredWithJournalsLost();
    await abandonPending(fixture);
    const other = "ab".repeat(32);
    const settle = async (
      retirements: { transactionHash: string; retirement: unknown }[],
    ) =>
      await fixture.store.settleOrphanedReservation!({
        orphan: await orphanOf(fixture),
        retirements,
      });
    const before = await fixture.records();
    expect(await settle([])).toBe(false);
    expect(
      await settle([
        { transactionHash: other, retirement: retirementFixture(other) },
      ]),
    ).toBe(false);
    await expect(
      settle([
        {
          transactionHash: fixture.transactionHash,
          retirement: retirementFixture(other),
        },
      ]),
    ).rejects.toThrow();
    expect(await fixture.records()).toEqual(before);
    expect(
      await settle([
        {
          transactionHash: fixture.transactionHash,
          retirement: retirementFixture(fixture.transactionHash),
        },
      ]),
    ).toBe(true);
    expect(await orphanOf(fixture)).toBeUndefined();
    expect(await fixture.store.readReservedOutRefs({})).toEqual([]);
  });
});
