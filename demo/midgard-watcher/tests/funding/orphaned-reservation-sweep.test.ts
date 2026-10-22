import { supersededAttemptReadSchedule } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { orphanReadingFactory } from "./orphaned-reservation-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** A sweep over signed funding whose workflow is gone, and the factory
 * raises its alerts into `alerts`. */
const sweeping = async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  fixture.workflowJournal.forgetObjective(fixture.objective);
  supersededAttemptReadSchedule.forget(fixture.transactionHash);
  const alerts: { subjectDigest: string; active: boolean }[] = [];
  let clock = 1_000_000;
  const { factory } = orphanReadingFactory(fixture, {
    observeSignedTransaction: fixture.observeSignedTransaction,
    now: () => clock,
    alertSink: () => ({
      setAlert: ({ subjectDigest, active }) => {
        alerts.push({ subjectDigest, active });
      },
    }),
  });
  const orphan = () =>
    fixture
      .records()
      .then((records) =>
        records.find(
          ({ reservationId }) => reservationId === fixture.plan.reservationId,
        ),
      );
  return {
    fixture,
    factory,
    alerts,
    orphan,
    later: () => {
      clock += 60_000;
    },
  };
};

describe("signed funding no workflow drives is settled from its own attempts", () => {
  it("holds it with an alert while its attempt is not retired past release finality, then releases it once it is", async () => {
    const { fixture, factory, alerts, orphan, later } = await sweeping();
    fixture.chain.mockResolvedValue({ kind: "not_found" });
    const before = await fixture.records();
    await factory.releaseUnused();
    expect(factory.decisionHolds()).toEqual([
      expect.objectContaining({
        kind: "reservation",
        reservationId: fixture.plan.reservationId,
        readiness: WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
      }),
    ]);
    expect(alerts).toEqual([
      { subjectDigest: fixture.plan.reservationId, active: true },
    ]);
    expect(await fixture.records()).toEqual(before);

    // A sweep before the next read is due neither reads nor raises again.
    const reads = fixture.observeSignedTransaction.mock.calls.length;
    await factory.releaseUnused();
    expect(fixture.observeSignedTransaction).toHaveBeenCalledTimes(reads);
    expect(alerts).toHaveLength(1);

    fixture.chain.mockImplementation(async (txHash) =>
      expiredNotFound(txHash!),
    );
    later();
    await factory.releaseUnused();
    expect(await orphan()).toMatchObject({
      state: "released",
      activeInputs: [],
      pendingTransition: null,
    });
    expect(factory.decisionHolds()).toEqual([]);
    expect(alerts).toEqual([
      { subjectDigest: fixture.plan.reservationId, active: true },
      { subjectDigest: fixture.plan.reservationId, active: false },
    ]);
    expect(await fixture.store.readReservedOutRefs({})).toEqual([]);
  });

  it("releases it once its attempt landed past release finality", async () => {
    const { fixture, factory, alerts, orphan } = await sweeping();
    fixture.chain.mockImplementation(async (txHash) => ({
      kind: "pending",
      txHash,
      retirement: { ...retirementFixture(txHash), reason: "included" },
    }));
    await factory.releaseUnused();
    expect(await orphan()).toMatchObject({ state: "released" });
    expect(factory.decisionHolds()).toEqual([]);
    expect(alerts).toEqual([]);
  });

  it("releases it once another transaction spent its input past release finality", async () => {
    const { fixture, factory, orphan } = await sweeping();
    fixture.chain.mockImplementation(async (txHash) => ({
      kind: "not_found",
      retirement: retirementFixture(txHash),
    }));
    await factory.releaseUnused();
    expect(await orphan()).toMatchObject({ state: "released" });
    expect(factory.decisionHolds()).toEqual([]);
  });

  it("keeps it while its attempt landed within release finality", async () => {
    const { fixture, factory, orphan } = await sweeping();
    fixture.chain.mockImplementation(async (txHash) => ({
      kind: "confirmed",
      txHash: txHash!,
    }));
    await factory.releaseUnused();
    expect(await orphan()).toMatchObject({ state: "active" });
    expect(factory.decisionHolds()).toEqual([
      expect.objectContaining({
        readiness: WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
      }),
    ]);
  });
});
