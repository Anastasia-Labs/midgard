import { rm } from "node:fs/promises";

import { supersededAttemptReadSchedule } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherOrphanedReservations,
  WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
} from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import type { WatcherProverFundingReservationStore } from "../../src/funding/prover-funding-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import {
  loseJournals,
  orphanReadingFactory,
} from "./orphaned-reservation-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** A sweep over signed funding no workflow drives: `lose` removes what
 * named it, and the factory raises its alerts into `alerts`. */
const sweeping = async (lose: "decision" | "workflow") => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  if (lose === "decision") await loseJournals(fixture, []);
  else await rm(fixture.journalDirectory, { recursive: true, force: true });
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
  for (const lose of ["decision", "workflow"] as const)
    it(`holds it with an alert while its attempt is not retired past release finality, then releases it once it is (${lose} lost)`, async () => {
      const { fixture, factory, alerts, orphan, later } = await sweeping(lose);
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
    const { fixture, factory, alerts, orphan } = await sweeping("decision");
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
    const { fixture, factory, orphan } = await sweeping("workflow");
    fixture.chain.mockImplementation(async (txHash) => ({
      kind: "not_found",
      retirement: retirementFixture(txHash),
    }));
    await factory.releaseUnused();
    expect(await orphan()).toMatchObject({ state: "released" });
    expect(factory.decisionHolds()).toEqual([]);
  });

  it("keeps it while its attempt landed within release finality", async () => {
    const { fixture, factory, orphan } = await sweeping("decision");
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

  it("reads at most its per-pass bound of reservations in one sweep", async () => {
    const fixture = await setupFundingRecoveryFixture(
      false,
      false,
      false,
      true,
    );
    const [record] = await fixture.records();
    const observe = vi.fn(fixture.observeSignedTransaction);
    fixture.chain.mockResolvedValue({ kind: "not_found" });
    const orphans = createWatcherOrphanedReservations({
      // Five reservations, each holding the same signed attempt.
      store: {
        readPendingTransition: async () =>
          await fixture.store.readPendingTransition(record!),
        settleOrphanedReservation: async () => false,
      } as unknown as WatcherProverFundingReservationStore,
      observe,
      now: () => 1_000_000,
      perPass: 2,
    });
    const five = [0, 1, 2, 3, 4].map((index) => ({
      ...record!,
      reservationId: `${index}`.repeat(64),
    }));
    const counts: number[] = [];
    for (let pass = 0; pass < 4; pass += 1) {
      observe.mockClear();
      expect(await orphans.sweep(five)).toHaveLength(5);
      counts.push(observe.mock.calls.length);
    }
    expect(counts).toEqual([2, 2, 1, 0]);
  });
});
