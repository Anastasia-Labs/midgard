import {
  computeFraudProofRawL1PointId,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { WatcherProverFundingOrphanedReservationError } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import {
  admitFresh,
  doublyLeased,
  fundWallet,
  loseJournals,
  orphanReadingFactory,
} from "./orphaned-reservation-fixture.js";
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import { fundingOf } from "./superseded-attempt-replacement.js";
import { walletAfter } from "./superseded-attempt-rollback.chain.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const lostWithAttemptInFlight = async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  await loseJournals(fixture, [fixture.fresh]);
  fundWallet(fixture);
  supersededAttemptReadSchedule.forget(fixture.transactionHash);
  return fixture;
};

describe("signed funding without its original decision waits on its attempt", () => {
  it("holds the fresh execution while the attempt can still land, reads it again only once due, and hands it over once it cannot", async () => {
    const fixture = await lostWithAttemptInFlight();
    fixture.useUnspentPendingInputs();
    fundWallet(fixture);
    fixture.chain.mockImplementation(async (txHash) => ({
      kind: "pending",
      txHash,
    }));
    let clock = 1_000_000;
    const { factory, plans } = orphanReadingFactory(fixture, {
      observeSignedTransaction: fixture.observeSignedTransaction,
      now: () => clock,
    });
    const before = await fixture.records();
    fixture.readWalletUtxos.mockClear();

    await expect(admitFresh(fixture, factory)).rejects.toBeInstanceOf(
      WatcherProverFundingOrphanedReservationError,
    );
    expect(fixture.observeSignedTransaction).toHaveBeenCalledTimes(1);
    expect(await fixture.records()).toEqual(before);
    expect(plans).toEqual([]);
    expect(fixture.readWalletUtxos).not.toHaveBeenCalled();

    // An immediate retry does not read the chain again.
    await expect(admitFresh(fixture, factory)).rejects.toThrow(
      "awaiting its next L1 read",
    );
    expect(fixture.observeSignedTransaction).toHaveBeenCalledTimes(1);
    expect(await fixture.records()).toEqual(before);

    // Its validity interval ends; once due, the attempt is read again.
    fixture.chain.mockResolvedValue({ kind: "not_found" });
    clock += 60_000;
    await admitFresh(fixture, factory);
    expect(fixture.observeSignedTransaction).toHaveBeenCalledTimes(2);
    expect(plans).toHaveLength(1);
    const records = await fixture.records();
    expect(
      records.find(
        ({ reservationId }) => reservationId === fixture.plan.reservationId,
      ),
    ).toMatchObject({ state: "released", activeInputs: [] });
    expect(
      records.find(
        ({ reservationId }) => reservationId === plans[0]!.reservationId,
      )!.activeInputs,
    ).toContainEqual(
      expect.objectContaining({ outRef: fundingOf(fixture).outRef }),
    );
    expect(await doublyLeased(fixture)).toEqual([]);
  });

  it("holds an attempt whose L1 read is unresolved", async () => {
    const fixture = await lostWithAttemptInFlight();
    fixture.chain.mockResolvedValue({
      kind: "unknown",
      reason: "provider unavailable",
    });
    const { factory, plans } = orphanReadingFactory(fixture);
    const before = await fixture.records();
    await expect(admitFresh(fixture, factory)).rejects.toThrow(
      `attempt ${fixture.transactionHash} reads unknown`,
    );
    expect(plans).toEqual([]);
    expect(await fixture.records()).toEqual(before);
  });

  it("hands over an attempt invalidated at the tip", async () => {
    const fixture = await lostWithAttemptInFlight();
    fixture.useUnspentPendingInputs();
    fundWallet(fixture);
    const tipBlock = {
      slot: "100",
      blockHash: "c1".repeat(32),
      blockNo: "100",
    };
    const tip = {
      ...tipBlock,
      pointId: computeFraudProofRawL1PointId(tipBlock),
    };
    fixture.observeSignedTransaction.mockImplementation(async (input) => ({
      ...input,
      status: "invalidated_at_tip",
      canonicalPoint: tip,
      releaseFinalPoint: tip,
      inputs: [],
      reason: "an input is spent at the tip",
    }));
    fixture.chain.mockResolvedValue({ kind: "not_found" });
    const { factory, plans } = orphanReadingFactory(fixture);
    await admitFresh(fixture, factory);
    expect(plans).toHaveLength(1);
    const fresh = (await fixture.records()).find(
      ({ reservationId }) => reservationId === plans[0]!.reservationId,
    )!;
    expect(fresh.activeInputs).toContainEqual(
      expect.objectContaining({ outRef: fundingOf(fixture).outRef }),
    );
    expect(await fixture.store.hasSignedHistory!(plans[0]!)).toBe(true);
  });

  it("settles an attempt that landed past release finality instead of handing it over", async () => {
    const fixture = await lostWithAttemptInFlight();
    walletAfter(fixture, fixture.transactionHash);
    fundWallet(fixture);
    fixture.chain.mockImplementation(async (txHash) => ({
      kind: "pending",
      txHash,
      retirement: { ...retirementFixture(txHash), reason: "included" },
    }));
    const { factory, plans } = orphanReadingFactory(fixture);
    await admitFresh(fixture, factory);
    expect(plans).toHaveLength(1);
    const records = await fixture.records();
    expect(
      records.find(
        ({ reservationId }) => reservationId === fixture.plan.reservationId,
      ),
    ).toMatchObject({ state: "released", activeInputs: [] });
    // The fresh reservation takes none of it: nothing of the landed attempt
    // remains to exclude or adopt.
    const fresh = records.find(
      ({ reservationId }) => reservationId === plans[0]!.reservationId,
    )!;
    expect(fresh.activeInputs).not.toContainEqual(
      expect.objectContaining({ outRef: fundingOf(fixture).outRef }),
    );
    expect(await fixture.store.hasSignedHistory!(plans[0]!)).toBe(false);
    expect(await doublyLeased(fixture)).toEqual([]);
  });
});
