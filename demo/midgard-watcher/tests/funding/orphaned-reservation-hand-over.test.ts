import {
  beginWorkflowFundingReservationAction,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import {
  admitFresh,
  doublyLeased,
  freshWorkflow,
  fundWallet,
  loseJournals,
  orphanReadingFactory,
  recordFreshReplacement,
} from "./orphaned-reservation-fixture.js";
import {
  fundingOf,
  signReplacement,
} from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** The decision journal and every workflow journal are lost while the
 * original attempt is in flight; the restarted process holds only the
 * fresh decision. */
const lostWithAttemptInFlight = async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  await loseJournals(fixture, [fixture.fresh]);
  fundWallet(fixture);
  supersededAttemptReadSchedule.forget(fixture.transactionHash);
  return fixture;
};

describe("signed funding without its original decision is handed over", () => {
  it("hands an expired attempt to one fresh reservation, which must spend its input", async () => {
    const fixture = await lostWithAttemptInFlight();
    const funding = fundingOf(fixture);
    fixture.useUnspentPendingInputs();
    fundWallet(fixture);
    fixture.chain.mockResolvedValue({ kind: "not_found" });
    const { factory, plans } = orphanReadingFactory(fixture);
    const journal = await admitFresh(fixture, factory);

    expect(plans).toHaveLength(1);
    const plan = plans[0]!;
    const records = await fixture.records();
    expect(records).toHaveLength(2);
    expect(
      records.find(
        ({ reservationId }) => reservationId === fixture.plan.reservationId,
      ),
    ).toMatchObject({
      state: "released",
      activeInputs: [],
      pendingTransition: null,
    });
    const fresh = records.find(
      ({ reservationId }) => reservationId === plan.reservationId,
    )!;
    expect(fresh.state).toBe("active");
    expect(fresh.activeInputs).toContainEqual(
      expect.objectContaining({ outRef: funding.outRef, role: "funding" }),
    );
    expect(await doublyLeased(fixture)).toEqual([]);

    // The fresh workflow is rebuilt with the attempt superseded.
    await fixture.run(journal);
    const { workflowId } = freshWorkflow(fixture);
    expect(
      (await journal.load(workflowId)).map(({ event }) => event.kind),
    ).toEqual([
      "started",
      "prepared",
      "preflight_passed",
      "submission_intent",
      "reconciled",
    ]);
    await beginWorkflowFundingReservationAction({
      journal,
      action: { actionId: "init", input: { actionKind: "proof.init" } },
    });
    const current = (await fixture.records()).find(
      ({ reservationId }) => reservationId === plan.reservationId,
    )!;
    const collateral = current.activeInputs
      .filter(({ role }) => role === "collateral")
      .map(({ outRef }) => outRef);
    const other = current.activeInputs.find(
      ({ role, outRef }) => role === "funding" && outRef !== funding.outRef,
    )!;
    await expect(
      recordFreshReplacement(
        fixture,
        journal,
        plan,
        signReplacement(other.outRef, BigInt(other.lovelace), 300n, collateral),
        2,
      ),
    ).rejects.toThrow("must share an input with each superseded attempt");
    const replacement = signReplacement(
      funding.outRef,
      BigInt(funding.lovelace),
      300n,
      collateral,
    );
    await recordFreshReplacement(fixture, journal, plan, replacement, 2);
    expect(
      (await fixture.records()).find(
        ({ reservationId }) => reservationId === plan.reservationId,
      )!.pendingTransition?.transactionHash,
    ).toBe(replacement.transactionHash);
  });

  it("adopts a landed attempt as the fresh workflow's action without a second copy", async () => {
    const fixture = await lostWithAttemptInFlight();
    // The chain holds the original attempt; the wallet holds its change.
    fixture.chain.mockImplementation(async (txHash) => ({
      kind: "confirmed",
      txHash,
    }));
    const { factory, plans } = orphanReadingFactory(fixture);
    const journal = await admitFresh(fixture, factory);
    expect(plans).toHaveLength(1);
    expect(await doublyLeased(fixture)).toEqual([]);

    await fixture.run(journal);
    const { workflowId } = freshWorkflow(fixture);
    const events = (await journal.load(workflowId)).map(({ event }) => event);
    expect(events.filter(({ kind }) => kind === "confirmed")).toEqual([
      { kind: "confirmed", actionId: "init", txHash: fixture.transactionHash },
    ]);
    expect(
      new Set(
        events.flatMap((event) =>
          event.kind === "submission_intent" ? [event.txHash] : [],
        ),
      ),
    ).toEqual(new Set([fixture.transactionHash]));
    expect(fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(fixture.adapter.submit).not.toHaveBeenCalled();
    const fresh = (await fixture.records()).find(
      ({ reservationId }) => reservationId === plans[0]!.reservationId,
    )!;
    expect(fresh.pendingTransition).toBeNull();
    expect(fresh.lastConfirmedTransitionDigest).not.toBeNull();
  });
});
