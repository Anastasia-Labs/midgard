import {
  beginWorkflowFundingReservationAction,
  type FraudProofWorkflowJournalStore,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import {
  admitFresh,
  freshWorkflow,
  fundWallet,
  loseJournals,
  orphanReadingFactory,
  recordFreshReplacement,
} from "./orphaned-reservation-fixture.js";
import {
  type Fixture,
  fundingOf,
  signReplacement,
} from "./superseded-attempt-replacement.js";
import {
  chainHolds,
  intentsOf,
  walletAfter,
} from "./superseded-attempt-rollback.chain.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** The original attempt expired at the tip with its journals lost; it is
 * handed over to the fresh execution, which records a replacement spending
 * its funding input. */
const handedOverAndReplaced = async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  await loseJournals(fixture, [fixture.fresh]);
  supersededAttemptReadSchedule.forget(fixture.transactionHash);
  fixture.useUnspentPendingInputs();
  fundWallet(fixture);
  fixture.chain.mockResolvedValue({ kind: "not_found" });
  const { factory, plans } = orphanReadingFactory(fixture);
  const journal = await admitFresh(fixture, factory);
  const plan = plans[0]!;
  await fixture.run(journal);
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const funding = fundingOf(fixture);
  const current = (await fixture.records()).find(
    ({ reservationId }) => reservationId === plan.reservationId,
  )!;
  const replacement = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    300n,
    current.activeInputs
      .filter(({ role }) => role === "collateral")
      .map(({ outRef }) => outRef),
  );
  await recordFreshReplacement(fixture, journal, plan, replacement, 2);
  for (const hash of [fixture.transactionHash, replacement.transactionHash])
    supersededAttemptReadSchedule.forget(hash);
  // The process restarts with the replacement in flight.
  return {
    fixture,
    journal: await admitFresh(fixture, factory),
    plan,
    replacement,
  };
};

const runMinutesApart = async (
  fixture: Fixture,
  journal: FraudProofWorkflowJournalStore,
  runs: number,
) => {
  for (let run = 1; run <= runs; run += 1)
    await fixture.run(journal, () => new Date(Date.now() + run * 60_000));
  return (await journal.load(freshWorkflow(fixture).workflowId)).map(
    ({ event }) => event,
  );
};

describe("a handed-over attempt that lands after a rollback", () => {
  it("is adopted when it lands instead of the pending replacement", async () => {
    const { fixture, journal, plan, replacement } =
      await handedOverAndReplaced();
    walletAfter(fixture, fixture.transactionHash);
    chainHolds(fixture, [fixture.transactionHash]);
    const events = await runMinutesApart(fixture, journal, 4);
    expect(events.filter(({ kind }) => kind === "confirmed")).toEqual([
      { kind: "confirmed", actionId: "init", txHash: fixture.transactionHash },
    ]);
    expect(intentsOf(events, replacement.transactionHash)).toBe(1);
    expect(fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(fixture.adapter.submit).not.toHaveBeenCalled();
    const fresh = (await fixture.records()).find(
      ({ reservationId }) => reservationId === plan.reservationId,
    )!;
    expect(fresh.pendingTransition).toBeNull();
    expect(fresh.lastConfirmedTransitionDigest).not.toBeNull();
  });

  it("is adopted when a rollback drops the confirmed replacement and lands it instead", async () => {
    const { fixture, journal, replacement } = await handedOverAndReplaced();
    walletAfter(fixture, replacement.transactionHash);
    chainHolds(fixture, [replacement.transactionHash]);
    await fixture.run(journal);
    expect(
      (await journal.load(freshWorkflow(fixture).workflowId)).at(-1)!.event,
    ).toEqual({
      kind: "confirmed",
      actionId: "init",
      txHash: replacement.transactionHash,
    });
    for (const hash of [fixture.transactionHash, replacement.transactionHash])
      supersededAttemptReadSchedule.forget(hash);

    // The rollback drops the replacement; the original lands instead.
    walletAfter(fixture, fixture.transactionHash);
    chainHolds(fixture, [fixture.transactionHash]);
    const events = await runMinutesApart(fixture, journal, 4);
    expect(events.at(-1)).toEqual({
      kind: "confirmed",
      actionId: "init",
      txHash: fixture.transactionHash,
    });
    expect(intentsOf(events, replacement.transactionHash)).toBe(1);
    expect(fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(fixture.adapter.submit).not.toHaveBeenCalled();
  });
});
