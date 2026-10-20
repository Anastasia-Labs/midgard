import {
  journalJsonDigest,
  normalizeJournalJson,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { WatcherProverFundingOrphanedReservationError } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import type { WatcherProverFundingReservationStore } from "../../src/funding/prover-funding-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { fundingTerminal } from "./funding-handoff-fixture.js";
import {
  admitFresh,
  doublyLeased,
  fundWallet,
  loseJournals,
  orphanReadingFactory,
} from "./orphaned-reservation-fixture.js";
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import { type Fixture, fundingOf } from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

type HandOver = Parameters<
  NonNullable<WatcherProverFundingReservationStore["handOverOrphanedAttempts"]>
>[0];

/** The original attempt expired at the tip with its journals lost. */
const expiredWithJournalsLost = async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  await loseJournals(fixture, [fixture.fresh]);
  supersededAttemptReadSchedule.forget(fixture.transactionHash);
  fixture.useUnspentPendingInputs();
  fundWallet(fixture);
  fixture.chain.mockResolvedValue({ kind: "not_found" });
  return fixture;
};

/** The funding factory over the fixture, whose hand-over runs `before`
 * first. */
const intercepting = (
  fixture: Fixture,
  before: (input: HandOver) => Promise<void>,
) =>
  orphanReadingFactory(
    fixture,
    { observeSignedTransaction: fixture.observeSignedTransaction },
    {
      handOverOrphanedAttempts: async (input) => {
        await before(input);
        return await fixture.store.handOverOrphanedAttempts!(input);
      },
    },
  );

const orphanOf = async (fixture: Fixture) =>
  (await fixture.records()).find(
    ({ reservationId }) => reservationId === fixture.plan.reservationId,
  )!;

describe("the store hands signed funding over only to a fresh reservation of its target", () => {
  it("refuses a hand-over that does not match, and changes nothing", async () => {
    const fixture = await expiredWithJournalsLost();
    const refusals: string[] = [];
    let handedOver: HandOver | undefined;
    const { factory, plans } = intercepting(fixture, async (input) => {
      handedOver = input;
      const records = await fixture.records();
      const orphan = await orphanOf(fixture);
      for (const [name, changed] of Object.entries({
        revision: { ...input, expectedRevision: orphan.revision },
        header: { ...input, headerHash: "00".repeat(32) },
        category: { ...input, category: "networkId" },
        empty: { ...input, orphans: [] },
        signed: {
          ...input,
          plan: fixture.plan,
          expectedRevision: orphan.revision,
        },
      })) {
        await expect(
          fixture.store.handOverOrphanedAttempts!(changed),
        ).rejects.toThrow("prover reservation hand-over mismatch");
        expect(await fixture.records()).toEqual(records);
        refusals.push(name);
      }
    });
    await admitFresh(fixture, factory);
    expect(refusals).toEqual([
      "revision",
      "header",
      "category",
      "empty",
      "signed",
    ]);
    const released = await orphanOf(fixture);
    expect(released).toMatchObject({ state: "released" });

    // The released reservation takes nothing over, not even the funding
    // just handed over.
    const records = await fixture.records();
    await expect(
      fixture.store.handOverOrphanedAttempts!({
        ...handedOver!,
        plan: fixture.plan,
        expectedRevision: released.revision,
        orphans: records.filter(
          ({ reservationId }) => reservationId === plans[0]!.reservationId,
        ),
      }),
    ).rejects.toThrow("prover reservation hand-over mismatch");
    expect(await fixture.records()).toEqual(records);
  });

  it("refuses an orphan that changed after it was read, and hands it over as it is on the next admission", async () => {
    const fixture = await expiredWithJournalsLost();
    let changed = false;
    const { factory, plans } = intercepting(fixture, async () => {
      if (changed) return;
      changed = true;
      // The attempt is abandoned between the read and the hand-over.
      const orphan = await orphanOf(fixture);
      await fixture.store.abandonPendingTransition({
        handoff: {
          workflowId: fixture.handoff.workflowId,
          identity: fixture.handoff.identity,
          preparedArtifactDigest: fixture.handoff.preparedArtifactDigest,
          expectedJournalSequence: fixture.handoff.expectedJournalSequence,
          submissionIntent: fixture.handoff.submissionIntent,
          reconciliation: {
            kind: "reconciled",
            actionId: "init",
            outcome: "not_found",
            txHash: fixture.transactionHash,
          },
        },
        plan: fixture.plan,
        expectedRevision: orphan.revision,
        transitionDigest: orphan.pendingTransition!.transitionDigest,
      });
    });
    await expect(admitFresh(fixture, factory)).rejects.toThrow(
      "prover reservation hand-over mismatch",
    );
    expect(await orphanOf(fixture)).toMatchObject({
      state: "active",
      pendingTransition: null,
    });
    supersededAttemptReadSchedule.forget(fixture.transactionHash);
    await admitFresh(fixture, factory);
    expect(await orphanOf(fixture)).toMatchObject({ state: "released" });
    const fresh = (await fixture.records()).find(
      ({ reservationId }) => reservationId === plans[0]!.reservationId,
    )!;
    expect(fresh.activeInputs).toContainEqual(
      expect.objectContaining({ outRef: fundingOf(fixture).outRef }),
    );
    expect(await fixture.store.hasSignedHistory!(fresh)).toBe(true);
    expect(await doublyLeased(fixture)).toEqual([]);
  });

  it("takes the hand-over on restart when the process stopped between the fresh reservation and its hand-over", async () => {
    const fixture = await expiredWithJournalsLost();
    const { factory, plans } = intercepting(fixture, async () => {
      throw new Error("process stopped");
    });
    await expect(admitFresh(fixture, factory)).rejects.toThrow(
      "process stopped",
    );
    // A restart's sweep reclaims the unused fresh reservation's inputs.
    await orphanReadingFactory(fixture).factory.releaseUnused();
    expect(
      (await fixture.records()).find(
        ({ reservationId }) => reservationId === plans[0]!.reservationId,
      ),
    ).toMatchObject({ state: "active", activeInputs: [] });
    expect(await orphanOf(fixture)).toMatchObject({ state: "active" });
    supersededAttemptReadSchedule.forget(fixture.transactionHash);

    await admitFresh(fixture, orphanReadingFactory(fixture).factory);
    expect(await orphanOf(fixture)).toMatchObject({
      state: "released",
      activeInputs: [],
    });
    const fresh = (await fixture.records()).find(
      ({ reservationId }) => reservationId === plans[0]!.reservationId,
    )!;
    expect(fresh.activeInputs).toContainEqual(
      expect.objectContaining({ outRef: fundingOf(fixture).outRef }),
    );
    expect(await fixture.store.hasSignedHistory!(fresh)).toBe(true);
    expect(await doublyLeased(fixture)).toEqual([]);
  });
});

describe("signed funding whose action completed without its journal", () => {
  it("is held, not handed over, until it settles release-final", async () => {
    const fixture = await setupFundingRecoveryFixture(
      false,
      false,
      false,
      true,
    );
    await fixture.run(await fixture.recover());
    const [confirmed] = await fixture.records();
    const terminal = {
      ...fundingTerminal(
        fixture.old.headerHash,
        fixture.transactionHash,
        "ee".repeat(32),
      ),
      observedAt: {
        slot: "4242",
        blockHash: "44".repeat(32),
        confirmationDepth: 5,
      },
    };
    await fixture.store.release({
      handoff: {
        workflowId: fixture.handoff.workflowId,
        identity: fixture.handoff.identity,
        preparedArtifactDigest: fixture.handoff.preparedArtifactDigest,
        expectedJournalSequence: fixture.handoff.expectedJournalSequence,
        completion: {
          kind: "completed",
          terminal,
          terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
        },
      },
      plan: fixture.plan,
      expectedRevision: confirmed!.revision,
    });
    expect(await orphanOf(fixture)).toMatchObject({ state: "active" });
    await loseJournals(fixture, [fixture.fresh]);
    fundWallet(fixture);
    supersededAttemptReadSchedule.forget(fixture.transactionHash);
    let clock = 1_000_000;
    const { factory, plans } = orphanReadingFactory(fixture, {
      observeSignedTransaction: fixture.observeSignedTransaction,
      now: () => clock,
    });
    const before = await fixture.records();
    // Landed within release finality.
    await expect(admitFresh(fixture, factory)).rejects.toBeInstanceOf(
      WatcherProverFundingOrphanedReservationError,
    );
    expect(await fixture.records()).toEqual(before);
    expect(plans).toEqual([]);

    // Past release finality it settles, and nothing is handed over.
    fixture.chain.mockImplementation(async (txHash) => ({
      kind: "pending",
      txHash,
      retirement: { ...retirementFixture(txHash), reason: "included" },
    }));
    clock += 60_000;
    await admitFresh(fixture, factory);
    expect(await orphanOf(fixture)).toMatchObject({
      state: "released",
      activeInputs: [],
    });
    expect(plans).toHaveLength(1);
    expect(await fixture.store.hasSignedHistory!(plans[0]!)).toBe(false);
  });
});
