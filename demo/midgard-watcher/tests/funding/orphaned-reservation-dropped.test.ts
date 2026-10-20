import {
  beginWorkflowFundingReservationAction,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { signedReadObservation } from "../support/fault-proof-funding-fixture.signed-read.js";
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
import {
  chainHolds,
  intentsOf,
  runMinutesApart as runFresh,
  walletAfter,
} from "./superseded-attempt-rollback.chain.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** How the follower reads an attempt without expiry that left the mempool:
 * with every input unspent, or spending an input whose history it lacks: a
 * wallet coin from before its origin, or an output it does not follow. */
const READS = {
  "inputs unspent": {
    status: "rebroadcast",
    reason:
      "Canonical transaction absent and every recorded input remains unspent",
  },
  "an input from before the follower's origin": {
    status: "unknown",
    reason: "A recorded input lacks exact canonical creation history",
  },
  "an input outside the followed outputs": {
    status: "unknown",
    reason:
      "A recorded input is outside the followed outputs and has no recorded spend",
  },
} as const;
type Read = keyof typeof READS;

/** The journals are lost while the original attempt, which has no expiry,
 * is in flight; the mempool then drops it. Reads of an attempt in `dropped`
 * answer as `read` says; every other read answers from the fixture chain. */
const droppedWithoutExpiry = async (read: Read) => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  await loseJournals(fixture, [fixture.fresh]);
  supersededAttemptReadSchedule.forget(fixture.transactionHash);
  fixture.useUnspentPendingInputs();
  fundWallet(fixture);
  fixture.chain.mockResolvedValue({ kind: "not_found" });
  const dropped = new Set([fixture.transactionHash]);
  fixture.observeSignedTransaction.mockImplementation(async (input) =>
    dropped.has(input.transactionHash)
      ? {
          ...signedReadObservation(input, { kind: "unknown", reason: "" }),
          ...READS[read],
        }
      : signedReadObservation(
          input,
          await fixture.chain(input.transactionHash),
        ),
  );
  const alerts: { subjectDigest: string; active: boolean }[] = [];
  let clock = 1_000_000;
  const { factory, plans } = orphanReadingFactory(fixture, {
    observeSignedTransaction: fixture.observeSignedTransaction,
    now: () => clock,
    alertSink: () => ({
      setAlert: ({ subjectDigest, active }) => {
        alerts.push({ subjectDigest, active });
      },
    }),
  });
  return {
    fixture,
    factory,
    plans,
    alerts,
    dropped,
    later: () => {
      clock += 60_000;
    },
  };
};

/** The fresh execution after the hand-over, with its init action begun,
 * and the replacement that spends the moved funding input. */
const handedOver = async (
  setup: Awaited<ReturnType<typeof droppedWithoutExpiry>>,
) => {
  const { fixture, factory, plans } = setup;
  const journal = await admitFresh(fixture, factory);
  const plan = plans[0]!;
  await fixture.run(journal);
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
  const funding = fundingOf(fixture);
  return {
    journal,
    plan,
    current,
    collateral,
    replacement: signReplacement(
      funding.outRef,
      BigInt(funding.lovelace),
      300n,
      collateral,
    ),
  };
};

describe.each(Object.keys(READS) as Read[])(
  "an orphaned attempt without expiry that left the mempool (%s)",
  (read) => {
    it("is handed over: a replacement must spend its inputs, the fresh execution runs once, and the hold and alert clear", async () => {
      const setup = await droppedWithoutExpiry(read);
      const { fixture, factory, plans, alerts } = setup;
      const funding = fundingOf(fixture);

      // Before any execution takes its target, the sweep holds it.
      await factory.releaseUnused();
      expect(factory.decisionHolds()).toEqual([
        expect.objectContaining({
          reservationId: fixture.plan.reservationId,
          readiness: WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
        }),
      ]);
      expect(alerts).toEqual([
        { subjectDigest: fixture.plan.reservationId, active: true },
      ]);

      const { journal, plan, current, collateral, replacement } =
        await handedOver(setup);
      expect(new Set(plans.map(({ reservationId }) => reservationId))).toEqual(
        new Set([plan.reservationId]),
      );
      const records = await fixture.records();
      expect(
        records.find(
          ({ reservationId }) => reservationId === fixture.plan.reservationId,
        ),
      ).toMatchObject({
        state: "released",
        activeInputs: [],
        pendingTransition: null,
      });
      expect(current.activeInputs).toContainEqual(
        expect.objectContaining({ outRef: funding.outRef, role: "funding" }),
      );
      expect(await doublyLeased(fixture)).toEqual([]);

      setup.later();
      await factory.releaseUnused();
      expect(factory.decisionHolds()).toEqual([]);
      expect(alerts).toEqual([
        { subjectDigest: fixture.plan.reservationId, active: true },
        { subjectDigest: fixture.plan.reservationId, active: false },
      ]);

      // A replacement that leaves the moved inputs alone is refused.
      const other = current.activeInputs.find(
        ({ role, outRef }) => role === "funding" && outRef !== funding.outRef,
      )!;
      await expect(
        recordFreshReplacement(
          fixture,
          journal,
          plan,
          signReplacement(
            other.outRef,
            BigInt(other.lovelace),
            300n,
            collateral,
          ),
          2,
        ),
      ).rejects.toThrow("must share an input with each superseded attempt");

      await recordFreshReplacement(fixture, journal, plan, replacement, 2);
      // The process restarts with the replacement in flight, and it lands.
      walletAfter(fixture, replacement.transactionHash);
      chainHolds(fixture, [replacement.transactionHash]);
      await fixture.run(await admitFresh(fixture, factory));
      const events = (
        await journal.load(freshWorkflow(fixture).workflowId)
      ).map(({ event }) => event);
      expect(events.filter(({ kind }) => kind === "confirmed")).toEqual([
        {
          kind: "confirmed",
          actionId: "init",
          txHash: replacement.transactionHash,
        },
      ]);
      expect(intentsOf(events, replacement.transactionHash)).toBe(1);
      expect(fixture.adapter.submit).not.toHaveBeenCalled();
      expect(await doublyLeased(fixture)).toEqual([]);
    });

    it("is adopted when it lands late, and the fresh replacement never lands", async () => {
      const setup = await droppedWithoutExpiry(read);
      const { fixture, dropped } = setup;
      const { journal, plan, replacement } = await handedOver(setup);
      await recordFreshReplacement(fixture, journal, plan, replacement, 2);
      for (const hash of [fixture.transactionHash, replacement.transactionHash])
        supersededAttemptReadSchedule.forget(hash);

      // The old attempt is submitted again elsewhere and lands.
      dropped.clear();
      walletAfter(fixture, fixture.transactionHash);
      chainHolds(fixture, [fixture.transactionHash]);
      const restarted = await admitFresh(fixture, setup.factory);
      await runFresh(fixture, restarted, 4);
      const events = (
        await journal.load(freshWorkflow(fixture).workflowId)
      ).map(({ event }) => event);
      expect(events.filter(({ kind }) => kind === "confirmed")).toEqual([
        {
          kind: "confirmed",
          actionId: "init",
          txHash: fixture.transactionHash,
        },
      ]);
      expect(intentsOf(events, replacement.transactionHash)).toBe(1);
      expect(fixture.adapter.preflight).not.toHaveBeenCalled();
      expect(fixture.adapter.submit).not.toHaveBeenCalled();
      const fresh = (await fixture.records()).find(
        ({ reservationId }) => reservationId === plan.reservationId,
      )!;
      expect(fresh.pendingTransition).toBeNull();
      expect(fresh.lastConfirmedTransitionDigest).not.toBeNull();
      expect(
        new Set(setup.plans.map(({ reservationId }) => reservationId)),
      ).toEqual(new Set([plan.reservationId]));
    });
  },
);
