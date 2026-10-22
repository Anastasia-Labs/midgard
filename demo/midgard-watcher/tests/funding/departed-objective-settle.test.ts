import { supersededAttemptReadSchedule } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import { openWatcherProofObjective } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import { WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
import { orphanReadingFactory } from "./orphaned-reservation-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** An objective held by name (a deadline or a missing decision) whose header
 * then leaves the finalized queue: nothing drives it again, and its workflow
 * journal, which holds the signed attempt's intent, stays. Without `swept`
 * the process stops before the departure's funding sweep. */
const departed = async (swept: boolean) => {
  const fixture = await setupFundingRecoveryFixture();
  supersededAttemptReadSchedule.forget(fixture.transactionHash);
  let clock = 1_000_000;
  const { factory } = orphanReadingFactory(fixture, {
    observeSignedTransaction: fixture.observeSignedTransaction,
    now: () => clock,
  });
  const sweeps: Promise<void>[] = [];
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot: fixture.journalRoot,
    workflowJournal: fixture.workflowJournal,
    deploymentFingerprint: deploymentIdentity.manifestId,
    categories: installedLaunchScope(fixture.old.launchScope),
    authenticationKey: TEST_JOURNAL_KEY,
    onObjectiveUnattended: () => {
      if (swept) sweeps.push(factory.settleUnattended());
    },
  });
  const admit = (revision: number, queued: boolean) =>
    authority.admit({
      observation: progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
        revision,
        ...(queued ? { header: fixture.fixture } : {}),
      }),
      rollbackGeneration: "2",
    });
  await admit(1, true);
  await authority.holdObjective({
    kind: "objective",
    category: "doubleSpend",
    headerHash: fixture.old.headerHash,
    decisionDigest: fixture.old.decisionDigest,
    detail: "held",
  });
  // Not yet retired on L1.
  fixture.chain.mockResolvedValue({ kind: "not_found" });
  await admit(2, false);
  await Promise.all(sweeps);
  expect(authority.unfinishedCount()).toBe(0);
  expect(
    fixture.workflowJournal
      .readExecution(fixture.objective)
      ?.entries.some(({ event }) => event.kind === "submission_intent"),
  ).toBe(true);
  const funding = () =>
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
    sweeps,
    funding,
    admit,
    retire: () => {
      clock += 60_000;
      fixture.chain.mockImplementation(async (txHash) =>
        expiredNotFound(txHash!),
      );
    },
  };
};

it("holds the funding of an objective whose header left the queue with a signed attempt, then settles it once that attempt is retired on L1", async () => {
  const { fixture, factory, sweeps, funding, retire } = await departed(true);
  // The departure swept it: held with an alert, its coins untouched.
  expect(sweeps).toHaveLength(1);
  expect(factory.decisionHolds()).toEqual([
    expect.objectContaining({
      reservationId: fixture.plan.reservationId,
      readiness: WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
    }),
  ]);
  expect(await funding()).toMatchObject({ state: "active" });
  retire();
  // As on each follower change while a reservation is held.
  await factory.recheckDecisionHolds();
  expect(await funding()).toMatchObject({
    state: "released",
    activeInputs: [],
  });
  expect(await fixture.store.readReservedOutRefs({})).toEqual([]);
  expect(factory.decisionHolds()).toEqual([]);
  // The forgotten objective's workflow rows went with the settle.
  expect(fixture.workflowJournal.readExecution(fixture.objective)).toBe(
    undefined,
  );
});

it("settles that funding at the next start once the attempt is retired, when the process stopped before the departure's sweep", async () => {
  const { fixture, funding, retire } = await departed(false);
  retire();
  const { factory } = orphanReadingFactory(fixture);
  await factory.releaseUnused();
  expect(await funding()).toMatchObject({
    state: "released",
    activeInputs: [],
  });
  expect(await fixture.store.readReservedOutRefs({})).toEqual([]);
  expect(fixture.workflowJournal.readExecution(fixture.objective)).toBe(
    undefined,
  );
});

it("leaves the funding and the workflow rows to the objective's run when its header returned before the sweep", async () => {
  const { fixture, funding, admit, retire } = await departed(false);
  // The header is queued again and its job registered, which records the
  // objective's row before any sweep runs.
  await admit(3, true);
  openWatcherJournalDatabase({
    journalRoot: fixture.journalRoot,
    authenticationKey: TEST_JOURNAL_KEY,
  }).transaction((tx) => openWatcherProofObjective(tx, fixture.objective));
  retire();
  const { factory } = orphanReadingFactory(fixture);
  await factory.releaseUnused();
  expect(await funding()).toMatchObject({ state: "active" });
  expect(
    fixture.workflowJournal
      .readExecution(fixture.objective)
      ?.entries.some(({ event }) => event.kind === "submission_intent"),
  ).toBe(true);
});
