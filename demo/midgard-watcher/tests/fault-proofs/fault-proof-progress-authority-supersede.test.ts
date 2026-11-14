import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  createWorkflowActuationPermitController,
  type FraudProofWorkflowJournalEvent,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import { listWatcherProofObjectives } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import { WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import { orphanReadingFactory } from "../funding/orphaned-reservation-fixture.js";
import { expiredNotFound } from "../funding/prover-funding-recovery.retirement-fixture.js";
import { retirementFixture } from "../funding/sqlite-prover-funding-reservation-store.signed-transition.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

// A durable fault decision that a fresh classification of the same
// observation no longer reproduces is superseded once nothing of it is in
// flight: its unused funding is released, its workflow rows and its decision
// row are forgotten, and the fresh fault is recorded. A job of it, or a
// signed attempt neither confirmed nor failed, holds it unchanged.
afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const setup = async (unused: boolean) => {
  const fixture = await setupFundingRecoveryFixture(unused);
  const deploymentFingerprint = deploymentIdentity.manifestId;
  const fundingFactory = fixture.fundingFactory();
  const start = () =>
    createWatcherFaultProofProgressAuthority({
      journalRoot: fixture.journalRoot,
      workflowJournal: fixture.workflowJournal,
      deploymentFingerprint,
      categories: installedLaunchScope(fixture.old.launchScope),
      authenticationKey: TEST_JOURNAL_KEY,
      retention: storelessProofRetention,
      releaseDecisionFunding: async (decision) =>
        await fundingFactory.releaseUnusedDecision(decision.decisionDigest),
    });
  const authority = start();
  const observation = progressObservation({
    deploymentFingerprint,
    header: fixture.fixture,
  });
  const decisions = async () =>
    (
      await (
        await openWatcherFaultDecisionJournal({
          directory: fixture.journalRoot,
          deploymentFingerprint,
          launchScope: installedLaunchScope(fixture.old.launchScope),
          authenticationKey: TEST_JOURNAL_KEY,
        })
      ).readAll()
    ).map(({ decision }) => decision.decisionDigest);
  const workflowRows = (): number => {
    const raw = new DatabaseSync(join(fixture.journalRoot, "watcher.sqlite"));
    try {
      return Number(
        (
          raw
            .prepare(
              "SELECT count(*) AS count FROM watcher_workflow_journal_v1 WHERE header_hash = ?",
            )
            .get(fixture.old.headerHash) as { count: number }
        ).count,
      );
    } finally {
      raw.close();
    }
  };
  const objectiveRows = () =>
    listWatcherProofObjectives(
      openWatcherJournalDatabase({
        journalRoot: fixture.journalRoot,
        authenticationKey: TEST_JOURNAL_KEY,
      }),
      ["doubleSpend"],
    ).length;
  // A released unused reservation keeps its row with no inputs held.
  const reservedInputs = async (decisionDigest: string) =>
    (await fixture.records())
      .filter((record) => record.decisionDigest === decisionDigest)
      .map(({ activeInputs }) => activeInputs.length);
  return {
    fixture,
    deploymentFingerprint,
    authority,
    start,
    observation,
    decisions,
    workflowRows,
    objectiveRows,
    reservedInputs,
  };
};

describe("proof progress authority supersede", () => {
  it("releases the unused funding before forgetting the workflow rows and the decision, and records the fresh fault", async () => {
    const harness = await setup(true);
    const { fixture } = harness;
    const reserved = await harness.reservedInputs(fixture.old.decisionDigest);
    expect(reserved).toHaveLength(1);
    expect(reserved[0]).toBeGreaterThan(0);
    expect(harness.workflowRows()).toBeGreaterThan(0);

    await expect(
      harness.authority.supersede({
        observation: harness.observation,
        prior: fixture.old,
        fresh: fixture.fresh,
      }),
    ).resolves.toEqual({ kind: "superseded" });

    // The funding release reads the decision row: had the row gone first,
    // the reservation would still hold its inputs.
    expect(await harness.reservedInputs(fixture.old.decisionDigest)).toEqual([
      0,
    ]);
    expect(await harness.decisions()).toEqual([fixture.fresh.decisionDigest]);
    expect(harness.workflowRows()).toBe(0);
    expect(harness.objectiveRows()).toBe(0);
    expect(harness.authority.unfinishedCount()).toBe(0);
    expect(harness.authority.cleanupFailures()).toEqual([]);
  });

  it("supersedes A to B and back to A, and a restart reads only A", async () => {
    const harness = await setup(true);
    const { fixture } = harness;
    await expect(
      harness.authority.supersede({
        observation: harness.observation,
        prior: fixture.old,
        fresh: fixture.fresh,
      }),
    ).resolves.toEqual({ kind: "superseded" });
    await expect(
      harness.authority.supersede({
        observation: harness.observation,
        prior: fixture.fresh,
        fresh: fixture.old,
      }),
    ).resolves.toEqual({ kind: "superseded" });
    expect(await harness.decisions()).toEqual([fixture.old.decisionDigest]);
    // A restarted authority over the same journals admits only A, and
    // supersedes it again to B.
    const restarted = harness.start();
    await restarted.admit({
      observation: harness.observation,
      rollbackGeneration: "2",
    });
    await expect(
      restarted.supersede({
        observation: harness.observation,
        prior: fixture.old,
        fresh: fixture.fresh,
      }),
    ).resolves.toEqual({ kind: "superseded" });
    expect(await harness.decisions()).toEqual([fixture.fresh.decisionDigest]);
  });

  it("holds a decision whose signed attempt is still in flight and changes nothing", async () => {
    const harness = await setup(false);
    const { fixture } = harness;
    const before = await harness.decisions();
    const rows = harness.workflowRows();
    const reserved = await harness.reservedInputs(fixture.old.decisionDigest);
    await expect(
      harness.authority.supersede({
        observation: harness.observation,
        prior: fixture.old,
        fresh: fixture.fresh,
      }),
    ).resolves.toMatchObject({ kind: "held", reason: "submission_unresolved" });
    expect(await harness.decisions()).toEqual(before);
    expect(harness.workflowRows()).toBe(rows);
    expect(await harness.reservedInputs(fixture.old.decisionDigest)).toEqual(
      reserved,
    );
  });

  it("holds while a job of the objective is queued or active, and supersedes once it finishes", async () => {
    const harness = await setup(true);
    const { fixture, deploymentFingerprint } = harness;
    const queue = await openWatcherFaultProofQueueJournal({
      journalRoot: fixture.journalRoot,
      deploymentFingerprint,
      authenticationKey: TEST_JOURNAL_KEY,
    });
    const identity = {
      ...fixture.objective,
      decisionDigest: fixture.old.decisionDigest,
      rollbackGeneration: "2",
    };
    // The job is this process's: registered after the authority started.
    await harness.authority.admit({
      observation: harness.observation,
      rollbackGeneration: "2",
    });
    await queue.register(identity, "1");
    const reserved = await harness.reservedInputs(fixture.old.decisionDigest);
    const supersede = () =>
      harness.authority.supersede({
        observation: harness.observation,
        prior: fixture.old,
        fresh: fixture.fresh,
      });
    await expect(supersede()).resolves.toMatchObject({
      kind: "held",
      reason: "job_pending",
    });
    const digest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity,
    });
    await queue.markStarted(digest, "2");
    await expect(supersede()).resolves.toMatchObject({
      kind: "held",
      reason: "job_pending",
    });
    expect(await harness.reservedInputs(fixture.old.decisionDigest)).toEqual(
      reserved,
    );
    expect(await harness.decisions()).toContain(fixture.old.decisionDigest);
    await queue.markFinished(digest, "3");
    await expect(supersede()).resolves.toEqual({ kind: "superseded" });
    expect(await harness.decisions()).toEqual([fixture.fresh.decisionDigest]);
    expect(await harness.reservedInputs(fixture.old.decisionDigest)).toEqual([
      0,
    ]);
  });
  // The init attempt as it resolves: reconciled not found (it failed), or
  // confirmed; and how L1 later reads it past release finality.
  const resolutions = [
    {
      outcome: "failed",
      events: (txHash: string): FraudProofWorkflowJournalEvent[] => [
        { kind: "reconciled", actionId: "init", outcome: "not_found", txHash },
      ],
      retired: async (txHash?: string) => expiredNotFound(txHash!),
    },
    {
      outcome: "confirmed",
      events: (txHash: string): FraudProofWorkflowJournalEvent[] => [
        { kind: "reconciled", actionId: "init", outcome: "confirmed", txHash },
        { kind: "confirmed", actionId: "init", txHash },
      ],
      retired: async (txHash?: string) => ({
        kind: "pending" as const,
        txHash: txHash!,
        retirement: {
          ...retirementFixture(txHash!),
          reason: "included" as const,
        },
      }),
    },
  ] as const;

  it.each(resolutions)(
    "supersedes a decision whose signed attempt $outcome, admits the fresh fault on the same objective, and settles the attempt's funding once it retires",
    async ({ events, retired }) => {
      const harness = await setup(false);
      const { fixture, deploymentFingerprint, observation } = harness;
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
        deploymentFingerprint,
        categories: installedLaunchScope(fixture.old.launchScope),
        authenticationKey: TEST_JOURNAL_KEY,
        retention: storelessProofRetention,
        releaseDecisionFunding: async (decision) =>
          await factory.releaseUnusedDecision(decision.decisionDigest),
        onObjectiveUnattended: () =>
          void sweeps.push(factory.settleUnattended()),
      });
      const supersede = () =>
        authority.supersede({
          observation,
          prior: fixture.old,
          fresh: fixture.fresh,
        });
      await expect(supersede()).resolves.toMatchObject({
        kind: "held",
        reason: "submission_unresolved",
      });
      for (const event of events(fixture.transactionHash))
        await fixture.append(event);
      // Resolved, but not yet retired past release finality on L1.
      fixture.chain.mockResolvedValue({ kind: "not_found" });

      await expect(supersede()).resolves.toEqual({ kind: "superseded" });
      await Promise.all(sweeps);
      expect(await harness.decisions()).toEqual([fixture.fresh.decisionDigest]);
      // No run continues the old execution: its rows go, so the fresh
      // decision's run starts its own on the same objective.
      expect(harness.workflowRows()).toBe(0);
      expect(harness.objectiveRows()).toBe(0);
      const contexts = await authority.admit({
        observation,
        rollbackGeneration: "2",
        fault: {
          decision: fixture.fresh,
          deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
          actuationPermit: createWorkflowActuationPermitController({
            decision: fixture.fresh,
            rollbackGeneration: "2",
          }).permit,
        },
      });
      expect(contexts).toEqual([
        expect.objectContaining({ decision: fixture.fresh }),
      ]);

      // The attempt's funding waits for the attempt to retire, then returns.
      const funding = async () =>
        (await fixture.records()).find(
          ({ reservationId }) => reservationId === fixture.plan.reservationId,
        );
      expect(sweeps).toHaveLength(1);
      expect(factory.decisionHolds()).toEqual([
        expect.objectContaining({
          reservationId: fixture.plan.reservationId,
          readiness: WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
        }),
      ]);
      expect(await funding()).toMatchObject({ state: "active" });
      clock += 60_000;
      fixture.chain.mockImplementation(retired);
      await factory.recheckDecisionHolds();
      expect(await funding()).toBeUndefined();
      expect(await fixture.store.readReservedOutRefs({})).toEqual([]);
      expect(factory.decisionHolds()).toEqual([]);
    },
  );
});
