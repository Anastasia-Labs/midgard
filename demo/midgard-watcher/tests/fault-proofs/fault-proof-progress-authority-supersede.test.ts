import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import { createWorkflowInFlight } from "@al-ft/midgard-fault-proofs/workflow/in-flight";
import { afterEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.create-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { listWatcherProofObjectives } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.validate-job.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import {
  cleanupClassifiedFaultFixtures,
  deploymentIdentity,
  setupClassifiedFaultFixture,
} from "../support/classified-fault-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { inFlightOf } from "../support/in-flight-fixture.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

// A durable fault decision that a fresh classification of the same
// observation no longer reproduces is superseded once nothing of it is in
// flight: its rows and its decision row are forgotten, and the fresh fault
// is recorded. A job of it, or a transaction this process sent for it that
// can still land, holds it unchanged.
afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupClassifiedFaultFixtures();
});

const setup = async () => {
  const fixture = await setupClassifiedFaultFixture();
  const deploymentFingerprint = deploymentIdentity.manifestId;
  const inFlight = createWorkflowInFlight();
  const start = () =>
    createWatcherFaultProofProgressAuthority({
      journalRoot: fixture.journalRoot,
      deploymentFingerprint,
      categories: installedLaunchScope(fixture.old.launchScope),
      authenticationKey: TEST_JOURNAL_KEY,
      retention: storelessProofRetention,
      ...inFlightOf(inFlight),
    });
  const authority = start();
  const observation = progressObservation({
    deploymentFingerprint,
    header: {
      header: fixture.fixture.header,
      headerHash: fixture.old.headerHash,
    },
  });
  const objective = {
    category: "doubleSpend",
    headerHash: fixture.old.headerHash,
  } as const;
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
  const objectiveRows = () =>
    listWatcherProofObjectives(
      openWatcherJournalDatabase({
        journalRoot: fixture.journalRoot,
        authenticationKey: TEST_JOURNAL_KEY,
      }),
      ["doubleSpend"],
    ).length;
  const faultOf = (decision: typeof fixture.old) => ({
    decision,
    deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
    actuationPermit: createWorkflowActuationPermitController({
      decision,
      rollbackGeneration: "2",
    }).permit,
  });
  return {
    fixture,
    deploymentFingerprint,
    inFlight,
    authority,
    start,
    observation,
    objective,
    decisions,
    objectiveRows,
    faultOf,
  };
};

describe("proof progress authority supersede", () => {
  it("forgets the objective's rows and the decision, and records the fresh fault", async () => {
    const harness = await setup();
    const { fixture } = harness;
    await harness.authority.admit({
      observation: harness.observation,
      rollbackGeneration: "2",
      fault: harness.faultOf(fixture.old),
    });
    await expect(
      harness.authority.supersede({
        observation: harness.observation,
        prior: fixture.old,
        fresh: fixture.fresh,
      }),
    ).resolves.toEqual({ kind: "superseded" });
    expect(await harness.decisions()).toEqual([fixture.fresh.decisionDigest]);
    expect(harness.objectiveRows()).toBe(0);
    expect(harness.authority.unfinishedCount()).toBe(0);
    expect(harness.authority.cleanupFailures()).toEqual([]);
  });

  it("supersedes A to B and back to A, and a restart reads only A", async () => {
    const harness = await setup();
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

  it("holds a decision a transaction this process sent can still settle, and changes nothing", async () => {
    const harness = await setup();
    const { fixture } = harness;
    const sends = harness.inFlight.forObjective(harness.objective);
    sends.record({
      actionId: "init",
      txHash: "ab".repeat(32),
      signedTransactionCborHex: "84a0",
      inputs: [`${"cd".repeat(32)}#0`],
      walletInputs: [`${"cd".repeat(32)}#0`],
      walletOnly: true,
      broadcastAtMs: 0,
    });
    const before = await harness.decisions();
    const supersede = () =>
      harness.authority.supersede({
        observation: harness.observation,
        prior: fixture.old,
        fresh: fixture.fresh,
      });
    await expect(supersede()).resolves.toMatchObject({
      kind: "held",
      reason: "submission_unresolved",
    });
    expect(await harness.decisions()).toEqual(before);
    // Once the transaction settles, nothing of the decision is in flight.
    sends.settle("ab".repeat(32));
    await expect(supersede()).resolves.toEqual({ kind: "superseded" });
    expect(await harness.decisions()).toEqual([fixture.fresh.decisionDigest]);
  });

  it("holds while a job of the objective is queued or active, and supersedes once it finishes", async () => {
    const harness = await setup();
    const { fixture, deploymentFingerprint } = harness;
    const queue = await openWatcherFaultProofQueueJournal({
      journalRoot: fixture.journalRoot,
      deploymentFingerprint,
      authenticationKey: TEST_JOURNAL_KEY,
    });
    const identity = {
      ...harness.objective,
      decisionDigest: fixture.old.decisionDigest,
      rollbackGeneration: "2",
    };
    // The job is this process's: registered after the authority started.
    await harness.authority.admit({
      observation: harness.observation,
      rollbackGeneration: "2",
    });
    await queue.register(identity, "1");
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
    expect(await harness.decisions()).toContain(fixture.old.decisionDigest);
    await queue.markFinished(digest, "3");
    await expect(supersede()).resolves.toEqual({ kind: "superseded" });
    expect(await harness.decisions()).toEqual([fixture.fresh.decisionDigest]);
  });

  it("admits the fresh fault on the same objective once superseded", async () => {
    const harness = await setup();
    const { fixture, observation } = harness;
    await harness.authority.admit({
      observation,
      rollbackGeneration: "2",
      fault: harness.faultOf(fixture.old),
    });
    await expect(
      harness.authority.supersede({
        observation,
        prior: fixture.old,
        fresh: fixture.fresh,
      }),
    ).resolves.toEqual({ kind: "superseded" });
    // No run continues the old decision: the fresh decision's run starts
    // its own on the same objective.
    const contexts = await harness.authority.admit({
      observation,
      rollbackGeneration: "2",
      fault: harness.faultOf(fixture.fresh),
    });
    expect(contexts).toEqual([
      expect.objectContaining({ decision: fixture.fresh }),
    ]);
  });
});
