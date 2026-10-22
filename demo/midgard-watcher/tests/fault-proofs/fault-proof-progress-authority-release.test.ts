import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import { readWatcherProofExecution } from "../../src/fault-proofs/fault-proof-objective-journal.js";
import { listWatcherProofObjectives } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import { watcherObjectiveScope } from "../../src/fault-proofs/watcher-journal-schema.js";
import type {
  WatcherProofRetention,
  WatcherProofRetentionTarget,
} from "../../src/l1-follower/proof-retention.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

// An objective whose header left the finalized queue is released, held or
// not: its pin and its in-memory entry go at once, but the rows and the
// workflow rows of a job still queued or running are left to that job,
// whose next queue transition needs its row. The job's run adopts the
// objective again, and a later observation releases it with its rows and
// workflow rows. A signed attempt keeps its workflow rows for the funding
// sweep.
afterEach(cleanupFundingRecoveryFixtures);

const name = (target: WatcherProofRetentionTarget) =>
  `${target.category}/${target.headerHash}`;

/** An authority over a funding fixture, with a retention that records pins. */
const setup = async (withoutSubmissionIntent: boolean) => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    false,
    withoutSubmissionIntent,
  );
  const deploymentFingerprint = deploymentIdentity.manifestId;
  const pinned = new Set<string>();
  const retention: WatcherProofRetention = {
    ...storelessProofRetention,
    pin: async (target) => (pinned.add(name(target)), { kind: "pinned" }),
    release: async (target) => void pinned.delete(name(target)),
  };
  // A restart is a fresh authority over the same journals.
  const start = () =>
    createWatcherFaultProofProgressAuthority({
      journalRoot: fixture.journalRoot,
      workflowJournal: fixture.workflowJournal,
      deploymentFingerprint,
      categories: installedLaunchScope(fixture.old.launchScope),
      authenticationKey: TEST_JOURNAL_KEY,
      retention,
    });
  let authority = start();
  const objective = {
    category: "doubleSpend",
    headerHash: fixture.old.headerHash,
  } as const;
  const rows = () =>
    listWatcherProofObjectives(
      openWatcherJournalDatabase({
        journalRoot: fixture.journalRoot,
        authenticationKey: TEST_JOURNAL_KEY,
      }),
      ["doubleSpend"],
    ).map(({ objective }) => name(objective));
  const database = () =>
    openWatcherJournalDatabase({
      journalRoot: fixture.journalRoot,
      authenticationKey: TEST_JOURNAL_KEY,
    });
  /** A second connection to the watcher store the workflow rows live in. */
  const store = () =>
    new DatabaseSync(join(fixture.journalRoot, "watcher.sqlite"));
  /** How many workflow rows the objective has, read through `store`. */
  const workflowRows = (): number => {
    const raw = store();
    try {
      return Number(
        (
          raw
            .prepare(
              "SELECT count(*) AS count FROM watcher_workflow_journal_v1 WHERE header_hash = ?",
            )
            .get(objective.headerHash) as { count: number }
        ).count,
      );
    } finally {
      raw.close();
    }
  };
  const admit = (revision: number, headerQueued: boolean) =>
    authority.admit({
      observation: progressObservation({
        deploymentFingerprint,
        revision,
        ...(headerQueued ? { header: fixture.fixture } : {}),
      }),
      rollbackGeneration: "2",
    });
  const holdIt = () =>
    authority.holdObjective({
      kind: "objective",
      ...objective,
      decisionDigest: fixture.old.decisionDigest,
      detail: name(objective),
    });
  return {
    fixture,
    deploymentFingerprint,
    pinned,
    get authority() {
      return authority;
    },
    restart: () => void (authority = start()),
    objective,
    database,
    store,
    workflowRows,
    rows,
    admit,
    holdIt,
  };
};

describe("proof progress authority release of a departed header", () => {
  it.each([
    ["an unheld objective with no signed attempt", false],
    ["a held objective", true],
  ] as const)(
    "releases %s but leaves a queued job its rows and workflow rows",
    async (_name, heldFirst) => {
      const {
        fixture,
        deploymentFingerprint,
        pinned,
        authority,
        objective,
        rows,
        workflowRows,
        admit,
        holdIt,
      } = await setup(true);
      // Adopted from its row while its header is queued.
      await admit(1, true);
      if (heldFirst) await holdIt();
      expect(authority.unfinishedCount()).toBe(1);
      expect([...pinned]).toEqual([name(objective)]);
      // A job of it is queued when its header leaves the queue.
      const queue = await openWatcherFaultProofQueueJournal({
        journalRoot: fixture.journalRoot,
        deploymentFingerprint,
        authenticationKey: TEST_JOURNAL_KEY,
      });
      const identity = {
        ...objective,
        decisionDigest: fixture.old.decisionDigest,
        rollbackGeneration: "2",
      };
      await queue.register(identity, "1");
      await admit(2, false);
      expect(authority.unfinishedCount()).toBe(0);
      expect(authority.decisionHolds()).toEqual([]);
      expect([...pinned]).toEqual([]);
      expect(rows()).toEqual([name(objective)]);
      expect(workflowRows()).toBeGreaterThan(0);
      // The job still starts and finishes on its row.
      const digest = watcherFaultProofQueueIdentityDigest({
        deploymentFingerprint,
        identity,
      });
      await queue.markStarted(digest, "2");
      const execution = readWatcherProofExecution({
        journal: fixture.workflowJournal,
        deploymentFingerprint,
        objective,
      });
      await authority.updateExecution({ objective, execution: execution! });
      await queue.markFinished(digest, "3");
      expect(authority.unfinishedCount()).toBe(1);
      expect([...pinned]).toEqual([name(objective)]);
      // With no job left, the next observation releases it with its rows
      // and its workflow rows.
      await admit(3, false);
      expect(authority.unfinishedCount()).toBe(0);
      expect([...pinned]).toEqual([]);
      expect(rows()).toEqual([]);
      expect(workflowRows()).toBe(0);
      expect(authority.cleanupFailures()).toEqual([]);
    },
  );

  it("releases a held objective with a signed attempt but keeps its workflow rows for the funding sweep", async () => {
    const { fixture, pinned, authority, objective, rows, admit, holdIt } =
      await setup(false);
    await admit(1, true);
    await holdIt();
    await admit(2, false);
    expect(authority.unfinishedCount()).toBe(0);
    expect([...pinned]).toEqual([]);
    expect(rows()).toEqual([]);
    expect(
      readWatcherProofExecution({
        journal: fixture.workflowJournal,
        deploymentFingerprint: deploymentIdentity.manifestId,
        objective,
      })?.entries.some(({ event }) => event.kind === "submission_intent"),
    ).toBe(true);
    expect(authority.cleanupFailures()).toEqual([]);
  });
});

describe("proof progress authority removal failure", () => {
  it("names a removal a storage error stopped, keeps its rows, and clears once the removal succeeds", async () => {
    const { authority, objective, rows, workflowRows, store, admit } =
      await setup(true);
    await admit(1, true);
    // Another connection holds the store's write lock past the busy timeout.
    const writer = store();
    writer.exec("BEGIN IMMEDIATE");
    try {
      await admit(2, false);
      expect(authority.cleanupFailures()).toMatchObject([
        { ...objective, detail: expect.stringContaining("locked") },
      ]);
      expect(rows()).toEqual([name(objective)]);
      expect(workflowRows()).toBeGreaterThan(0);
    } finally {
      writer.exec("ROLLBACK");
      writer.close();
    }
    await admit(3, false);
    expect(authority.cleanupFailures()).toEqual([]);
    expect(rows()).toEqual([]);
    expect(workflowRows()).toBe(0);
  }, 30_000);
});

// A restart over workflow rows an earlier process left unreadable starts,
// names what it holds, and settles it once the header leaves the finalized
// queue: no exit, no operator action.
describe("proof progress authority restart over unreadable workflow rows", () => {
  /** Deletes the first workflow row of the objective's execution, then
   * reopens the store and restarts the authority over it. */
  const removeFirstRow = async (harness: Awaited<ReturnType<typeof setup>>) => {
    expect(harness.workflowRows()).toBeGreaterThan(1);
    const raw = harness.store();
    try {
      raw
        .prepare(
          "DELETE FROM watcher_workflow_journal_v1 WHERE header_hash = ? AND sequence = 0",
        )
        .run(harness.objective.headerHash);
    } finally {
      raw.close();
    }
    await harness.fixture.restartStore();
    harness.restart();
  };

  it("holds an open objective over a partly removed execution by name, then forgets its rows once its header leaves", async () => {
    const harness = await setup(true);
    const { objective, rows, workflowRows, pinned, admit } = harness;
    await admit(1, true);
    await removeFirstRow(harness);
    // Startup does not fail: the objective is held by name and pinned.
    await expect(admit(2, true)).resolves.toEqual([]);
    const [hold] = harness.authority.decisionHolds();
    expect(hold).toMatchObject({
      kind: "objective",
      ...objective,
      decisionDigest: null,
      readiness: "fault_proof_objective_unreadable",
    });
    expect(hold!.detail).toContain(`${name(objective)}: `);
    expect(harness.authority.unfinishedCount()).toBe(1);
    expect([...pinned]).toEqual([name(objective)]);
    expect(rows()).toEqual([name(objective)]);
    // Once its header leaves, the hold clears and its rows go; what the
    // workflow rows hold is neither trusted nor deleted.
    await admit(3, false);
    expect(harness.authority.decisionHolds()).toEqual([]);
    expect(harness.authority.unfinishedCount()).toBe(0);
    expect([...pinned]).toEqual([]);
    expect(rows()).toEqual([]);
    expect(harness.authority.cleanupFailures()).toEqual([]);
    expect(workflowRows()).toBeGreaterThan(0);
    // A further restart starts clean.
    harness.restart();
    await admit(4, false);
    expect(harness.authority.decisionHolds()).toEqual([]);
  });

  it("holds a completion marked final over a partly removed execution by name, then deletes its workflow rows once its header leaves", async () => {
    const harness = await setup(true);
    const { objective, database, rows, workflowRows, pinned, admit } = harness;
    // The row an earlier process marked final before the rows broke.
    const k = storelessProofRetention.securityParameter;
    const scope = watcherObjectiveScope(
      objective.category,
      objective.headerHash,
    );
    database().transaction((tx) =>
      tx.put("fault_proof_objectives", {
        key: scope,
        scope,
        state: "marked",
        body: {
          ...objective,
          marker: {
            workflowId: "cd".repeat(32),
            journalDigest: "ef".repeat(32),
            confirmationDepth: k + 1,
            recoveryDepth: k.toString(),
          },
        },
      }),
    );
    await removeFirstRow(harness);
    await expect(admit(1, true)).resolves.toEqual([]);
    expect(harness.authority.decisionHolds()).toMatchObject([
      { ...objective, readiness: "fault_proof_objective_unreadable" },
    ]);
    expect([...pinned]).toEqual([name(objective)]);
    await admit(2, false);
    expect(harness.authority.decisionHolds()).toEqual([]);
    expect([...pinned]).toEqual([]);
    expect(rows()).toEqual([]);
    expect(harness.authority.cleanupFailures()).toEqual([]);
    expect(workflowRows()).toBe(0);
  });
});
