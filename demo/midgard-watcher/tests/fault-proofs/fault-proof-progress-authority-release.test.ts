import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import { afterEach, describe, expect, it } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { listWatcherProofObjectives } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.validate-job.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import { WATCHER_JOURNAL_DATABASE_FILE } from "../../src/fault-proofs/watcher-journal-database.types.js";
import type {
  WatcherProofRetention,
  WatcherProofRetentionTarget,
} from "../../src/l1-follower/proof-retention.js";
import {
  cleanupClassifiedFaultFixtures,
  deploymentIdentity,
  setupClassifiedFaultFixture,
} from "../support/classified-fault-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { inFlightOf } from "../support/in-flight-fixture.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

// An objective whose header left the finalized queue is released, held or
// not: its pin and its in-memory entry go at once, but the rows of a job
// still queued or running are left to that job, whose next queue
// transition needs its row. A later observation releases them once no job
// of it is left.
afterEach(cleanupClassifiedFaultFixtures);

const name = (target: WatcherProofRetentionTarget) =>
  `${target.category}/${target.headerHash}`;

/** An authority over one live fault, with a retention that records pins. */
const setup = async () => {
  const fixture = await setupClassifiedFaultFixture();
  const deploymentFingerprint = deploymentIdentity.manifestId;
  const pinned = new Set<string>();
  const retention: WatcherProofRetention = {
    ...storelessProofRetention,
    pin: async (target) => (pinned.add(name(target)), { kind: "pinned" }),
    release: async (target) => void pinned.delete(name(target)),
  };
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot: fixture.journalRoot,
    deploymentFingerprint,
    categories: installedLaunchScope(fixture.old.launchScope),
    authenticationKey: TEST_JOURNAL_KEY,
    retention,
    ...inFlightOf(),
  });
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
  /** A second connection to the store the objective rows live in. */
  const store = () =>
    new DatabaseSync(join(fixture.journalRoot, WATCHER_JOURNAL_DATABASE_FILE));
  /** An observation at `revision`; with the header queued it carries the
   * live fault. */
  const admit = (revision: number, headerQueued: boolean) => {
    const observation = progressObservation({
      deploymentFingerprint,
      revision,
      ...(headerQueued
        ? {
            header: {
              header: fixture.fixture.header,
              headerHash: fixture.old.headerHash,
            },
          }
        : {}),
    });
    return authority.admit({
      observation,
      rollbackGeneration: "2",
      ...(headerQueued
        ? {
            fault: {
              decision: fixture.old,
              deadline: watcherFaultProofDeadline(
                observation.finalizedHeaders[0]!,
              ),
              actuationPermit: createWorkflowActuationPermitController({
                decision: fixture.old,
                rollbackGeneration: "2",
              }).permit,
            },
          }
        : {}),
    });
  };
  const holdIt = () =>
    authority.holdObjective({
      kind: "objective",
      ...objective,
      decisionDigest: fixture.old.decisionDigest,
      detail: name(objective),
      readiness: "fault_proof_start_deadline_passed",
    });
  /** Registers a job of the objective in the queue journal, which opens
   * its row. */
  const queueJob = async () => {
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
    const digest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity,
    });
    return {
      start: () => queue.markStarted(digest, "2"),
      finish: () => queue.markFinished(digest, "3"),
    };
  };
  return {
    pinned,
    authority,
    objective,
    store,
    rows,
    admit,
    holdIt,
    queueJob,
  };
};

describe("proof progress authority release of a departed header", () => {
  it.each([
    ["an unheld objective", false],
    ["a held objective", true],
  ] as const)(
    "releases %s but leaves a queued job its rows",
    async (_name, heldFirst) => {
      const { pinned, authority, objective, rows, admit, holdIt, queueJob } =
        await setup();
      await admit(1, true);
      if (heldFirst) await holdIt();
      expect(authority.unfinishedCount()).toBe(1);
      expect([...pinned]).toEqual([name(objective)]);
      // A job of it is queued when its header leaves the queue.
      const job = await queueJob();
      await admit(2, false);
      expect(authority.unfinishedCount()).toBe(0);
      expect(authority.decisionHolds()).toEqual([]);
      expect([...pinned]).toEqual([]);
      expect(rows()).toEqual([name(objective)]);
      // The job still starts and finishes on its row.
      await job.start();
      await job.finish();
      // With no job left, the next observation releases its rows.
      await admit(3, false);
      expect(authority.unfinishedCount()).toBe(0);
      expect([...pinned]).toEqual([]);
      expect(rows()).toEqual([]);
      expect(authority.cleanupFailures()).toEqual([]);
    },
  );
});

describe("proof progress authority removal failure", () => {
  it("names a removal a storage error stopped, keeps its rows, and clears once the removal succeeds", async () => {
    const { authority, objective, rows, store, admit, queueJob } =
      await setup();
    await admit(1, true);
    const job = await queueJob();
    await job.start();
    await job.finish();
    expect(rows()).toEqual([name(objective)]);
    // Another connection holds the store's write lock past the busy timeout.
    const writer = store();
    writer.exec("BEGIN IMMEDIATE");
    try {
      await admit(2, false);
      expect(authority.cleanupFailures()).toMatchObject([
        { ...objective, detail: expect.stringContaining("locked") },
      ]);
      expect(rows()).toEqual([name(objective)]);
    } finally {
      writer.exec("ROLLBACK");
      writer.close();
    }
    await admit(3, false);
    expect(authority.cleanupFailures()).toEqual([]);
    expect(rows()).toEqual([]);
  }, 30_000);
});
