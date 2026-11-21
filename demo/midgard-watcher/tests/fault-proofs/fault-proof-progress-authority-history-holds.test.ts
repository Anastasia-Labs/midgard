/**
 * Startup and the proof roster over a real follower store: a restart
 * forgets every objective row but releases no hold for it, a re-created
 * authority releases nothing either and the roster's floor stays lifted,
 * history older than the retained depth stays held across a restart, and a
 * completion short of depth keeps its history for a rollback that returns
 * its header.
 */
import type { WorkflowActuationPermit } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import { afterEach, describe, expect, it, vi } from "vitest";

import { listWatcherProofObjectives } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.validate-job.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import { createWatcherFaultProofL1Source } from "../../src/l1-follower/fault-proof-l1-source.js";
import { watcherUnitHistoryPolicies } from "../../src/l1-follower/projection.js";
import {
  createWatcherProofRetention,
  type WatcherProofRetention,
} from "../../src/l1-follower/proof-retention.js";
import {
  createWatcherProofRosterRetention,
  WATCHER_PROOF_ROSTER_FLOOR,
} from "../../src/l1-follower/proof-roster-retention.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { D, K } from "../support/l1-follower-raw-reads-fixture.js";
import {
  pendingOver,
  RELEASE,
  requestFor,
  SOURCE_ID,
} from "../support/l1-follower-raw-source-fixture.js";
import {
  closeRemovedHeaders,
  departedRows,
  historyRows,
  nodeUnit,
  type Removed,
  removedHeader,
} from "../support/proof-retention-removed-header.js";
import { advance, floorLagsOf, reportAtTip } from "../support/proof-roster.js";
import {
  journalDirectory,
  recordObjectives,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

// Decisions as minimal records; the follower store, its pins and its
// pruning are real. The one fault this file admits is that minimal record
// under a plain submission permit: its authentication is not under test.
const journalState = vi.hoisted(() => ({ headers: [] as string[] }));
const digestOf = (headerHash: string): string => headerHash.padEnd(64, "d");
vi.mock("@al-ft/midgard-fault-proofs", async (importOriginal) => ({
  ...(await importOriginal()),
  requireRunnableHeaderFault: (decision: object) => ({
    ...decision,
    launchScope: ["doubleSpend"],
  }),
  assertWorkflowActuationPermitIdentity: ({ permit }: { permit: object }) =>
    permit,
}));
vi.mock("../../src/fault-proofs/fault-decision-journal.js", () => ({
  openWatcherFaultDecisionJournal: async () => ({
    readAll: async () =>
      journalState.headers.map((headerHash) => ({
        decision: decisionOf(headerHash),
      })),
  }),
}));
const deploymentFingerprint = "ab".repeat(32);
const decisionOf = (headerHash: string) => ({
  decision: "fault_detected",
  category: "doubleSpend",
  headerHash,
  decisionDigest: digestOf(headerHash),
});

afterEach(async () => {
  journalState.headers = [];
  await closeRemovedHeaders();
  await removeJournalDirectories();
});

/** A journal naming one open objective over the fixture's header. */
const journalOver = async (r: Removed) => {
  const journalRoot = await journalDirectory("watcher-history-holds");
  journalState.headers = [r.header];
  recordObjectives(journalRoot, [
    { category: "doubleSpend", headerHash: r.header },
  ]);
  return journalRoot;
};

const authorityOver = (journalRoot: string, retention: WatcherProofRetention) =>
  createWatcherFaultProofProgressAuthority({
    journalRoot,
    deploymentFingerprint,
    categories: ["doubleSpend"],
    authenticationKey: TEST_JOURNAL_KEY,
    retention,
  });

/** An admission over an observation of a block the follower does not hold. */
const admitOffChain = (
  authority: ReturnType<typeof authorityOver>,
  revision = 1,
) =>
  authority.admit({
    observation: progressObservation({ deploymentFingerprint, revision }),
    rollbackGeneration: "0",
  });

/** An admission over the fixture's tip, with the follower at tip. */
const admitAtTip = (
  r: Removed,
  authority: ReturnType<typeof authorityOver>,
) => {
  reportAtTip(r);
  const { slot, blockHash, blockNo } = r.h.tipPoint();
  return authority.admit({
    observation: progressObservation({
      deploymentFingerprint,
      at: { slot, blockHash, blockNo },
    }),
    rollbackGeneration: "0",
  });
};

/** An admission at the fixture's tip, with the follower at tip, of the
 * fault on the fixture's header, which the queue holds again. */
const admitFaultAtTip = (
  r: Removed,
  authority: ReturnType<typeof authorityOver>,
) => {
  reportAtTip(r);
  const { slot, blockHash, blockNo } = r.h.tipPoint();
  const observation = progressObservation({
    deploymentFingerprint,
    at: { slot, blockHash, blockNo },
    header: { header: r.headerBody, headerHash: r.header },
  });
  const decision = decisionOf(r.header);
  return authority.admit({
    observation,
    rollbackGeneration: "1",
    fault: {
      decision: decision as never,
      deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
      actuationPermit: {
        authority: "submission",
        deploymentFingerprint,
        headerHash: r.header,
        decisionDigest: decision.decisionDigest,
      } as unknown as WorkflowActuationPermit,
    },
  });
};

/** The objective rows the journal root holds. */
const objectiveRows = (journalRoot: string) =>
  listWatcherProofObjectives(
    openWatcherJournalDatabase({
      journalRoot,
      authenticationKey: TEST_JOURNAL_KEY,
    }),
    ["doubleSpend"],
  ).map(({ objective }) => objective.headerHash);

/** The node unit's history, read through a capture of the header. */
const captured = async (r: Removed, retention = r.retention) => {
  const snapshot = (
    await createWatcherFaultProofL1Source({
      store: r.h.store,
      rawReads: r.h.reads(true),
      pending: pendingOver(r.h.store),
      sourceId: SOURCE_ID,
      proofRetention: retention,
    })
      .snapshotAuthority({
        releaseFinality: RELEASE,
        observationDepth: "inclusion",
      })
      .capture(requestFor(r.header, { scopes: [] }))
  ).snapshot as {
    history: readonly { unit: string; transactionHashes: readonly string[] }[];
  };
  return snapshot.history.find(({ unit }) => unit === nodeUnit(r.header))
    ?.transactionHashes;
};

describe("startup forgets the rows and releases no hold", () => {
  it("restarted over an open objective's row, forgets the row but keeps its history through a forced prune, and its capture reads it", async () => {
    // No roster floor in this store: only the pin holds the rows.
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK();
    const journalRoot = await journalOver(r);
    const authority = authorityOver(journalRoot, r.retention);
    expect(await admitOffChain(authority)).toEqual([]);
    expect(objectiveRows(journalRoot)).toEqual([]);
    expect(await r.retention.pinned()).toEqual([r.target]);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await departedRows(r)).toBe(1);
    expect([...((await captured(r)) ?? [])].sort()).toEqual(
      [r.commitHash, r.removalHash].sort(),
    );
  });

  it("re-created mid-process (a journal-busy requeue), initializes again releasing nothing, and the floor stays lifted", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      roster: createWatcherProofRosterRetention(),
    });
    const journalRoot = await journalOver(r);
    // The first pass at tip lifts the floor; the removal is one block deep,
    // so the hold nothing owns any more is kept.
    expect(
      await admitAtTip(r, authorityOver(journalRoot, r.retention)),
    ).toEqual([]);
    expect(r.roster.lifted()).toBe(true);
    expect(await r.retention.pinned()).toEqual([r.target]);

    const requeued = authorityOver(journalRoot, r.retention);
    expect(await admitOffChain(requeued)).toEqual([]);
    expect(await r.retention.pinned()).toEqual([r.target]);
    expect(r.roster.lifted()).toBe(true);
    expect((await floorLagsOf(r)).map(({ floor }) => floor)).not.toContain(
      WATCHER_PROOF_ROSTER_FLOOR,
    );

    // Its own pass at tip releases the hold once the departure is deep.
    await advance(r, K + 1);
    expect(await admitAtTip(r, requeued)).toEqual([]);
    expect(await r.retention.pinned()).toEqual([]);
  });
});

describe("history across restarts", () => {
  it("holds history older than the retained depth across a restart", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK(3 * K + 4);
    // A restart: a new retention and roster over the same store.
    const restarted = createWatcherProofRetention(r.h.store, {
      unitHistoryPolicies: watcherUnitHistoryPolicies(D),
      stateQueuePolicyId: D.stateQueueMint,
      roster: createWatcherProofRosterRetention(),
      fraudProofSpend: D.fraudProofSpend,
    });
    const authority = authorityOver(await journalOver(r), restarted);
    await admitOffChain(authority);
    expect(await restarted.pinned()).toEqual([r.target]);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBeGreaterThan(0);
    expect([...((await captured(r, restarted)) ?? [])].sort()).toEqual(
      [r.commitHash, r.removalHash].sort(),
    );
  });

  // A restart forgets every objective row, so nothing of this process owns
  // the hold of a completion it verified short of depth; the hold stays
  // until a pass at tip finds its departure k deep, which no rollback the
  // watcher recovers from can undo.
  it("keeps a completion short of depth held across a restart, a pass at tip and a forced prune, and a rollback returning its header admits the fault again over that history", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      roster: createWatcherProofRosterRetention(),
    });
    const journalRoot = await journalDirectory("watcher-history-holds");
    journalState.headers = [r.header];
    // The first process verified the completion one block deep: short of
    // k + 1, so no marker holds and the hold stays.
    await authorityOver(journalRoot, r.retention).markCompleted(r.target, {
      workflowId: `workflow-${r.header}`,
      terminalTxHash: r.removalHash,
      confirmationDepth: 1,
    });
    expect(objectiveRows(journalRoot)).toEqual([r.header]);
    expect(await r.retention.pinned()).toEqual([r.target]);

    // The restart forgets the row; its first pass at tip lifts the floor
    // and releases nothing, and a forced prune deletes none of the history.
    const restarted = authorityOver(journalRoot, r.retention);
    expect(await admitAtTip(r, restarted)).toEqual([]);
    expect(r.roster.lifted()).toBe(true);
    expect(objectiveRows(journalRoot)).toEqual([]);
    expect(await r.retention.pinned()).toEqual([r.target]);
    await r.h.pruneAll();
    expect(await historyRows(r)).toBeGreaterThan(0);

    // A rollback drops the removal: the header is queued again, its fault
    // is admitted afresh, and its capture reads the held history.
    await r.h.backward(1);
    const contexts = await admitFaultAtTip(r, restarted);
    expect(contexts).toMatchObject([{ decision: { headerHash: r.header } }]);
    expect(restarted.unfinishedCount()).toBe(1);
    expect(await r.retention.pinned()).toEqual([r.target]);
    expect(await captured(r)).toEqual([r.commitHash]);
  });
});
