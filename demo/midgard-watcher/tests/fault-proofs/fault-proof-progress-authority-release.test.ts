import { existsSync, readdirSync } from "node:fs";
import {
  chmod,
  mkdir,
  mkdtemp,
  readdir,
  readFile,
  rename,
  rm,
  symlink,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  readWatcherProofExecution,
  removeWatcherProofObjectiveDirectory,
  watcherProofTombstoneDirectory,
} from "../../src/fault-proofs/fault-proof-objective-journal.js";
import {
  forgetWatcherProofObjective,
  listWatcherProofObjectives,
} from "../../src/fault-proofs/fault-proof-objective-table.js";
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
// workflow directory of a job still queued or running are left to that job,
// whose next queue transition needs its row. The job's run adopts the
// objective again, and a later observation releases it with its rows and
// directory. A signed attempt keeps its directory for the funding sweep.
const temporary: string[] = [];
afterEach(async () => {
  await cleanupFundingRecoveryFixtures();
  for (const root of temporary.splice(0))
    await rm(root, { recursive: true, force: true });
});

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
      deploymentFingerprint,
      categories: fixture.old.launchScope,
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
    "releases %s but leaves a queued job its rows and directory",
    async (_name, heldFirst) => {
      const {
        fixture,
        deploymentFingerprint,
        pinned,
        authority,
        objective,
        rows,
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
      expect(existsSync(fixture.journalDirectory)).toBe(true);
      // The job still starts and finishes on its row.
      const digest = watcherFaultProofQueueIdentityDigest({
        deploymentFingerprint,
        identity,
      });
      await queue.markStarted(digest, "2");
      const execution = await readWatcherProofExecution({
        journalRoot: fixture.journalRoot,
        deploymentFingerprint,
        objective,
      });
      await authority.updateExecution({ objective, execution: execution! });
      await queue.markFinished(digest, "3");
      expect(authority.unfinishedCount()).toBe(1);
      expect([...pinned]).toEqual([name(objective)]);
      // With no job left, the next observation releases it with its rows
      // and its workflow directory.
      await admit(3, false);
      expect(authority.unfinishedCount()).toBe(0);
      expect([...pinned]).toEqual([]);
      expect(rows()).toEqual([]);
      expect(existsSync(fixture.journalDirectory)).toBe(false);
      expect(authority.cleanupFailures()).toEqual([]);
    },
  );

  it("releases a held objective with a signed attempt but keeps its directory for the funding sweep", async () => {
    const { fixture, pinned, authority, objective, rows, admit, holdIt } =
      await setup(false);
    await admit(1, true);
    await holdIt();
    await admit(2, false);
    expect(authority.unfinishedCount()).toBe(0);
    expect([...pinned]).toEqual([]);
    expect(rows()).toEqual([]);
    expect(existsSync(fixture.journalDirectory)).toBe(true);
    expect(
      (
        await readWatcherProofExecution({
          journalRoot: fixture.journalRoot,
          deploymentFingerprint: deploymentIdentity.manifestId,
          objective,
        })
      )?.entries.some(({ event }) => event.kind === "submission_intent"),
    ).toBe(true);
    expect(authority.cleanupFailures()).toEqual([]);
  });

  it("forgets a released objective whose workflow directory is a symlink, leaving the link and its target in place", async () => {
    const { fixture, authority, rows, admit } = await setup(true);
    await admit(1, true);
    const directory = fixture.journalDirectory;
    const target = `${directory}-elsewhere`;
    await rename(directory, target);
    await symlink(target, directory);
    await admit(2, false);
    expect(authority.unfinishedCount()).toBe(0);
    // The read refuses the link: the rows go, nothing behind it is trusted
    // or deleted, and no removal is left to retry.
    expect(authority.cleanupFailures()).toEqual([]);
    expect(rows()).toEqual([]);
    expect(existsSync(directory)).toBe(true);
    expect(existsSync(target)).toBe(true);
  });
});

describe("proof progress authority removal failure", () => {
  it("names a removal a filesystem error stopped, keeps its rows, and clears once the removal succeeds", async () => {
    const { fixture, authority, objective, rows, admit } = await setup(true);
    await admit(1, true);
    // The category directory refuses the rename.
    const category = dirname(fixture.journalDirectory);
    await chmod(category, 0o500);
    try {
      await admit(2, false);
      expect(authority.cleanupFailures()).toMatchObject([
        { ...objective, detail: expect.stringContaining("EACCES") },
      ]);
      expect(rows()).toEqual([name(objective)]);
      expect(existsSync(fixture.journalDirectory)).toBe(true);
    } finally {
      await chmod(category, 0o700);
    }
    await admit(3, false);
    expect(authority.cleanupFailures()).toEqual([]);
    expect(rows()).toEqual([]);
    expect(existsSync(fixture.journalDirectory)).toBe(false);
  });
});

// A restart over journals an earlier process left mid-removal or with an
// unreadable workflow directory starts, names what it holds, and settles it
// once the header leaves the finalized queue: no exit, no operator action.
describe("proof progress authority restart over an interrupted removal", () => {
  /** Removes the first journal entry of the objective's execution, as a
   * crash part way through a recursive delete leaves it. */
  const removeFirstEntry = async (directory: string) => {
    const [execution] = await readdir(directory);
    const names = (await readdir(join(directory, execution!))).sort();
    expect(names.length).toBeGreaterThan(1);
    await rm(join(directory, execution!, names[0]!));
  };

  it("sweeps the tombstone a crash left during its delete", async () => {
    const harness = await setup(true);
    const { fixture, objective, database, rows, admit } = harness;
    await admit(1, true);
    expect(harness.authority.unfinishedCount()).toBe(1);
    // The crash came after the rename and the forget, during the delete.
    const tombstones = watcherProofTombstoneDirectory(fixture.journalRoot);
    await mkdir(tombstones, { recursive: true });
    const tombstone = join(tombstones, `doubleSpend.${objective.headerHash}.x`);
    await rename(fixture.journalDirectory, tombstone);
    forgetWatcherProofObjective(database(), objective, { decisions: false });
    await removeFirstEntry(tombstone);
    harness.restart();
    await admit(2, true);
    expect(await readdir(tombstones)).toEqual([]);
    expect(rows()).toEqual([]);
    expect(harness.authority.unfinishedCount()).toBe(0);
    expect(harness.authority.decisionHolds()).toEqual([]);
    expect(harness.authority.cleanupFailures()).toEqual([]);
  });

  const unreadable = [
    ["a partly removed directory", false, "journal entry sequence gap"],
    [
      "a symlinked directory",
      true,
      "proof objective journal traverses a symlink",
    ],
  ] as const;
  const breakDirectory = async (directory: string, linked: boolean) => {
    if (!linked) return removeFirstEntry(directory);
    await rename(directory, `${directory}-elsewhere`);
    await symlink(`${directory}-elsewhere`, directory);
  };

  it.each(unreadable)(
    "holds an open objective over %s by name, then forgets its rows once its header leaves",
    async (_name, linked, failure) => {
      const harness = await setup(true);
      const { fixture, objective, rows, pinned, admit } = harness;
      await admit(1, true);
      await breakDirectory(fixture.journalDirectory, linked);
      harness.restart();
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
      expect(hold!.detail).toContain(failure);
      expect(harness.authority.unfinishedCount()).toBe(1);
      expect([...pinned]).toEqual([name(objective)]);
      expect(rows()).toEqual([name(objective)]);
      // Once its header leaves, the hold clears and its rows go; what the
      // directory holds is neither trusted nor deleted.
      await admit(3, false);
      expect(harness.authority.decisionHolds()).toEqual([]);
      expect(harness.authority.unfinishedCount()).toBe(0);
      expect([...pinned]).toEqual([]);
      expect(rows()).toEqual([]);
      expect(harness.authority.cleanupFailures()).toEqual([]);
      expect(existsSync(fixture.journalDirectory)).toBe(true);
      // A further restart starts clean.
      harness.restart();
      await admit(4, false);
      expect(harness.authority.decisionHolds()).toEqual([]);
    },
  );

  it.each(unreadable)(
    "holds a completion marked final over %s by name, then removes its directory once its header leaves",
    async (_name, linked) => {
      const harness = await setup(true);
      const { fixture, objective, database, rows, pinned, admit } = harness;
      // The row an earlier process marked final before the directory broke.
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
      await breakDirectory(fixture.journalDirectory, linked);
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
      // The final directory goes; a link goes as a link, its target kept.
      expect(existsSync(fixture.journalDirectory)).toBe(false);
      if (linked)
        expect(existsSync(`${fixture.journalDirectory}-elsewhere`)).toBe(true);
      expect(
        await readdir(watcherProofTombstoneDirectory(fixture.journalRoot)),
      ).toEqual([]);
    },
  );
});

describe("proof objective directory removal", () => {
  const objective = {
    category: "doubleSpend",
    headerHash: "ab".repeat(28),
  } as const;

  it("refuses a directory whose path resolves through a symlinked parent", async () => {
    const root = await mkdtemp(join(tmpdir(), "objective-removal-"));
    temporary.push(root);
    const real = join(root, "real");
    const kept = join(
      real,
      "fault-proofs",
      "doubleSpend",
      objective.headerHash,
    );
    await mkdir(kept, { recursive: true });
    await writeFile(join(kept, "evidence"), "kept");
    await symlink(real, join(root, "linked"));
    let forgotten = 0;
    const forget = () => void (forgotten += 1);
    await expect(
      removeWatcherProofObjectiveDirectory(
        join(root, "linked"),
        objective,
        forget,
      ),
    ).resolves.toBe("refused");
    expect(forgotten).toBe(0);
    expect(await readFile(join(kept, "evidence"), "utf8")).toBe("kept");
    // Through its real path it goes, and a missing one is already gone.
    await expect(
      removeWatcherProofObjectiveDirectory(real, objective, forget),
    ).resolves.toBe("removed");
    expect(existsSync(kept)).toBe(false);
    await expect(
      removeWatcherProofObjectiveDirectory(real, objective, forget),
    ).resolves.toBe("removed");
    expect(forgotten).toBe(2);
  });

  it("forgets after the rename and before the delete, and moves a symlinked directory as a link", async () => {
    const root = await mkdtemp(join(tmpdir(), "objective-removal-"));
    temporary.push(root);
    const directory = join(
      root,
      "fault-proofs",
      "doubleSpend",
      objective.headerHash,
    );
    await mkdir(join(directory, "execution"), { recursive: true });
    await writeFile(join(directory, "execution", "entry"), "entry");
    const tombstones = watcherProofTombstoneDirectory(root);
    const seen: string[][] = [];
    await removeWatcherProofObjectiveDirectory(root, objective, () => {
      // The objective's path is gone and its whole content is in one
      // tombstone when the rows are forgotten.
      expect(existsSync(directory)).toBe(false);
      seen.push(readdirSync(tombstones));
    });
    expect(seen).toHaveLength(1);
    expect(seen[0]).toHaveLength(1);
    expect(seen[0]![0]).toMatch(
      new RegExp(`^doubleSpend\\.${objective.headerHash}\\.`, "u"),
    );
    expect(await readdir(tombstones)).toEqual([]);
    // A symlinked directory is renamed as a link: its target stays.
    const elsewhere = join(root, "elsewhere");
    await mkdir(elsewhere);
    await writeFile(join(elsewhere, "evidence"), "kept");
    await symlink(elsewhere, directory);
    await expect(
      removeWatcherProofObjectiveDirectory(root, objective),
    ).resolves.toBe("removed");
    expect(existsSync(directory)).toBe(false);
    expect(await readFile(join(elsewhere, "evidence"), "utf8")).toBe("kept");
    expect(await readdir(tombstones)).toEqual([]);
  });
});
