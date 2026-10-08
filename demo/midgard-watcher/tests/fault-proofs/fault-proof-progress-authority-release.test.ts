import { existsSync } from "node:fs";
import {
  mkdir,
  mkdtemp,
  readFile,
  rename,
  rm,
  symlink,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  readWatcherProofExecution,
  removeWatcherProofObjectiveDirectory,
} from "../../src/fault-proofs/fault-proof-objective-journal.js";
import { listWatcherProofObjectives } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
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
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot: fixture.journalRoot,
    deploymentFingerprint,
    categories: fixture.old.launchScope,
    authenticationKey: TEST_JOURNAL_KEY,
    retention,
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
    authority,
    objective,
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

  it("names a workflow directory reached through a symlink, keeps its rows and the target, and removes it once repaired", async () => {
    const { fixture, authority, objective, rows, admit } = await setup(true);
    await admit(1, true);
    const directory = fixture.journalDirectory;
    const target = `${directory}-elsewhere`;
    await rename(directory, target);
    await symlink(target, directory);
    await admit(2, false);
    expect(authority.unfinishedCount()).toBe(0);
    expect(authority.cleanupFailures()).toEqual([
      {
        ...objective,
        detail: `${name(objective)}: proof objective journal traverses a symlink`,
      },
    ]);
    expect(rows()).toEqual([name(objective)]);
    expect(existsSync(target)).toBe(true);
    // The next pass retries it once the operator replaced the link.
    await rm(directory);
    await rename(target, directory);
    await admit(3, false);
    expect(authority.cleanupFailures()).toEqual([]);
    expect(rows()).toEqual([]);
    expect(existsSync(directory)).toBe(false);
  });
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
    await expect(
      removeWatcherProofObjectiveDirectory(join(root, "linked"), objective),
    ).rejects.toThrow("proof objective journal traverses a symlink");
    expect(await readFile(join(kept, "evidence"), "utf8")).toBe("kept");
    // Through its real path it goes, and a missing one is already gone.
    await removeWatcherProofObjectiveDirectory(real, objective);
    expect(existsSync(kept)).toBe(false);
    await removeWatcherProofObjectiveDirectory(real, objective);
  });
});
