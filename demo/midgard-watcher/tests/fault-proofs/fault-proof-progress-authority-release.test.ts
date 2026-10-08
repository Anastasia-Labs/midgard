import { afterEach, describe, expect, it } from "vitest";

import { readWatcherProofExecution } from "../../src/fault-proofs/fault-proof-objective-journal.js";
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
// not: its pin and its in-memory entry go at once, but the rows of a job
// still queued or running are left to that job, whose next queue transition
// needs its row. The job's run adopts the objective again, and a later
// observation releases it with its rows.
afterEach(cleanupFundingRecoveryFixtures);

describe("proof progress authority release of a departed header", () => {
  it.each([
    ["an unheld objective with no signed attempt", false],
    ["a held objective", true],
  ] as const)(
    "releases %s but leaves a queued job its rows",
    async (_name, heldFirst) => {
      const fixture = await setupFundingRecoveryFixture(false, false, true);
      const deploymentFingerprint = deploymentIdentity.manifestId;
      const pinned = new Set<string>();
      const name = (target: WatcherProofRetentionTarget) =>
        `${target.category}/${target.headerHash}`;
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
      // Adopted from its row while its header is queued.
      await admit(1, true);
      if (heldFirst)
        await authority.holdObjective({
          kind: "objective",
          ...objective,
          decisionDigest: fixture.old.decisionDigest,
          detail: name(objective),
        });
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
      // With no job left, the next observation releases it with its rows.
      await admit(3, false);
      expect(authority.unfinishedCount()).toBe(0);
      expect([...pinned]).toEqual([]);
      expect(rows()).toEqual([]);
    },
  );
});
