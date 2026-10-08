import { mkdirSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  closeWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
  watcherJournalIntegrityFailure,
  watcherJournalUnavailable,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { openWatcherProverFundingRuntime } from "../../src/runtime/watcher-prover-funding-runtime.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

/**
 * Startup reclaims reservations left before any signed attempt, which reads
 * the decision journal. Journals that fail integrity, or cannot be opened,
 * must keep those reservations and let startup reach the operations server,
 * where readiness names the reason; they must never fail startup.
 */
const startOver = async (damage: (databasePath: string) => void) => {
  const test = await setupFundingRecoveryFixture(false, false, true);
  const before = await test.records();
  expect(
    before.some(
      (record) =>
        record.state === "active" &&
        record.activeInputs.length > 0 &&
        record.pendingTransition === null,
    ),
  ).toBe(true);
  closeWatcherJournalDatabase(test.journalRoot);
  const databasePath = join(test.journalRoot, WATCHER_JOURNAL_DATABASE_FILE);
  for (const suffix of ["-wal", "-shm"])
    rmSync(`${databasePath}${suffix}`, { force: true });
  damage(databasePath);
  const runtime = await openWatcherProverFundingRuntime({
    deploymentIdentity,
    path: join(test.journalRoot, "watcher.sqlite"),
    authenticationKey: Buffer.alloc(32, 0x91),
    createProtocolParameters: async () => test.protocolParameters,
    launchScope: test.old.launchScope,
    journalRoot: test.journalRoot,
  });
  runtime.store.close();
  expect(await test.records()).toEqual(before);
  return test.journalRoot;
};

describe("prover funding runtime startup over refused journals", () => {
  it("keeps an unused reservation and starts when the decision journal fails integrity", async () => {
    const journalRoot = await startOver((path) =>
      writeFileSync(path, Buffer.alloc(8_192, 0x5a)),
    );
    expect(watcherJournalIntegrityFailure(journalRoot)).toContain(
      "file is not a database",
    );
  });

  it("keeps an unused reservation and starts when the journals cannot be opened", async () => {
    const journalRoot = await startOver((path) => {
      rmSync(path);
      mkdirSync(path);
    });
    expect(watcherJournalIntegrityFailure(journalRoot)).toBeNull();
    expect(watcherJournalUnavailable(journalRoot)).toContain(
      "unable to open database file",
    );
  });
});
