import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it } from "vitest";

import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";
import {
  closeWatcherJournalDatabase,
  isWatcherJournalConfigurationError,
  isWatcherJournalIntegrityError,
  openWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
  watcherJournalIntegrityFailure,
} from "../../src/fault-proofs/watcher-journal-database.js";
import {
  journalDirectory,
  removeJournalDirectories,
} from "../support/watcher-journal-fixture.js";

const deploymentFingerprint = "11".repeat(32);
const authenticationKey = Uint8Array.from({ length: 32 }, () => 0x42);
const identity = Object.freeze({
  category: "doubleSpend",
  headerHash: "22".repeat(28),
  decisionDigest: "33".repeat(32),
  rollbackGeneration: "7",
});
const digest = watcherFaultProofQueueIdentityDigest({
  deploymentFingerprint,
  identity,
});

afterEach(removeJournalDirectories);

const opened = async () => {
  const journalRoot = await journalDirectory("midgard-proof-queue");
  const input = { journalRoot, deploymentFingerprint, authenticationKey };
  return {
    journalRoot,
    input,
    journal: await openWatcherFaultProofQueueJournal(input),
  };
};

/** A process exit and restart: the shared connection is closed first. */
const restart = async (input: {
  journalRoot: string;
  deploymentFingerprint: string;
  authenticationKey: Uint8Array;
}) => {
  closeWatcherJournalDatabase(input.journalRoot);
  return await openWatcherFaultProofQueueJournal(input);
};

const queueRows = (journalRoot: string) => {
  const database = new DatabaseSync(
    join(journalRoot, WATCHER_JOURNAL_DATABASE_FILE),
    { readOnly: true },
  );
  try {
    return database
      .prepare(
        "SELECT row_key, state, body FROM watcher_fault_proof_queue ORDER BY row_key",
      )
      .all() as { row_key: string; state: string; body: string }[];
  } finally {
    database.close();
  }
};

describe("production fault-proof queue journal", () => {
  it("requeues a finished job in place, keeping its original queue time", async () => {
    const { journalRoot, input, journal } = await opened();
    await journal.register(identity, "1000");
    await journal.markStarted(digest, "1001");
    await journal.markFinished(digest, "1002");
    const restarted = await restart(input);
    await expect(restarted.register(identity, "-1")).rejects.toThrow(
      "enqueue time is invalid",
    );
    const registrations = await Promise.all([
      restarted.register(identity, "1004"),
      restarted.register(identity, "1005"),
    ]);
    expect(registrations).toEqual([
      { queuedAtMs: "1000" },
      { queuedAtMs: "1000" },
    ]);
    expect(queueRows(journalRoot).map(({ state }) => state)).toEqual([
      "queued",
    ]);
    const recovered = await restart(input);
    expect(recovered.status()).toEqual({
      queuedJobCount: 1,
      oldestQueuedAtMs: "1000",
    });
    await recovered.markStarted(digest, "1006");
    await recovered.markFinished(digest, "1007");
    expect(queueRows(journalRoot)).toHaveLength(1);
  });

  it("replaces an older identity of the same objective instead of growing the table", async () => {
    const { journalRoot, input, journal } = await opened();
    await journal.register(identity, "1000");
    await journal.markStarted(digest, "1001");
    const next = { ...identity, rollbackGeneration: "8" };
    const nextDigest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity: next,
    });
    await expect(journal.register(next, "1002")).resolves.toEqual({
      queuedAtMs: "1002",
    });
    expect(queueRows(journalRoot).map(({ row_key }) => row_key)).toEqual([
      nextDigest,
    ]);
    await expect(journal.markFinished(digest, "1003")).rejects.toThrow(
      "has no admitted predecessor",
    );
    const restarted = await restart(input);
    expect(restarted.status()).toEqual({
      queuedJobCount: 1,
      oldestQueuedAtMs: "1002",
    });
  });

  it("preserves backward wall-clock observations through completion and recovery", async () => {
    const { journalRoot, input, journal } = await opened();
    await journal.register(identity, "1000");
    await journal.markStarted(digest, "999");
    const recovered = await restart(input);
    await expect(recovered.register(identity, "998")).resolves.toEqual({
      queuedAtMs: "1000",
    });
    await recovered.markStarted(digest, "997");
    await recovered.markFinished(digest, "996");
    const [row] = queueRows(journalRoot);
    expect(row?.state).toBe("finished");
    expect(JSON.parse(row!.body)).toMatchObject({
      queuedAtMs: "1000",
      observedAtMs: "996",
    });
  });

  it("rejects malformed times, unadmitted identities and illegal transitions without writing", async () => {
    const { journalRoot, journal } = await opened();
    const otherGeneration = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity: { ...identity, rollbackGeneration: "8" },
    });
    await journal.register(identity, "1000");
    const before = queueRows(journalRoot);
    await expect(journal.markStarted("invalid", "999")).rejects.toThrow(
      "identity digest is invalid",
    );
    await expect(journal.markStarted(otherGeneration, "999")).rejects.toThrow(
      "has no admitted predecessor",
    );
    for (const time of ["-1", "1.5", "NaN", "01", ""]) {
      await expect(journal.markStarted(digest, time)).rejects.toThrow(
        "observation time is invalid",
      );
    }
    await expect(journal.markFinished(digest, "999")).rejects.toThrow(
      "requires active predecessor, found queued",
    );
    expect(queueRows(journalRoot)).toEqual(before);
    await journal.markStarted(digest, "999");
    await expect(journal.markStarted(digest, "998")).rejects.toThrow(
      "requires queued predecessor, found active",
    );
    await journal.markFinished(digest, "998");
    await expect(journal.markFinished(digest, "997")).rejects.toThrow(
      "requires active predecessor, found finished",
    );
    await expect(journal.markStarted(digest, "997")).rejects.toThrow(
      "requires queued predecessor, found finished",
    );
  });

  it("refuses a wrong authentication key on restart as configuration", async () => {
    const { input, journal } = await opened();
    await journal.register(identity, "1000");
    closeWatcherJournalDatabase(input.journalRoot);
    const failure = await openWatcherFaultProofQueueJournal({
      ...input,
      authenticationKey: Uint8Array.from({ length: 32 }, () => 0x43),
    }).catch((error: unknown) => error);
    expect(isWatcherJournalConfigurationError(failure)).toBe(true);
    expect(String(failure)).toContain(
      "the journals were written under another authentication key",
    );
    // Configuration is not corruption: nothing is latched.
    expect(watcherJournalIntegrityFailure(input.journalRoot)).toBeNull();
  });

  it("refuses a row written for another deployment on restart", async () => {
    const { input, journal } = await opened();
    await journal.register(identity, "1000");
    closeWatcherJournalDatabase(input.journalRoot);
    const failure = await openWatcherFaultProofQueueJournal({
      ...input,
      deploymentFingerprint: "12".repeat(32),
    }).catch((error: unknown) => error);
    expect(isWatcherJournalIntegrityError(failure)).toBe(true);
    expect(String(failure)).toContain("differs from its job identity");
    // The refusal is latched: every later use of the journals throws it.
    expect(watcherJournalIntegrityFailure(input.journalRoot)).toContain(
      "differs from its job identity",
    );
    expect(() =>
      openWatcherJournalDatabase({
        journalRoot: input.journalRoot,
        authenticationKey,
      }).head("fault_proof_queue"),
    ).toThrow("differs from its job identity");
  });
});
