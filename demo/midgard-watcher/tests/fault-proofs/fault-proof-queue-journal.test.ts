import { mkdtemp, rm } from "node:fs/promises";

import { afterEach, describe, expect, it } from "vitest";

import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";

const directories: string[] = [];
const deploymentFingerprint = "11".repeat(32);
const authenticationKey = Uint8Array.from({ length: 32 }, () => 0x42);
const identity = Object.freeze({
  category: "doubleSpend",
  headerHash: "22".repeat(28),
  decisionDigest: "33".repeat(32),
  rollbackGeneration: "7",
});

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => rm(path, { recursive: true, force: true })),
  );
});

describe("production fault-proof queue journal V1", () => {
  it("preserves original queued time through authenticated restart and retry", async () => {
    const journalRoot = await mkdtemp("/var/tmp/midgard-proof-queue-");
    directories.push(journalRoot);
    const first = await openWatcherFaultProofQueueJournal({
      journalRoot,
      deploymentFingerprint,
      authenticationKey,
    });
    await expect(first.register(identity, "1000")).resolves.toEqual({
      queuedAtMs: "1000",
      finished: false,
    });
    const digest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity,
    });
    await first.markStarted(digest, "1001");

    const recovered = await openWatcherFaultProofQueueJournal({
      journalRoot,
      deploymentFingerprint,
      authenticationKey,
    });
    await expect(recovered.register(identity, "9000")).resolves.toEqual({
      queuedAtMs: "1000",
      finished: false,
    });
    expect(recovered.status()).toEqual({
      queuedJobCount: 1,
      oldestQueuedAtMs: "1000",
    });
    await recovered.markStarted(digest, "9001");
    await recovered.markFinished(digest, "9002");
    expect(recovered.status()).toEqual({
      queuedJobCount: 0,
      oldestQueuedAtMs: null,
    });
    await expect(recovered.register(identity, "10000")).resolves.toEqual({
      queuedAtMs: "1000",
      finished: true,
    });
  });

  it("rejects a wrong queue authentication key on restart", async () => {
    const journalRoot = await mkdtemp("/var/tmp/midgard-proof-queue-");
    directories.push(journalRoot);
    const first = await openWatcherFaultProofQueueJournal({
      journalRoot,
      deploymentFingerprint,
      authenticationKey,
    });
    await first.register(identity, "1000");
    await expect(
      openWatcherFaultProofQueueJournal({
        journalRoot,
        deploymentFingerprint,
        authenticationKey: Uint8Array.from({ length: 32 }, () => 0x43),
      }),
    ).rejects.toThrow("authentication failed");
  });
});
