import { mkdtemp, readdir, readFile, rm } from "node:fs/promises";
import { join } from "node:path";

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
  it("durably reopens an explicitly authorized finished job and deduplicates retries", async () => {
    const journalRoot = await mkdtemp("/var/tmp/midgard-proof-queue-");
    directories.push(journalRoot);
    const input = { journalRoot, deploymentFingerprint, authenticationKey };
    const journal = await openWatcherFaultProofQueueJournal(input);
    const digest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity,
    });
    await journal.register(identity, "1000");
    await journal.markStarted(digest, "1001");
    await journal.markFinished(digest, "1002");
    const directory = join(journalRoot, "fault-proof-queue-v1");
    const original = await Promise.all(
      (await readdir(directory))
        .sort()
        .map(async (file) => readFile(join(directory, file), "utf8")),
    );
    const restarted = await openWatcherFaultProofQueueJournal(input);
    await expect(restarted.register(identity, "1003")).resolves.toEqual({
      queuedAtMs: "1000",
      finished: true,
    });
    await expect(
      restarted.register(identity, "-1", { reopenFinished: true }),
    ).rejects.toThrow("enqueue time is invalid");
    const registrations = await Promise.all([
      restarted.register(identity, "1004", { reopenFinished: true }),
      restarted.register(identity, "1005", { reopenFinished: true }),
    ]);
    expect(registrations).toEqual([
      { queuedAtMs: "1000", finished: false },
      { queuedAtMs: "1000", finished: false },
    ]);
    const records = await Promise.all(
      (await readdir(directory))
        .sort()
        .map(async (file) => readFile(join(directory, file), "utf8")),
    );
    expect(records.slice(0, original.length)).toEqual(original);
    expect(records).toHaveLength(4);
    expect(JSON.parse(records[3]!).event.kind).toBe("reopened");
    const recovered = await openWatcherFaultProofQueueJournal(input);
    expect(recovered.status()).toEqual({
      queuedJobCount: 1,
      oldestQueuedAtMs: "1000",
    });
    await recovered.markStarted(digest, "1006");
    await recovered.markFinished(digest, "1007");
    expect(await recovered.register(identity, "1008")).toEqual({
      queuedAtMs: "1000",
      finished: true,
    });
  });

  it("keeps a completed job finished when a retry arrives during its durable finish", async () => {
    const journalRoot = await mkdtemp("/var/tmp/midgard-proof-queue-");
    directories.push(journalRoot);
    const input = { journalRoot, deploymentFingerprint, authenticationKey };
    const journal = await openWatcherFaultProofQueueJournal(input);
    await journal.register(identity, "1000");
    const digest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity,
    });
    await journal.markStarted(digest, "1001");

    // The runner has finished while the coordinator still sees the same fault.
    // Its next enqueue races the finish record's asynchronous fsync.
    const finishing = journal.markFinished(digest, "1002");
    const retry = journal.register(identity, "1003");
    await finishing;
    await expect(retry).resolves.toEqual({
      queuedAtMs: "1000",
      finished: true,
    });
    expect(journal.status().queuedJobCount).toBe(0);
    const restarted = await openWatcherFaultProofQueueJournal(input);
    await expect(restarted.register(identity, "1004")).resolves.toEqual({
      queuedAtMs: "1000",
      finished: true,
    });
  });

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

  it("preserves backward wall-clock observations through completion, recovery, and explicit reopening", async () => {
    const journalRoot = await mkdtemp("/var/tmp/midgard-proof-queue-");
    directories.push(journalRoot);
    const input = { journalRoot, deploymentFingerprint, authenticationKey };
    const digest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity,
    });
    const first = await openWatcherFaultProofQueueJournal(input);
    await first.register(identity, "1000");
    await first.markStarted(digest, "999");
    // Restart after a clock adjustment while the durable job remains active.
    const recovered = await openWatcherFaultProofQueueJournal(input);
    await expect(recovered.register(identity, "998")).resolves.toEqual({
      queuedAtMs: "1000",
      finished: false,
    });
    await recovered.markStarted(digest, "997");
    await recovered.markFinished(digest, "996");
    await expect(recovered.register(identity, "995")).resolves.toEqual({
      queuedAtMs: "1000",
      finished: true,
    });
    await recovered.register(identity, "994", { reopenFinished: true });
    await recovered.markStarted(digest, "993");
    await recovered.markFinished(digest, "992");

    const directory = join(journalRoot, "fault-proof-queue-v1");
    const records = await Promise.all(
      (await readdir(directory))
        .sort()
        .map(async (file) =>
          JSON.parse(await readFile(join(directory, file), "utf8")),
        ),
    );
    expect(
      records.map(({ event }) => event.observedAtMs ?? event.queuedAtMs),
    ).toEqual(["1000", "999", "998", "997", "996", "994", "993", "992"]);
    const restarted = await openWatcherFaultProofQueueJournal(input);
    await expect(restarted.register(identity, "991")).resolves.toEqual({
      queuedAtMs: "1000",
      finished: true,
    });
  });

  it("rejects malformed times, unadmitted identities, and illegal state transitions without appending", async () => {
    const journalRoot = await mkdtemp("/var/tmp/midgard-proof-queue-");
    directories.push(journalRoot);
    const journal = await openWatcherFaultProofQueueJournal({
      journalRoot,
      deploymentFingerprint,
      authenticationKey,
    });
    const digest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity,
    });
    const otherGeneration = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint,
      identity: { ...identity, rollbackGeneration: "8" },
    });
    await journal.register(identity, "1000");
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
    expect(
      await readdir(join(journalRoot, "fault-proof-queue-v1")),
    ).toHaveLength(3);
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
