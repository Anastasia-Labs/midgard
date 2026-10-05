import { randomUUID } from "node:crypto";
import { mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";

const directories: string[] = [];
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

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => rm(path, { recursive: true, force: true })),
  );
});

const recordPath = (directory: string, revision: number): string =>
  join(directory, `${revision.toString().padStart(20, "0")}.json`);

/** Enqueued, started and finished: records 0, 1 and 2. */
const makeJournal = async () => {
  const journalRoot = await mkdtemp("/var/tmp/midgard-proof-queue-");
  directories.push(journalRoot);
  const input = { journalRoot, deploymentFingerprint, authenticationKey };
  const journal = await openWatcherFaultProofQueueJournal(input);
  await journal.register(identity, "1000");
  await journal.markStarted(digest, "1001");
  await journal.markFinished(digest, "1002");
  return {
    input,
    directory: join(journalRoot, "fault-proof-queue-v1"),
  };
};

describe("fault-proof queue journal crash recovery", () => {
  it("drops an empty or torn final record left by an interrupted append", async () => {
    const reference = await makeJournal();
    const complete = await readFile(recordPath(reference.directory, 2), "utf8");
    for (const torn of [
      "",
      complete.slice(0, -2),
      complete.slice(0, 7),
      "\0".repeat(complete.length),
    ]) {
      const { input, directory } = await makeJournal();
      await writeFile(recordPath(directory, 2), torn, "utf8");
      // The finish never returned, so the job is still active: a retry
      // requeues it instead of reading a finish.
      const recovered = await openWatcherFaultProofQueueJournal(input);
      expect(await readdir(directory)).toHaveLength(2);
      await expect(recovered.register(identity, "1003")).resolves.toEqual({
        queuedAtMs: "1000",
        finished: false,
      });
      expect(
        JSON.parse(await readFile(recordPath(directory, 2), "utf8")).event.kind,
      ).toBe("requeued");
      const restarted = await openWatcherFaultProofQueueJournal(input);
      expect(restarted.status()).toEqual({
        queuedJobCount: 1,
        oldestQueuedAtMs: "1000",
      });
    }
  });

  it("fails closed on a torn record that is not final, keeping every record", async () => {
    for (const [torn, failure] of [
      ["", /record size is invalid/u],
      ["{", /record is malformed/u],
    ] as const) {
      const { input, directory } = await makeJournal();
      await writeFile(recordPath(directory, 1), torn, "utf8");
      await expect(openWatcherFaultProofQueueJournal(input)).rejects.toThrow(
        failure,
      );
      expect(await readdir(directory)).toHaveLength(3);
    }

    // A torn final record is not dropped while an earlier record is torn.
    const both = await makeJournal();
    await writeFile(recordPath(both.directory, 1), "", "utf8");
    await writeFile(recordPath(both.directory, 2), "", "utf8");
    await expect(openWatcherFaultProofQueueJournal(both.input)).rejects.toThrow(
      "record size is invalid",
    );
    expect(await readdir(both.directory)).toHaveLength(3);

    // Nor when it does not continue the chain.
    const gap = await makeJournal();
    await writeFile(recordPath(gap.directory, 4), "", "utf8");
    await expect(openWatcherFaultProofQueueJournal(gap.input)).rejects.toThrow(
      "contains an invalid record",
    );
    expect(await readdir(gap.directory)).toHaveLength(4);
  });

  it("keeps a parseable final record under full authentication", async () => {
    const { input, directory } = await makeJournal();
    const forged = JSON.parse(
      await readFile(recordPath(directory, 2), "utf8"),
    ) as { event: { observedAtMs: string } };
    forged.event.observedAtMs = "9999";
    await writeFile(
      recordPath(directory, 2),
      `${watcherCanonicalJson(forged)}\n`,
      "utf8",
    );
    await expect(openWatcherFaultProofQueueJournal(input)).rejects.toThrow(
      "authentication failed",
    );
    expect(await readdir(directory)).toHaveLength(3);
  });

  it("reopens at the previous revision after a crash between staging and link", async () => {
    const { input, directory } = await makeJournal();
    const finished = await readFile(recordPath(directory, 2), "utf8");
    // The link never happened, so the revision name does not exist.
    await rm(recordPath(directory, 2));
    await writeFile(
      join(directory, `.staged-${randomUUID()}.tmp`),
      finished,
      "utf8",
    );
    await writeFile(join(directory, `.staged-${randomUUID()}.tmp`), "{");

    const recovered = await openWatcherFaultProofQueueJournal(input);
    expect((await readdir(directory)).sort()).toEqual([
      "00000000000000000000.json",
      "00000000000000000001.json",
    ]);
    await recovered.markFinished(digest, "1003");
    const restarted = await openWatcherFaultProofQueueJournal(input);
    await expect(restarted.register(identity, "1004")).resolves.toEqual({
      queuedAtMs: "1000",
      finished: true,
    });
  });

  it("refuses the losing writer when two journals append one revision", async () => {
    const { input, directory } = await makeJournal();
    const [winner, loser] = [
      await openWatcherFaultProofQueueJournal(input),
      await openWatcherFaultProofQueueJournal(input),
    ];
    await winner.register(identity, "1003", { reopenFinished: true });
    await expect(
      loser.register({ ...identity, rollbackGeneration: "8" }, "1004"),
    ).rejects.toMatchObject({ code: "EEXIST" });
    expect((await readdir(directory)).sort()).toEqual([
      "00000000000000000000.json",
      "00000000000000000001.json",
      "00000000000000000002.json",
      "00000000000000000003.json",
    ]);
    const restarted = await openWatcherFaultProofQueueJournal(input);
    expect(restarted.status()).toEqual({
      queuedJobCount: 1,
      oldestQueuedAtMs: "1000",
    });
  });
});
