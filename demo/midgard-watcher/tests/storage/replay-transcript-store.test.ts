import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import type { CorrectionLockDatum } from "@al-ft/midgard-sdk";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { unsafeAdmitWatcherReplayTranscriptCompletionForTest } from "../../src/storage/replay-transcript-completion.js";
import {
  createWatcherSqliteReplayTranscriptStore,
  type WatcherReplayTranscriptStorageLimits,
} from "../../src/storage/replay-transcript-store.js";
import { openWatcherSqliteDurableBackend } from "../../src/storage/sqlite-durable-backend.js";
import {
  assertWatcherAuthenticatedReplayTranscript,
  createWatcherAuthenticatedReplayTranscript,
  type WatcherAuthenticatedReplayTranscript,
  watcherAuthenticatedReplayTranscriptCborHex,
} from "../../src/verification/authenticated-replay-transcript.js";
import { makeWatcherTranscriptArchiveFixture } from "../support/replay-transcript-archive-fixture.js";

const resources = new Set<{ close(): void }>();
const directories: string[] = [];
let first: WatcherAuthenticatedReplayTranscript;
let second: WatcherAuthenticatedReplayTranscript;
let third: WatcherAuthenticatedReplayTranscript;

beforeAll(async () => {
  const { createInput } = await makeWatcherTranscriptArchiveFixture();
  [first, second, third] = (await Promise.all(
    ["peer-1", "peer-2", "peer-3"].map((sourceId) =>
      createWatcherAuthenticatedReplayTranscript({
        ...createInput,
        daProvenance: { ...createInput.daProvenance, sourceId },
      }),
    ),
  )) as [
    WatcherAuthenticatedReplayTranscript,
    WatcherAuthenticatedReplayTranscript,
    WatcherAuthenticatedReplayTranscript,
  ];
}, 60_000);

afterEach(async () => {
  for (const resource of resources) resource.close();
  resources.clear();
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});
const open = async (path?: string) => {
  if (path === undefined) {
    const directory = await mkdtemp("/var/tmp/midgard-replay-transcripts-");
    directories.push(directory);
    path = join(directory, "watcher.sqlite");
  }
  const backend = await openWatcherSqliteDurableBackend({ path });
  resources.add(backend);
  return { backend, path };
};
describe("durable authenticated replay transcript archive", () => {
  it("reclaims a whole completed identity after restart and both horizons while retaining every pending dependency", async () => {
    const { createInput, retentionWindow, retirementObservation } =
      await makeWatcherTranscriptArchiveFixture();
    const { backend, path } = await open();
    const database = new DatabaseSync(path);
    resources.add(database);
    const store = createWatcherSqliteReplayTranscriptStore(database, {
      maximumRows: 2,
    });
    const operations = ["a1".repeat(32), "a2".repeat(32)];
    await store.compareAndSwap({
      expectedTranscriptDigest: null,
      transcript: first,
      lifecycle: {
        header: createInput.header,
        operationDigest: operations[0]!,
        retentionWindow,
        deploymentIdentity: createInput.deploymentIdentity,
      },
    });
    await store.compareAndSwap({
      expectedTranscriptDigest: first.transcriptDigest,
      transcript: second,
      lifecycle: {
        header: createInput.header,
        operationDigest: operations[1]!,
        retentionWindow,
        deploymentIdentity: createInput.deploymentIdentity,
      },
    });
    const completedAtSlot = "4300";
    const completion = (operationDigest: string) =>
      unsafeAdmitWatcherReplayTranscriptCompletionForTest({
        deploymentFingerprint: first.deploymentFingerprint,
        headerHash: first.headerHash,
        operationDigest,
        completedAtSlot,
      });
    const lateSlot = (
      BigInt(completedAtSlot) +
      BigInt(MIDGARD_RETENTION_WINDOW.retentionDays * 86400) +
      1n
    ).toString();
    let sweepBlockNo = "12000";
    const sweep = (
      slot = lateSlot,
      live = false,
      lock: CorrectionLockDatum | null = "Idle",
    ) => ({
      observation: retirementObservation({
        slot,
        live,
        lock,
        blockNo: sweepBlockNo,
      }),
      network: "Preprod" as const,
    });
    expect(await store.retireExpired(sweep())).toBe(0);
    await store.completeOperation(completion(operations[0]!));
    backend.close();
    resources.delete(backend);
    database.close();
    resources.delete(database);
    const reopened = new DatabaseSync(path);
    resources.add(reopened);
    const restarted = createWatcherSqliteReplayTranscriptStore(reopened, {
      maximumRows: 2,
    });
    expect(await restarted.retireExpired(sweep())).toBe(0);
    expect((await restarted.read(first))?.chainLength).toBe(2);
    await expect(
      restarted.compareAndSwap({
        expectedTranscriptDigest: second.transcriptDigest,
        transcript: third,
      }),
    ).rejects.toThrow("storage limits");
    await restarted.completeOperation(completion(operations[1]!));
    expect(await restarted.retireExpired(sweep(completedAtSlot))).toBe(0);
    expect(await restarted.retireExpired(sweep(lateSlot, true))).toBe(0);
    expect(
      await restarted.retireExpired(
        sweep(lateSlot, false, {
          Locked: {
            target_header_hash: first.headerHash,
            correction_identity: {
              AvailabilityChallenge: {
                challenge_asset_name: `44414348${"c7".repeat(28)}`,
              },
            },
          },
        }),
      ),
    ).toBe(0);
    expect(await restarted.retireExpired(sweep(lateSlot, false, null))).toBe(0);
    expect(await restarted.retireExpired(sweep())).toBe(0);
    sweepBlockNo = "14161";
    await expect(
      restarted.completeOperation({ ...completion(operations[0]!) }),
    ).rejects.toThrow("not admitted");
    const pin = reopened
      .prepare(
        "SELECT * FROM watcher_replay_transcript_operation WHERE operation_digest = ?",
      )
      .get(operations[0]!) as {
      identity: string;
      operation_digest: string;
      end_time: string;
      retention_days: number;
      network: string;
      completed_slot: string;
      checksum: string;
      classification_complete: number;
      proof_started: number;
    };
    reopened
      .prepare(
        "DELETE FROM watcher_replay_transcript_operation WHERE operation_digest = ?",
      )
      .run(operations[0]!);
    await expect(restarted.retireExpired(sweep())).rejects.toThrow(
      "incomplete or corrupt",
    );
    expect((await restarted.read(first))?.chainLength).toBe(2);
    reopened
      .prepare(
        "INSERT INTO watcher_replay_transcript_operation VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
      )
      .run(
        pin.identity,
        pin.operation_digest,
        pin.end_time,
        pin.retention_days,
        pin.network,
        pin.completed_slot,
        pin.checksum,
        pin.classification_complete,
        pin.proof_started,
      );
    reopened
      .prepare(
        "UPDATE watcher_replay_transcript_operation SET completed_slot = '0' WHERE operation_digest = ?",
      )
      .run(operations[0]!);
    await expect(restarted.retireExpired(sweep())).rejects.toThrow(
      "lifecycle is corrupt",
    );
    reopened
      .prepare(
        "UPDATE watcher_replay_transcript_operation SET completed_slot = ? WHERE operation_digest = ?",
      )
      .run(pin.completed_slot, operations[0]!);
    reopened.exec(
      "CREATE TRIGGER refuse_retirement BEFORE DELETE ON watcher_replay_transcript_head BEGIN SELECT RAISE(ABORT, 'retirement failure'); END;",
    );
    await expect(restarted.retireExpired(sweep())).rejects.toThrow(
      "retirement failure",
    );
    expect((await restarted.read(first))?.chainLength).toBe(2);
    expect(
      reopened
        .prepare(
          "SELECT count(*) AS count FROM watcher_replay_transcript_operation",
        )
        .get(),
    ).toMatchObject({ count: 2 });
    reopened.exec("DROP TRIGGER refuse_retirement");
    const allocatedPages = reopened.prepare("PRAGMA page_count").get() as {
      page_count: number;
    };
    expect(await restarted.retireExpired(sweep())).toBe(1);
    expect(await restarted.read(first)).toBeNull();
    expect(
      reopened
        .prepare("SELECT count(*) AS count FROM watcher_replay_transcript")
        .get(),
    ).toMatchObject({ count: 0 });
    expect(
      await restarted.compareAndSwap({
        expectedTranscriptDigest: null,
        transcript: third,
      }),
    ).toBe(true);
    expect(
      (reopened.prepare("PRAGMA page_count").get() as { page_count: number })
        .page_count,
    ).toBeLessThanOrEqual(allocatedPages.page_count);
    expect(await restarted.retireExpired(sweep())).toBe(0);
    const legacyOperation = "a3".repeat(32);
    expect(
      await restarted.compareAndSwap({
        expectedTranscriptDigest: third.transcriptDigest,
        transcript: third,
        lifecycle: {
          header: createInput.header,
          deploymentIdentity: createInput.deploymentIdentity,
          retentionWindow,
          operationDigest: legacyOperation,
        },
      }),
    ).toBe(true);
    await restarted.completeOperation(completion(legacyOperation));
    expect(await restarted.retireExpired(sweep())).toBe(0);
    expect((await restarted.read(third))?.chainLength).toBe(1);
  });

  it("archives admitted fresh replays across restart while keeping the original bytes and link", async () => {
    const { backend, path } = await open();
    expect(await backend.replayTranscripts.read(first)).toBeNull();
    expect(
      await backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: null,
        transcript: first,
      }),
    ).toBe(true);
    expect(
      await backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: first.transcriptDigest,
        transcript: first,
      }),
    ).toBe(true);
    expect(
      await backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: first.transcriptDigest,
        transcript: second,
      }),
    ).toBe(true);
    backend.close();
    resources.delete(backend);

    const restarted = (await open(path)).backend;
    const persisted = await restarted.replayTranscripts.read(first);
    expect(persisted).toEqual({
      headTranscriptDigest: second.transcriptDigest,
      previousTranscriptDigest: first.transcriptDigest,
      persistedTranscriptCborHex:
        watcherAuthenticatedReplayTranscriptCborHex(second),
      chainLength: 2,
    });
    expect(() =>
      assertWatcherAuthenticatedReplayTranscript(
        persisted as unknown as WatcherAuthenticatedReplayTranscript,
      ),
    ).toThrow("not admitted");
    await expect(
      restarted.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: second.transcriptDigest,
        transcript: { ...third },
      }),
    ).rejects.toThrow("not admitted");
    await expect(
      restarted.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: second.transcriptDigest,
        transcript: first,
      }),
    ).rejects.toThrow("cannot revive");
    const database = new DatabaseSync(path);
    resources.add(database);
    const original = database
      .prepare(
        "SELECT bytes FROM watcher_replay_transcript WHERE transcript_digest = ?",
      )
      .get(first.transcriptDigest) as { bytes: Uint8Array };
    expect(Buffer.from(original.bytes).toString("hex")).toBe(
      watcherAuthenticatedReplayTranscriptCborHex(first),
    );
    expect(
      database
        .prepare("SELECT root_digest FROM watcher_replay_transcript_head")
        .get(),
    ).toMatchObject({ root_digest: first.transcriptDigest });
    expect(
      await restarted.replayTranscripts.read({
        ...first,
        deploymentFingerprint: "ff".repeat(32),
      }),
    ).toBeNull();
    expect(
      await restarted.replayTranscripts.read({
        ...first,
        inclusionPoint: { ...first.inclusionPoint, blockHash: "ff".repeat(32) },
      }),
    ).toBeNull();
  });

  it("allows one concurrent CAS winner and refuses a stale expected head without appending", async () => {
    const one = await open();
    const two = await open(one.path);
    const results = await Promise.all([
      one.backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: null,
        transcript: first,
      }),
      two.backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: null,
        transcript: second,
      }),
    ]);
    expect(results).toEqual([true, false]);
    expect((await two.backend.replayTranscripts.read(first))?.chainLength).toBe(
      1,
    );
    expect(
      await two.backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: first.transcriptDigest,
        transcript: second,
      }),
    ).toBe(true);
    expect(
      await one.backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: first.transcriptDigest,
        transcript: third,
      }),
    ).toBe(false);
    expect((await one.backend.replayTranscripts.read(first))?.chainLength).toBe(
      2,
    );
  });

  it("rolls back the new archive row when the head write fails", async () => {
    const { backend, path } = await open();
    await backend.replayTranscripts.compareAndSwap({
      expectedTranscriptDigest: null,
      transcript: first,
    });
    const database = new DatabaseSync(path);
    resources.add(database);
    database.exec(`CREATE TRIGGER refuse_head_write
      BEFORE UPDATE ON watcher_replay_transcript_head
      BEGIN SELECT RAISE(ABORT, 'injected head persistence failure'); END`);
    await expect(
      backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: first.transcriptDigest,
        transcript: second,
      }),
    ).rejects.toThrow("injected head persistence failure");
    expect(
      (await backend.replayTranscripts.read(first))?.headTranscriptDigest,
    ).toBe(first.transcriptDigest);
    expect(
      database
        .prepare("SELECT count(*) AS count FROM watcher_replay_transcript")
        .get(),
    ).toMatchObject({ count: 1 });
  });

  it.each([
    "DELETE FROM watcher_replay_transcript_head",
    "DELETE FROM watcher_replay_transcript WHERE previous_digest IS NULL",
    "UPDATE watcher_replay_transcript SET bytes = x'00' WHERE previous_digest IS NULL",
    "UPDATE watcher_replay_transcript SET previous_digest = transcript_digest WHERE previous_digest IS NOT NULL",
    "UPDATE watcher_replay_transcript_head SET current_digest = root_digest, chain_length = 1",
    "UPDATE watcher_replay_transcript_head SET root_digest = current_digest",
  ])(
    "fails closed on missing, corrupted, cyclic, or rewound history: %s",
    async (corruption) => {
      const { backend, path } = await open();
      await backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: null,
        transcript: first,
      });
      await backend.replayTranscripts.compareAndSwap({
        expectedTranscriptDigest: first.transcriptDigest,
        transcript: second,
      });
      const database = new DatabaseSync(path);
      resources.add(database);
      database.exec(corruption);
      await expect(backend.replayTranscripts.read(first)).rejects.toThrow(
        /watcher replay transcript/u,
      );
      await expect(
        backend.replayTranscripts.compareAndSwap({
          expectedTranscriptDigest: second.transcriptDigest,
          transcript: third,
        }),
      ).rejects.toThrow(/watcher replay transcript/u);
    },
  );

  it.each(["maximumChainLength", "maximumRows", "maximumTotalBytes"] as const)(
    "enforces %s without dropping earlier rows",
    async (limit) => {
      const database = new DatabaseSync(":memory:");
      resources.add(database);
      const firstBytes =
        watcherAuthenticatedReplayTranscriptCborHex(first).length / 2;
      const limits: Partial<WatcherReplayTranscriptStorageLimits> = {
        [limit]: limit === "maximumTotalBytes" ? firstBytes : 1,
      };
      const store = createWatcherSqliteReplayTranscriptStore(database, limits);
      expect(
        await store.compareAndSwap({
          expectedTranscriptDigest: null,
          transcript: first,
        }),
      ).toBe(true);
      await expect(
        store.compareAndSwap({
          expectedTranscriptDigest: first.transcriptDigest,
          transcript: second,
        }),
      ).rejects.toThrow("exceeds storage limits");
      expect((await store.read(first))?.persistedTranscriptCborHex).toBe(
        watcherAuthenticatedReplayTranscriptCborHex(first),
      );
      expect(
        database
          .prepare("SELECT count(*) AS count FROM watcher_replay_transcript")
          .get(),
      ).toMatchObject({ count: 1 });
    },
  );

  it("rejects oversized writes and invalid identities before an archive is created", async () => {
    const database = new DatabaseSync(":memory:");
    resources.add(database);
    const store = createWatcherSqliteReplayTranscriptStore(database, {
      maximumTranscriptBytes: 1,
    });
    await expect(
      store.compareAndSwap({
        expectedTranscriptDigest: null,
        transcript: first,
      }),
    ).rejects.toThrow("exceeds its bound");
    await expect(store.read({ ...first, headerHash: "" })).rejects.toThrow(
      "identity is invalid",
    );
    expect(
      database
        .prepare("SELECT count(*) AS count FROM watcher_replay_transcript")
        .get(),
    ).toMatchObject({ count: 0 });
  });
});
