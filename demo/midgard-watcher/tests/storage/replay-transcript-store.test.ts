import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterEach, beforeAll, describe, expect, it } from "vitest";

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
