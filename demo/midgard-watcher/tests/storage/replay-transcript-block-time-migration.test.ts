import { createHash } from "node:crypto";
import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it } from "vitest";

import { unsafeAdmitWatcherReplayTranscriptClassificationForTest } from "../../src/storage/replay-transcript-completion.js";
import { createWatcherSqliteReplayTranscriptStore } from "../../src/storage/replay-transcript-store.js";
import { createWatcherAuthenticatedReplayTranscript } from "../../src/verification/authenticated-replay-transcript.js";
import { makeWatcherTranscriptArchiveFixture } from "../support/replay-transcript-archive-fixture.js";

const resources: DatabaseSync[] = [];
afterEach(() => resources.splice(0).forEach((database) => database.close()));
const hash = (fields: readonly unknown[]) =>
  createHash("sha256").update(JSON.stringify(fields)).digest("hex");

const legacyArchive = async (corrupt = false) => {
  const fixture = await makeWatcherTranscriptArchiveFixture();
  const transcript = await createWatcherAuthenticatedReplayTranscript(
    fixture.createInput,
  );
  const database = new DatabaseSync(":memory:");
  resources.push(database);
  const store = createWatcherSqliteReplayTranscriptStore(database);
  const operationDigest = "a7".repeat(32);
  await store.compareAndSwap({
    expectedTranscriptDigest: null,
    transcript,
    lifecycle: {
      header: fixture.createInput.header,
      deploymentIdentity: fixture.createInput.deploymentIdentity,
      retentionWindow: fixture.retentionWindow,
      operationDigest,
    },
  });
  await store.completeClassification(
    unsafeAdmitWatcherReplayTranscriptClassificationForTest({
      deploymentFingerprint: transcript.deploymentFingerprint,
      headerHash: transcript.headerHash,
      operationDigest,
      headTranscriptDigest: transcript.transcriptDigest,
    }),
  );
  const pin = database
    .prepare("SELECT * FROM watcher_replay_transcript_operation")
    .get()!;
  // The prior format knew the coverage end but carried no authenticated commit clock.
  database.exec(
    "ALTER TABLE watcher_replay_transcript_operation DROP COLUMN block_time",
  );
  const checksum = corrupt
    ? "ff".repeat(32)
    : hash([
        pin.identity,
        pin.operation_digest,
        pin.end_time,
        pin.retention_days,
        pin.network,
        pin.completed_slot,
        pin.classification_complete,
        pin.proof_started,
      ]);
  database
    .prepare("UPDATE watcher_replay_transcript_operation SET checksum = ?")
    .run(checksum);
  database
    .prepare("UPDATE watcher_replay_transcript_lifecycle SET pins_digest = ?")
    .run(hash([pin.identity, 0, hash([[pin.operation_digest, checksum]])]));
  return { ...fixture, database, transcript, checksum };
};

describe("replay transcript commit-clock migration", () => {
  it("preserves an old archive and never infers its unknown commit clock from coverage", async () => {
    const { database, transcript, retirementObservation, retentionWindow } =
      await legacyArchive();
    const store = createWatcherSqliteReplayTranscriptStore(database);
    expect(
      database
        .prepare(
          "SELECT end_time, block_time FROM watcher_replay_transcript_operation",
        )
        .get(),
    ).toMatchObject({ end_time: "2", block_time: null });
    expect(await store.read(transcript)).not.toBeNull();
    const slot = (
      4300n +
      BigInt(retentionWindow.retentionDays * 86400) +
      1n
    ).toString();
    for (const blockNo of ["12000", "14161", "16322"]) {
      expect(
        await store.retireExpired({
          observation: retirementObservation({ slot, blockNo }),
          network: "Preprod",
        }),
      ).toBe(0);
    }
    expect(await store.read(transcript)).not.toBeNull();
  });

  it("refuses a corrupt prior pin atomically without rewriting its clock or checksum", async () => {
    const { database, checksum } = await legacyArchive(true);
    expect(() => createWatcherSqliteReplayTranscriptStore(database)).toThrow(
      "lifecycle is corrupt",
    );
    expect(
      database
        .prepare("PRAGMA table_info(watcher_replay_transcript_operation)")
        .all()
        .some((column) => column.name === "block_time"),
    ).toBe(false);
    expect(
      database
        .prepare("SELECT checksum FROM watcher_replay_transcript_operation")
        .get(),
    ).toMatchObject({ checksum });
    expect(
      database
        .prepare("SELECT count(*) AS count FROM watcher_replay_transcript_head")
        .get(),
    ).toMatchObject({ count: 1 });
  });
});
