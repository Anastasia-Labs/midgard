import { createHash } from "node:crypto";
import { DatabaseSync } from "node:sqlite";

import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it, vi } from "vitest";

import { archiveWatcherValidationCapture } from "../../src/fault-proofs/fault-proof-application.archive-validation-capture.js";
import { watcherReplayTranscriptClassification } from "../../src/storage/replay-transcript-completion.js";
import { createWatcherSqliteReplayTranscriptStore } from "../../src/storage/replay-transcript-store.js";
import {
  createWatcherAuthenticatedReplayTranscript,
  replayWatcherAuthenticatedReplayTranscript,
  WATCHER_AUTHENTICATED_REPLAY_TRANSCRIPT,
  type WatcherAuthenticatedReplayTranscript,
} from "../../src/verification/authenticated-replay-transcript.js";
import {
  classifyValidationCapture,
  setupValidationCapture,
} from "../support/validation-capture-fixture.js";

// Count fresh captures and persisted-head replays; both still run for real.
// The config's mockReset clears the counts before each test.
vi.mock(
  "../../src/verification/authenticated-replay-transcript.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../../src/verification/authenticated-replay-transcript.js")
      >();
    return {
      ...actual,
      createWatcherAuthenticatedReplayTranscript: vi.fn(
        actual.createWatcherAuthenticatedReplayTranscript,
      ),
      replayWatcherAuthenticatedReplayTranscript: vi.fn(
        actual.replayWatcherAuthenticatedReplayTranscript,
      ),
    };
  },
);

const PRE_FOLLOWER_TRANSCRIPT =
  "midgard-watcher-production-authenticated-replay-transcript-v1";
const sha256 = (bytes: Uint8Array | string) =>
  createHash("sha256").update(bytes).digest("hex");

/**
 * Rewrites the store's sole archived head in place as the transcript a
 * pre-follower watcher persisted for the same header: the v1 schema, its own
 * digest, and the row and head digests the store audits.
 */
const persistPreFollowerHead = (
  database: DatabaseSync,
  transcript: WatcherAuthenticatedReplayTranscript,
): string => {
  const { transcriptDigest: _current, ...current } = transcript;
  const material = { ...current, schemaVersion: PRE_FOLLOWER_TRANSCRIPT };
  const digest = computeDeploymentManifestJsonDigest(material);
  const bytes = encodeCbor({ ...material, transcriptDigest: digest });
  const point = transcript.inclusionPoint;
  const key = JSON.stringify([
    transcript.deploymentFingerprint,
    transcript.headerHash,
    point.transactionHash,
    point.blockHash,
    point.blockNo,
    point.slot,
    point.chainPointId,
  ]);
  const bytesDigest = sha256(bytes);
  const row = database
    .prepare(
      "UPDATE watcher_replay_transcript SET transcript_digest = ?, bytes_sha256 = ?, record_sha256 = ?, bytes = ? WHERE identity = ? AND previous_digest IS NULL",
    )
    .run(
      digest,
      bytesDigest,
      sha256(JSON.stringify([key, digest, null, bytesDigest])),
      bytes,
      key,
    );
  const head = database
    .prepare(
      "UPDATE watcher_replay_transcript_head SET root_digest = ?, current_digest = ? WHERE identity = ? AND chain_length = 1",
    )
    .run(digest, digest, key);
  if (row.changes !== 1 || head.changes !== 1)
    throw new Error("the archive did not hold exactly one transcript");
  return digest;
};

/** The capture an archive call made; a hold fails the call. */
const captured = async (
  input: Parameters<typeof archiveWatcherValidationCapture>[0],
) => {
  const archived = await archiveWatcherValidationCapture(input);
  if (archived.kind !== "captured")
    throw new Error(`expected a capture: ${archived.detail}`);
  return archived.capture;
};

const operationDigests = (database: DatabaseSync) =>
  (
    database
      .prepare(
        "SELECT operation_digest FROM watcher_replay_transcript_operation",
      )
      .all() as { operation_digest: string }[]
  ).map((row) => row.operation_digest);

describe("archiving a validation capture over a pre-follower transcript head", () => {
  it.each(["normal", "forced"] as const)(
    "recaptures the %s header fresh on top of the v1 head, then replays the v2 head",
    async (kind) => {
      const context = await setupValidationCapture(kind);
      const database = new DatabaseSync(":memory:");
      try {
        const store = createWatcherSqliteReplayTranscriptStore(database);
        const observed = await context.queue.observeFresh();
        const decision = await classifyValidationCapture(context, observed);
        const input = {
          deploymentAuthority: context.deploymentAuthority,
          stateQueueObservation: observed.observation,
          header: observed.header,
          decision,
          userEvents: observed.follower.userEvents,
          replayTranscriptStore: store,
        };
        const identity = {
          deploymentFingerprint:
            context.deploymentAuthority.deploymentIdentity.manifestId,
          headerHash: observed.header.headerHash,
          inclusionPoint: {
            transactionHash: observed.header.observedTransactionHash,
            blockHash: observed.header.observedBlockHash,
            blockNo: observed.header.observedBlockNo,
            slot: observed.header.observedSlot,
            chainPointId: observed.header.observedChainPointId,
          },
        };
        // The pre-follower watcher archived this header and pinned its
        // decision; its transcript is v1.
        const original = await captured(input);
        const preFollower = persistPreFollowerHead(
          database,
          original.transcript,
        );
        expect(await store.read(identity)).toMatchObject({
          headTranscriptDigest: preFollower,
          previousTranscriptDigest: null,
          chainLength: 1,
        });
        expect(operationDigests(database)).toEqual([decision.decisionDigest]);
        vi.mocked(createWatcherAuthenticatedReplayTranscript).mockClear();
        vi.mocked(replayWatcherAuthenticatedReplayTranscript).mockClear();

        const recaptured = await captured(input);
        expect(
          createWatcherAuthenticatedReplayTranscript,
        ).toHaveBeenCalledOnce();
        expect(
          replayWatcherAuthenticatedReplayTranscript,
        ).not.toHaveBeenCalled();
        expect(recaptured.transcript.schemaVersion).toBe(
          WATCHER_AUTHENTICATED_REPLAY_TRANSCRIPT,
        );
        expect(recaptured.transcript.headerHash).toBe(
          original.transcript.headerHash,
        );
        expect(recaptured.transcript.inclusionPoint).toEqual(
          original.transcript.inclusionPoint,
        );
        expect(recaptured.transcript.coordinate).toEqual(
          original.transcript.coordinate,
        );
        expect(recaptured.transcript.eventAuthorityRecordsCborHex).toHaveLength(
          kind === "forced" ? 1 : 0,
        );
        // The decision is the one already pinned; no other operation is.
        expect(recaptured.decisionDigest).toBe(decision.decisionDigest);
        expect(operationDigests(database)).toEqual([decision.decisionDigest]);
        expect(await store.read(identity)).toMatchObject({
          headTranscriptDigest: recaptured.transcript.transcriptDigest,
          previousTranscriptDigest: preFollower,
          chainLength: 2,
        });
        vi.mocked(createWatcherAuthenticatedReplayTranscript).mockClear();

        const replayed = await captured(input);
        expect(
          replayWatcherAuthenticatedReplayTranscript,
        ).toHaveBeenCalledOnce();
        expect(
          createWatcherAuthenticatedReplayTranscript,
        ).not.toHaveBeenCalled();
        expect(replayed.transcript.transcriptDigest).toBe(
          recaptured.transcript.transcriptDigest,
        );
        expect(await store.read(identity)).toMatchObject({
          headTranscriptDigest: recaptured.transcript.transcriptDigest,
          previousTranscriptDigest: preFollower,
          chainLength: 2,
        });
        expect(operationDigests(database)).toEqual([decision.decisionDigest]);
        await observed.close();
      } finally {
        database.close();
        await context.close();
      }
    },
    180_000,
  );
});

describe("archiving over a pre-follower transcript head whose proof is open", () => {
  it.each(["normal", "forced"] as const)(
    "holds the %s header: no recapture, no re-key, the v1 head stays, at every start",
    async (kind) => {
      const context = await setupValidationCapture(kind);
      const database = new DatabaseSync(":memory:");
      try {
        const store = createWatcherSqliteReplayTranscriptStore(database);
        const observed = await context.queue.observeFresh();
        const decision = await classifyValidationCapture(context, observed);
        const input = {
          deploymentAuthority: context.deploymentAuthority,
          stateQueueObservation: observed.observation,
          header: observed.header,
          decision,
          userEvents: observed.follower.userEvents,
          replayTranscriptStore: store,
        };
        const identity = {
          deploymentFingerprint:
            context.deploymentAuthority.deploymentIdentity.manifestId,
          headerHash: observed.header.headerHash,
          inclusionPoint: {
            transactionHash: observed.header.observedTransactionHash,
            blockHash: observed.header.observedBlockHash,
            blockNo: observed.header.observedBlockNo,
            slot: observed.header.observedSlot,
            chainPointId: observed.header.observedChainPointId,
          },
        };
        // The pre-follower watcher archived this header, classified it and
        // handed its challenge to a proof, which has not completed.
        const original = await captured(input);
        await store.completeClassification(
          watcherReplayTranscriptClassification(original),
        );
        expect(await store.proofOperationOpen(identity)).toBe(false);
        await store.beginProofOperation(
          watcherReplayTranscriptClassification(original),
        );
        expect(await store.proofOperationOpen(identity)).toBe(true);
        const preFollower = persistPreFollowerHead(
          database,
          original.transcript,
        );
        vi.mocked(createWatcherAuthenticatedReplayTranscript).mockClear();
        vi.mocked(replayWatcherAuthenticatedReplayTranscript).mockClear();

        for (let start = 0; start < 2; start += 1) {
          const held = await archiveWatcherValidationCapture(input);
          expect(held).toMatchObject({
            kind: "held_pre_follower",
            preFollowerTranscriptDigest: preFollower,
          });
          expect(held.kind === "held_pre_follower" && held.detail).toMatch(
            /held until the header leaves the finalized queue/u,
          );
          expect(
            createWatcherAuthenticatedReplayTranscript,
          ).not.toHaveBeenCalled();
          expect(
            replayWatcherAuthenticatedReplayTranscript,
          ).not.toHaveBeenCalled();
          expect(await store.read(identity)).toMatchObject({
            headTranscriptDigest: preFollower,
            previousTranscriptDigest: null,
            chainLength: 1,
          });
          expect(operationDigests(database)).toEqual([decision.decisionDigest]);
          expect(await store.proofOperationOpen(identity)).toBe(true);
        }
        await observed.close();
      } finally {
        database.close();
        await context.close();
      }
    },
    180_000,
  );
});
