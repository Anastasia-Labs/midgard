import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeAll, expect, it } from "vitest";

import { DaPayloadsDB } from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { pruneFinalizedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import { computeChallengeableCutoff } from "../src/database/retention-policy.js";
import { insertLiveQueueNode } from "./helpers/queue-terminal-rows.js";
import { recordMergeJob } from "./history-retention-prune.fixtures.js";
import { journalFixture } from "./local-mutation-job-abandonment.journal-fixture.js";
import {
  daPayloadFixture,
  deploymentManifest,
  NOW,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";
import {
  FINAL_THROUGH,
  seedPublished,
} from "./retention-enforcement.terminal-merge.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const clear = resetApplicationTables;
const run = <A>(work: Effect.Effect<A, unknown, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(work) as Effect.Effect<A, never>);
const identity = () => Buffer.from(deploymentManifest.manifestId, "hex");
const OLD = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);

beforeAll(async () => {
  await run(
    MigrationRunner.migrate({
      appVersion: "review",
      actor: "review-da-merge-race",
    }),
  );
}, 120_000);

/**
 * A rollback deletes a landed merge's terminal row and puts the header's
 * node back in the follower's facts. A sweep that read its L1 view before
 * the rollback (the header neither queued nor the confirmed head) still keeps
 * the payload and the journal: both prunes re-read the facts in the
 * statement that deletes.
 */
it.each(["pre-rollback", "restored"] as const)(
  "retains a header a rollback put back in the queue with a %s topology view",
  async (topology) => {
    const outcome = await run(
      clear.pipe(
        Effect.zipRight(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const seeded = yield* seedPublished(OLD);
            const unrelatedHeader = deterministicFixtureBytes(
              "stable-aged-unrelated-payload",
              28,
            );
            yield* DaPayloadsDB.upsertAvailable({
              ...daPayloadFixture("stable-aged-unrelated-payload", OLD),
              [DaPayloadsDB.Columns.HEADER_HASH]: unrelatedHeader,
            });
            // Producer journals survive confirmation and their completed
            // merge jobs. A later finalized journal is the newest-boundary
            // hold.
            const laterHeaderHash = deterministicFixtureBytes(
              "later-head-before-rollback",
              28,
            );
            for (const [hash, ageDays] of [
              [seeded.headerHash, 40],
              [laterHeaderHash, 0],
            ] as const) {
              const fixture = journalFixture(hash);
              yield* PendingBlockFinalizationsDB.preparePendingSubmission({
                ...fixture,
                metadata: {
                  ...fixture.metadata,
                  baseTailOutRef: `${hash.toString("hex")}#0`,
                },
              });
              yield* sql`UPDATE pending_block_finalizations SET status = 'locally_applied',
        block_end_time = ${new Date(NOW.getTime() - ageDays * RETENTION_MS_PER_DAY)}
        WHERE header_hash = ${hash}`;
              yield* recordMergeJob(hash, "completed");
            }
            // The view read before the rollback excludes the formerly merged
            // header.
            const view = {
              confirmedHeadHash: laterHeaderHash,
              liveQueueHeaderHashes:
                topology === "restored" ? [seeded.headerHash] : [],
              finalThroughHeight: FINAL_THROUGH,
            };
            // The rollback: the follower's rewind deletes the merge row, and
            // the header's node is live again.
            yield* sql`DELETE FROM node_l1_queue_terminals
              WHERE header_hash = ${seeded.headerHash}`;
            yield* insertLiveQueueNode(seeded.headerHash);
            const deleted = yield* DaPayloadsDB.pruneBeyondRetention({
              challengeableCutoff: computeChallengeableCutoff(NOW),
              view,
              deploymentIdentityDigest: identity(),
            });
            const journalsDeleted = yield* pruneFinalizedBeyondChallengeability(
              {
                challengeableCutoff: computeChallengeableCutoff(NOW),
                view,
                deploymentIdentityDigest: identity(),
              },
            );
            const journals =
              yield* sql`SELECT header_hash FROM pending_block_finalizations WHERE header_hash = ${seeded.headerHash}`;
            return {
              deleted,
              journalsDeleted,
              journalsRemaining: journals.length,
              unrelated:
                yield* DaPayloadsDB.retrieveByHeaderHash(unrelatedHeader),
              payload: yield* DaPayloadsDB.retrieveByHeaderHash(
                seeded.headerHash,
              ),
            };
          }),
        ),
        Effect.ensuring(Effect.orDie(clear)),
      ),
    );
    expect({
      deleted: outcome.deleted,
      payload: outcome.payload._tag,
      journalsDeleted: outcome.journalsDeleted,
      journalsRemaining: outcome.journalsRemaining,
      unrelated: outcome.unrelated._tag,
    }).toEqual({
      deleted: 1,
      payload: "Some",
      journalsDeleted: 0,
      journalsRemaining: 1,
      unrelated: "None",
    });
  },
);

it("does not hold an unrelated eligible payload for another header's live node", async () => {
  const result = await run(
    clear.pipe(
      Effect.zipRight(
        Effect.gen(function* () {
          const bytes = daPayloadFixture("live-node-elsewhere", OLD);
          yield* DaPayloadsDB.upsertAvailable(bytes);
          yield* insertLiveQueueNode(
            deterministicFixtureBytes("some-other-live-header", 28),
          );
          const deleted = yield* DaPayloadsDB.pruneBeyondRetention({
            challengeableCutoff: computeChallengeableCutoff(NOW),
            view: {
              confirmedHeadHash: deterministicFixtureBytes(
                "unrelated-confirmed",
                28,
              ),
              liveQueueHeaderHashes: [],
              finalThroughHeight: FINAL_THROUGH,
            },
            deploymentIdentityDigest: identity(),
          });
          return {
            deleted,
            payload: yield* DaPayloadsDB.retrieveByHeaderHash(
              bytes.header_hash,
            ),
          };
        }),
      ),
      Effect.ensuring(Effect.orDie(clear)),
    ),
  );
  expect({ deleted: result.deleted, payload: result.payload._tag }).toEqual({
    deleted: 1,
    payload: "None",
  });
});
