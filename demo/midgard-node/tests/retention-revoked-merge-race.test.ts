import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { beforeAll, expect, it } from "vitest";

import {
  DaPayloadsDB,
  DaPayloadTerminalOutcomesDB,
} from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { pruneFinalizedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import { computeChallengeableCutoff } from "../src/database/retention-policy.js";
import { createDatabaseStateQueueCorrectionObserverStore } from "../src/services/state-queue-correction-observer.js";
import {
  makeState,
  STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
} from "../src/services/state-queue-correction-observer.parse-state-queue-correction-observer-state.js";
import { reconcileStateQueueCorrectionObserver } from "../src/services/state-queue-correction-observer.reconcile-state-queue-correction-observer.js";
import { recordMergeJob } from "./history-retention-prune.fixtures.js";
import { journalFixture } from "./local-mutation-job-abandonment.journal-fixture.js";
import {
  daPayloadFixture,
  deploymentManifest,
  NOW,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";
import { seedPublished } from "./retention-enforcement.terminal-merge.js";
import { deterministicFixtureBytes, provideDatabaseLayers } from "./utils.js";
const clear = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`DELETE FROM state_queue_terminal_observer_states`;
  yield* sql`TRUNCATE TABLE pending_block_finalizations, local_mutation_jobs,
    event_history_authority, event_history_recovery_plans RESTART IDENTITY CASCADE`;
  yield* DaPayloadTerminalOutcomesDB.clear;
  yield* DaPayloadsDB.clear;
});
const run = <A>(work: Effect.Effect<A, unknown, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(work) as Effect.Effect<A, never>);
beforeAll(async () => {
  await run(
    MigrationRunner.migrate({
      appVersion: "review",
      actor: "review-da-merge-race",
    }),
  );
}, 120_000);
it.each(["pre-rollback", "post-save", "restored"] as const)(
  "retains a revived live header with a %s topology view",
  async (topology) => {
    const outcome = await run(
      clear.pipe(
        Effect.zipRight(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const seeded = yield* seedPublished(
              new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY),
            );
            const transition = seeded.transition;
            const unrelatedHeader = deterministicFixtureBytes(
              "stable-aged-unrelated-payload",
              28,
            );
            yield* DaPayloadsDB.upsertAvailable({
              ...daPayloadFixture(
                "stable-aged-unrelated-payload",
                new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY),
              ),
              [DaPayloadsDB.Columns.HEADER_HASH]: unrelatedHeader,
            });
            // Real producer journals survive confirmation and their completed merge
            // jobs. A subsequent finalized journal supplies the newest-boundary hold.
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
            const store = createDatabaseStateQueueCorrectionObserverStore({
              sql,
              deploymentManifest,
            });
            yield* Effect.promise(() =>
              store.save(
                makeState({
                  schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
                  deploymentIdentityDigest: deploymentManifest.manifestId,
                  stateQueuePolicyId:
                    deploymentManifest.contracts.stateQueueMint.scriptHash,
                  cursorQueue: transition.nextQueue,
                  pending: [],
                  admitted: [transition],
                  retractedTransactionHashes: [],
                  postFinalityRollbackIncidents: [],
                }),
              ),
            );
            // A legitimate pre-rollback topology snapshot excludes this formerly merged header.
            const earlierView = {
              confirmedHeadHash: laterHeaderHash,
              liveQueueHeaderHashes:
                topology === "restored" ? [seeded.headerHash] : [],
              retirementProofs: [],
              retirementProofUnavailable: true,
            };
            let deleted = -1;
            yield* Effect.promise(() =>
              reconcileStateQueueCorrectionObserver({
                deploymentIdentityDigest: deploymentManifest.manifestId,
                stateQueuePolicyId:
                  deploymentManifest.contracts.stateQueueMint.scriptHash,
                requiredFinalityDepth: BigInt(
                  deploymentManifest.l1Finality.confirmationDepth,
                ),
                source: {
                  readQueue: async () => transition.previousQueue,
                  canonicalDepth: async () => null,
                  observeTransitions: async () => {
                    throw new Error(
                      "Unexpected replay after exact restoration",
                    );
                  },
                },
                store,
                provenFinal: new Set(),
                reinclude: async () => undefined,
                restoreAfterRollback: async () => {
                  throw new Error("Merge should not displace journals");
                },
                revokeTerminal: async (revoked) => {
                  await run(
                    DaPayloadTerminalOutcomesDB.revokeAuthenticatedTransition(
                      revoked,
                      deploymentManifest,
                    ),
                  );
                  // This is the actual production ordering: terminal revoke precedes observer save.
                  if (topology !== "post-save")
                    deleted = await run(
                      DaPayloadsDB.pruneBeyondRetention({
                        challengeableCutoff: computeChallengeableCutoff(NOW),
                        view: earlierView,
                        deploymentIdentityDigest: Buffer.from(
                          deploymentManifest.manifestId,
                          "hex",
                        ),
                      }),
                    );
                },
              }),
            );
            if (topology === "post-save")
              deleted = yield* DaPayloadsDB.pruneBeyondRetention({
                challengeableCutoff: computeChallengeableCutoff(NOW),
                view: earlierView,
                deploymentIdentityDigest: Buffer.from(
                  deploymentManifest.manifestId,
                  "hex",
                ),
              });
            // The observer has now saved the rollback (admitted H was removed). The
            // same already-running sweep next executes its history-prune statement.
            const journalsDeleted = yield* pruneFinalizedBeyondChallengeability(
              {
                challengeableCutoff: computeChallengeableCutoff(NOW),
                view: earlierView,
                deploymentIdentityDigest: Buffer.from(
                  deploymentManifest.manifestId,
                  "hex",
                ),
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

it("retires stable superseded admitted terminal bytes only with an exact current proof", async () => {
  const result = await run(
    clear.pipe(
      Effect.zipRight(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const first = yield* seedPublished(
            new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY),
          );
          const latest = yield* seedPublished(NOW, 2);
          const store = createDatabaseStateQueueCorrectionObserverStore({
            sql,
            deploymentManifest,
          });
          yield* Effect.promise(() =>
            store.save(
              makeState({
                schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
                deploymentIdentityDigest: deploymentManifest.manifestId,
                stateQueuePolicyId:
                  deploymentManifest.contracts.stateQueueMint.scriptHash,
                cursorQueue: latest.transition.nextQueue,
                pending: [],
                admitted: [first.transition, latest.transition],
                retractedTransactionHashes: [],
                postFinalityRollbackIncidents: [],
              }),
            ),
          );
          const bytes = Option.getOrThrow(
            yield* DaPayloadsDB.retrieveByHeaderHash(first.headerHash),
          );
          const proof = {
            headerHash: first.headerHash.toString("hex"),
            payloadSha256: bytes.payload_sha256.toString("hex"),
            transactionHash: first.transition.transactionHash,
            blockHash: first.transition.blockHash,
            transitionDigest: first.transition.transitionDigest,
          };
          const prune = (proofs: (typeof proof)[]) =>
            DaPayloadsDB.pruneBeyondRetention({
              challengeableCutoff: computeChallengeableCutoff(NOW),
              view: {
                confirmedHeadHash: latest.headerHash,
                liveQueueHeaderHashes: [],
                retirementProofs: proofs,
              },
              deploymentIdentityDigest: Buffer.from(
                deploymentManifest.manifestId,
                "hex",
              ),
            });
          const stale = yield* prune([
            { ...proof, transactionHash: "ff".repeat(32) },
          ]);
          const current = yield* prune([proof]);
          return {
            stale,
            current,
            payload: yield* DaPayloadsDB.retrieveByHeaderHash(first.headerHash),
          };
        }),
      ),
      Effect.ensuring(Effect.orDie(clear)),
    ),
  );
  expect({
    stale: result.stale,
    current: result.current,
    payload: result.payload._tag,
  }).toEqual({ stale: 0, current: 1, payload: "None" });
});

it.each(["foreign identity", "null root"] as const)(
  "does not hold an unrelated eligible payload for a %s cursor",
  async (kind) => {
    const result = await run(
      clear.pipe(
        Effect.zipRight(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const bytes = daPayloadFixture(
              `cursor-${kind}`,
              new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY),
            );
            yield* DaPayloadsDB.upsertAvailable(bytes);
            const identity =
              kind === "foreign identity"
                ? "fe".repeat(32)
                : deploymentManifest.manifestId;
            const state = makeState({
              schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
              deploymentIdentityDigest: identity,
              stateQueuePolicyId:
                deploymentManifest.contracts.stateQueueMint.scriptHash,
              cursorQueue: [
                { headerHash: null, outRef: `${"00".repeat(32)}#0` },
                ...(kind === "foreign identity"
                  ? [
                      {
                        headerHash: bytes.header_hash.toString("hex"),
                        outRef: `${"01".repeat(32)}#0`,
                      },
                    ]
                  : []),
              ],
              pending: [],
              admitted: [],
              retractedTransactionHashes: [],
              postFinalityRollbackIncidents: [],
            });
            yield* sql`INSERT INTO state_queue_terminal_observer_states (
        deployment_identity_digest,state_queue_policy_id,state_digest,state_record
      ) VALUES (${Buffer.from(identity, "hex")},${Buffer.from(state.stateQueuePolicyId, "hex")},
        ${Buffer.from(state.stateDigest, "hex")},${JSON.stringify(state)})`;
            const deleted = yield* DaPayloadsDB.pruneBeyondRetention({
              challengeableCutoff: computeChallengeableCutoff(NOW),
              view: {
                confirmedHeadHash: deterministicFixtureBytes(
                  "unrelated-confirmed",
                  28,
                ),
                liveQueueHeaderHashes: [],
              },
              deploymentIdentityDigest: Buffer.from(
                deploymentManifest.manifestId,
                "hex",
              ),
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
  },
);
