import { randomUUID } from "node:crypto";
import { hostname } from "node:os";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Either, Metric, Option, Schedule } from "effect";

import {
  type DaProducerCommitteePeer,
  loadDaProducerPublicationManifestFromEnv,
  publishDaPayloadAnnouncementFromEnv,
  reconcileDaPayloadPeerFromEnv,
} from "../da/libp2p-producer.js";
import {
  DaPayloadAnnouncementsDB,
  DaPayloadPublicationsDB,
  DaPayloadsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { deploymentIdentityDigestOf } from "../l1-queue-terminals/index.js";
import { NodeConfig } from "../services/config.js";
import type { Database } from "../services/database.js";
import { Globals } from "../services/globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import { ContractDeploymentIdentity } from "../services/midgard-contracts.js";

const daPublishReconcilerAttemptsCounter = Metric.counter(
  "da_publish_reconciler_attempts_total",
  { description: "Durable DA publication reconciliation attempts" },
);
const daPublishReconcilerBacklogGauge = Metric.gauge(
  "da_publish_reconciler_backlog",
  {
    description:
      "Incomplete durable DA peer publication rows; while the DA manifest is unavailable, the payloads awaiting outbox seeding",
  },
);

/** The source `da_publication_manifest_unavailable` is raised under. */
const DA_PUBLICATION_RECONCILER_SOURCE = "da_publication_reconciler";

/**
 * The DA manifest a reconciler pass read is missing or invalid: the pass
 * seeds and delivers nothing, and the next pass reads the manifest again.
 * Nothing owed is lost meanwhile: the landed-block rebase stores an own
 * block's payload with no manifest read, and the first pass with a valid
 * manifest clears the reason, seeds the payloads stored since the last seed
 * and delivers them.
 */
export const DA_PUBLICATION_MANIFEST_UNAVAILABLE =
  "da_publication_manifest_unavailable";
/** How long `da_publication_manifest_unavailable` stays raised before it is
 * escalated: ten minutes (twenty passes at the default reconcile interval).
 * A manifest read while it is being replaced fails one pass and the next
 * clears it, so a failure that outlasts this is a manifest left missing or
 * invalid. */
const DA_PUBLICATION_MANIFEST_UNAVAILABLE_ESCALATION_MS = 10 * 60_000;

/**
 * How far outbox seeding has reached. The reconciler fiber holds one across
 * passes, so a restart begins from a fresh one, `{}`.
 */
export type DaPublicationSeedState = {
  /** The peer set and retention window the last seed covered. */
  key?: string;
  /** Database time at which the last seed began. */
  seededFrom?: Date;
};

/**
 * A payload whose insert began up to this long before a seed and committed
 * after it is still seeded by the next one.
 *
 * Delivery of a stored payload depends on this bound. `da_payloads.created_at`
 * is stamped with its inserting transaction's start (`DEFAULT now()`), not
 * its commit. A payload whose transaction (the landed-block rebase, for an
 * own block) commits more than `SEED_OVERLAP_MS` after it began, with a seed
 * run while it was open, is older than every later incremental seed's
 * cursor: it gets no outbox row, so no delivery, until the next full seed,
 * which is the first pass after a restart or after the committee's peer set
 * or the retention window changes. Nothing bounds that transaction's length.
 * Selecting every retained payload with no outbox row in each pass instead
 * would close this, but costs a scan of the whole retention window per pass
 * and per peer (Postgres plans a hash anti join over both tables: about
 * 0.2 s per statement at 300,000 retained payloads, against about 5 ms for
 * the cursor's index range), which the cursor exists to avoid.
 */
const SEED_OVERLAP_MS = 60 * 60 * 1_000;

/** Where an incremental seed from `state` starts: the last seed less
 * `SEED_OVERLAP_MS`; none before the first seed. */
const seedCursor = (state: DaPublicationSeedState): Date | undefined =>
  state.seededFrom === undefined
    ? undefined
    : new Date(state.seededFrom.getTime() - SEED_OVERLAP_MS);

/**
 * Seeds the peer publication and announcement outboxes. The first seed, and
 * the first after the peer set or retention window changes, covers the whole
 * retention window, so a new peer receives every retained payload; later
 * seeds cover only payloads created since the last one began, less
 * `SEED_OVERLAP_MS`.
 */
export const seedPublicationOutboxes = (
  state: DaPublicationSeedState,
  {
    peers,
    retentionDays,
  }: {
    readonly peers: readonly DaProducerCommitteePeer[];
    readonly retentionDays: number;
  },
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const key = JSON.stringify({
      retentionDays,
      peers: peers
        .map(({ peerId, signerIndex }) => `${signerIndex.toString()}:${peerId}`)
        .sort(),
    });
    const since = state.key === key ? seedCursor(state) : undefined;
    const startedAt = yield* DaPayloadPublicationsDB.databaseNow;
    yield* DaPayloadPublicationsDB.seedRecentPayloads({
      peers,
      retentionDays,
      ...(since === undefined ? {} : { since }),
    });
    yield* DaPayloadAnnouncementsDB.seedRecentPayloads(retentionDays, since);
    state.key = key;
    state.seededFrom = startedAt;
  });

export type DaPublicationReconcileSummary = {
  /** False when the DA manifest did not load: the pass seeded and delivered
   * nothing. */
  readonly manifestLoaded: boolean;
  readonly claimed: number;
  readonly attempted: number;
  readonly conflicts: number;
  readonly backlog: number;
};

export const reconcileDaPublicationsOnce = ({
  limit = 100,
  leaseOwner = `${hostname()}:${process.pid.toString()}`,
  deploymentIdentityDigest,
  seed = {},
}: {
  readonly limit?: number;
  readonly leaseOwner?: string;
  /** Verified manifest ID; absent for a derived contract bundle, which
   * consults no removal (see `owedPayload` in `l1-queue-terminals`). */
  readonly deploymentIdentityDigest?: Buffer;
  /** Seeding progress carried across passes; without one, every pass
   * seeds the whole retention window. */
  readonly seed?: DaPublicationSeedState;
} = {}): Effect.Effect<
  DaPublicationReconcileSummary,
  DatabaseError,
  Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const loaded = yield* Effect.either(
      Effect.tryPromise({
        try: () => loadDaProducerPublicationManifestFromEnv(),
        catch: (cause) => cause,
      }),
    );
    if (Either.isLeft(loaded)) {
      yield* raiseLivenessIncident(
        globals,
        DA_PUBLICATION_RECONCILER_SOURCE,
        DA_PUBLICATION_MANIFEST_UNAVAILABLE,
        `DA publication holds until the DA manifest loads: ${formatUnknownError(loaded.left)}`,
        { escalateAfterMs: DA_PUBLICATION_MANIFEST_UNAVAILABLE_ESCALATION_MS },
      );
      // The backlog while held: the payloads the next seed has to seed,
      // counted from the cursor over its index range. A process that has
      // not seeded has no cursor, and with no manifest no retention window
      // to bound the count by, so it leaves the gauge alone.
      const since = seedCursor(seed);
      let backlog = 0;
      if (since !== undefined) {
        backlog = yield* DaPayloadAnnouncementsDB.awaitingSeedCount(since);
        yield* daPublishReconcilerBacklogGauge(Effect.succeed(backlog));
      }
      return {
        manifestLoaded: false,
        claimed: 0,
        attempted: 0,
        conflicts: 0,
        backlog,
      };
    }
    yield* clearLivenessIncident(globals, DA_PUBLICATION_RECONCILER_SOURCE);
    const manifest = loaded.right;
    const retentionDays = manifest.retentionDays;
    const nodeConfig = yield* NodeConfig;
    const concurrency = Math.max(
      1,
      Math.min(limit, nodeConfig.MIDGARD_DA_PUBLISH_CONCURRENCY),
    );
    const peerLeaseToken = randomUUID();
    // The one seeding of the outboxes: the landed-block rebase stores an
    // own block's payload alone, so its rows are seeded here, as are a
    // newly added peer's.
    yield* seedPublicationOutboxes(seed, {
      peers: manifest.committeePeers,
      retentionDays,
    });
    const claimed = yield* DaPayloadPublicationsDB.claimDue({
      retentionDays,
      limit: concurrency,
      leaseOwner,
      leaseToken: peerLeaseToken,
      leaseMs: Math.max(30_000, manifest.requestTimeoutMs * 2),
      deploymentIdentityDigest,
    });
    let attempted = 0;
    let conflicts = 0;
    yield* Effect.forEach(
      claimed,
      (publication) =>
        Effect.gen(function* () {
          const payload = yield* DaPayloadsDB.retrieveByHeaderHash(
            publication.header_hash,
          );
          if (Option.isNone(payload)) {
            yield* DaPayloadPublicationsDB.releaseClaim({
              headerHash: publication.header_hash,
              peerId: publication.peer_id,
              leaseOwner,
              leaseToken: peerLeaseToken,
            });
            return;
          }
          attempted += 1;
          yield* Metric.increment(daPublishReconcilerAttemptsCounter);
          const result = yield* reconcileDaPayloadPeerFromEnv(
            payload.value,
            publication.peer_id,
            { owner: leaseOwner, token: peerLeaseToken },
          );
          if (result === null) {
            yield* DaPayloadPublicationsDB.releaseClaim({
              headerHash: publication.header_hash,
              peerId: publication.peer_id,
              leaseOwner,
              leaseToken: peerLeaseToken,
            });
            return;
          }
          if (result?.status === "conflict") {
            conflicts += 1;
            yield* Effect.logError(
              `DA publication conflict header=${publication.header_hash.toString("hex")},peer=${publication.peer_id}; retries stopped for evidence preservation`,
            );
          }
        }).pipe(
          Effect.catchAll((error) =>
            DaPayloadPublicationsDB.releaseClaim({
              headerHash: publication.header_hash,
              peerId: publication.peer_id,
              leaseOwner,
              leaseToken: peerLeaseToken,
            }).pipe(
              Effect.catchAll(() => Effect.void),
              Effect.zipRight(
                Effect.logWarning(
                  `DA publication reconciliation attempt failed header=${publication.header_hash.toString("hex")},peer=${publication.peer_id}: ${error.message}`,
                ),
              ),
            ),
          ),
        ),
      { concurrency, discard: true },
    );
    const announcementLeaseToken = randomUUID();
    const announcementClaims = yield* DaPayloadAnnouncementsDB.claimDue({
      retentionDays,
      limit: concurrency,
      leaseOwner,
      leaseToken: announcementLeaseToken,
      leaseMs: Math.max(30_000, manifest.requestTimeoutMs * 2),
      deploymentIdentityDigest,
    });
    yield* Effect.forEach(
      announcementClaims,
      (announcement) =>
        Effect.gen(function* () {
          const payload = yield* DaPayloadsDB.retrieveByHeaderHash(
            announcement.header_hash,
          );
          if (Option.isNone(payload)) {
            yield* DaPayloadAnnouncementsDB.releaseClaim({
              headerHash: announcement.header_hash,
              leaseOwner,
              leaseToken: announcementLeaseToken,
            });
            return;
          }
          const accepted = yield* DaPayloadPublicationsDB.acceptedCount(
            announcement.header_hash,
          );
          if (accepted < manifest.threshold) {
            yield* DaPayloadAnnouncementsDB.releaseClaim({
              headerHash: announcement.header_hash,
              leaseOwner,
              leaseToken: announcementLeaseToken,
            });
            return;
          }
          const result = yield* Effect.either(
            publishDaPayloadAnnouncementFromEnv(payload.value),
          );
          const recipients =
            result._tag === "Right"
              ? (result.right?.recipients.length ?? 0)
              : 0;
          const recorded = yield* DaPayloadAnnouncementsDB.recordAttempt({
            headerHash: announcement.header_hash,
            published: recipients > 0,
            ...(recipients > 0
              ? {}
              : {
                  error:
                    result._tag === "Left"
                      ? result.left.message
                      : "gossip publication reached zero recipients",
                }),
            retryBackoffMs: nodeConfig.MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS,
            retryBackoffMaxMs:
              nodeConfig.MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS,
            lease: { owner: leaseOwner, token: announcementLeaseToken },
          });
          if (!recorded) {
            return yield* Effect.fail(
              new DatabaseError({
                table: DaPayloadAnnouncementsDB.tableName,
                message:
                  "DA announcement claim fence was lost before completion",
                cause: `header_hash=${announcement.header_hash.toString("hex")},lease_owner=${leaseOwner},lease_token=${announcementLeaseToken}`,
              }),
            );
          }
        }).pipe(
          Effect.catchAll((error) =>
            DaPayloadAnnouncementsDB.releaseClaim({
              headerHash: announcement.header_hash,
              leaseOwner,
              leaseToken: announcementLeaseToken,
            }).pipe(
              Effect.catchAll(() => Effect.void),
              Effect.zipRight(
                Effect.logWarning(
                  `DA announcement reconciliation attempt failed header=${announcement.header_hash.toString("hex")}: ${error.message}`,
                ),
              ),
            ),
          ),
        ),
      { concurrency, discard: true },
    );
    const [peerBacklog, announcementBacklog] = yield* Effect.all(
      [
        DaPayloadPublicationsDB.backlogCount(
          retentionDays,
          deploymentIdentityDigest,
        ),
        DaPayloadAnnouncementsDB.backlogCount(
          retentionDays,
          deploymentIdentityDigest,
        ),
      ],
      { concurrency: 2 },
    );
    const backlog = peerBacklog + announcementBacklog;
    yield* daPublishReconcilerBacklogGauge(Effect.succeed(backlog));
    return {
      manifestLoaded: true,
      claimed: claimed.length,
      attempted,
      conflicts,
      backlog,
    };
  });

export const daPublicationReconcilerFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Database | ContractDeploymentIdentity | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟢 DA publication reconciler fiber started.");
    const deploymentIdentityDigest = deploymentIdentityDigestOf(
      yield* ContractDeploymentIdentity,
    );
    const seed: DaPublicationSeedState = {};
    yield* Effect.repeat(
      reconcileDaPublicationsOnce({ deploymentIdentityDigest, seed }).pipe(
        Effect.catchAllCause((cause) =>
          Effect.logWarning(
            `DA publication reconciler iteration failed; continuing: ${String(cause)}`,
          ),
        ),
      ),
      schedule,
    );
  });
