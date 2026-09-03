import { randomUUID } from "node:crypto";
import { hostname } from "node:os";

import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import { Effect, Metric, Option, Schedule } from "effect";

import { readDaHardeningConfig } from "../da/hardening-config.js";
import {
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
import type { Database } from "../services/database.js";

const daPublishReconcilerAttemptsCounter = Metric.counter(
  "da_publish_reconciler_attempts_total",
  { description: "Durable DA publication reconciliation attempts" },
);
const daPublishReconcilerBacklogGauge = Metric.gauge(
  "da_publish_reconciler_backlog",
  { description: "Incomplete durable DA peer publication rows" },
);

export type DaPublicationReconcileSummary = {
  readonly configured: boolean;
  readonly claimed: number;
  readonly attempted: number;
  readonly conflicts: number;
  readonly backlog: number;
};

export const reconcileDaPublicationsOnce = ({
  limit = 100,
  leaseOwner = `${hostname()}:${process.pid.toString()}`,
}: {
  readonly limit?: number;
  readonly leaseOwner?: string;
} = {}): Effect.Effect<
  DaPublicationReconcileSummary,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const manifest = yield* Effect.tryPromise({
      try: () => loadDaProducerPublicationManifestFromEnv(),
      catch: (cause) =>
        new DatabaseError({
          table: DaPayloadPublicationsDB.tableName,
          message: "Failed to load DA manifest for publication reconciliation",
          cause,
        }),
    });
    if (manifest === null) {
      return {
        configured: false,
        claimed: 0,
        attempted: 0,
        conflicts: 0,
        backlog: 0,
      };
    }
    const retentionDays = DA_TRANSPORT_LIMITS_V1.minimumRetentionDays;
    const hardeningConfig = readDaHardeningConfig();
    const concurrency = Math.max(
      1,
      Math.min(limit, hardeningConfig.publishConcurrency),
    );
    const peerLeaseToken = randomUUID();
    // This scan closes the crash window after da_payloads commit but before
    // per-peer outbox rows are seeded, and also seeds newly added peers.
    yield* DaPayloadPublicationsDB.seedRecentPayloads({
      peers: manifest.committeePeers,
      retentionDays,
    });
    yield* DaPayloadAnnouncementsDB.seedRecentPayloads(retentionDays);
    const claimed = yield* DaPayloadPublicationsDB.claimDue({
      retentionDays,
      limit: concurrency,
      leaseOwner,
      leaseToken: peerLeaseToken,
      leaseMs: Math.max(30_000, manifest.requestTimeoutMs * 2),
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
            retryBackoffMs: hardeningConfig.retryBackoffMs,
            retryBackoffMaxMs: hardeningConfig.retryBackoffMaxMs,
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
        DaPayloadPublicationsDB.backlogCount(retentionDays),
        DaPayloadAnnouncementsDB.backlogCount(retentionDays),
      ],
      { concurrency: 2 },
    );
    const backlog = peerBacklog + announcementBacklog;
    yield* daPublishReconcilerBacklogGauge(Effect.succeed(backlog));
    return {
      configured: true,
      claimed: claimed.length,
      attempted,
      conflicts,
      backlog,
    };
  });

export const daPublicationReconcilerFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Database> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟢 DA publication reconciler fiber started.");
    yield* Effect.repeat(
      reconcileDaPublicationsOnce().pipe(
        Effect.catchAllCause((cause) =>
          Effect.logWarning(
            `DA publication reconciler iteration failed; continuing: ${String(cause)}`,
          ),
        ),
      ),
      schedule,
    );
  });
