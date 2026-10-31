import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  DaPayloadAnnouncementsDB,
  DaPayloadPublicationsDB,
  DaPayloadsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import type { Database } from "../services/database.js";
import { readDaHardeningConfig } from "./hardening-config.js";
import {
  createDaLibp2pProducerTransport,
  verifyPayloadHash,
} from "./libp2p-producer.create-da-libp2p-producer-transport.js";
import {
  type DaProducerPublicationManifest,
  type DaProducerPublicationReport,
  type DaProducerTransport,
} from "./libp2p-producer.parse-committee-peers.js";
import { loadDaProducerPublicationManifestFromEnv } from "./libp2p-producer.parse-da-producer-publication-manifest.js";
import {
  publicationTransportKey,
  publishDaPayloadInsert,
} from "./libp2p-producer.publish-da-payload-insert.js";

let cachedPublicationTransport:
  | Promise<{
      readonly key: string;
      readonly transport: DaProducerTransport;
    }>
  | undefined;

export const getPublicationTransport = async (
  manifest: DaProducerPublicationManifest,
): Promise<DaProducerTransport> => {
  const key = publicationTransportKey(manifest);
  const existing = cachedPublicationTransport;
  if (existing !== undefined) {
    const resolved = await existing;
    if (resolved.key === key) {
      return resolved.transport;
    }
    await resolved.transport.close?.();
  }
  const created = createDaLibp2pProducerTransport(manifest, {
    mode: "dial-only",
  }).then((transport) => ({ key, transport }));
  cachedPublicationTransport = created;
  try {
    return (await created).transport;
  } catch (error) {
    if (cachedPublicationTransport === created) {
      cachedPublicationTransport = undefined;
    }
    throw error;
  }
};

const invalidatePublicationTransport = async (
  transport: DaProducerTransport,
): Promise<void> => {
  const cached = cachedPublicationTransport;
  if (cached === undefined) {
    return;
  }
  const resolved = await cached.catch(() => undefined);
  if (resolved?.transport !== transport) {
    return;
  }
  cachedPublicationTransport = undefined;
  await transport.close?.().catch(() => undefined);
};

export const closeDaLibp2pPublicationTransport = async (): Promise<void> => {
  const cached = cachedPublicationTransport;
  cachedPublicationTransport = undefined;
  const resolved = await cached?.catch(() => undefined);
  await resolved?.transport.close?.();
};

export const getDaPublicationTransportForTest = getPublicationTransport;

/**
 * Persist the durable peer-delivery and gossip outboxes without performing
 * network I/O. Local block finalization calls this before marking its mutation
 * job complete so a crash cannot leave publication dependent on the direct
 * best-effort publish path.
 */
export const seedDaPayloadPublicationOutboxFromEnv = (
  insert: DaPayloadsDB.InsertInput,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const manifest = yield* Effect.tryPromise({
      try: () => loadDaProducerPublicationManifestFromEnv(),
      catch: (cause) =>
        new DatabaseError({
          table: DaPayloadPublicationsDB.tableName,
          message:
            "Failed to load DA manifest while seeding publication outbox",
          cause,
        }),
    });
    if (manifest === null) {
      return;
    }
    yield* Effect.all(
      [
        DaPayloadPublicationsDB.seedForPayload(
          insert[DaPayloadsDB.Columns.HEADER_HASH],
          manifest.committeePeers,
        ),
        DaPayloadAnnouncementsDB.seedForPayload(
          insert[DaPayloadsDB.Columns.HEADER_HASH],
        ),
      ],
      { concurrency: 1, discard: true },
    );
  });

export const publishDaPayloadInsertFromEnv = (
  insert: DaPayloadsDB.InsertInput,
): Effect.Effect<DaProducerPublicationReport, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const hardeningConfig = readDaHardeningConfig();
    return yield* Effect.tryPromise({
      try: async () => {
        const manifest = await loadDaProducerPublicationManifestFromEnv();
        const headerHash =
          insert[DaPayloadsDB.Columns.HEADER_HASH].toString("hex");
        const payloadHash = verifyPayloadHash(insert).toString("hex");
        if (manifest === null) {
          return {
            configured: false,
            headerHash,
            payloadHash,
            acceptedPeers: 0,
            peerResults: [],
            reason: "no libp2p DA manifest configured",
          };
        }
        await Effect.runPromise(
          Effect.all(
            [
              DaPayloadPublicationsDB.seedForPayload(
                insert[DaPayloadsDB.Columns.HEADER_HASH],
                manifest.committeePeers,
              ),
              DaPayloadAnnouncementsDB.seedForPayload(
                insert[DaPayloadsDB.Columns.HEADER_HASH],
              ),
            ],
            { concurrency: 1, discard: true },
          ).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
        );
        const transport = await getPublicationTransport(manifest);
        try {
          const report = await publishDaPayloadInsert({
            insert,
            manifest,
            transport,
            onPeerResult: async ({ peer, result }) => {
              const status: Exclude<
                DaPayloadPublicationsDB.PublicationStatus,
                "pending"
              > = result.status === "deferred" ? "rejected" : result.status;
              const recorded = await Effect.runPromise(
                DaPayloadPublicationsDB.recordAttempt({
                  headerHash: insert[DaPayloadsDB.Columns.HEADER_HASH],
                  peer,
                  status,
                  error: result.error,
                  retryBackoffMs: hardeningConfig.retryBackoffMs,
                  retryBackoffMaxMs: hardeningConfig.retryBackoffMaxMs,
                }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
              ).catch((error) => {
                console.warn(
                  `Failed to persist DA publication result header=${headerHash},peer=${peer.peerId}: ${formatUnknownError(error)}`,
                );
                return undefined;
              });
              if (recorded === false) {
                console.warn(
                  `Ignored unfenced DA publication result because an active reconciler claim owns the row header=${headerHash},peer=${peer.peerId}`,
                );
              }
            },
          });
          const recipients = report.announcement?.recipients.length ?? 0;
          const announcementRecorded = await Effect.runPromise(
            DaPayloadAnnouncementsDB.recordAttempt({
              headerHash: insert[DaPayloadsDB.Columns.HEADER_HASH],
              published: recipients > 0,
              ...(recipients > 0
                ? {}
                : { error: "gossip publication reached zero recipients" }),
              retryBackoffMs: hardeningConfig.retryBackoffMs,
              retryBackoffMaxMs: hardeningConfig.retryBackoffMaxMs,
            }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
          );
          if (!announcementRecorded) {
            console.warn(
              `Ignored unfenced DA announcement result because an active reconciler claim owns the row header=${headerHash}`,
            );
          }
          return report;
        } catch (error) {
          const announcementRecorded = await Effect.runPromise(
            DaPayloadAnnouncementsDB.recordAttempt({
              headerHash: insert[DaPayloadsDB.Columns.HEADER_HASH],
              published: false,
              error: formatUnknownError(error),
              retryBackoffMs: hardeningConfig.retryBackoffMs,
              retryBackoffMaxMs: hardeningConfig.retryBackoffMaxMs,
            }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
          ).catch((recordError) => {
            console.warn(
              `Failed to persist DA announcement failure header=${headerHash}: ${formatUnknownError(recordError)}`,
            );
            return undefined;
          });
          if (announcementRecorded === false) {
            console.warn(
              `Ignored unfenced DA announcement failure because an active reconciler claim owns the row header=${headerHash}`,
            );
          }
          await invalidatePublicationTransport(transport);
          throw error;
        }
      },
      catch: (cause) =>
        new DatabaseError({
          table: DaPayloadsDB.tableName,
          message: "Failed to publish DA payload over libp2p",
          cause,
        }),
    });
  });

export const publicationSatisfied = (
  report: DaProducerPublicationReport,
): boolean =>
  report.configured &&
  report.threshold !== undefined &&
  report.acceptedPeers >= report.threshold;
