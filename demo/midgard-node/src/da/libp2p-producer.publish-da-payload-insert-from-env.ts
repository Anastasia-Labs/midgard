import { Effect } from "effect";

import {
  DaPayloadAnnouncementsDB,
  DaPayloadPublicationsDB,
  DaPayloadsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import type { Database } from "../services/database.js";
import { createDaLibp2pProducerTransport } from "./libp2p-producer.create-da-libp2p-producer-transport.js";
import {
  type DaProducerPublicationManifest,
  type DaProducerPublicationReport,
  type DaProducerTransport,
} from "./libp2p-producer.parse-committee-peers.js";
import { loadDaProducerPublicationManifestFromEnv } from "./libp2p-producer.parse-da-producer-publication-manifest.js";
import { publicationTransportKey } from "./libp2p-producer.publish-da-payload-insert.js";

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

export const closeDaLibp2pPublicationTransport = async (): Promise<void> => {
  const cached = cachedPublicationTransport;
  cachedPublicationTransport = undefined;
  const resolved = await cached?.catch(() => undefined);
  await resolved?.transport.close?.();
};

export const getDaPublicationTransportForTest = getPublicationTransport;

/**
 * Persist the durable peer-delivery and gossip outboxes without performing
 * network I/O. Landed-block processing calls this as it applies this node's
 * own block; the DA publication reconciler delivers from the outboxes.
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

export const publicationSatisfied = (
  report: DaProducerPublicationReport,
): boolean =>
  report.configured &&
  report.threshold !== undefined &&
  report.acceptedPeers >= report.threshold;
