import { encodeDaStreamFrame } from "@al-ft/midgard-core/da-stream-codec";
import {
  daDeploymentFingerprintFromHex,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaMetadataByHeaderResponseCbor,
  encodeDaPayloadSubmitRequestCbor,
} from "@al-ft/midgard-core/da-transport";
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
  publishDaPayloadAnnouncement,
  verifyPayloadHash,
} from "./libp2p-producer.create-da-libp2p-producer-transport.js";
import { assertProducerIdentityInManifest } from "./libp2p-producer.create-da-libp2p-retained-payload-request-handlers.js";
import {
  type DaLibp2pPreflightFailure,
  type DaLibp2pPreflightListenCheck,
  type DaLibp2pPreflightMode,
  type DaLibp2pPreflightPeerResult,
  type DaProducerAnnouncementResult,
  type DaProducerCommitteePeer,
  type DaProducerPeerResult,
  type DaProducerProbeTransport,
  type DaProducerPublicationManifest,
} from "./libp2p-producer.parse-committee-peers.js";
import { loadDaProducerPublicationManifestFromEnv } from "./libp2p-producer.parse-da-producer-publication-manifest.js";
import { submitPayloadToPeer } from "./libp2p-producer.publish-da-payload-insert.js";
import { getPublicationTransport } from "./libp2p-producer.publish-da-payload-insert-from-env.js";

export const reconcileDaPayloadPeerFromEnv = (
  insert: DaPayloadsDB.InsertInput,
  peerId: string,
  lease?: { readonly owner: string; readonly token: string },
): Effect.Effect<DaProducerPeerResult | null, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const config = readDaHardeningConfig();
    return yield* Effect.tryPromise({
      try: async () => {
        const manifest = await loadDaProducerPublicationManifestFromEnv();
        if (manifest === null) {
          return null;
        }
        const peer = manifest.committeePeers.find(
          (candidate) => candidate.peerId === peerId,
        );
        if (peer === undefined) {
          throw new Error(
            `DA reconciliation peer ${peerId} is not in the current committee manifest`,
          );
        }
        const headerHash = insert[DaPayloadsDB.Columns.HEADER_HASH];
        const payloadBytes = insert[DaPayloadsDB.Columns.PAYLOAD_CBOR];
        const payloadHash = verifyPayloadHash(insert);
        const protocolId = daRequestResponseProtocolId(
          manifest.deploymentFingerprint,
          DaRequestResponseProtocol.payloadSubmit,
        );
        const request = encodeDaPayloadSubmitRequestCbor({
          deploymentFingerprint: daDeploymentFingerprintFromHex(
            manifest.deploymentFingerprint,
          ),
          headerHash,
          payloadHash,
          payloadSchemaVersion: insert[DaPayloadsDB.Columns.VERSION],
          mode: "inline",
          payloadBytes,
          chunkManifest: null,
        });
        const transport = await getPublicationTransport(manifest);
        const result = await submitPayloadToPeer({
          peer,
          headerHash,
          protocolId,
          request,
          requestFrame: encodeDaStreamFrame(request, {
            maxFrameBytes: manifest.maxPayloadBytes,
          }),
          timeoutMs: manifest.requestTimeoutMs,
          maxChunkBytes: manifest.maxChunkBytes,
          payloadHash,
          transport,
        });
        const status: Exclude<
          DaPayloadPublicationsDB.PublicationStatus,
          "pending"
        > = result.status === "deferred" ? "rejected" : result.status;
        const recorded = await Effect.runPromise(
          DaPayloadPublicationsDB.recordAttempt({
            headerHash,
            peer,
            status,
            error: result.error,
            retryBackoffMs: config.retryBackoffMs,
            retryBackoffMaxMs: config.retryBackoffMaxMs,
            lease,
          }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
        );
        if (!recorded) {
          throw new Error(
            `DA publication claim fence was lost for header=${headerHash.toString("hex")},peer=${peer.peerId}`,
          );
        }
        return result;
      },
      catch: (cause) =>
        new DatabaseError({
          table: DaPayloadPublicationsDB.tableName,
          message: "Failed to reconcile DA payload publication peer",
          cause,
        }),
    });
  });

export const publishDaPayloadAnnouncementFromEnv = (
  insert: DaPayloadsDB.InsertInput,
): Effect.Effect<DaProducerAnnouncementResult | null, DatabaseError> =>
  Effect.tryPromise({
    try: async () => {
      const manifest = await loadDaProducerPublicationManifestFromEnv();
      if (manifest === null) {
        return null;
      }
      return publishDaPayloadAnnouncement({
        insert,
        manifest,
        transport: await getPublicationTransport(manifest),
      });
    },
    catch: (cause) =>
      new DatabaseError({
        table: DaPayloadAnnouncementsDB.tableName,
        message: "Failed to reconcile DA payload announcement",
        cause,
      }),
  });

export const preflightIdentityFailure = (
  manifest: DaProducerPublicationManifest,
  localPeerId: string,
): DaLibp2pPreflightFailure | undefined => {
  try {
    assertProducerIdentityInManifest(manifest, localPeerId);
    return undefined;
  } catch (error) {
    return {
      phase: "identity",
      kind: "identity_mismatch",
      error: formatUnknownError(error),
      remediation:
        "Use the same DA_LIBP2P_PRIVATE_KEY_SOURCE that generated the producer announce_multiaddrs.",
    };
  }
};

export const boundListenCheck = (
  manifest: DaProducerPublicationManifest,
): DaLibp2pPreflightListenCheck => ({
  checked: true,
  status: "bound",
  listenMultiaddrs: manifest.listenMultiaddrs,
  announceMultiaddrs: manifest.announceMultiaddrs,
});

export const skippedListenCheck = (
  listenMultiaddrs: readonly string[],
  announceMultiaddrs: readonly string[],
): DaLibp2pPreflightListenCheck => ({
  checked: false,
  status: "skipped",
  listenMultiaddrs,
  announceMultiaddrs,
});

export const preflightWarnings = (
  mode: DaLibp2pPreflightMode,
): readonly string[] =>
  mode === "dial-only"
    ? [
        "dial-only preflight does not bind, announce, or validate the producer listener; use bind-listen before producer startup for listener validation.",
      ]
    : [];

export const uniqueReachableSignerIndexes = (
  peerResults: readonly DaLibp2pPreflightPeerResult[],
): readonly number[] =>
  [
    ...new Set(
      peerResults
        .filter(
          (result) =>
            result.status === "reachable" || result.status === "not_found",
        )
        .map((result) => result.signerIndex),
    ),
  ].sort((left, right) => left - right);

export const preflightPeerFailures = (
  peerResults: readonly DaLibp2pPreflightPeerResult[],
): readonly DaLibp2pPreflightFailure[] =>
  peerResults.flatMap((result): DaLibp2pPreflightFailure[] => {
    if (result.status === "protocol_error") {
      return [
        {
          phase: "protocol",
          kind: "protocol_mismatch",
          peerId: result.peerId,
          signerIndex: result.signerIndex,
          error: result.error ?? "metadata probe protocol mismatch",
        },
      ];
    }
    if (result.status === "transport_error") {
      return [
        {
          phase: "dial",
          kind: "peer_unreachable",
          peerId: result.peerId,
          signerIndex: result.signerIndex,
          error: result.error ?? "metadata probe transport error",
        },
      ];
    }
    return [];
  });

export const preflightCommitteePeer = async ({
  peer,
  protocolId,
  request,
  timeoutMs,
  transport,
}: {
  readonly peer: DaProducerCommitteePeer;
  readonly protocolId: string;
  readonly request: Uint8Array;
  readonly timeoutMs: number;
  readonly transport: DaProducerProbeTransport;
}): Promise<DaLibp2pPreflightPeerResult> => {
  try {
    const responseBytes = await transport.request(
      peer,
      protocolId,
      request,
      timeoutMs,
    );
    const response = decodeDaMetadataByHeaderResponseCbor(responseBytes);
    if (response.status === "not_found" || response.status === "found") {
      return {
        peerId: peer.peerId,
        signerIndex: peer.signerIndex,
        address: peer.multiaddrs,
        protocolId,
        status: response.status === "not_found" ? "not_found" : "reachable",
        metadataStatus: response.status,
      };
    }
    return {
      peerId: peer.peerId,
      signerIndex: peer.signerIndex,
      address: peer.multiaddrs,
      protocolId,
      status: "protocol_error",
      metadataStatus: response.status,
      error: `metadata probe returned ${response.status}`,
    };
  } catch (error) {
    return {
      peerId: peer.peerId,
      signerIndex: peer.signerIndex,
      address: peer.multiaddrs,
      protocolId,
      status: "transport_error",
      error: formatUnknownError(error),
    };
  }
};
