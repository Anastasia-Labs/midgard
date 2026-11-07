import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "@al-ft/midgard-core/da-stream-codec";
import {
  computeDaSha256Hash,
  daDeploymentFingerprintFromHex,
  type DaPayloadChunkManifest,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaPayloadByHeaderRequestCbor,
  decodeDaPayloadChunkRequestCbor,
  encodeDaMetadataByHeaderResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
  encodeDaPayloadChunkResponseCbor,
} from "@al-ft/midgard-core/da-transport";

import {
  type DaProducerPublicationManifest,
  type DaProducerStreamHandler,
  type DaRetainedPayloadLookup,
} from "./libp2p-producer.parse-committee-peers.js";
import {
  emptyRetainedPayloadMetadataResponse,
  metadataForRetainedPayload,
  resolveRetainedPayloadRow,
  retainedPayloadAbsentResponse,
  retainedPayloadMetadataAbsentResponse,
  type RetainedPayloadResolution,
} from "./libp2p-producer.parse-da-producer-publication-manifest.js";

export const createDaLibp2pRetainedPayloadRequestHandlers = ({
  manifest,
  retrieveByHeaderHash,
}: {
  readonly manifest: DaProducerPublicationManifest;
  readonly retrieveByHeaderHash: DaRetainedPayloadLookup;
}): ReadonlyMap<string, DaProducerStreamHandler> => {
  const deploymentFingerprint = daDeploymentFingerprintFromHex(
    manifest.deploymentFingerprint,
  );
  const handlerMap = new Map<string, DaProducerStreamHandler>();
  const addHandler = (
    protocol: DaRequestResponseProtocol,
    handle: (requestCbor: Uint8Array) => Promise<Buffer>,
  ): void => {
    const protocolId = daRequestResponseProtocolId(
      manifest.deploymentFingerprint,
      protocol,
    );
    handlerMap.set(protocolId, async (stream) => {
      const requestCbor = await withTimeout(
        readSingleDaStreamFrame(stream, {
          maxFrameBytes: manifest.maxPayloadBytes,
        }),
        manifest.requestTimeoutMs,
      ).catch((error: unknown) => {
        stream.abort?.(
          error instanceof Error ? error : new Error(String(error)),
        );
        throw error;
      });
      const responseCbor = await handle(requestCbor);
      await writeDaStreamFrame(stream, responseCbor, {
        maxFrameBytes: manifest.maxPayloadBytes,
        close: true,
      });
    });
  };

  const resolveRow = async (
    headerHash: Buffer,
  ): Promise<RetainedPayloadResolution> => {
    const row = await retrieveByHeaderHash(headerHash);
    if (row === undefined) {
      return { kind: "missing" };
    }
    return resolveRetainedPayloadRow(row, manifest.maxPayloadBytes);
  };

  addHandler(DaRequestResponseProtocol.payloadByHeader, async (requestCbor) => {
    const request = decodeRetainedPayloadRequest(
      () => decodeDaPayloadByHeaderRequestCbor(requestCbor),
      "payload-by-header request",
    );
    const headerHash = request.headerHash;
    if (!request.deploymentFingerprint.equals(deploymentFingerprint)) {
      return encodeDaPayloadByHeaderResponseCbor({
        status: "rejected",
        headerHash,
        payloadHash: null,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: "deployment_fingerprint_mismatch",
      });
    }

    const resolved = await resolveRow(headerHash);
    if (resolved.kind !== "found") {
      return encodeDaPayloadByHeaderResponseCbor(
        retainedPayloadAbsentResponse(headerHash, resolved),
      );
    }
    if (
      request.acceptedPayloadHashes !== null &&
      !containsHash(request.acceptedPayloadHashes, resolved.payloadHash)
    ) {
      return encodeDaPayloadByHeaderResponseCbor({
        status: "conflict",
        headerHash,
        payloadHash: resolved.payloadHash,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: "payload_hash_not_accepted",
      });
    }

    const inlineLimit = Math.min(
      request.maxInlineBytes,
      manifest.maxInlineResponseBytes,
    );
    if (resolved.payloadBytes.length <= inlineLimit) {
      return encodeDaPayloadByHeaderResponseCbor({
        status: "found_inline",
        headerHash,
        payloadHash: resolved.payloadHash,
        payloadBytes: resolved.payloadBytes,
        chunkManifest: null,
        reasonCode: null,
      });
    }

    return encodeDaPayloadByHeaderResponseCbor({
      status: "found_chunked",
      headerHash,
      payloadHash: resolved.payloadHash,
      payloadBytes: null,
      chunkManifest: retainedPayloadChunkManifestFor(
        resolved.payloadBytes,
        manifest.maxChunkBytes,
      ),
      reasonCode: null,
    });
  });

  addHandler(DaRequestResponseProtocol.payloadChunk, async (requestCbor) => {
    const request = decodeRetainedPayloadRequest(
      () => decodeDaPayloadChunkRequestCbor(requestCbor),
      "payload-chunk request",
    );
    const headerHash = request.headerHash;
    const payloadHash = request.payloadHash;
    if (!request.deploymentFingerprint.equals(deploymentFingerprint)) {
      return encodeDaPayloadChunkResponseCbor({
        status: "rejected",
        headerHash,
        payloadHash,
        chunkIndex: request.chunkIndex,
        chunkBytes: null,
        chunkHash: null,
      });
    }

    const resolved = await resolveRow(headerHash);
    if (
      resolved.kind !== "found" ||
      !resolved.payloadHash.equals(payloadHash)
    ) {
      return encodeRetainedPayloadChunkNotFound(
        headerHash,
        payloadHash,
        request.chunkIndex,
      );
    }

    const offset = request.chunkIndex * manifest.maxChunkBytes;
    if (offset >= resolved.payloadBytes.length) {
      return encodeRetainedPayloadChunkNotFound(
        headerHash,
        payloadHash,
        request.chunkIndex,
      );
    }
    const chunkBytes = resolved.payloadBytes.subarray(
      offset,
      Math.min(offset + manifest.maxChunkBytes, resolved.payloadBytes.length),
    );
    return encodeDaPayloadChunkResponseCbor({
      status: "found",
      headerHash,
      payloadHash,
      chunkIndex: request.chunkIndex,
      chunkBytes,
      chunkHash: computeDaSha256Hash(chunkBytes),
    });
  });

  addHandler(
    DaRequestResponseProtocol.metadataByHeader,
    async (requestCbor) => {
      const request = decodeRetainedPayloadRequest(
        () => decodeDaPayloadByHeaderRequestCbor(requestCbor),
        "metadata-by-header request",
      );
      const headerHash = request.headerHash;
      if (!request.deploymentFingerprint.equals(deploymentFingerprint)) {
        return encodeDaMetadataByHeaderResponseCbor({
          ...emptyRetainedPayloadMetadataResponse(headerHash),
          status: "rejected",
        });
      }

      const resolved = await resolveRow(headerHash);
      if (resolved.kind !== "found") {
        return encodeDaMetadataByHeaderResponseCbor(
          retainedPayloadMetadataAbsentResponse(headerHash, resolved),
        );
      }
      if (
        request.acceptedPayloadHashes !== null &&
        !containsHash(request.acceptedPayloadHashes, resolved.payloadHash)
      ) {
        return encodeDaMetadataByHeaderResponseCbor({
          ...emptyRetainedPayloadMetadataResponse(headerHash),
          status: "conflict",
          payloadHash: resolved.payloadHash,
        });
      }

      return encodeDaMetadataByHeaderResponseCbor(
        metadataForRetainedPayload(headerHash, resolved),
      );
    },
  );

  return handlerMap;
};

export const loadAndValidateProducerIdentity = async (
  manifest: DaProducerPublicationManifest,
) => {
  const identity = await loadDaLibp2pIdentity(manifest.localPrivateKeySource);
  assertProducerIdentityInManifest(manifest, identity.peerId);
  return identity;
};

export const assertProducerIdentityInManifest = (
  manifest: DaProducerPublicationManifest,
  localPeerId: string,
): void => {
  if (
    !manifest.announceMultiaddrs.some((addr) =>
      addr.endsWith(`/p2p/${localPeerId}`),
    )
  ) {
    throw new Error(
      `DA libp2p private key peer id ${localPeerId} is not present in announce_multiaddrs`,
    );
  }
};

const retainedPayloadChunkManifestFor = (
  payloadBytes: Buffer,
  chunkSize: number,
): DaPayloadChunkManifest => {
  const chunkHashes: Buffer[] = [];
  for (let offset = 0; offset < payloadBytes.length; offset += chunkSize) {
    chunkHashes.push(
      computeDaSha256Hash(payloadBytes.subarray(offset, offset + chunkSize)),
    );
  }
  return {
    payloadHash: computeDaSha256Hash(payloadBytes),
    totalBytes: payloadBytes.length,
    chunkSize,
    chunkHashes,
  };
};

const encodeRetainedPayloadChunkNotFound = (
  headerHash: Buffer,
  payloadHash: Buffer,
  chunkIndex: number,
): Buffer =>
  encodeDaPayloadChunkResponseCbor({
    status: "not_found",
    headerHash,
    payloadHash,
    chunkIndex,
    chunkBytes: null,
    chunkHash: null,
  });

const decodeRetainedPayloadRequest = <T>(decode: () => T, label: string): T => {
  try {
    return decode();
  } catch (cause) {
    throw new Error(`invalid ${label}`, { cause });
  }
};

const containsHash = (hashes: readonly Buffer[], target: Buffer): boolean =>
  hashes.some((hash) => hash.equals(target));

const withTimeout = async <T>(
  promise: Promise<T>,
  timeoutMs: number,
): Promise<T> => {
  let timeout: NodeJS.Timeout | undefined;
  try {
    return await Promise.race([
      promise,
      new Promise<never>((_resolve, reject) => {
        timeout = setTimeout(
          () => reject(new Error("libp2p DA request timed out")),
          timeoutMs,
        );
      }),
    ]);
  } finally {
    if (timeout !== undefined) {
      clearTimeout(timeout);
    }
  }
};
