import {
  computeDaSha256Hash,
  daDeploymentFingerprintFromHex,
  DaRequestResponseProtocol,
  decodeDaMetadataByHeaderResponseV1Cbor,
  decodeDaPayloadByHeaderResponseV1Cbor,
  decodeDaPayloadChunkResponseV1Cbor,
  encodeDaPayloadByHeaderRequestV1Cbor,
  encodeDaPayloadChunkRequestV1Cbor,
} from "@al-ft/midgard-core/da-transport";

import type { Libp2pDaTransportLimits } from "../../config.js";
import type { WatcherStore } from "../../store.js";
import { hexToBytes } from "../../utils/hex.js";
import type {
  DaPayloadCandidate,
  DaPayloadCandidatesResult,
  DaPayloadFetchFailure,
  DaPayloadFetchFailureStatus,
  DaPayloadSource,
} from "../source.js";
import type { DaLibp2pStreamHandler } from "./DaLibp2pNode.js";
import { DaLibp2pNode } from "./DaLibp2pNode.js";
import type { DaPeerRegistry, DaPeerRegistryEntry } from "./DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "./DaProtocols.js";
import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "./DaStreamCodec.js";
import { DaLibp2pPayloadProtocolHandlers } from "./payload-protocols.js";

export type DaLibp2pPayloadSourceOptions = {
  readonly deploymentFingerprint: string;
  readonly node: DaLibp2pNode;
  readonly registry: DaPeerRegistry;
  readonly limits: Libp2pDaTransportLimits;
  readonly peers?: readonly DaPeerRegistryEntry[];
};

export class DaLibp2pPayloadSource implements DaPayloadSource {
  private readonly deploymentFingerprint: Buffer;
  private readonly node: DaLibp2pNode;
  private readonly limits: Libp2pDaTransportLimits;
  private readonly peers: readonly DaPeerRegistryEntry[];
  private readonly protocolIds: ReturnType<typeof createDaProtocolAllowlist>;

  constructor(options: DaLibp2pPayloadSourceOptions) {
    this.deploymentFingerprint = daDeploymentFingerprintFromHex(
      options.deploymentFingerprint,
    );
    this.node = options.node;
    this.limits = options.limits;
    this.peers =
      options.peers ??
      dedupePeers([
        ...options.registry.peersForRole("retrieval"),
        ...options.registry.entries().filter((peer) => peer.bootstrap),
        ...options.registry.peersForRole("committee"),
      ]);
    this.protocolIds = createDaProtocolAllowlist(options.deploymentFingerprint);
  }

  async fetchPayloadCandidates(
    headerHashHex: string,
  ): Promise<DaPayloadCandidatesResult> {
    const headerHash = hexToBytes(headerHashHex, "header_hash", 28);
    const attempts: Array<DaPayloadFetchFailure["attempts"][number]> = [];
    const candidates: DaPayloadCandidate[] = [];
    for (const peer of this.peers) {
      try {
        const candidate = await this.fetchPayloadFromPeer(peer, headerHash);
        if (candidate === undefined) {
          attempts.push({
            sourcePeerId: peer.peerId,
            status: "not_found",
            detail: "payload not found",
          });
          continue;
        }
        candidates.push(candidate);
      } catch (error) {
        attempts.push({
          sourcePeerId: peer.peerId,
          status: statusFromError(error),
          detail: error instanceof Error ? error.message : String(error),
        });
      }
    }
    return candidates.length > 0
      ? { ok: true, candidates, attempts }
      : { ok: false, attempts };
  }

  private async fetchPayloadFromPeer(
    peer: DaPeerRegistryEntry,
    headerHash: Buffer,
  ): Promise<DaPayloadCandidate | undefined> {
    const byHeaderResponse = decodeDaPayloadByHeaderResponseV1Cbor(
      await this.node.request({
        peer,
        protocolId: this.protocolId(DaRequestResponseProtocol.payloadByHeader),
        timeoutMs: this.limits.requestTimeoutMs,
        payload: encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: this.deploymentFingerprint,
          headerHash,
          acceptedPayloadHashes: null,
          maxInlineBytes: this.limits.maxInlineResponseBytes,
        }),
      }),
    );
    switch (byHeaderResponse.status) {
      case "found_inline": {
        if (
          byHeaderResponse.payloadHash === null ||
          byHeaderResponse.payloadBytes === null
        ) {
          throw new InvalidDaPayloadSourceResponseError(
            "inline payload response is missing payload bytes",
          );
        }
        const payloadCbor = Buffer.from(byHeaderResponse.payloadBytes);
        assertPayloadHash(payloadCbor, byHeaderResponse.payloadHash);
        return {
          sourcePeerId: peer.peerId,
          payloadCbor,
          metadata: await this.fetchMetadata(peer, headerHash),
        };
      }
      case "found_chunked": {
        if (
          byHeaderResponse.payloadHash === null ||
          byHeaderResponse.chunkManifest === null
        ) {
          throw new InvalidDaPayloadSourceResponseError(
            "chunked payload response is missing chunk manifest",
          );
        }
        const payloadCbor = await this.fetchPayloadChunks(
          peer,
          headerHash,
          byHeaderResponse.payloadHash,
          byHeaderResponse.chunkManifest.chunkHashes,
        );
        if (payloadCbor.length !== byHeaderResponse.chunkManifest.totalBytes) {
          throw new InvalidDaPayloadSourceResponseError(
            "chunked payload total size does not match manifest",
          );
        }
        assertPayloadHash(payloadCbor, byHeaderResponse.payloadHash);
        return {
          sourcePeerId: peer.peerId,
          payloadCbor,
          metadata: await this.fetchMetadata(peer, headerHash),
        };
      }
      case "not_found":
        return undefined;
      case "conflict":
        throw new DaPayloadSourcePeerStatusError(
          "conflict",
          byHeaderResponse.reasonCode ?? "peer reported payload conflict",
        );
      case "rejected":
        throw new DaPayloadSourcePeerStatusError(
          "rejected",
          byHeaderResponse.reasonCode ?? "peer rejected payload request",
        );
    }
  }

  private async fetchPayloadChunks(
    peer: DaPeerRegistryEntry,
    headerHash: Buffer,
    payloadHash: Buffer,
    chunkHashes: readonly Buffer[],
  ): Promise<Buffer> {
    const chunks: Buffer[] = [];
    for (let index = 0; index < chunkHashes.length; index += 1) {
      const response = decodeDaPayloadChunkResponseV1Cbor(
        await this.node.request({
          peer,
          protocolId: this.protocolId(DaRequestResponseProtocol.payloadChunk),
          timeoutMs: this.limits.requestTimeoutMs,
          payload: encodeDaPayloadChunkRequestV1Cbor({
            deploymentFingerprint: this.deploymentFingerprint,
            headerHash,
            payloadHash,
            chunkIndex: index,
          }),
        }),
      );
      if (response.status !== "found" || response.chunkBytes === null) {
        throw new InvalidDaPayloadSourceResponseError(
          `payload chunk ${index.toString()} was not found`,
        );
      }
      const chunk = Buffer.from(response.chunkBytes);
      const expectedChunkHash = chunkHashes[index]!;
      assertPayloadHash(
        chunk,
        expectedChunkHash,
        "payload chunk hash mismatch",
      );
      if (
        response.chunkHash !== null &&
        !response.chunkHash.equals(expectedChunkHash)
      ) {
        throw new InvalidDaPayloadSourceResponseError(
          "payload chunk response hash does not match manifest",
        );
      }
      chunks.push(chunk);
    }
    return Buffer.concat(chunks);
  }

  private async fetchMetadata(
    peer: DaPeerRegistryEntry,
    headerHash: Buffer,
  ): Promise<unknown> {
    const response = decodeDaMetadataByHeaderResponseV1Cbor(
      await this.node.request({
        peer,
        protocolId: this.protocolId(DaRequestResponseProtocol.metadataByHeader),
        timeoutMs: this.limits.requestTimeoutMs,
        payload: encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: this.deploymentFingerprint,
          headerHash,
          acceptedPayloadHashes: null,
          maxInlineBytes: 0,
        }),
      }),
    );
    return response.status === "found" ? response : undefined;
  }

  private protocolId(protocol: DaRequestResponseProtocol): string {
    return this.protocolIds.protocolIdByName.get(protocol)!;
  }
}

export const createDaLibp2pPayloadRequestHandlers = ({
  deploymentFingerprint,
  store,
  limits,
}: {
  readonly deploymentFingerprint: string;
  readonly store: Pick<WatcherStore, "getDaPayload" | "saveDaPayload">;
  readonly limits: Libp2pDaTransportLimits;
}): ReadonlyMap<string, DaLibp2pStreamHandler> => {
  const protocolIds = createDaProtocolAllowlist(deploymentFingerprint);
  const handlers = new DaLibp2pPayloadProtocolHandlers({
    deploymentFingerprint,
    store,
    limits,
  });
  const handlerMap = new Map<string, DaLibp2pStreamHandler>();
  const addHandler = (
    protocol: DaRequestResponseProtocol,
    handle: (requestCbor: Uint8Array) => Promise<Buffer>,
  ): void => {
    const protocolId = protocolIds.protocolIdByName.get(protocol)!;
    handlerMap.set(protocolId, async ({ stream }) => {
      const requestCbor = await readSingleDaStreamFrame(stream, {
        maxFrameBytes: limits.maxPayloadBytes,
      });
      const responseCbor = await handle(requestCbor);
      await writeDaStreamFrame(stream, responseCbor, {
        maxFrameBytes: limits.maxPayloadBytes,
        close: true,
      });
    });
  };
  addHandler(DaRequestResponseProtocol.payloadSubmit, (request) =>
    handlers.handlePayloadSubmit(request),
  );
  addHandler(DaRequestResponseProtocol.payloadByHeader, (request) =>
    handlers.handlePayloadByHeader(request),
  );
  addHandler(DaRequestResponseProtocol.payloadChunk, (request) =>
    handlers.handlePayloadChunk(request),
  );
  addHandler(DaRequestResponseProtocol.metadataByHeader, (request) =>
    handlers.handleMetadataByHeader(request),
  );
  return handlerMap;
};

class InvalidDaPayloadSourceResponseError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "InvalidDaPayloadSourceResponseError";
  }
}

class DaPayloadSourcePeerStatusError extends Error {
  readonly status: DaPayloadFetchFailureStatus;

  constructor(status: "rejected" | "conflict", message: string) {
    super(message);
    this.name = "DaPayloadSourcePeerStatusError";
    this.status = status;
  }
}

const dedupePeers = (
  peers: readonly DaPeerRegistryEntry[],
): readonly DaPeerRegistryEntry[] => [
  ...new Map(peers.map((peer) => [peer.peerId, peer])).values(),
];

const assertPayloadHash = (
  payload: Buffer,
  expectedHash: Buffer,
  message = "payload hash mismatch",
): void => {
  if (!computeDaSha256Hash(payload).equals(expectedHash)) {
    throw new InvalidDaPayloadSourceResponseError(message);
  }
};

const statusFromError = (error: unknown): DaPayloadFetchFailureStatus => {
  if (error instanceof DaPayloadSourcePeerStatusError) {
    return error.status;
  }
  if (error instanceof InvalidDaPayloadSourceResponseError) {
    return "invalid_content";
  }
  if (error instanceof Error && error.name === "TimeoutError") {
    return "timeout";
  }
  return "transport_error";
};
