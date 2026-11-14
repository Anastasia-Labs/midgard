import {
  computeDaSha256Hash,
  daDeploymentFingerprintFromHex,
  DaRequestResponseProtocol,
  decodeDaMetadataByHeaderResponseCbor,
  decodeDaPayloadByHeaderResponseCbor,
  decodeDaPayloadChunkResponseCbor,
  encodeDaPayloadByHeaderRequestCbor,
  encodeDaPayloadChunkRequestCbor,
} from "@al-ft/midgard-core/da-transport";

import type { Libp2pDaTransportLimits } from "../../config.js";
import { hexToBytes } from "../../utils/hex.js";
import type {
  DaPayloadCandidate,
  DaPayloadCandidatesResult,
  DaPayloadFetchFailure,
  DaPayloadFetchFailureStatus,
  DaPayloadSource,
} from "../source.js";
import { DaLibp2pNode } from "./DaLibp2pNode.js";
import type { DaPeerRegistryEntry } from "./DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "./DaProtocols.js";
import {
  type DaLibp2pPayloadSourceOptions,
  dedupePeers,
} from "./payload-source.da-libp2p-payload-source-options.js";

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
    const byHeaderResponse = decodeDaPayloadByHeaderResponseCbor(
      await this.node.request({
        peer,
        protocolId: this.protocolId(DaRequestResponseProtocol.payloadByHeader),
        timeoutMs: this.limits.requestTimeoutMs,
        payload: encodeDaPayloadByHeaderRequestCbor({
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
        const metadata = await this.fetchMetadata(peer, headerHash);
        assertCanonicalPayloadMetadata(metadata);
        return {
          sourcePeerId: peer.peerId,
          payloadCbor,
          payloadSchemaVersion: 1,
          metadata,
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
        const metadata = await this.fetchMetadata(peer, headerHash);
        assertCanonicalPayloadMetadata(metadata);
        return {
          sourcePeerId: peer.peerId,
          payloadCbor,
          payloadSchemaVersion: 1,
          metadata,
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
      const response = decodeDaPayloadChunkResponseCbor(
        await this.node.request({
          peer,
          protocolId: this.protocolId(DaRequestResponseProtocol.payloadChunk),
          timeoutMs: this.limits.requestTimeoutMs,
          payload: encodeDaPayloadChunkRequestCbor({
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
  ): Promise<
    ReturnType<typeof decodeDaMetadataByHeaderResponseCbor> | undefined
  > {
    const response = decodeDaMetadataByHeaderResponseCbor(
      await this.node.request({
        peer,
        protocolId: this.protocolId(DaRequestResponseProtocol.metadataByHeader),
        timeoutMs: this.limits.requestTimeoutMs,
        payload: encodeDaPayloadByHeaderRequestCbor({
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

const assertCanonicalPayloadMetadata = (
  metadata: ReturnType<typeof decodeDaMetadataByHeaderResponseCbor> | undefined,
): void => {
  if (metadata?.payloadSchemaVersion !== 1) {
    throw new InvalidDaPayloadSourceResponseError(
      "payload metadata is missing canonical V1 schema binding",
    );
  }
};

export class DaPayloadSubmitAdmission {
  readonly limit: number;
  #active = 0;
  #maxObservedActive = 0;
  readonly #waiters: Array<{
    readonly grant: () => void;
    readonly signal?: AbortSignal;
  }> = [];

  constructor(limit = 1) {
    if (!Number.isSafeInteger(limit) || limit <= 0) {
      throw new RangeError(
        "DA payload-submit admission limit must be a positive integer",
      );
    }
    this.limit = limit;
  }

  get active(): number {
    return this.#active;
  }

  get maxObservedActive(): number {
    return this.#maxObservedActive;
  }

  async run<T>(
    operation: () => Promise<T> | T,
    { signal }: { readonly signal?: AbortSignal } = {},
  ): Promise<T> {
    await this.acquire(signal);
    try {
      signal?.throwIfAborted();
      return await operation();
    } finally {
      this.release();
    }
  }

  private async acquire(signal?: AbortSignal): Promise<void> {
    signal?.throwIfAborted();
    if (this.#active < this.limit && this.#waiters.length === 0) {
      this.#active += 1;
      this.#maxObservedActive = Math.max(this.#maxObservedActive, this.#active);
      return;
    }
    await new Promise<void>((resolve, reject) => {
      const onAbort = (): void => {
        const index = this.#waiters.indexOf(waiter);
        if (index >= 0) {
          this.#waiters.splice(index, 1);
        }
        const reason = signal?.reason;
        reject(
          reason instanceof Error
            ? reason
            : new Error("DA payload admission cancelled", { cause: reason }),
        );
      };
      const waiter = {
        signal,
        grant: (): void => {
          signal?.removeEventListener("abort", onAbort);
          this.#active += 1;
          this.#maxObservedActive = Math.max(
            this.#maxObservedActive,
            this.#active,
          );
          resolve();
        },
      };
      this.#waiters.push(waiter);
      signal?.addEventListener("abort", onAbort, { once: true });
      if (signal?.aborted === true) {
        onAbort();
      }
    });
  }

  private release(): void {
    this.#active -= 1;
    while (this.#waiters.length > 0) {
      const waiter = this.#waiters.shift()!;
      if (waiter.signal?.aborted === true) {
        continue;
      }
      waiter.grant();
      break;
    }
  }
}

/** Shared by every handler map created in this process. */
export const processWideDaPayloadSubmitAdmission = new DaPayloadSubmitAdmission(
  1,
);

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
