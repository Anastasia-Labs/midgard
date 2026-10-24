import {
  DA_PAYLOAD_INNER_SCHEMA_VERSION,
  DaPayloadContentEncoding,
} from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
  daDeploymentFingerprintFromHex,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaPayloadByHeaderRequestCbor,
  encodeDaCapabilitiesResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { vi } from "vitest";

import { WatcherPublicDaLibp2pTransport } from "../../src/storage/public-da-libp2p-transport.js";

/** Serve the exact staged envelopes at the public transport boundary. */
export const serveEmulatorRetainedDa = (
  input: Readonly<{
    deploymentFingerprint: string;
    blocks: readonly Readonly<{
      headerHash: string;
      payloadEnvelopeCbor: Uint8Array;
    }>[];
  }>,
) => {
  const fingerprint = daDeploymentFingerprintFromHex(
    input.deploymentFingerprint,
  );
  const payloads = new Map(
    input.blocks.map((block) => [
      block.headerHash,
      Buffer.from(block.payloadEnvelopeCbor),
    ]),
  );
  vi.spyOn(
    WatcherPublicDaLibp2pTransport.prototype,
    "start",
  ).mockResolvedValue();
  vi.spyOn(
    WatcherPublicDaLibp2pTransport.prototype,
    "stop",
  ).mockResolvedValue();
  const requests = vi
    .spyOn(WatcherPublicDaLibp2pTransport.prototype, "request")
    .mockImplementation(async (request) => {
      request.signal.throwIfAborted();
      if (
        request.protocolId !==
        daRequestResponseProtocolId(
          input.deploymentFingerprint,
          request.protocol,
        )
      ) {
        throw new Error("Fixture DA request belongs to another deployment");
      }
      if (request.protocol === DaRequestResponseProtocol.capabilities) {
        return encodeDaCapabilitiesResponseCbor({
          deploymentFingerprint: fingerprint,
          transportProtocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
          payloadSchemaVersions: [DA_PAYLOAD_INNER_SCHEMA_VERSION],
          envelopeContentEncodings: [DaPayloadContentEncoding.identity],
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
          maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
          maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
          maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
          requestTimeoutMs: DA_TRANSPORT_LIMITS.requestTimeoutMs,
        });
      }
      if (request.protocol === DaRequestResponseProtocol.payloadByHeader) {
        const decoded = decodeDaPayloadByHeaderRequestCbor(request.requestCbor);
        const payload = payloads.get(decoded.headerHash.toString("hex"));
        if (payload === undefined) {
          return encodeDaPayloadByHeaderResponseCbor({
            status: "not_found",
            headerHash: decoded.headerHash,
            payloadHash: null,
            payloadBytes: null,
            chunkManifest: null,
            reasonCode: null,
          });
        }
        return encodeDaPayloadByHeaderResponseCbor({
          status: "found_inline",
          headerHash: decoded.headerHash,
          payloadHash: computeDaSha256Hash(payload),
          payloadBytes: payload,
          chunkManifest: null,
          reasonCode: null,
        });
      }
      throw new Error(`Fixture has no DA request for ${request.protocol}`);
    });
  return {
    requests,
    addBlock: (
      block: Readonly<{ headerHash: string; payloadEnvelopeCbor: Uint8Array }>,
    ) => {
      const bytes = Buffer.from(block.payloadEnvelopeCbor);
      const previous = payloads.get(block.headerHash);
      if (previous !== undefined && !previous.equals(bytes))
        throw new Error(
          "Retained DA fixture cannot replace an existing header payload",
        );
      payloads.set(block.headerHash, bytes);
    },
  };
};
