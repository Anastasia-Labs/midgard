import {
  DA_TRANSPORT_LIMITS,
  DaRequestResponseProtocol,
  decodeDaCapabilitiesResponseCbor,
  encodeDaCapabilitiesRequestCbor,
} from "@al-ft/midgard-core/da-transport";

import {
  decodeResponse,
  invalidContent,
  type NegotiatedLimits,
  validateCapabilities,
} from "./public-da-client.strict-inner-payload.js";

export const negotiatePublicDaLimits = async (
  deploymentFingerprint: Buffer,
  request: (cbor: Buffer) => Promise<Buffer>,
): Promise<NegotiatedLimits> => {
  const response = decodeResponse(
    await request(encodeDaCapabilitiesRequestCbor({ deploymentFingerprint })),
    decodeDaCapabilitiesResponseCbor,
    DaRequestResponseProtocol.capabilities,
  );
  if (!response.deploymentFingerprint.equals(deploymentFingerprint)) {
    invalidContent(DaRequestResponseProtocol.capabilities);
  }
  validateCapabilities(response);
  return {
    maxPayloadBytes: Math.min(
      response.maxPayloadBytes,
      DA_TRANSPORT_LIMITS.maxPayloadBytes,
    ),
    maxInlineResponseBytes: Math.min(
      response.maxInlineResponseBytes,
      DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
    ),
    maxChunkBytes: Math.min(
      response.maxChunkBytes,
      DA_TRANSPORT_LIMITS.maxChunkBytes,
    ),
  };
};
