import "./libp2p-producer.publish-da-payload-insert-from-env.js";

import {
  type DaCapabilitiesResponse,
  daDeploymentFingerprintFromHex,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaCapabilitiesResponseCbor,
  encodeDaCapabilitiesRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";

import { writeSharedFrameChunks } from "./libp2p-producer.create-da-libp2p-producer-transport.js";
import {
  type DaEnvelopeCapabilityMode,
  type DaEnvelopeCapabilityPeerResult,
  type DaLibp2pPreflightMode,
  type DaLibp2pPreflightReport,
  type DaProducerProbeTransport,
  type DaProducerPublicationManifest,
} from "./libp2p-producer.parse-committee-peers.js";
import { loadDaProducerPublicationManifestFromEnv } from "./libp2p-producer.parse-da-producer-publication-manifest.js";
import {
  preflightWarnings,
  skippedListenCheck,
} from "./libp2p-producer.reconcile-da-payload-peer-from-env.js";
import {
  createDaLibp2pProducerProbeTransport,
  preflightStartupFailureReport,
  runDaLibp2pPreflight,
} from "./libp2p-producer.run-da-libp2p-preflight.js";

export const runDaLibp2pPreflightFromEnv = async (
  env: NodeJS.ProcessEnv = process.env,
  options: {
    readonly mode?: DaLibp2pPreflightMode;
  } = {},
): Promise<DaLibp2pPreflightReport> => {
  const mode = options.mode ?? "bind-listen";
  const manifest = await loadDaProducerPublicationManifestFromEnv(env);
  if (manifest === null) {
    return {
      configured: false,
      mode,
      reachableCommitteePeers: 0,
      reachableCommitteeSignerIndexes: [],
      listenCheck: skippedListenCheck([], []),
      failures: [],
      warnings: preflightWarnings(mode),
      passed: false,
      peerResults: [],
      reason: "no libp2p DA manifest configured",
    };
  }
  let transport: DaProducerProbeTransport;
  try {
    transport = await createDaLibp2pProducerProbeTransport(manifest, { mode });
  } catch (error) {
    return preflightStartupFailureReport({
      manifest,
      mode,
      error,
    });
  }
  try {
    return await runDaLibp2pPreflight({ manifest, transport, mode });
  } finally {
    await transport.close?.();
  }
};

const capabilityMismatch = (
  manifest: DaProducerPublicationManifest,
  mode: DaEnvelopeCapabilityMode,
  response: DaCapabilitiesResponse,
): string | undefined => {
  if (
    !response.deploymentFingerprint.equals(
      daDeploymentFingerprintFromHex(manifest.deploymentFingerprint),
    )
  ) {
    return "deployment fingerprint mismatch";
  }
  if (!response.payloadSchemaVersions.includes(1)) {
    return "payload schema version 1 is not supported";
  }
  const requiredEncoding = mode === "identity" ? 0 : 1;
  if (!response.envelopeContentEncodings.includes(requiredEncoding)) {
    return `${mode} envelope content encoding is not supported`;
  }
  const limitMismatches = [
    ["max_payload_bytes", response.maxPayloadBytes, manifest.maxPayloadBytes],
    [
      "max_inline_response_bytes",
      response.maxInlineResponseBytes,
      manifest.maxInlineResponseBytes,
    ],
    ["max_chunk_bytes", response.maxChunkBytes, manifest.maxChunkBytes],
    [
      "max_streams_per_peer",
      response.maxStreamsPerPeer,
      manifest.maxStreamsPerPeer,
    ],
    [
      "request_timeout_ms",
      response.requestTimeoutMs,
      manifest.requestTimeoutMs,
    ],
  ] as const;
  const mismatch = limitMismatches.find(
    ([, actual, expected]) => actual !== expected,
  );
  return mismatch === undefined
    ? undefined
    : `${mismatch[0]}=${mismatch[1].toString()} does not match manifest ${mismatch[2].toString()}`;
};

export const probeDaEnvelopeCapabilities = async ({
  manifest,
  mode,
  transport,
}: {
  readonly manifest: DaProducerPublicationManifest;
  readonly mode: DaEnvelopeCapabilityMode;
  readonly transport: DaProducerProbeTransport;
}): Promise<readonly DaEnvelopeCapabilityPeerResult[]> => {
  const protocolId = daRequestResponseProtocolId(
    manifest.deploymentFingerprint,
    DaRequestResponseProtocol.capabilities,
  );
  const request = encodeDaCapabilitiesRequestCbor({
    deploymentFingerprint: daDeploymentFingerprintFromHex(
      manifest.deploymentFingerprint,
    ),
  });
  return Promise.all(
    manifest.committeePeers.map(async (peer) => {
      try {
        const capabilities = decodeDaCapabilitiesResponseCbor(
          await transport.request(
            peer,
            protocolId,
            request,
            manifest.requestTimeoutMs,
          ),
        );
        const mismatch = capabilityMismatch(manifest, mode, capabilities);
        return {
          peerId: peer.peerId,
          signerIndex: peer.signerIndex,
          capable: mismatch === undefined,
          capabilities,
          ...(mismatch === undefined ? {} : { error: mismatch }),
        };
      } catch (cause) {
        return {
          peerId: peer.peerId,
          signerIndex: peer.signerIndex,
          capable: false,
          error: formatUnknownError(cause),
        };
      }
    }),
  );
};

export const assertDaEnvelopeCapabilityQuorum = async ({
  manifest,
  mode,
  transport: providedTransport,
}: {
  readonly manifest: DaProducerPublicationManifest;
  readonly mode: DaEnvelopeCapabilityMode;
  readonly transport?: DaProducerProbeTransport;
}): Promise<readonly DaEnvelopeCapabilityPeerResult[]> => {
  const transport =
    providedTransport ??
    (await createDaLibp2pProducerProbeTransport(manifest, {
      mode: "dial-only",
    }));
  try {
    const results = await probeDaEnvelopeCapabilities({
      manifest,
      mode,
      transport,
    });
    const capableSignerIndexes = new Set(
      results
        .filter((result) => result.capable)
        .map((result) => result.signerIndex),
    );
    if (capableSignerIndexes.size < manifest.threshold) {
      const details = results
        .filter((result) => !result.capable)
        .map(
          (result) =>
            `${result.peerId}[${result.signerIndex.toString()}]=${result.error ?? "incapable"}`,
        )
        .join(",");
      throw new Error(
        `DA ${mode} envelope capability quorum failed: capable_signers=${capableSignerIndexes.size.toString()},threshold=${manifest.threshold.toString()},peers=${details}`,
      );
    }
    return results;
  } finally {
    if (providedTransport === undefined) {
      await transport.close?.();
    }
  }
};

export const writeSharedDaFrameChunksForTest = writeSharedFrameChunks;
