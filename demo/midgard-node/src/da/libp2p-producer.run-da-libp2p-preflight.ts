import { withDaRequestDeadline } from "@al-ft/midgard-core/da-request-deadline";
import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "@al-ft/midgard-core/da-stream-codec";
import {
  daDeploymentFingerprintFromHex,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  encodeDaPayloadByHeaderRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";

import { loadAndValidateProducerIdentity } from "./libp2p-producer.create-da-libp2p-retained-payload-request-handlers.js";
import {
  type DaLibp2pPreflightFailure,
  type DaLibp2pPreflightListenCheck,
  type DaLibp2pPreflightMode,
  type DaLibp2pPreflightReport,
  type DaProducerProbeTransport,
  type DaProducerPublicationManifest,
} from "./libp2p-producer.parse-committee-peers.js";
import { isRecord } from "./libp2p-producer.parse-da-producer-publication-manifest.js";
import {
  boundListenCheck,
  preflightCommitteePeer,
  preflightIdentityFailure,
  preflightPeerFailures,
  preflightWarnings,
  skippedListenCheck,
  uniqueReachableSignerIndexes,
} from "./libp2p-producer.reconcile-da-payload-peer-from-env.js";

export const runDaLibp2pPreflight = async ({
  manifest,
  transport,
  mode = "bind-listen",
  listenCheck,
}: {
  readonly manifest: DaProducerPublicationManifest;
  readonly transport: DaProducerProbeTransport;
  readonly mode?: DaLibp2pPreflightMode;
  readonly listenCheck?: DaLibp2pPreflightListenCheck;
}): Promise<DaLibp2pPreflightReport> => {
  const protocolId = daRequestResponseProtocolId(
    manifest.deploymentFingerprint,
    DaRequestResponseProtocol.metadataByHeader,
  );
  const request = encodeDaPayloadByHeaderRequestCbor({
    deploymentFingerprint: daDeploymentFingerprintFromHex(
      manifest.deploymentFingerprint,
    ),
    headerHash: Buffer.alloc(28),
    acceptedPayloadHashes: null,
    maxInlineBytes: 0,
  });
  const localPeerId = await transport.localPeerId();
  const identityFailure = preflightIdentityFailure(manifest, localPeerId);
  if (identityFailure !== undefined) {
    return {
      configured: true,
      mode,
      deploymentFingerprint: manifest.deploymentFingerprint,
      localPeerId,
      threshold: manifest.threshold,
      reachableCommitteePeers: 0,
      reachableCommitteeSignerIndexes: [],
      listenCheck:
        listenCheck ??
        (mode === "bind-listen"
          ? boundListenCheck(manifest)
          : skippedListenCheck(
              manifest.listenMultiaddrs,
              manifest.announceMultiaddrs,
            )),
      failures: [identityFailure],
      warnings: preflightWarnings(mode),
      passed: false,
      peerResults: [],
      reason: identityFailure.error,
    };
  }
  const peerResults = await Promise.all(
    manifest.committeePeers.map((peer) =>
      preflightCommitteePeer({
        peer,
        protocolId,
        request,
        timeoutMs: manifest.requestTimeoutMs,
        transport,
      }),
    ),
  );
  const reachableCommitteeSignerIndexes =
    uniqueReachableSignerIndexes(peerResults);
  const reachableCommitteePeers = reachableCommitteeSignerIndexes.length;
  const thresholdFailure =
    reachableCommitteePeers >= manifest.threshold
      ? []
      : [
          {
            phase: "dial" as const,
            kind: "peer_unreachable" as const,
            error: `reachable committee signer indexes ${reachableCommitteePeers.toString()} below threshold ${manifest.threshold.toString()}`,
            remediation:
              "Start enough DA committee listeners, verify the runtime manifest addresses, and rerun the preflight.",
          },
        ];
  const failures = [...preflightPeerFailures(peerResults), ...thresholdFailure];
  const passed = reachableCommitteePeers >= manifest.threshold;
  return {
    configured: true,
    mode,
    deploymentFingerprint: manifest.deploymentFingerprint,
    localPeerId,
    threshold: manifest.threshold,
    reachableCommitteePeers,
    reachableCommitteeSignerIndexes,
    listenCheck:
      listenCheck ??
      (mode === "bind-listen"
        ? boundListenCheck(manifest)
        : skippedListenCheck(
            manifest.listenMultiaddrs,
            manifest.announceMultiaddrs,
          )),
    failures,
    warnings: preflightWarnings(mode),
    passed,
    peerResults,
    ...(passed
      ? {}
      : {
          reason: `reachable committee signer indexes ${reachableCommitteePeers.toString()} below threshold ${manifest.threshold.toString()}`,
        }),
  };
};

export const createDaLibp2pProducerProbeTransport = async (
  manifest: DaProducerPublicationManifest,
  {
    mode = "bind-listen",
  }: {
    readonly mode?: DaLibp2pPreflightMode;
  } = {},
): Promise<DaProducerProbeTransport> => {
  const [
    { createLibp2p },
    { tcp },
    { noise },
    { yamux },
    { bootstrap },
    { identify },
    { multiaddr },
  ] = await Promise.all([
    import("libp2p"),
    import("@libp2p/tcp"),
    import("@chainsafe/libp2p-noise"),
    import("@chainsafe/libp2p-yamux"),
    import("@libp2p/bootstrap"),
    import("@libp2p/identify"),
    import("@multiformats/multiaddr"),
  ]);
  const identity = await loadAndValidateProducerIdentity(manifest);
  const privateKey = identity.privateKey;
  const localPeerId = identity.peerId;
  const node = await createLibp2p({
    privateKey,
    addresses: {
      listen: mode === "bind-listen" ? [...manifest.listenMultiaddrs] : [],
      announce: mode === "bind-listen" ? [...manifest.announceMultiaddrs] : [],
    },
    transports: [tcp()],
    connectionEncrypters: [noise()],
    streamMuxers: [yamux()],
    peerDiscovery:
      mode === "bind-listen" && manifest.bootstrapMultiaddrs.length > 0
        ? [bootstrap({ list: [...manifest.bootstrapMultiaddrs] })]
        : [],
    services: {
      identify: identify(),
    },
  });
  return {
    localPeerId: () => localPeerId,
    request: async (peer, protocolId, payload, timeoutMs) =>
      withDaRequestDeadline({
        timeoutMs,
        open: (signal) =>
          node.dialProtocol(
            peer.multiaddrs.map((address) => multiaddr(address)),
            protocolId,
            { signal },
          ),
        run: async (stream) => {
          await writeDaStreamFrame(stream, payload, {
            maxFrameBytes: manifest.maxPayloadBytes,
            close: true,
          });
          return readSingleDaStreamFrame(stream, {
            maxFrameBytes: manifest.maxPayloadBytes,
          });
        },
        abort: (stream, error) => stream.abort?.(error),
      }),
    close: async () => {
      await node.stop();
    },
  };
};

export const preflightStartupFailureReport = ({
  manifest,
  mode,
  error,
}: {
  readonly manifest: DaProducerPublicationManifest;
  readonly mode: DaLibp2pPreflightMode;
  readonly error: unknown;
}): DaLibp2pPreflightReport => {
  const failure = classifyPreflightStartupFailure(error, mode);
  const listenCheck =
    failure.phase === "listen"
      ? failedListenCheck(manifest, failure.error)
      : skippedListenCheck(
          manifest.listenMultiaddrs,
          manifest.announceMultiaddrs,
        );
  return {
    configured: true,
    mode,
    deploymentFingerprint: manifest.deploymentFingerprint,
    threshold: manifest.threshold,
    reachableCommitteePeers: 0,
    reachableCommitteeSignerIndexes: [],
    listenCheck,
    failures: [failure],
    warnings: preflightWarnings(mode),
    passed: false,
    peerResults: [],
    reason: failure.error,
  };
};

const classifyPreflightStartupFailure = (
  error: unknown,
  mode: DaLibp2pPreflightMode,
): DaLibp2pPreflightFailure => {
  const message = formatUnknownError(error);
  if (isProducerPortAlreadyBound(error, message)) {
    return {
      phase: "listen",
      kind: "producer_port_already_bound",
      error: message,
      remediation:
        "Stop the stale producer or run bind-listen preflight before starting the producer; do not use dial-only as fresh-deployment listener proof.",
    };
  }
  if (message.includes("is not present in announce_multiaddrs")) {
    return {
      phase: "identity",
      kind: "identity_mismatch",
      error: message,
      remediation:
        "Use the same DA_LIBP2P_PRIVATE_KEY_SOURCE that generated the producer announce_multiaddrs.",
    };
  }
  return {
    phase: mode === "bind-listen" ? "listen" : "identity",
    kind: "unexpected",
    error: message,
  };
};

const isProducerPortAlreadyBound = (
  error: unknown,
  formatted: string,
): boolean => {
  if (formatted.includes("EADDRINUSE")) {
    return true;
  }
  let current: unknown = error;
  while (current !== undefined && current !== null) {
    if (isRecord(current) && current.code === "EADDRINUSE") {
      return true;
    }
    current = isRecord(current) ? current.cause : undefined;
  }
  return false;
};

const failedListenCheck = (
  manifest: DaProducerPublicationManifest,
  error: string,
): DaLibp2pPreflightListenCheck => ({
  checked: true,
  status: "failed",
  listenMultiaddrs: manifest.listenMultiaddrs,
  announceMultiaddrs: manifest.announceMultiaddrs,
  error,
});
