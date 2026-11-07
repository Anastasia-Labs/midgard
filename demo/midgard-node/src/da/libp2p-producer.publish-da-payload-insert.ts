import { encodeDaStreamFrame } from "@al-ft/midgard-core/da-stream-codec";
import {
  daDeploymentFingerprintFromHex,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaPayloadSubmitResponseCbor,
  encodeDaPayloadSubmitRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Duration, Effect, Metric } from "effect";

import { DaPayloadsDB } from "../database/index.js";
import { readDaHardeningConfig } from "./hardening-config.js";
import {
  publishDaPayloadAnnouncement,
  verifyPayloadHash,
} from "./libp2p-producer.create-da-libp2p-producer-transport.js";
import {
  ACCEPTED_RESPONSE_STATUSES,
  DaPayloadPublicationError,
  type DaProducerCommitteePeer,
  type DaProducerPeerResult,
  type DaProducerPublicationManifest,
  type DaProducerPublicationReport,
  type DaProducerTransport,
  daPublishAllPeerDurationTimer,
  daPublishConflictCounter,
  daPublishPeerDurationTimer,
  daPublishRejectedCounter,
  daPublishStragglerCounter,
  daPublishThresholdDurationTimer,
} from "./libp2p-producer.parse-committee-peers.js";

export const publishDaPayloadInsert = async ({
  insert,
  manifest,
  transport,
  announcedAtSlot = 0,
  onPeerResult,
}: {
  readonly insert: DaPayloadsDB.InsertInput;
  readonly manifest: DaProducerPublicationManifest;
  readonly transport: DaProducerTransport;
  readonly announcedAtSlot?: number;
  readonly onPeerResult?: (args: {
    readonly peer: DaProducerCommitteePeer;
    readonly result: DaProducerPeerResult;
  }) => Promise<void>;
}): Promise<DaProducerPublicationReport> => {
  const headerHash = insert[DaPayloadsDB.Columns.HEADER_HASH];
  const payloadBytes = insert[DaPayloadsDB.Columns.PAYLOAD_CBOR];
  const payloadHash = verifyPayloadHash(insert);
  if (payloadBytes.length > manifest.maxPayloadBytes) {
    throw new Error(
      `DA payload ${headerHash.toString(
        "hex",
      )} is ${payloadBytes.length.toString()} bytes, exceeding max_payload_bytes=${manifest.maxPayloadBytes.toString()}`,
    );
  }
  const deploymentFingerprint = daDeploymentFingerprintFromHex(
    manifest.deploymentFingerprint,
  );
  const protocolId = daRequestResponseProtocolId(
    manifest.deploymentFingerprint,
    DaRequestResponseProtocol.payloadSubmit,
  );
  const request = encodeDaPayloadSubmitRequestCbor({
    deploymentFingerprint,
    headerHash,
    payloadHash,
    payloadSchemaVersion: insert[DaPayloadsDB.Columns.VERSION],
    mode: "inline",
    payloadBytes,
    chunkManifest: null,
  });
  const sharedRequestFrame = encodeDaStreamFrame(request, {
    maxFrameBytes: manifest.maxPayloadBytes,
  });
  const publishStartedAt = Date.now();
  const peerResults: DaProducerPeerResult[] = [];
  let acceptedPeers = 0;
  let settledPeers = 0;
  let nextPeerIndex = 0;
  let activePeers = 0;
  let thresholdReached = false;
  const configuredConcurrency = readDaHardeningConfig().publishConcurrency;
  const concurrency = Math.max(
    1,
    Math.min(configuredConcurrency, manifest.committeePeers.length),
  );
  let resolveDecision!: (reached: boolean) => void;
  let resolveAll!: () => void;
  const decision = new Promise<boolean>((resolve) => {
    resolveDecision = resolve;
  });
  const allSettled = new Promise<void>((resolve) => {
    resolveAll = resolve;
  });
  const launch = (): void => {
    while (
      activePeers < concurrency &&
      nextPeerIndex < manifest.committeePeers.length
    ) {
      const peer = manifest.committeePeers[nextPeerIndex++]!;
      activePeers += 1;
      const peerStartedAt = Date.now();
      void submitPayloadToPeer({
        peer,
        headerHash,
        protocolId,
        request,
        requestFrame: sharedRequestFrame,
        timeoutMs: manifest.requestTimeoutMs,
        maxChunkBytes: manifest.maxChunkBytes,
        payloadHash,
        transport,
      }).then((result) => {
        activePeers -= 1;
        settledPeers += 1;
        peerResults.push(result);
        void Promise.resolve(onPeerResult?.({ peer, result })).catch(
          () => undefined,
        );
        const wasStraggler = thresholdReached;
        Effect.runSync(
          Metric.update(
            Metric.tagged(
              Metric.tagged(
                daPublishPeerDurationTimer,
                "peer_id",
                result.peerId,
              ),
              "status",
              result.status,
            ),
            Duration.millis(Date.now() - peerStartedAt),
          ),
        );
        if (result.status === "rejected") {
          Effect.runSync(
            Metric.increment(
              Metric.tagged(
                daPublishRejectedCounter,
                "reason_code",
                result.error ?? "unknown",
              ),
            ),
          );
        }
        if (result.status === "conflict") {
          Effect.runSync(Metric.increment(daPublishConflictCounter));
          console.error(
            `DA publication conflict header=${headerHash.toString("hex")},peer=${result.peerId}; durable conflict evidence retained`,
          );
        }
        if (wasStraggler) {
          Effect.runSync(
            Metric.increment(
              Metric.tagged(daPublishStragglerCounter, "status", result.status),
            ),
          );
        }
        if (ACCEPTED_RESPONSE_STATUSES.has(result.status)) {
          acceptedPeers += 1;
          if (!thresholdReached && acceptedPeers >= manifest.threshold) {
            thresholdReached = true;
            resolveDecision(true);
          }
        }
        launch();
        if (settledPeers === manifest.committeePeers.length) {
          if (!thresholdReached) {
            resolveDecision(false);
          }
          resolveAll();
        }
      });
    }
  };
  launch();
  const reachedThreshold = await decision;
  const allPeerResults = allSettled.then(() => [...peerResults]);
  void allSettled.then(() => {
    Effect.runSync(
      Metric.update(
        daPublishAllPeerDurationTimer,
        Duration.millis(Date.now() - publishStartedAt),
      ),
    );
  });
  const reportWithoutAnnouncement: DaProducerPublicationReport = {
    configured: true,
    headerHash: headerHash.toString("hex"),
    payloadHash: payloadHash.toString("hex"),
    deploymentFingerprint: manifest.deploymentFingerprint,
    threshold: manifest.threshold,
    acceptedPeers,
    peerResults: [...peerResults],
    allPeerResults,
  };
  if (!reachedThreshold) {
    throw new DaPayloadPublicationError(
      `DA payload publication accepted by ${acceptedPeers.toString()} peer(s), below threshold ${manifest.threshold.toString()}`,
      reportWithoutAnnouncement,
    );
  }
  Effect.runSync(
    Metric.update(
      daPublishThresholdDurationTimer,
      Duration.millis(Date.now() - publishStartedAt),
    ),
  );
  const announcement = await publishDaPayloadAnnouncement({
    insert,
    manifest,
    transport,
    announcedAtSlot,
  });
  return {
    ...reportWithoutAnnouncement,
    announcement,
  };
};

export const publicationTransportKey = (
  manifest: DaProducerPublicationManifest,
): string =>
  JSON.stringify({
    deploymentFingerprint: manifest.deploymentFingerprint,
    localPrivateKeySource: manifest.localPrivateKeySource,
    committeePeers: manifest.committeePeers,
  });

export const submitPayloadToPeer = async ({
  peer,
  headerHash,
  protocolId,
  request,
  requestFrame,
  timeoutMs,
  maxChunkBytes,
  payloadHash,
  transport,
}: {
  readonly peer: DaProducerCommitteePeer;
  readonly headerHash: Buffer;
  readonly protocolId: string;
  readonly request: Uint8Array;
  readonly requestFrame?: Uint8Array;
  readonly timeoutMs: number;
  readonly maxChunkBytes?: number;
  readonly payloadHash: Buffer;
  readonly transport: DaProducerTransport;
}): Promise<DaProducerPeerResult> => {
  try {
    const responseBytes =
      requestFrame !== undefined &&
      maxChunkBytes !== undefined &&
      transport.requestFramed !== undefined
        ? await transport.requestFramed(
            peer,
            protocolId,
            requestFrame,
            timeoutMs,
            maxChunkBytes,
          )
        : await transport.request(peer, protocolId, request, timeoutMs);
    const response = decodeDaPayloadSubmitResponseCbor(responseBytes);
    if (!response.headerHash.equals(headerHash)) {
      throw new Error("payload-submit response header_hash mismatch");
    }
    if (!response.payloadHash.equals(payloadHash)) {
      throw new Error("payload-submit response payload_hash mismatch");
    }
    return {
      peerId: peer.peerId,
      signerIndex: peer.signerIndex,
      protocolId,
      status: response.status,
      payloadHash: response.payloadHash.toString("hex"),
      ...(response.reasonCode === null ? {} : { error: response.reasonCode }),
    };
  } catch (error) {
    return {
      peerId: peer.peerId,
      signerIndex: peer.signerIndex,
      protocolId,
      status: "transport_error",
      payloadHash: payloadHash.toString("hex"),
      error: formatUnknownError(error),
    };
  }
};
