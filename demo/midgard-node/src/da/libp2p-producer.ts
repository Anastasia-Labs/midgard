import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_PROTOCOL_VERSION,
  daDeploymentFingerprintFromHex,
  DaGossipTopic,
  daGossipTopic,
  type DaMetadataByHeaderResponseV1,
  type DaPayloadByHeaderResponseV1,
  type DaPayloadChunkManifestV1,
  type DaPayloadSubmitResponseV1,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  DaTransportSigningDomain,
  decodeDaMetadataByHeaderResponseV1Cbor,
  decodeDaPayloadByHeaderRequestV1Cbor,
  decodeDaPayloadChunkRequestV1Cbor,
  decodeDaPayloadSubmitResponseV1Cbor,
  encodeDaMetadataByHeaderResponseV1Cbor,
  encodeDaPayloadAnnouncementV1Cbor,
  encodeDaPayloadByHeaderRequestV1Cbor,
  encodeDaPayloadByHeaderResponseV1Cbor,
  encodeDaPayloadChunkResponseV1Cbor,
  encodeDaPayloadSubmitRequestV1Cbor,
  parseDaLibp2pRuntimeManifestDeploymentIdentity,
} from "@al-ft/midgard-core/da-transport";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect } from "effect";

import { loadDaLibp2pIdentity } from "@/da/libp2p-identity.js";
import { DaPayloadsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";

const MANIFEST_PATH_ENV = "MIDGARD_DEPLOYMENT_MANIFEST_PATH";
const FALLBACK_MANIFEST_PATH_ENV = "MIDGARD_DA_DEPLOYMENT_MANIFEST_PATH";
const LIBP2P_PRIVATE_KEY_SOURCE_ENV = "DA_LIBP2P_PRIVATE_KEY_SOURCE";
const ACCEPTED_RESPONSE_STATUSES = new Set(["accepted", "duplicate"]);
const FORBIDDEN_LIBP2P_DA_CONFIG_KEYS = new Set([
  "baseUrl",
  "base_url",
  "baseUrls",
  "base_urls",
  "endpoint",
  "endpoints",
  "url",
  "urls",
  "httpEndpoint",
  "http_endpoint",
  "committeeEndpoint",
  "committee_endpoint",
  "daEndpoint",
  "da_endpoint",
  "gateway",
  "objectStore",
  "object_store",
  "bucket",
  "s3",
]);

export type DaProducerCommitteePeer = {
  readonly signerIndex: number;
  readonly daVkey: string;
  readonly peerId: string;
  readonly multiaddrs: readonly string[];
  readonly roles: readonly string[];
};

export type DaProducerPublicationManifest = {
  readonly deploymentFingerprint: string;
  readonly localPrivateKeySource: string;
  readonly threshold: number;
  readonly requestTimeoutMs: number;
  readonly maxPayloadBytes: number;
  readonly maxInlineResponseBytes: number;
  readonly maxChunkBytes: number;
  readonly maxStreamsPerPeer: number;
  readonly maxGossipMessageBytes: number;
  readonly listenMultiaddrs: readonly string[];
  readonly announceMultiaddrs: readonly string[];
  readonly bootstrapMultiaddrs: readonly string[];
  readonly committeePeers: readonly DaProducerCommitteePeer[];
};

export type DaProducerPeerResult = {
  readonly peerId: string;
  readonly signerIndex: number;
  readonly protocolId: string;
  readonly status: DaPayloadSubmitResponseV1["status"] | "transport_error";
  readonly payloadHash: string;
  readonly error?: string;
};

export type DaProducerAnnouncementResult = {
  readonly topic: string;
  readonly payloadHash: string;
  readonly recipients: readonly string[];
};

export type DaProducerPublicationReport = {
  readonly configured: boolean;
  readonly headerHash: string;
  readonly payloadHash: string;
  readonly deploymentFingerprint?: string;
  readonly threshold?: number;
  readonly acceptedPeers: number;
  readonly peerResults: readonly DaProducerPeerResult[];
  readonly announcement?: DaProducerAnnouncementResult;
  readonly reason?: string;
};

export type DaRetainedPayloadLookup = (
  headerHash: Buffer,
) => Promise<DaPayloadsDB.Row | undefined>;

export type DaRetainedPayloadServer = {
  readonly configured: boolean;
  readonly deploymentFingerprint?: string;
  readonly localPeerId?: string;
  readonly listenMultiaddrs?: readonly string[];
  readonly announceMultiaddrs?: readonly string[];
  readonly close?: () => Promise<void>;
  readonly reason?: string;
};

export type DaLibp2pPreflightPeerStatus =
  | "reachable"
  | "not_found"
  | "protocol_error"
  | "transport_error";

export type DaLibp2pPreflightPeerResult = {
  readonly peerId: string;
  readonly signerIndex: number;
  readonly address: readonly string[];
  readonly protocolId: string;
  readonly status: DaLibp2pPreflightPeerStatus;
  readonly metadataStatus?: DaMetadataByHeaderResponseV1["status"];
  readonly error?: string;
};

export type DaLibp2pPreflightMode = "bind-listen" | "dial-only";

export type DaLibp2pPreflightFailure = {
  readonly phase: "listen" | "identity" | "dial" | "protocol" | "ordering";
  readonly kind?:
    | "producer_port_already_bound"
    | "identity_mismatch"
    | "peer_unreachable"
    | "protocol_mismatch"
    | "unexpected";
  readonly peerId?: string;
  readonly signerIndex?: number;
  readonly error: string;
  readonly remediation?: string;
};

export type DaLibp2pPreflightListenCheck = {
  readonly checked: boolean;
  readonly status: "bound" | "skipped" | "failed";
  readonly listenMultiaddrs: readonly string[];
  readonly announceMultiaddrs: readonly string[];
  readonly error?: string;
};

export type DaLibp2pPreflightReport = {
  readonly configured: boolean;
  readonly mode: DaLibp2pPreflightMode;
  readonly deploymentFingerprint?: string;
  readonly localPeerId?: string;
  readonly threshold?: number;
  readonly reachableCommitteePeers: number;
  readonly reachableCommitteeSignerIndexes: readonly number[];
  readonly listenCheck: DaLibp2pPreflightListenCheck;
  readonly failures: readonly DaLibp2pPreflightFailure[];
  readonly warnings: readonly string[];
  readonly passed: boolean;
  readonly peerResults: readonly DaLibp2pPreflightPeerResult[];
  readonly reason?: string;
};

export type DaProducerProbeTransport = {
  readonly localPeerId: () => string | Promise<string>;
  readonly request: (
    peer: DaProducerCommitteePeer,
    protocolId: string,
    payload: Uint8Array,
    timeoutMs: number,
  ) => Promise<Uint8Array>;
  readonly close?: () => Promise<void>;
};

export type DaProducerTransport = DaProducerProbeTransport & {
  readonly sign: (message: Uint8Array) => Uint8Array | Promise<Uint8Array>;
  readonly publish: (
    topic: string,
    payload: Uint8Array,
  ) => Promise<{ readonly recipients: readonly string[] }>;
};

export type DaProducerStream = AsyncIterable<
  Uint8Array | { subarray: () => Uint8Array }
> & {
  send(data: Uint8Array): boolean;
  onDrain?(): Promise<void>;
  close?(): Promise<void> | void;
  abort?(error: Error): void;
};

export type DaProducerStreamHandler = (
  stream: DaProducerStream,
) => Promise<void>;

export type DaProducerTransportOptions = {
  readonly mode?: DaLibp2pPreflightMode;
  readonly requestHandlers?: ReadonlyMap<string, DaProducerStreamHandler>;
};

export class DaPayloadPublicationError extends Error {
  readonly report: DaProducerPublicationReport;

  constructor(message: string, report: DaProducerPublicationReport) {
    super(message);
    this.name = "DaPayloadPublicationError";
    this.report = report;
  }
}

export const parseDaProducerPublicationManifest = (
  manifest: Record<string, unknown>,
  env: NodeJS.ProcessEnv = process.env,
): DaProducerPublicationManifest | null => {
  const rawDaTransport =
    objectAt(manifest, ["da_transport"]) ?? objectAt(manifest, ["daTransport"]);
  if (rawDaTransport === undefined) {
    return null;
  }
  const kind = stringAt(rawDaTransport, [["kind"], ["mode"]]);
  if (kind !== "libp2p") {
    return null;
  }
  const deploymentIdentity = parseDaLibp2pRuntimeManifestDeploymentIdentity(
    manifest,
    "producer DA libp2p runtime manifest",
  );
  const daTransport = rawDaTransport;
  rejectUrlShapedLibp2pDaConfig(daTransport, "da_transport");
  if (daTransport.no_http_da_transport !== true) {
    throw new Error(
      "da_transport.no_http_da_transport must be true in libp2p DA mode",
    );
  }
  const daCommittee =
    objectAt(manifest, ["da_committee"]) ?? objectAt(manifest, ["daCommittee"]);
  if (daCommittee === undefined) {
    throw new Error("da_committee is required for libp2p DA publication");
  }
  rejectUrlShapedLibp2pDaConfig(daCommittee, "da_committee");
  const threshold = numberAt(daCommittee, ["threshold"]);
  if (
    threshold === undefined ||
    !Number.isSafeInteger(threshold) ||
    threshold <= 0
  ) {
    throw new Error("da_committee.threshold must be a positive safe integer");
  }
  const peers = parseCommitteePeers(daCommittee);
  const committeePeers = peers.filter((peer) =>
    peer.roles.includes("committee"),
  );
  if (committeePeers.length < threshold) {
    throw new Error(
      "libp2p DA publication requires at least threshold committee peers",
    );
  }
  const limits = objectAt(daTransport, ["limits"]);
  return {
    deploymentFingerprint: deploymentIdentity.fingerprint,
    localPrivateKeySource: requiredLibp2pPrivateKeySource(env),
    threshold,
    requestTimeoutMs: exactLimit(
      limits,
      "request_timeout_ms",
      DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
    ),
    maxPayloadBytes: exactLimit(
      limits,
      "max_payload_bytes",
      DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
    ),
    maxInlineResponseBytes: exactLimit(
      limits,
      "max_inline_response_bytes",
      DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
    ),
    maxChunkBytes: exactLimit(
      limits,
      "max_chunk_bytes",
      DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
    ),
    maxStreamsPerPeer: exactLimit(
      limits,
      "max_streams_per_peer",
      DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
    ),
    maxGossipMessageBytes:
      numberAt(daTransport, [["gossip", "max_gossip_message_bytes"]]) ??
      DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
    listenMultiaddrs: stringArrayAt(daTransport, [
      ["listen_multiaddrs"],
      ["listenMultiaddrs"],
    ]),
    announceMultiaddrs: stringArrayAt(daTransport, [
      ["announce_multiaddrs"],
      ["announceMultiaddrs"],
    ]),
    bootstrapMultiaddrs: stringArrayAt(daTransport, [
      ["bootstrap_multiaddrs"],
      ["bootstrapMultiaddrs"],
    ]),
    committeePeers,
  };
};

export const loadDaProducerPublicationManifestFromEnv = async (
  env: NodeJS.ProcessEnv = process.env,
): Promise<DaProducerPublicationManifest | null> => {
  const manifestPath =
    optionalNonEmpty(env[MANIFEST_PATH_ENV]) ??
    optionalNonEmpty(env[FALLBACK_MANIFEST_PATH_ENV]);
  if (manifestPath === undefined) {
    return null;
  }
  const raw = await readFile(manifestPath, "utf8");
  const parsed = JSON.parse(raw) as unknown;
  if (!isRecord(parsed)) {
    throw new Error(`${manifestPath} must contain a JSON object`);
  }
  return parseDaProducerPublicationManifest(parsed, env);
};

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
      const requestCbor = await readSingleFrame(
        stream,
        manifest.maxPayloadBytes,
        manifest.requestTimeoutMs,
      );
      const responseCbor = await handle(requestCbor);
      await writeSingleFrame(stream, responseCbor, manifest.maxPayloadBytes);
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
      () => decodeDaPayloadByHeaderRequestV1Cbor(requestCbor),
      "payload-by-header request",
    );
    const headerHash = request.headerHash;
    if (!request.deploymentFingerprint.equals(deploymentFingerprint)) {
      return encodeDaPayloadByHeaderResponseV1Cbor({
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
      return encodeDaPayloadByHeaderResponseV1Cbor(
        retainedPayloadAbsentResponse(headerHash, resolved),
      );
    }
    if (
      request.acceptedPayloadHashes !== null &&
      !containsHash(request.acceptedPayloadHashes, resolved.payloadHash)
    ) {
      return encodeDaPayloadByHeaderResponseV1Cbor({
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
      return encodeDaPayloadByHeaderResponseV1Cbor({
        status: "found_inline",
        headerHash,
        payloadHash: resolved.payloadHash,
        payloadBytes: resolved.payloadBytes,
        chunkManifest: null,
        reasonCode: null,
      });
    }

    return encodeDaPayloadByHeaderResponseV1Cbor({
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
      () => decodeDaPayloadChunkRequestV1Cbor(requestCbor),
      "payload-chunk request",
    );
    const headerHash = request.headerHash;
    const payloadHash = request.payloadHash;
    if (!request.deploymentFingerprint.equals(deploymentFingerprint)) {
      return encodeDaPayloadChunkResponseV1Cbor({
        status: "rejected",
        headerHash,
        payloadHash,
        chunkIndex: request.chunkIndex,
        chunkBytes: null,
        chunkHash: null,
      });
    }

    const resolved = await resolveRow(headerHash);
    if (resolved.kind !== "found" || !resolved.payloadHash.equals(payloadHash)) {
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
      Math.min(
        offset + manifest.maxChunkBytes,
        resolved.payloadBytes.length,
      ),
    );
    return encodeDaPayloadChunkResponseV1Cbor({
      status: "found",
      headerHash,
      payloadHash,
      chunkIndex: request.chunkIndex,
      chunkBytes,
      chunkHash: computeDaSha256Hash(chunkBytes),
    });
  });

  addHandler(DaRequestResponseProtocol.metadataByHeader, async (requestCbor) => {
    const request = decodeRetainedPayloadRequest(
      () => decodeDaPayloadByHeaderRequestV1Cbor(requestCbor),
      "metadata-by-header request",
    );
    const headerHash = request.headerHash;
    if (!request.deploymentFingerprint.equals(deploymentFingerprint)) {
      return encodeDaMetadataByHeaderResponseV1Cbor({
        ...emptyRetainedPayloadMetadataResponse(headerHash),
        status: "rejected",
      });
    }

    const resolved = await resolveRow(headerHash);
    if (resolved.kind !== "found") {
      return encodeDaMetadataByHeaderResponseV1Cbor(
        retainedPayloadMetadataAbsentResponse(headerHash, resolved),
      );
    }
    if (
      request.acceptedPayloadHashes !== null &&
      !containsHash(request.acceptedPayloadHashes, resolved.payloadHash)
    ) {
      return encodeDaMetadataByHeaderResponseV1Cbor({
        ...emptyRetainedPayloadMetadataResponse(headerHash),
        status: "conflict",
        payloadHash: resolved.payloadHash,
      });
    }

    return encodeDaMetadataByHeaderResponseV1Cbor(
      metadataForRetainedPayload(headerHash, resolved),
    );
  });

  return handlerMap;
};

export const startDaLibp2pRetainedPayloadServerFromEnv = async ({
  retrieveByHeaderHash,
  env = process.env,
}: {
  readonly retrieveByHeaderHash: DaRetainedPayloadLookup;
  readonly env?: NodeJS.ProcessEnv;
}): Promise<DaRetainedPayloadServer> => {
  const manifest = await loadDaProducerPublicationManifestFromEnv(env);
  if (manifest === null) {
    return {
      configured: false,
      reason: "no libp2p DA manifest configured",
    };
  }
  const requestHandlers = createDaLibp2pRetainedPayloadRequestHandlers({
    manifest,
    retrieveByHeaderHash,
  });
  const transport = await createDaLibp2pProducerTransport(manifest, {
    mode: "bind-listen",
    requestHandlers,
  });
  return {
    configured: true,
    deploymentFingerprint: manifest.deploymentFingerprint,
    localPeerId: await transport.localPeerId(),
    listenMultiaddrs: manifest.listenMultiaddrs,
    announceMultiaddrs: manifest.announceMultiaddrs,
    close: async () => {
      await transport.close?.();
    },
  };
};

export const publishDaPayloadInsert = async ({
  insert,
  manifest,
  transport,
  announcedAtSlot = 0,
}: {
  readonly insert: DaPayloadsDB.InsertInput;
  readonly manifest: DaProducerPublicationManifest;
  readonly transport: DaProducerTransport;
  readonly announcedAtSlot?: number;
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
  const topic = daGossipTopic(
    manifest.deploymentFingerprint,
    DaGossipTopic.payloadAnnouncements,
  );
  const request = encodeDaPayloadSubmitRequestV1Cbor({
    deploymentFingerprint,
    headerHash,
    payloadHash,
    payloadSchemaVersion: insert[DaPayloadsDB.Columns.VERSION],
    mode: "inline",
    payloadBytes,
    chunkManifest: null,
  });
  const peerResults = await Promise.all(
    manifest.committeePeers.map((peer) =>
      submitPayloadToPeer({
        peer,
        headerHash,
        protocolId,
        request,
        timeoutMs: manifest.requestTimeoutMs,
        payloadHash,
        transport,
      }),
    ),
  );
  const acceptedPeers = peerResults.filter((result) =>
    ACCEPTED_RESPONSE_STATUSES.has(result.status),
  ).length;
  const reportWithoutAnnouncement: DaProducerPublicationReport = {
    configured: true,
    headerHash: headerHash.toString("hex"),
    payloadHash: payloadHash.toString("hex"),
    deploymentFingerprint: manifest.deploymentFingerprint,
    threshold: manifest.threshold,
    acceptedPeers,
    peerResults,
  };
  if (acceptedPeers < manifest.threshold) {
    throw new DaPayloadPublicationError(
      `DA payload publication accepted by ${acceptedPeers.toString()} peer(s), below threshold ${manifest.threshold.toString()}`,
      reportWithoutAnnouncement,
    );
  }
  const localPeerId = await transport.localPeerId();
  const announcementWithoutSignature = {
    deploymentFingerprint,
    headerHash,
    payloadHash,
    payloadSchemaVersion: insert[DaPayloadsDB.Columns.VERSION],
    payloadBytes: payloadBytes.length,
    chunkSize: payloadBytes.length,
    chunkCount: 1,
    rootSummaryHash: rootSummaryHash(insert),
    announcedByPeerId: localPeerId,
    announcedAtSlot,
  };
  const signature = Buffer.from(
    await transport.sign(
      payloadAnnouncementSigningPreimage(announcementWithoutSignature),
    ),
  );
  const announcementBytes = encodeDaPayloadAnnouncementV1Cbor({
    ...announcementWithoutSignature,
    signature,
  });
  if (announcementBytes.length > manifest.maxGossipMessageBytes) {
    throw new Error(
      `DA payload announcement is ${announcementBytes.length.toString()} bytes, exceeding max_gossip_message_bytes=${manifest.maxGossipMessageBytes.toString()}`,
    );
  }
  const published = await transport.publish(topic, announcementBytes);
  return {
    ...reportWithoutAnnouncement,
    announcement: {
      topic,
      payloadHash: payloadHash.toString("hex"),
      recipients: published.recipients,
    },
  };
};

export const publishDaPayloadInsertFromEnv = (
  insert: DaPayloadsDB.InsertInput,
): Effect.Effect<DaProducerPublicationReport, DatabaseError> =>
  Effect.tryPromise({
    try: async () => {
      const manifest = await loadDaProducerPublicationManifestFromEnv();
      const headerHash =
        insert[DaPayloadsDB.Columns.HEADER_HASH].toString("hex");
      const payloadHash = verifyPayloadHash(insert).toString("hex");
      if (manifest === null) {
        return {
          configured: false,
          headerHash,
          payloadHash,
          acceptedPeers: 0,
          peerResults: [],
          reason: "no libp2p DA manifest configured",
        };
      }
      const transport = await createDaLibp2pProducerTransport(manifest, {
        mode: "dial-only",
      });
      try {
        return await publishDaPayloadInsert({
          insert,
          manifest,
          transport,
        });
      } finally {
        await transport.close?.();
      }
    },
    catch: (cause) =>
      new DatabaseError({
        table: DaPayloadsDB.tableName,
        message: "Failed to publish DA payload over libp2p",
        cause,
      }),
  });

export const publicationSatisfied = (
  report: DaProducerPublicationReport,
): boolean =>
  report.configured &&
  report.threshold !== undefined &&
  report.acceptedPeers >= report.threshold;

export const waitForPublicationGate = async ({
  probe,
  timeoutMs = 120_000,
  intervalMs = 5_000,
  sleep = (milliseconds) =>
    new Promise((resolve) => setTimeout(resolve, milliseconds)),
}: {
  readonly probe: () => Promise<DaProducerPublicationReport>;
  readonly timeoutMs?: number;
  readonly intervalMs?: number;
  readonly sleep?: (milliseconds: number) => Promise<void>;
}): Promise<
  DaProducerPublicationReport & {
    readonly attempts: number;
    readonly timedOut: boolean;
  }
> => {
  const startedAt = Date.now();
  let attempts = 0;
  for (;;) {
    attempts += 1;
    const report = await probe();
    if (publicationSatisfied(report)) {
      return { ...report, attempts, timedOut: false };
    }
    if (Date.now() - startedAt >= timeoutMs) {
      return { ...report, attempts, timedOut: true };
    }
    await sleep(intervalMs);
  }
};

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
  const request = encodeDaPayloadByHeaderRequestV1Cbor({
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

export const createDaLibp2pProducerTransport = async (
  manifest: DaProducerPublicationManifest,
  {
    mode = "bind-listen",
    requestHandlers,
  }: DaProducerTransportOptions = {},
): Promise<DaProducerTransport> => {
  const [
    { createLibp2p },
    { tcp },
    { noise },
    { yamux },
    { bootstrap },
    { identify },
    { gossipsub, StrictSign },
    { multiaddr },
  ] = await Promise.all([
    import("libp2p"),
    import("@libp2p/tcp"),
    import("@chainsafe/libp2p-noise"),
    import("@chainsafe/libp2p-yamux"),
    import("@libp2p/bootstrap"),
    import("@libp2p/identify"),
    import("@libp2p/gossipsub"),
    import("@multiformats/multiaddr"),
  ]);
  const allowedTopic = daGossipTopic(
    manifest.deploymentFingerprint,
    DaGossipTopic.payloadAnnouncements,
  );
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
      manifest.bootstrapMultiaddrs.length === 0
        ? []
        : [bootstrap({ list: [...manifest.bootstrapMultiaddrs] })],
    services: {
      identify: identify(),
      pubsub: gossipsub({
        globalSignaturePolicy: StrictSign,
        allowPublishToZeroTopicPeers: true,
        allowedTopics: new Set([allowedTopic]),
        maxInboundDataLength: manifest.maxGossipMessageBytes,
      }),
    },
  });
  for (const [protocolId, handler] of requestHandlers ?? []) {
    await node.handle(
      protocolId,
      async (stream) => {
        await handler(stream as DaProducerStream);
      },
      {
        maxInboundStreams: manifest.maxStreamsPerPeer,
        maxOutboundStreams: manifest.maxStreamsPerPeer,
        runOnLimitedConnection: false,
      },
    );
  }
  return {
    localPeerId: () => localPeerId,
    sign: (message) => privateKey.sign(message),
    request: async (peer, protocolId, payload, timeoutMs) => {
      const stream = await node.dialProtocol(
        peer.multiaddrs.map((address) => multiaddr(address)),
        protocolId,
        { signal: AbortSignal.timeout(timeoutMs) },
      );
      stream.send(encodeFrame(payload, manifest.maxPayloadBytes));
      await stream.close();
      return readSingleFrame(stream, manifest.maxPayloadBytes, timeoutMs);
    },
    publish: async (topic, payload) => {
      const result = await node.services.pubsub.publish(topic, payload);
      return {
        recipients: result.recipients.map((peerId) => peerId.toString()),
      };
    },
    close: async () => {
      for (const protocolId of requestHandlers?.keys() ?? []) {
        await node.unhandle(protocolId);
      }
      await node.stop();
    },
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
    request: async (peer, protocolId, payload, timeoutMs) => {
      const stream = await node.dialProtocol(
        peer.multiaddrs.map((address) => multiaddr(address)),
        protocolId,
        { signal: AbortSignal.timeout(timeoutMs) },
      );
      stream.send(encodeFrame(payload, manifest.maxPayloadBytes));
      await stream.close();
      return readSingleFrame(stream, manifest.maxPayloadBytes, timeoutMs);
    },
    close: async () => {
      await node.stop();
    },
  };
};

const loadAndValidateProducerIdentity = async (
  manifest: DaProducerPublicationManifest,
) => {
  const identity = await loadDaLibp2pIdentity(manifest.localPrivateKeySource);
  assertProducerIdentityInManifest(manifest, identity.peerId);
  return identity;
};

const assertProducerIdentityInManifest = (
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

const preflightIdentityFailure = (
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

const preflightStartupFailureReport = ({
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

const boundListenCheck = (
  manifest: DaProducerPublicationManifest,
): DaLibp2pPreflightListenCheck => ({
  checked: true,
  status: "bound",
  listenMultiaddrs: manifest.listenMultiaddrs,
  announceMultiaddrs: manifest.announceMultiaddrs,
});

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

const skippedListenCheck = (
  listenMultiaddrs: readonly string[],
  announceMultiaddrs: readonly string[],
): DaLibp2pPreflightListenCheck => ({
  checked: false,
  status: "skipped",
  listenMultiaddrs,
  announceMultiaddrs,
});

const preflightWarnings = (mode: DaLibp2pPreflightMode): readonly string[] =>
  mode === "dial-only"
    ? [
        "dial-only preflight does not bind, announce, or validate the producer listener; use bind-listen before producer startup for listener validation.",
      ]
    : [];

const uniqueReachableSignerIndexes = (
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

const preflightPeerFailures = (
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

const submitPayloadToPeer = async ({
  peer,
  headerHash,
  protocolId,
  request,
  timeoutMs,
  payloadHash,
  transport,
}: {
  readonly peer: DaProducerCommitteePeer;
  readonly headerHash: Buffer;
  readonly protocolId: string;
  readonly request: Uint8Array;
  readonly timeoutMs: number;
  readonly payloadHash: Buffer;
  readonly transport: DaProducerTransport;
}): Promise<DaProducerPeerResult> => {
  try {
    const responseBytes = await transport.request(
      peer,
      protocolId,
      request,
      timeoutMs,
    );
    const response = decodeDaPayloadSubmitResponseV1Cbor(responseBytes);
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

const preflightCommitteePeer = async ({
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
    const response = decodeDaMetadataByHeaderResponseV1Cbor(responseBytes);
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

type RetainedPayloadResolution =
  | { readonly kind: "missing" }
  | { readonly kind: "invalid"; readonly reasonCode: string }
  | {
      readonly kind: "found";
      readonly row: DaPayloadsDB.Row;
      readonly payloadBytes: Buffer;
      readonly payloadHash: Buffer;
    };

const resolveRetainedPayloadRow = (
  row: DaPayloadsDB.Row,
  maxPayloadBytes: number,
): RetainedPayloadResolution => {
  const payloadBytes = row[DaPayloadsDB.Columns.PAYLOAD_CBOR];
  if (payloadBytes.length === 0) {
    return { kind: "invalid", reasonCode: "empty_payload" };
  }
  if (payloadBytes.length > maxPayloadBytes) {
    return { kind: "invalid", reasonCode: "payload_too_large" };
  }
  const payloadHash = row[DaPayloadsDB.Columns.PAYLOAD_SHA256];
  if (payloadHash.length !== 32) {
    return { kind: "invalid", reasonCode: "stored_payload_hash_malformed" };
  }
  if (!computeDaSha256Hash(payloadBytes).equals(payloadHash)) {
    return { kind: "invalid", reasonCode: "stored_payload_hash_mismatch" };
  }
  return {
    kind: "found",
    row,
    payloadBytes,
    payloadHash,
  };
};

const retainedPayloadAbsentResponse = (
  headerHash: Buffer,
  resolution: Exclude<RetainedPayloadResolution, { readonly kind: "found" }>,
): DaPayloadByHeaderResponseV1 => {
  switch (resolution.kind) {
    case "missing":
      return {
        status: "not_found",
        headerHash,
        payloadHash: null,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: null,
      };
    case "invalid":
      return {
        status: "rejected",
        headerHash,
        payloadHash: null,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: resolution.reasonCode,
      };
  }
};

const emptyRetainedPayloadMetadataResponse = (
  headerHash: Buffer,
): Omit<DaMetadataByHeaderResponseV1, "status"> => ({
  headerHash,
  payloadHash: null,
  payloadSchemaVersion: null,
  payloadBytes: null,
  rootSummaryHash: null,
  proofBundleHash: null,
  transitionTraceRoot: null,
  eventToStepRoot: null,
  retainedUntilSlot: null,
  localStatus: null,
});

const retainedPayloadMetadataAbsentResponse = (
  headerHash: Buffer,
  resolution: Exclude<RetainedPayloadResolution, { readonly kind: "found" }>,
): DaMetadataByHeaderResponseV1 => {
  switch (resolution.kind) {
    case "missing":
      return {
        ...emptyRetainedPayloadMetadataResponse(headerHash),
        status: "not_found",
      };
    case "invalid":
      return {
        ...emptyRetainedPayloadMetadataResponse(headerHash),
        status: "rejected",
      };
  }
};

const metadataForRetainedPayload = (
  headerHash: Buffer,
  resolved: Extract<RetainedPayloadResolution, { readonly kind: "found" }>,
): DaMetadataByHeaderResponseV1 => ({
  status: "found",
  headerHash,
  payloadHash: resolved.payloadHash,
  payloadSchemaVersion: resolved.row[DaPayloadsDB.Columns.VERSION],
  payloadBytes: resolved.payloadBytes.length,
  rootSummaryHash: rootSummaryHash(resolved.row),
  proofBundleHash: null,
  transitionTraceRoot: rootHexToBytes(
    resolved.row[DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT],
    DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT,
  ),
  eventToStepRoot: rootHexToBytes(
    resolved.row[DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT],
    DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT,
  ),
  retainedUntilSlot: null,
  localStatus: "verified",
});

const rootHexToBytes = (value: string, fieldName: string): Buffer =>
  Buffer.from(normalizeHex32(value, fieldName), "hex");

const retainedPayloadChunkManifestFor = (
  payloadBytes: Buffer,
  chunkSize: number,
): DaPayloadChunkManifestV1 => {
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
  encodeDaPayloadChunkResponseV1Cbor({
    status: "not_found",
    headerHash,
    payloadHash,
    chunkIndex,
    chunkBytes: null,
    chunkHash: null,
  });

const decodeRetainedPayloadRequest = <T>(
  decode: () => T,
  label: string,
): T => {
  try {
    return decode();
  } catch (cause) {
    throw new Error(`invalid ${label}`, { cause });
  }
};

const containsHash = (hashes: readonly Buffer[], target: Buffer): boolean =>
  hashes.some((hash) => hash.equals(target));

const payloadAnnouncementSigningPreimage = (message: {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly payloadSchemaVersion: number;
  readonly payloadBytes: number;
  readonly chunkSize: number;
  readonly chunkCount: number;
  readonly rootSummaryHash: Buffer;
  readonly announcedByPeerId: string;
  readonly announcedAtSlot: number;
}): Buffer =>
  Buffer.concat([
    Buffer.from(DaTransportSigningDomain.payloadAnnouncement, "utf8"),
    Buffer.from([0]),
    Buffer.from([DA_TRANSPORT_PROTOCOL_VERSION]),
    encodeDaPayloadAnnouncementV1Cbor({
      ...message,
      signature: Buffer.alloc(0),
    }),
  ]);

const rootSummaryHash = (insert: DaPayloadsDB.InsertInput): Buffer =>
  createHash("sha256")
    .update(
      JSON.stringify([
        ["utxos_root", insert[DaPayloadsDB.Columns.UTXOS_ROOT]],
        [
          "forced_transactions_root",
          insert[DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT],
        ],
        ["transactions_root", insert[DaPayloadsDB.Columns.TRANSACTIONS_ROOT]],
        ["deposits_root", insert[DaPayloadsDB.Columns.DEPOSITS_ROOT]],
        ["withdrawals_root", insert[DaPayloadsDB.Columns.WITHDRAWALS_ROOT]],
        [
          "transition_trace_root",
          insert[DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT],
        ],
        ["event_to_step_root", insert[DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]],
        [
          "withdrawal_count",
          insert[DaPayloadsDB.Columns.WITHDRAWAL_COUNT].toString(),
        ],
        [
          "forced_transaction_count",
          insert[DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT].toString(),
        ],
        [
          "l2_transaction_count",
          insert[DaPayloadsDB.Columns.L2_TRANSACTION_COUNT].toString(),
        ],
        [
          "deposit_count",
          insert[DaPayloadsDB.Columns.DEPOSIT_COUNT].toString(),
        ],
        [
          "total_event_count",
          insert[DaPayloadsDB.Columns.TOTAL_EVENT_COUNT].toString(),
        ],
        [
          "transition_step_count",
          insert[DaPayloadsDB.Columns.TRANSITION_STEP_COUNT].toString(),
        ],
      ]),
    )
    .digest();

const verifyPayloadHash = (insert: DaPayloadsDB.InsertInput): Buffer => {
  const expected = insert[DaPayloadsDB.Columns.PAYLOAD_SHA256];
  const actual = computeDaSha256Hash(insert[DaPayloadsDB.Columns.PAYLOAD_CBOR]);
  if (!expected.equals(actual)) {
    throw new Error(
      `DA payload hash mismatch for ${insert[
        DaPayloadsDB.Columns.HEADER_HASH
      ].toString("hex")}`,
    );
  }
  return expected;
};

const parseCommitteePeers = (
  daCommittee: Record<string, unknown>,
): readonly DaProducerCommitteePeer[] => {
  const members = daCommittee.members;
  if (!Array.isArray(members) || members.length === 0) {
    throw new Error("da_committee.members must be a non-empty array");
  }
  const seenSignerIndexes = new Set<number>();
  const seenPeerIds = new Set<string>();
  return members
    .map((member, index) => {
      if (!isRecord(member)) {
        throw new Error("da_committee.members entries must be objects");
      }
      const signerIndex = numberAt(member, [
        ["signer_index"],
        ["signerIndex"],
        ["index"],
      ]);
      if (
        signerIndex === undefined ||
        !Number.isSafeInteger(signerIndex) ||
        signerIndex < 0 ||
        signerIndex > 255
      ) {
        throw new Error(
          `da_committee.members[${index.toString()}].signer_index must fit in one byte`,
        );
      }
      if (seenSignerIndexes.has(signerIndex)) {
        throw new Error(
          `duplicate da_committee.members signer_index ${signerIndex.toString()}`,
        );
      }
      seenSignerIndexes.add(signerIndex);
      const peerId = stringAt(member, [["peer_id"], ["peerId"]]);
      if (peerId === undefined || peerId.trim().length === 0) {
        throw new Error(
          `da_committee.members[${index.toString()}].peer_id is required`,
        );
      }
      if (seenPeerIds.has(peerId)) {
        throw new Error(`duplicate da_committee.members peer_id ${peerId}`);
      }
      seenPeerIds.add(peerId);
      const multiaddrs = stringArrayAt(member, [["multiaddrs"]]);
      if (multiaddrs.length === 0) {
        throw new Error(
          `da_committee.members[${index.toString()}].multiaddrs must be non-empty`,
        );
      }
      for (const multiaddr of multiaddrs) {
        if (!multiaddr.endsWith(`/p2p/${peerId}`)) {
          throw new Error(
            `da_committee.members[${index.toString()}].multiaddrs peer id must match ${peerId}`,
          );
        }
      }
      const roles = stringArrayAt(member, [["roles"]]);
      if (roles.length === 0) {
        throw new Error(
          `da_committee.members[${index.toString()}].roles must be non-empty`,
        );
      }
      return {
        signerIndex,
        daVkey: normalizeHex32(
          stringAt(member, [["da_vkey"], ["daVkey"], ["vkey"]]) ?? "",
          `da_committee.members[${index.toString()}].da_vkey`,
        ),
        peerId,
        multiaddrs,
        roles,
      };
    })
    .sort((left, right) => left.signerIndex - right.signerIndex);
};

const requiredLibp2pPrivateKeySource = (env: NodeJS.ProcessEnv): string => {
  const source = optionalNonEmpty(env[LIBP2P_PRIVATE_KEY_SOURCE_ENV]);
  if (source === undefined) {
    throw new Error(
      `${LIBP2P_PRIVATE_KEY_SOURCE_ENV} is required in libp2p DA mode`,
    );
  }
  validateLibp2pPrivateKeySource(source);
  return source;
};

const validateLibp2pPrivateKeySource = (source: string): void => {
  if (source.startsWith("seed:")) {
    normalizeHexBytes(
      source.slice("seed:".length),
      `${LIBP2P_PRIVATE_KEY_SOURCE_ENV} seed`,
      32,
    );
    return;
  }
  if (source.startsWith("hex:")) {
    const encoded = source.slice("hex:".length);
    if (encoded.length === 0) {
      throw new Error(
        `${LIBP2P_PRIVATE_KEY_SOURCE_ENV} must include a hex key`,
      );
    }
    normalizeHexBytes(encoded, `${LIBP2P_PRIVATE_KEY_SOURCE_ENV} protobuf key`);
    return;
  }
  if (source.startsWith("file:")) {
    if (source.slice("file:".length).trim() === "") {
      throw new Error(
        `${LIBP2P_PRIVATE_KEY_SOURCE_ENV} must include a file path`,
      );
    }
    return;
  }
  throw new Error(
    `${LIBP2P_PRIVATE_KEY_SOURCE_ENV} must use seed:, hex:, or file:`,
  );
};

const encodeFrame = (payload: Uint8Array, maxBytes: number): Buffer => {
  if (payload.length > maxBytes) {
    throw new Error(
      `libp2p DA frame exceeds max bytes: ${payload.length.toString()} > ${maxBytes.toString()}`,
    );
  }
  const frame = Buffer.alloc(4 + payload.length);
  frame.writeUInt32BE(payload.length, 0);
  Buffer.from(payload).copy(frame, 4);
  return frame;
};

const readSingleFrame = async (
  stream: AsyncIterable<Uint8Array | { subarray: () => Uint8Array }>,
  maxBytes: number,
  timeoutMs: number,
): Promise<Buffer> =>
  withTimeout(
    (async () => {
      const chunks: Buffer[] = [];
      for await (const chunk of stream) {
        chunks.push(
          Buffer.from(chunk instanceof Uint8Array ? chunk : chunk.subarray()),
        );
      }
      const bytes = Buffer.concat(chunks);
      if (bytes.length < 4) {
        throw new Error("libp2p DA response frame is truncated");
      }
      const frameLength = bytes.readUInt32BE(0);
      if (frameLength > maxBytes) {
        throw new Error("libp2p DA response frame exceeds max bytes");
      }
      if (bytes.length !== 4 + frameLength) {
        throw new Error("libp2p DA response frame length mismatch");
      }
      return bytes.subarray(4);
    })(),
    timeoutMs,
  );

const writeSingleFrame = async (
  stream: DaProducerStream,
  payload: Uint8Array,
  maxBytes: number,
): Promise<void> => {
  const accepted = stream.send(encodeFrame(payload, maxBytes));
  if (!accepted) {
    await stream.onDrain?.();
  }
  await stream.close?.();
};

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

const exactLimit = (
  limits: Record<string, unknown> | undefined,
  key: string,
  expected: number,
): number => {
  if (limits === undefined) {
    return expected;
  }
  const configured = limits[key];
  if (configured === undefined) {
    return expected;
  }
  if (configured !== expected) {
    throw new Error(
      `da_transport.limits.${key} must be ${expected.toString()}`,
    );
  }
  return expected;
};

const rejectUrlShapedLibp2pDaConfig = (value: unknown, path: string): void => {
  if (Array.isArray(value)) {
    value.forEach((entry, index) =>
      rejectUrlShapedLibp2pDaConfig(entry, `${path}[${index.toString()}]`),
    );
    return;
  }
  if (!isRecord(value)) {
    if (typeof value === "string" && /^https?:\/\//i.test(value.trim())) {
      throw new Error(`${path} must not contain HTTP(S) URL values`);
    }
    return;
  }
  for (const [key, entry] of Object.entries(value)) {
    const entryPath = `${path}.${key}`;
    if (FORBIDDEN_LIBP2P_DA_CONFIG_KEYS.has(key)) {
      throw new Error(`${entryPath} is not allowed in libp2p DA mode`);
    }
    rejectUrlShapedLibp2pDaConfig(entry, entryPath);
  }
};

const optionalNonEmpty = (value: string | undefined): string | undefined => {
  const trimmed = value?.trim();
  return trimmed === undefined || trimmed.length === 0 ? undefined : trimmed;
};

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const objectAt = (
  source: Record<string, unknown>,
  path: readonly string[],
): Record<string, unknown> | undefined => {
  let current: unknown = source;
  for (const key of path) {
    if (!isRecord(current)) {
      return undefined;
    }
    current = current[key];
  }
  return isRecord(current) ? current : undefined;
};

const valueAt = (
  source: Record<string, unknown>,
  path: readonly string[],
): unknown => {
  let current: unknown = source;
  for (const key of path) {
    if (!isRecord(current)) {
      return undefined;
    }
    current = current[key];
  }
  return current;
};

const stringAt = (
  source: Record<string, unknown>,
  paths: readonly string[] | readonly (readonly string[])[],
): string | undefined => {
  for (const path of normalizePaths(paths)) {
    const value = valueAt(source, path);
    if (typeof value === "string" && value.trim().length > 0) {
      return value.trim();
    }
  }
  return undefined;
};

const numberAt = (
  source: Record<string, unknown>,
  paths: readonly string[] | readonly (readonly string[])[],
): number | undefined => {
  for (const path of normalizePaths(paths)) {
    const value = valueAt(source, path);
    if (typeof value === "number") {
      return value;
    }
  }
  return undefined;
};

const stringArrayAt = (
  source: Record<string, unknown>,
  paths: readonly string[] | readonly (readonly string[])[],
): readonly string[] => {
  for (const path of normalizePaths(paths)) {
    const value = valueAt(source, path);
    if (Array.isArray(value)) {
      return value.map((entry, index) => {
        if (typeof entry !== "string" || entry.trim().length === 0) {
          throw new Error(
            `${path.join(".")}[${index.toString()}] must be a string`,
          );
        }
        return entry.trim();
      });
    }
  }
  return [];
};

const normalizePaths = (
  paths: readonly string[] | readonly (readonly string[])[],
): readonly (readonly string[])[] =>
  typeof paths[0] === "string"
    ? [paths as readonly string[]]
    : (paths as readonly (readonly string[])[]);

const normalizeHex32 = (value: string, fieldName: string): string => {
  return normalizeHexBytes(value, fieldName, 32);
};

const normalizeHexBytes = (
  value: string,
  fieldName: string,
  byteLength?: number,
): string => {
  const normalized = value.trim().toLowerCase();
  if (!/^[0-9a-f]*$/.test(normalized) || normalized.length % 2 !== 0) {
    throw new Error(`${fieldName} must be even-length hex`);
  }
  if (byteLength !== undefined && normalized.length !== byteLength * 2) {
    throw new Error(`${fieldName} must be 32-byte hex`);
  }
  return normalized;
};

export const encodeLengthPrefixedDaFrameForTest = encodeFrame;
export const decodeLengthPrefixedDaFrameForTest = (
  frame: Buffer,
  maxBytes = DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
): Buffer => {
  if (frame.length < 4) {
    throw new Error("frame is truncated");
  }
  const frameLength = frame.readUInt32BE(0);
  if (frameLength > maxBytes || frame.length !== 4 + frameLength) {
    throw new Error("frame length mismatch");
  }
  return frame.subarray(4);
};
