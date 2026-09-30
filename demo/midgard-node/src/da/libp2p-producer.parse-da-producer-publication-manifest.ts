import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import {
  computeDaSha256Hash,
  type DaMetadataByHeaderResponse,
  type DaPayloadByHeaderResponse,
  parseDaLibp2pRuntimeManifest,
} from "@al-ft/midgard-core/da-transport";

import { DaPayloadsDB } from "../database/index.js";
import {
  type DaProducerPublicationManifest,
  FORBIDDEN_LIBP2P_DA_CONFIG_KEYS,
  LIBP2P_PRIVATE_KEY_SOURCE_ENV,
  MANIFEST_PATH_ENV,
  normalizeHex32,
  normalizeHexBytes,
  parseCommitteePeers,
} from "./libp2p-producer.parse-committee-peers.js";

export const parseDaProducerPublicationManifest = (
  value: unknown,
  env: NodeJS.ProcessEnv = process.env,
): DaProducerPublicationManifest => {
  const manifest = parseDaLibp2pRuntimeManifest(value);
  if (manifest.runtime_topology.target !== "producer") {
    throw new Error("runtime_topology.target must be producer");
  }
  const { da_committee: daCommittee, da_transport: daTransport } = manifest;
  rejectUrlShapedLibp2pDaConfig(daTransport, "da_transport");
  rejectUrlShapedLibp2pDaConfig(daCommittee, "da_committee");
  const threshold = daCommittee.threshold;
  const peers = parseCommitteePeers(daCommittee);
  const committeePeers = peers.filter((peer) =>
    peer.roles.includes("committee"),
  );
  if (committeePeers.length < threshold) {
    throw new Error(
      "libp2p DA publication requires at least threshold committee peers",
    );
  }
  return {
    deploymentFingerprint: manifest.deployment.fingerprint,
    contractDeploymentManifestId:
      manifest.deployment.contract_deployment_manifest_id,
    localPrivateKeySource: requiredLibp2pPrivateKeySource(env),
    threshold,
    requestTimeoutMs: daTransport.limits.request_timeout_ms,
    maxPayloadBytes: daTransport.limits.max_payload_bytes,
    maxInlineResponseBytes: daTransport.limits.max_inline_response_bytes,
    maxChunkBytes: daTransport.limits.max_chunk_bytes,
    maxStreamsPerPeer: daTransport.limits.max_streams_per_peer,
    maxGossipMessageBytes: daTransport.gossip.max_gossip_message_bytes,
    listenMultiaddrs: daTransport.listen_multiaddrs.map((address) =>
      address.trim(),
    ),
    announceMultiaddrs: daTransport.announce_multiaddrs.map((address) =>
      address.trim(),
    ),
    bootstrapMultiaddrs: daTransport.bootstrap_multiaddrs.map((address) =>
      address.trim(),
    ),
    committeePeers,
  };
};

export const loadDaProducerPublicationManifestFromEnv = async (
  env: NodeJS.ProcessEnv = process.env,
): Promise<DaProducerPublicationManifest> => {
  const manifestPath = optionalNonEmpty(env[MANIFEST_PATH_ENV]);
  if (manifestPath === undefined) {
    throw new Error(`${MANIFEST_PATH_ENV} is required for libp2p DA`);
  }
  const raw = await readFile(manifestPath, "utf8");
  const parsed = JSON.parse(raw) as unknown;
  if (!isRecord(parsed)) {
    throw new Error(`${manifestPath} must contain a JSON object`);
  }
  return parseDaProducerPublicationManifest(parsed, env);
};

export type RetainedPayloadResolution =
  | { readonly kind: "missing" }
  | { readonly kind: "invalid"; readonly reasonCode: string }
  | {
      readonly kind: "found";
      readonly row: DaPayloadsDB.Row;
      readonly payloadBytes: Buffer;
      readonly payloadHash: Buffer;
    };

export const resolveRetainedPayloadRow = (
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

export const retainedPayloadAbsentResponse = (
  headerHash: Buffer,
  resolution: Exclude<RetainedPayloadResolution, { readonly kind: "found" }>,
): DaPayloadByHeaderResponse => {
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

export const emptyRetainedPayloadMetadataResponse = (
  headerHash: Buffer,
): Omit<DaMetadataByHeaderResponse, "status"> => ({
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

export const retainedPayloadMetadataAbsentResponse = (
  headerHash: Buffer,
  resolution: Exclude<RetainedPayloadResolution, { readonly kind: "found" }>,
): DaMetadataByHeaderResponse => {
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

export const metadataForRetainedPayload = (
  headerHash: Buffer,
  resolved: Extract<RetainedPayloadResolution, { readonly kind: "found" }>,
): DaMetadataByHeaderResponse => ({
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

export const rootSummaryHash = (insert: DaPayloadsDB.InsertInput): Buffer =>
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

export const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);
