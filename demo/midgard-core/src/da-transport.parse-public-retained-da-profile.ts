import { sha256 } from "@noble/hashes/sha2.js";

import { asArray, asBytes } from "./codec/cbor.js";
import { MidgardTxCodecErrorCodes } from "./codec/errors.js";
import {
  DA_ON_CHAIN_ATTESTATION_DOMAIN,
  DA_PUBLIC_RETAINED_DA_ACCESS_POLICY,
  DA_PUBLIC_RETAINED_DA_PROFILE,
  DA_PUBLIC_RETAINED_DA_PROTOCOLS,
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
  DaGossipTopic,
  type DaLibp2pRuntimeManifest,
  DaRequestResponseProtocol,
} from "./da-transport.da-libp2p-runtime-manifest.js";
import {
  ensureUint,
  enumCode,
  enumLabel,
  exactStringEnumValue,
  fail,
  type NumericEnumLabel,
  type NumericEnumTable,
} from "./da-transport.enum-label.js";
import {
  ensureDaDeploymentFingerprint,
  ensureDaHash32,
  ensureDaHeaderHash,
  ensureDaPayloadHash,
  exactRecordKeys,
  nonEmptyStringValue,
  normalizeDaDeploymentFingerprintHex,
  parseDaLibp2pRuntimeManifestDeployment,
  parseDaLibp2pRuntimeTopology,
  recordValue,
  safeIntegerValue,
  stringArrayValue,
} from "./da-transport.parse-da-libp2p-runtime-manifest-deployment.js";
import {
  parseDaLibp2pRuntimeCommittee,
  parseDaLibp2pRuntimeTransport,
} from "./da-transport.parse-da-libp2p-runtime-transport.js";

const parsePublicRetainedDaProfile = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["public_retained_da"] => {
  const profile = recordValue(value, fieldName);
  exactRecordKeys(
    profile,
    [
      "profile",
      "access_policy",
      "peer_id",
      "listen_multiaddrs",
      "announce_multiaddrs",
      "protocols",
      "limits",
    ],
    fieldName,
  );
  if (profile.profile !== DA_PUBLIC_RETAINED_DA_PROFILE) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.profile must be ${DA_PUBLIC_RETAINED_DA_PROFILE}`,
      String(profile.profile),
    );
  }
  if (profile.access_policy !== DA_PUBLIC_RETAINED_DA_ACCESS_POLICY) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.access_policy must be ${DA_PUBLIC_RETAINED_DA_ACCESS_POLICY}`,
      String(profile.access_policy),
    );
  }
  const protocols = stringArrayValue(
    profile.protocols,
    `${fieldName}.protocols`,
  );
  if (
    protocols.length !== DA_PUBLIC_RETAINED_DA_PROTOCOLS.length ||
    protocols.some(
      (protocol, index) => protocol !== DA_PUBLIC_RETAINED_DA_PROTOCOLS[index],
    )
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.protocols must equal the public retained DA protocol allowlist`,
    );
  }
  const limitsFieldName = `${fieldName}.limits`;
  const limits = recordValue(profile.limits, limitsFieldName);
  exactRecordKeys(
    limits,
    [
      "max_streams_per_peer",
      "max_inflight_requests",
      "max_inflight_requests_per_peer",
      "max_inflight_proof_requests",
      "request_timeout_ms",
    ],
    limitsFieldName,
  );
  const maxStreamsPerPeer = safeIntegerValue(
    limits.max_streams_per_peer,
    `${limitsFieldName}.max_streams_per_peer`,
    1,
    DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
  );
  const maxInflightRequests = safeIntegerValue(
    limits.max_inflight_requests,
    `${limitsFieldName}.max_inflight_requests`,
    1,
    256,
  );
  const maxInflightRequestsPerPeer = safeIntegerValue(
    limits.max_inflight_requests_per_peer,
    `${limitsFieldName}.max_inflight_requests_per_peer`,
    1,
    maxInflightRequests,
  );
  const maxInflightProofRequests = safeIntegerValue(
    limits.max_inflight_proof_requests,
    `${limitsFieldName}.max_inflight_proof_requests`,
    1,
    maxInflightRequests,
  );
  const requestTimeoutMs = safeIntegerValue(
    limits.request_timeout_ms,
    `${limitsFieldName}.request_timeout_ms`,
    100,
    DA_TRANSPORT_LIMITS.requestTimeoutMs,
  );
  const peerId = nonEmptyStringValue(profile.peer_id, `${fieldName}.peer_id`);
  const announceMultiaddrs = stringArrayValue(
    profile.announce_multiaddrs,
    `${fieldName}.announce_multiaddrs`,
  );
  if (
    !announceMultiaddrs.every((address) => address.endsWith(`/p2p/${peerId}`))
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.announce_multiaddrs must bind public_retained_da.peer_id`,
    );
  }
  return {
    profile: DA_PUBLIC_RETAINED_DA_PROFILE,
    access_policy: DA_PUBLIC_RETAINED_DA_ACCESS_POLICY,
    peer_id: peerId,
    listen_multiaddrs: stringArrayValue(
      profile.listen_multiaddrs,
      `${fieldName}.listen_multiaddrs`,
    ),
    announce_multiaddrs: announceMultiaddrs,
    protocols: [...DA_PUBLIC_RETAINED_DA_PROTOCOLS],
    limits: {
      max_streams_per_peer: maxStreamsPerPeer,
      max_inflight_requests: maxInflightRequests,
      max_inflight_requests_per_peer: maxInflightRequestsPerPeer,
      max_inflight_proof_requests: maxInflightProofRequests,
      request_timeout_ms: requestTimeoutMs,
    },
  };
};

export const parseDaLibp2pRuntimeManifest = (
  value: unknown,
): DaLibp2pRuntimeManifest => {
  const fieldName = "DA libp2p runtime manifest";
  const manifest = recordValue(value, fieldName);
  exactRecordKeys(
    manifest,
    [
      "schemaVersion",
      "network",
      "deployment",
      "runtime_topology",
      "da_transport",
      "public_retained_da",
      "da_committee",
    ],
    fieldName,
  );
  if (manifest.schemaVersion !== DA_RUNTIME_MANIFEST_SCHEMA_VERSION) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.schemaVersion must be ${DA_RUNTIME_MANIFEST_SCHEMA_VERSION}`,
      String(manifest.schemaVersion),
    );
  }
  const publicRetainedDa = parsePublicRetainedDaProfile(
    manifest.public_retained_da,
    `${fieldName}.public_retained_da`,
  );
  const daCommittee = parseDaLibp2pRuntimeCommittee(
    manifest.da_committee,
    `${fieldName}.da_committee`,
  );
  const topology = parseDaLibp2pRuntimeTopology(
    manifest.runtime_topology,
    `${fieldName}.runtime_topology`,
  );
  if (
    publicRetainedDa.peer_id === topology.producer_peer_id ||
    daCommittee.members.some(
      (member) => member.peer_id === publicRetainedDa.peer_id,
    )
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.public_retained_da.peer_id must not be a producer or committee peer identity`,
    );
  }
  return {
    schemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
    network: nonEmptyStringValue(manifest.network, `${fieldName}.network`),
    deployment: parseDaLibp2pRuntimeManifestDeployment(
      manifest.deployment,
      `${fieldName}.deployment`,
    ),
    runtime_topology: topology,
    da_transport: parseDaLibp2pRuntimeTransport(
      manifest.da_transport,
      `${fieldName}.da_transport`,
    ),
    public_retained_da: publicRetainedDa,
    da_committee: daCommittee,
  };
};

export const computeDaSha256Hash = (value: Uint8Array): Buffer =>
  Buffer.from(sha256(value));

export const encodeDaAttestationPreimage = (headerHash: Uint8Array): Buffer =>
  Buffer.concat([
    Buffer.from(DA_ON_CHAIN_ATTESTATION_DOMAIN, "utf8"),
    ensureDaHeaderHash(headerHash),
  ]);

export const daGossipTopic = (
  deploymentFingerprint: string | Uint8Array,
  topic: DaGossipTopic,
): string =>
  `/midgard/${normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  )}/da/${exactStringEnumValue(
    DaGossipTopic,
    topic,
    "gossip_topic",
  )}/${DA_TRANSPORT_PROTOCOL_VERSION}`;

export const daRequestResponseProtocolId = (
  deploymentFingerprint: string | Uint8Array,
  protocol: DaRequestResponseProtocol,
): string =>
  `/midgard/${normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  )}/da/${exactStringEnumValue(
    DaRequestResponseProtocol,
    protocol,
    "request_response_protocol",
  )}/${DA_TRANSPORT_PROTOCOL_VERSION}`;

export const hashValue = (value: unknown, fieldName: string): Buffer =>
  ensureDaHash32(asBytes(value, fieldName), fieldName);

export const payloadHashValue = (value: unknown, fieldName: string): Buffer =>
  ensureDaPayloadHash(asBytes(value, fieldName), fieldName);

export const optionalPayloadHashValue = (
  value: unknown,
  fieldName: string,
): Buffer | null => (value == null ? null : payloadHashValue(value, fieldName));

export const headerHashValue = (value: unknown, fieldName: string): Buffer =>
  ensureDaHeaderHash(asBytes(value, fieldName), fieldName);

export const deploymentFingerprintValue = (
  value: unknown,
  fieldName: string,
): Buffer =>
  ensureDaDeploymentFingerprint(asBytes(value, fieldName), fieldName);

export const cborEnum = <T extends NumericEnumTable>(
  table: T,
  label: NumericEnumLabel<T>,
  fieldName: string,
): bigint => BigInt(enumCode(table, label, fieldName));

export const decodedEnum = <T extends NumericEnumTable>(
  table: T,
  value: unknown,
  fieldName: string,
): NumericEnumLabel<T> =>
  enumLabel(table, ensureUint(value, fieldName), fieldName);

export const cborOptionalEnum = <T extends NumericEnumTable>(
  table: T,
  label: NumericEnumLabel<T> | null,
  fieldName: string,
): bigint | null => (label == null ? null : cborEnum(table, label, fieldName));

export const decodedOptionalEnum = <T extends NumericEnumTable>(
  table: T,
  value: unknown,
  fieldName: string,
): NumericEnumLabel<T> | null =>
  value == null ? null : decodedEnum(table, value, fieldName);

export const hashArrayValue = (value: unknown, fieldName: string): Buffer[] => {
  const items = asArray(value, fieldName);
  return items.map((item, index) => hashValue(item, `${fieldName}[${index}]`));
};

export const optionalHashArrayValue = (
  value: unknown,
  fieldName: string,
): Buffer[] | null => (value == null ? null : hashArrayValue(value, fieldName));

export const sortedUintArrayValue = (
  value: unknown,
  fieldName: string,
): number[] => {
  const result = asArray(value, fieldName).map((item, index) =>
    ensureUint(item, `${fieldName}[${index}]`),
  );
  for (let index = 1; index < result.length; index += 1) {
    if (result[index - 1]! >= result[index]!) {
      fail(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `${fieldName} must be strictly increasing`,
      );
    }
  }
  return result;
};
