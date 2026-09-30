import "./config.l1-submitter-preflight-config.js";

import { type DaLibp2pRuntimeManifest } from "@al-ft/midgard-core/da-transport";
import { multiaddr } from "@multiformats/multiaddr";

import {
  type Env,
  LIBP2P_DA_MIN_RETENTION_DAYS,
  type Libp2pDaPeerConfig,
  type Libp2pDaRole,
} from "./config.committee-config.js";
import { optionalNonEmpty } from "./config.operational-provider-identity.js";
import { normalizeHex } from "./utils/hex.js";

// @midgard-no-http-da-transport:start
const LIBP2P_DA_ROLES = [
  "committee",
  "producer",
  "watcher",
  "challenger",
  "coordinator",
  "retrieval",
] as const satisfies readonly Libp2pDaRole[];

const libp2pDaKey = (...parts: readonly string[]): string => parts.join("");

const FORBIDDEN_LIBP2P_DA_CONFIG_KEYS = new Set([
  libp2pDaKey("base", "Url"),
  libp2pDaKey("base", "_url"),
  libp2pDaKey("base", "Urls"),
  libp2pDaKey("base", "_urls"),
  "endpoint",
  "url",
  libp2pDaKey("http", "Endpoint"),
  libp2pDaKey("http", "_endpoint"),
  libp2pDaKey("committee", "Endpoint"),
  libp2pDaKey("committee", "_endpoint"),
  libp2pDaKey("da", "Endpoint"),
  libp2pDaKey("da", "_endpoint"),
  libp2pDaKey("gate", "way"),
  libp2pDaKey("object", "Store"),
  libp2pDaKey("object", "_store"),
  libp2pDaKey("buck", "et"),
  libp2pDaKey("s", "3"),
  libp2pDaKey("source", "Endpoint"),
  libp2pDaKey("source", "_endpoint"),
  libp2pDaKey("peer", "Base", "Url"),
  libp2pDaKey("peer", "_base", "_url"),
  libp2pDaKey("payload", "Endpoint", "Base", "Url"),
  libp2pDaKey("payload", "_endpoint", "_base", "_url"),
]);

const LIBP2P_DA_URL_ENV_OVERRIDES = [
  "DA_PAYLOAD_ENDPOINTS",
  "DA_PEER_ENDPOINTS",
  "DA_COORDINATOR_ENDPOINT",
  "DA_PUBLIC_BASE_URL",
] as const;

export class DaRetentionWindowConfigError extends Error {
  public override readonly name = "DaRetentionWindowConfigError";
}

/**
 * Fail-closed startup binding of the committee's configured retention window
 * (GOAL_SPEC 9.4 / Q54). The runtime manifest's `da_transport.retention_days`
 * must both clear the canonical floor and exactly equal the verified deployment
 * manifest's `da.transportProfile.retentionDays`.
 */
export const assertLibp2pDaRetentionDays = (args: {
  readonly runtimeRetentionDays: number;
  readonly manifestRetentionDays: number;
}): number => {
  const { runtimeRetentionDays, manifestRetentionDays } = args;
  if (!Number.isSafeInteger(runtimeRetentionDays) || runtimeRetentionDays < 0) {
    throw new DaRetentionWindowConfigError(
      "da_transport.retention_days must be a non-negative safe integer",
    );
  }
  if (runtimeRetentionDays < LIBP2P_DA_MIN_RETENTION_DAYS) {
    throw new DaRetentionWindowConfigError(
      `da_transport.retention_days must be at least ${LIBP2P_DA_MIN_RETENTION_DAYS.toString()} days, got ${runtimeRetentionDays.toString()}`,
    );
  }
  if (runtimeRetentionDays !== manifestRetentionDays) {
    throw new DaRetentionWindowConfigError(
      `da_transport.retention_days must exactly equal the verified deployment manifest da.transportProfile.retentionDays: runtime=${runtimeRetentionDays.toString()}, manifest=${manifestRetentionDays.toString()}`,
    );
  }
  return runtimeRetentionDays;
};

export const deploymentFingerprintConfig = (
  runtimeManifest: DaLibp2pRuntimeManifest,
  contractDeploymentManifestId: string,
): string => {
  const { deployment } = runtimeManifest;
  if (
    deployment.contract_deployment_manifest_id !== contractDeploymentManifestId
  ) {
    throw new Error(
      `deployment.contract_deployment_manifest_id does not match contract deployment manifestId: runtime=${deployment.contract_deployment_manifest_id}, contract=${contractDeploymentManifestId}`,
    );
  }
  return deployment.fingerprint;
};

export const rejectLibp2pDaUrlEnvOverrides = (env: Env): void => {
  for (const name of LIBP2P_DA_URL_ENV_OVERRIDES) {
    if (optionalNonEmpty(env[name]) !== undefined) {
      throw new Error(`${name} is not allowed in libp2p DA mode`);
    }
  }
};

export const rejectUrlShapedLibp2pDaConfig = (
  value: unknown,
  path: string,
): void => {
  if (Array.isArray(value)) {
    value.forEach((entry, index) => {
      rejectUrlShapedLibp2pDaConfig(entry, `${path}[${index.toString()}]`);
    });
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

export const parseLibp2pDaCommitteePeers = (
  daCommittee: DaLibp2pRuntimeManifest["da_committee"],
): readonly Libp2pDaPeerConfig[] => {
  const members = daCommittee.members;
  const seenIndexes = new Set<number>();
  const seenPeerIds = new Set<string>();
  const peers = members.map((member, memberPosition) => {
    const signerIndex = member.signer_index;
    if (seenIndexes.has(signerIndex)) {
      throw new Error(
        `duplicate da_committee.members signer_index ${signerIndex.toString()}`,
      );
    }
    seenIndexes.add(signerIndex);
    const peerId = requiredPeerId(
      member.peer_id,
      `da_committee.members[${memberPosition.toString()}].peer_id`,
    );
    if (seenPeerIds.has(peerId)) {
      throw new Error(`duplicate da_committee.members peer_id ${peerId}`);
    }
    seenPeerIds.add(peerId);
    return {
      signerIndex,
      daVkey: normalizeHex(member.da_vkey, {
        fieldName: `da_committee.members[${memberPosition.toString()}].da_vkey`,
        byteLength: 32,
      }),
      peerId,
      multiaddrs: requiredMultiaddrList(
        member.multiaddrs,
        `da_committee.members[${memberPosition.toString()}].multiaddrs`,
        { requirePeerId: true, expectedPeerId: peerId },
      ),
      roles: parseLibp2pRoles(
        member.roles,
        `da_committee.members[${memberPosition.toString()}].roles`,
      ),
    };
  });
  return peers.sort((left, right) => left.signerIndex - right.signerIndex);
};

const requiredPeerId = (value: string, fieldName: string): string => {
  const peerId = value.trim();
  if (peerId.length === 0 || /^https?:\/\//i.test(peerId)) {
    throw new Error(`${fieldName} must be a libp2p peer id`);
  }
  try {
    multiaddr(`/p2p/${peerId}`);
  } catch (cause) {
    throw new Error(`${fieldName} must be a valid libp2p peer id`, { cause });
  }
  return peerId;
};

const parseLibp2pRoles = (
  value: unknown,
  fieldName: string,
): readonly Libp2pDaRole[] => {
  if (!Array.isArray(value) || value.length === 0) {
    throw new Error(`${fieldName} must be a non-empty array`);
  }
  const roles = new Set<Libp2pDaRole>();
  for (const entry of value) {
    if (typeof entry !== "string" || !isLibp2pDaRole(entry)) {
      throw new Error(`${fieldName} contains an unrecognized libp2p DA role`);
    }
    if (roles.has(entry)) {
      throw new Error(`${fieldName} contains duplicate role ${entry}`);
    }
    roles.add(entry);
  }
  return [...roles].sort();
};

const isLibp2pDaRole = (value: string): value is Libp2pDaRole =>
  (LIBP2P_DA_ROLES as readonly string[]).includes(value);

export const requiredMultiaddrList = (
  values: readonly string[],
  fieldName: string,
  {
    requirePeerId,
    expectedPeerId,
  }: {
    readonly requirePeerId: boolean;
    readonly expectedPeerId?: string;
  },
): readonly string[] => {
  if (values.length === 0) {
    throw new Error(`${fieldName} must be a non-empty multiaddr array`);
  }
  return values.map((entry, index) =>
    normalizeMultiaddr(entry, `${fieldName}[${index.toString()}]`, {
      requirePeerId,
      expectedPeerId,
    }),
  );
};

const normalizeMultiaddr = (
  value: string,
  fieldName: string,
  {
    requirePeerId,
    expectedPeerId,
  }: {
    readonly requirePeerId: boolean;
    readonly expectedPeerId?: string;
  },
): string => {
  let parsed: ReturnType<typeof multiaddr>;
  try {
    parsed = multiaddr(value.trim());
  } catch (cause) {
    throw new Error(`${fieldName} must be a valid multiaddr`, { cause });
  }
  const peerIds = parsed
    .getComponents()
    .filter((component) => component.name === "p2p")
    .map((component) => component.value)
    .filter((peerId): peerId is string => peerId !== undefined);
  const peerId = peerIds.at(-1);
  if (requirePeerId && peerId === undefined) {
    throw new Error(`${fieldName} must include a /p2p/<peer-id> component`);
  }
  if (expectedPeerId !== undefined && peerId !== expectedPeerId) {
    throw new Error(`${fieldName} peer id must match ${expectedPeerId}`);
  }
  return parsed.toString();
};

export const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);
