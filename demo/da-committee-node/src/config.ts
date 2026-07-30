import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import {
  assertMidgardConsensusV1ReleaseReady,
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  type DaLibp2pRuntimeManifest,
  parseDaLibp2pRuntimeManifest,
} from "@al-ft/midgard-core/da-transport";
import { verifyFinalizedDeploymentManifestV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { multiaddr } from "@multiformats/multiaddr";
import { blake2b } from "@noble/hashes/blake2.js";

import type { DaCommitteeMember } from "./domain.js";
import {
  type MidgardNodeDeployment,
  parseMidgardNodeDeploymentInfo,
} from "./l1/deployment.js";
import { bytesToHex, hexToBytes, normalizeHex } from "./utils/hex.js";

export type DaParamsConfig = {
  readonly committeeHex: string;
  readonly committeeSignersHash: string;
  readonly threshold: number;
};

export type LocalStateConfig =
  | { readonly kind: "file"; readonly path: string }
  | { readonly kind: "database"; readonly url: string };

export type CardanoL1SourceConfig =
  | {
      readonly sourceMode: "local_node";
      readonly authorityNodeId: string;
      readonly authorityDigest: string;
      readonly networkMagic: number;
    }
  | {
      readonly sourceMode: "external_providers";
      readonly providerAuthorityIds: readonly string[];
      readonly authorityDigest: string;
      readonly networkMagic: number;
    };

export type L1SourceConfig =
  | {
      readonly sourceMode: "local_node";
      readonly authorityNodeId: string;
      readonly chainSyncProviderUrl: string;
      readonly chainSyncCursorPath?: string;
      readonly queryProviderUrls: readonly string[];
    }
  | {
      readonly sourceMode: "external_providers";
      readonly providers: readonly {
        readonly identity: string;
        readonly url: string;
        readonly operationalIdentity: {
          readonly operatorId: string;
          readonly transport: "blockfrost_https" | "kupmios" | "fixture";
          readonly normalizedEndpoints: readonly string[];
          readonly backendKey: string;
        };
      }[];
    };

export const l1SourceAuthorityDigest = (
  network: string,
  source: L1SourceConfig,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify(
        source.sourceMode === "local_node"
          ? {
              network,
              sourceMode: source.sourceMode,
              authorityNodeId: source.authorityNodeId,
              chainSyncProviderUrl: source.chainSyncProviderUrl,
              queryProviderUrls: source.queryProviderUrls,
            }
          : {
              network,
              sourceMode: source.sourceMode,
              providers: source.providers,
            },
      ),
    )
    .digest("hex");

export type WatcherConfig = {
  readonly network: string;
  readonly deploymentManifestPath: string;
  readonly contractDeploymentInfoPath: string;
  readonly deploymentFingerprint: string;
  readonly deploymentManifestSha256: string;
  readonly contractDeploymentInfoSha256: string;
  readonly deploymentManifestRaw: string;
  readonly deploymentManifest: Record<string, unknown>;
  readonly contractDeploymentInfo: Record<string, unknown>;
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly midgardNodeDeployment: MidgardNodeDeployment;
  readonly l1Source: L1SourceConfig;
  readonly cardanoProviderUrls: readonly string[];
  readonly finalityDepth: number;
  readonly daTransport: Libp2pDaTransportConfig;
  readonly libp2pPrivateKeySource?: string;
  readonly signerIndex?: number;
  readonly signerKeySource?: string;
  readonly l1SubmitterKeySource?: string;
  readonly l1SubmissionEnabled: boolean;
  readonly l1SubmitterPreflight: L1SubmitterPreflightConfig;
  readonly l1SubmitterId?: string;
  readonly l1SubmitterIds: readonly string[];
  readonly l1LeaderFailoverMs: number;
  readonly localState: LocalStateConfig;
  readonly daParams: DaParamsConfig;
  readonly daCommitteeMembers: readonly DaCommitteeMember[];
  readonly l1SubmitterSignerIndexes: readonly number[];
  readonly daAttestationPolicyId: string;
  readonly daAttestationAddress: string;
  readonly daParamsGovernorPolicyId: string;
  readonly daParamsGovernorAddress: string;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly peerRequestTimeoutMs: number;
  readonly peerReplayWindowMs: number;
  readonly peerMaxBodyBytes: number;
  readonly peerRetryInitialDelayMs: number;
  readonly peerRetryMaxDelayMs: number;
  readonly peerRetryMaxAttempts: number;
  readonly peerRateLimitWindowMs: number;
  readonly peerRateLimitMaxRequests: number;
  readonly apiHost: string;
  readonly apiPort: number;
  readonly pollIntervalMs: number;
};

export type LoadedWatcherConfig = WatcherConfig & {
  readonly cardanoL1Source: CardanoL1SourceConfig;
};

export type Libp2pDaRole =
  | "committee"
  | "producer"
  | "watcher"
  | "challenger"
  | "coordinator"
  | "retrieval";

export type Libp2pDaTransportLimits = {
  readonly maxPayloadBytes: number;
  readonly maxInlineResponseBytes: number;
  readonly maxChunkBytes: number;
  readonly maxStreamsPerPeer: number;
  readonly requestTimeoutMs: number;
};

export type Libp2pDaGossipConfig = {
  readonly strictSign: true;
  readonly emitSelf: false;
  readonly allowedTopicsOnly: true;
  readonly maxGossipMessageBytes: number;
};

export type Libp2pDaPeerConfig = {
  readonly signerIndex: number;
  readonly daVkey: string;
  readonly peerId: string;
  readonly multiaddrs: readonly string[];
  readonly roles: readonly Libp2pDaRole[];
};

export type Libp2pDaTransportConfig = {
  readonly kind: "libp2p";
  readonly deploymentFingerprint: string;
  readonly noHttpDaTransport: true;
  readonly threshold: number;
  readonly listenMultiaddrs: readonly string[];
  readonly announceMultiaddrs: readonly string[];
  readonly bootstrapMultiaddrs: readonly string[];
  readonly gossip: Libp2pDaGossipConfig;
  readonly limits: Libp2pDaTransportLimits;
  readonly retentionDays: number;
  readonly peers: readonly Libp2pDaPeerConfig[];
};

export type L1SubmitterPreflightConfig = {
  readonly enabled: boolean;
  readonly minPlainAdaLovelace: bigint;
  readonly minCollateralLovelace: bigint;
  readonly minSpendableUtxoCount: number;
  readonly autoFundKeySource?: string;
  readonly autoFundBufferLovelace: bigint;
  readonly retryCount: number;
  readonly retryDelayMs: number;
};

type Env = Record<string, string | undefined>;

export const DEFAULT_L1_SUBMITTER_PREFLIGHT = {
  minPlainAdaLovelace: 50_000_000n,
  minCollateralLovelace: 5_000_000n,
  minSpendableUtxoCount: 2,
  autoFundBufferLovelace: 10_000_000n,
  retryCount: 3,
  retryDelayMs: 5_000,
} as const;

export const LIBP2P_DA_TRANSPORT_LIMITS = {
  maxPayloadBytes: MIDGARD_CONSENSUS_LIMITS_V1.maxDaPayloadBytes,
  maxInlineResponseBytes: 1_048_576,
  maxChunkBytes: 1_048_576,
  maxStreamsPerPeer: 16,
  requestTimeoutMs: 15_000,
} as const satisfies Libp2pDaTransportLimits;

export const LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES = 65_536;
export const LIBP2P_DA_MIN_RETENTION_DAYS = 15;
export const loadWatcherConfig = async (
  env: Env = process.env,
): Promise<LoadedWatcherConfig> => {
  const deploymentManifestPath = requireEnv(
    env,
    "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
  );
  const contractDeploymentInfoPath = requireEnv(
    env,
    "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
  );
  const deploymentManifestRaw = await readFile(deploymentManifestPath, "utf8");
  const contractDeploymentInfoRaw = await readFile(
    contractDeploymentInfoPath,
    "utf8",
  );
  const deploymentManifest = parseJsonObject(
    deploymentManifestRaw,
    deploymentManifestPath,
  );
  const runtimeManifest = parseDaLibp2pRuntimeManifest(deploymentManifest);
  const contractDeploymentInfo = parseJsonObject(
    contractDeploymentInfoRaw,
    contractDeploymentInfoPath,
  );
  const deploymentManifestSha256 = createHash("sha256")
    .update(deploymentManifestRaw)
    .digest("hex");
  const contractDeploymentInfoSha256 = createHash("sha256")
    .update(contractDeploymentInfoRaw)
    .digest("hex");
  const {
    manifestId: contractDeploymentManifestId,
    consensusProfile,
    network: contractDeploymentNetwork,
  } = contractDeploymentManifestConfig(
    contractDeploymentInfo,
    contractDeploymentInfoPath,
  );
  const network = runtimeManifest.network;
  if (network !== contractDeploymentNetwork) {
    throw new Error(
      `DA runtime manifest network must exactly match contract deployment manifest network: runtime=${network}, contract=${contractDeploymentNetwork}`,
    );
  }
  if (env.MIDGARD_NETWORK !== undefined && env.MIDGARD_NETWORK !== network) {
    throw new Error(
      `MIDGARD_NETWORK must exactly match runtime manifest network ${network}`,
    );
  }
  const deploymentFingerprint = deploymentFingerprintConfig(
    runtimeManifest,
    contractDeploymentManifestId,
  );
  const libp2pDaTransport = libp2pDaTransportConfig({
    env,
    runtimeManifest,
    deploymentFingerprint,
  });
  const libp2pPrivateKeySource = libp2pPrivateKeySourceConfig(env);
  const midgardNodeDeployment = parseMidgardNodeDeploymentInfo(
    contractDeploymentInfo,
    network,
  );

  const daAttestationPolicyId = midgardNodeDeployment.daAttestation.policyId;
  const daAttestationAddress =
    midgardNodeDeployment.daAttestation.spendingScriptAddress;
  const daParamsGovernorPolicyId =
    midgardNodeDeployment.daParamsGovernor.policyId;
  const daParamsGovernorAddress =
    midgardNodeDeployment.daParamsGovernor.spendingScriptAddress;
  const stateQueuePolicyId = midgardNodeDeployment.stateQueue.policyId;
  const stateQueueAddress =
    midgardNodeDeployment.stateQueue.spendingScriptAddress;
  const l1SubmitterKeySource = optionalKeySource(
    env.L1_SUBMITTER_KEY_SOURCE,
    "L1_SUBMITTER_KEY_SOURCE",
  );
  const l1SubmitterId = optionalNonEmpty(env.DA_L1_SUBMITTER_ID);
  const l1SubmitterIds = optionalSplitList(env.DA_L1_SUBMITTER_IDS);
  if (
    l1SubmitterIds.length > 0 &&
    (l1SubmitterId === undefined || !l1SubmitterIds.includes(l1SubmitterId))
  ) {
    throw new Error(
      "DA_L1_SUBMITTER_ID must be present in DA_L1_SUBMITTER_IDS",
    );
  }
  const cardanoProviderUrls = splitList(
    requireEnv(env, "CARDANO_PROVIDER_URLS"),
  );
  const cardanoL1Source = cardanoL1SourceConfig({
    env,
    network,
    cardanoProviderUrls,
  });
  const l1Source = parseL1SourceConfig(env, cardanoProviderUrls);
  const daCommitteeMembers = libp2pDaTransport.peers.map((member) => ({
    index: member.signerIndex,
    vkey: member.daVkey,
    canSubmitL1: member.roles.includes("coordinator"),
  }));
  const l1SubmissionEnabled = booleanEnv(
    env.DA_L1_SUBMISSION_ENABLED,
    l1SubmitterKeySource !== undefined,
  );
  if (l1SubmissionEnabled && l1SubmitterKeySource === undefined) {
    throw new Error(
      "L1_SUBMITTER_KEY_SOURCE is required when DA_L1_SUBMISSION_ENABLED=true",
    );
  }
  if (l1SubmissionEnabled) {
    if (!isLiveLucidProviderUrl(cardanoProviderUrls[0]!)) {
      throw new Error(
        "L1 submission requires a blockfrost: or kupmios: CARDANO_PROVIDER_URLS entry",
      );
    }
  }
  const l1SubmitterPreflight = l1SubmitterPreflightConfig({
    env,
    l1SubmissionEnabled,
    l1SubmitterKeySource,
  });
  const maybeSigner = optionalSignerConfig(env);
  const daParams = daParamsConfig(env, runtimeManifest, daCommitteeMembers);
  validateLibp2pCommitteeMatchesDaParams(libp2pDaTransport, daParams);
  const l1SubmitterSignerIndexes = parseL1SubmitterSignerIndexes(
    env,
    daCommitteeMembers,
  );

  return {
    network,
    cardanoL1Source,
    deploymentManifestPath,
    contractDeploymentInfoPath,
    deploymentFingerprint,
    deploymentManifestSha256,
    contractDeploymentInfoSha256,
    deploymentManifestRaw,
    deploymentManifest,
    contractDeploymentInfo,
    consensusProfile,
    midgardNodeDeployment,
    l1Source,
    cardanoProviderUrls,
    finalityDepth: nonNegativeInt(
      requireEnv(env, "CARDANO_FINALITY_DEPTH"),
      "CARDANO_FINALITY_DEPTH",
    ),
    daTransport: libp2pDaTransport,
    libp2pPrivateKeySource,
    ...maybeSigner,
    l1SubmitterKeySource,
    l1SubmissionEnabled,
    l1SubmitterPreflight,
    ...(l1SubmitterId === undefined ? {} : { l1SubmitterId }),
    l1SubmitterIds:
      l1SubmitterIds.length > 0
        ? l1SubmitterIds
        : l1SubmitterId === undefined
          ? []
          : [l1SubmitterId],
    l1LeaderFailoverMs: nonNegativeInt(
      env.DA_L1_LEADER_FAILOVER_MS ?? "15000",
      "DA_L1_LEADER_FAILOVER_MS",
    ),
    localState: localState(env),
    daParams,
    daCommitteeMembers,
    l1SubmitterSignerIndexes,
    daAttestationPolicyId: normalizeHex(daAttestationPolicyId, {
      fieldName: "DA attestation policy id",
      byteLength: 28,
    }),
    daAttestationAddress,
    daParamsGovernorPolicyId: normalizeHex(daParamsGovernorPolicyId, {
      fieldName: "DA params governor policy id",
      byteLength: 28,
    }),
    daParamsGovernorAddress,
    stateQueuePolicyId: normalizeHex(stateQueuePolicyId, {
      fieldName: "state queue policy id",
      byteLength: 28,
    }),
    stateQueueAddress,
    peerRequestTimeoutMs: positiveInt(
      env.DA_PEER_REQUEST_TIMEOUT_MS ?? "5000",
      "DA_PEER_REQUEST_TIMEOUT_MS",
    ),
    peerReplayWindowMs: positiveInt(
      env.DA_PEER_REPLAY_WINDOW_MS ?? "300000",
      "DA_PEER_REPLAY_WINDOW_MS",
    ),
    peerMaxBodyBytes: positiveInt(
      env.DA_PEER_MAX_BODY_BYTES ?? "1048576",
      "DA_PEER_MAX_BODY_BYTES",
    ),
    peerRetryInitialDelayMs: positiveInt(
      env.DA_PEER_RETRY_INITIAL_DELAY_MS ?? "1000",
      "DA_PEER_RETRY_INITIAL_DELAY_MS",
    ),
    peerRetryMaxDelayMs: positiveInt(
      env.DA_PEER_RETRY_MAX_DELAY_MS ?? "60000",
      "DA_PEER_RETRY_MAX_DELAY_MS",
    ),
    peerRetryMaxAttempts: positiveInt(
      env.DA_PEER_RETRY_MAX_ATTEMPTS ?? "12",
      "DA_PEER_RETRY_MAX_ATTEMPTS",
    ),
    peerRateLimitWindowMs: positiveInt(
      env.DA_PEER_RATE_LIMIT_WINDOW_MS ?? "60000",
      "DA_PEER_RATE_LIMIT_WINDOW_MS",
    ),
    peerRateLimitMaxRequests: positiveInt(
      env.DA_PEER_RATE_LIMIT_MAX_REQUESTS ?? "120",
      "DA_PEER_RATE_LIMIT_MAX_REQUESTS",
    ),
    apiHost: env.WATCHER_API_HOST ?? "127.0.0.1",
    apiPort: positiveInt(env.WATCHER_API_PORT ?? "8787", "WATCHER_API_PORT"),
    pollIntervalMs: positiveInt(
      env.WATCHER_POLL_INTERVAL_MS ?? "15000",
      "WATCHER_POLL_INTERVAL_MS",
    ),
  };
};

const requireEnv = (env: Env, name: string): string => {
  const value = env[name];
  if (value === undefined || value.trim() === "") {
    throw new Error(`${name} is required`);
  }
  return value.trim();
};

const optionalNonEmpty = (value: string | undefined): string | undefined => {
  const trimmed = value?.trim();
  return trimmed === undefined || trimmed === "" ? undefined : trimmed;
};

const optionalKeySource = (
  value: string | undefined,
  name: string,
): string | undefined => {
  const source = optionalNonEmpty(value);
  if (source === undefined) {
    return undefined;
  }
  validateKeySourceSyntax(source, name);
  return source;
};

const validateKeySourceSyntax = (source: string, name: string): void => {
  const prefixedSources = [
    "file:",
    "seed:",
    "mnemonic:",
    "private-key:",
    "privateKey:",
  ];
  for (const prefix of prefixedSources) {
    if (source === prefix) {
      throw new Error(`${name} must include a value after ${prefix}`);
    }
  }
};

const splitList = (value: string): readonly string[] => {
  const values = value
    .split(",")
    .map((part) => part.trim())
    .filter((part) => part.length > 0);
  if (values.length === 0) {
    throw new Error("expected a non-empty comma-separated list");
  }
  return values;
};

const optionalSplitList = (value: string | undefined): readonly string[] => {
  const trimmed = optionalNonEmpty(value);
  return trimmed === undefined ? [] : splitList(trimmed);
};

export const parseL1SourceConfig = (
  env: Env,
  cardanoProviderUrls: readonly string[],
): L1SourceConfig => {
  const sourceMode = requireEnv(env, "CARDANO_L1_SOURCE_MODE");
  const testMode = booleanEnv(env.CARDANO_L1_TEST_MODE, false);
  if (sourceMode !== "local_node" && sourceMode !== "external_providers") {
    throw new Error(
      "CARDANO_L1_SOURCE_MODE must be local_node or external_providers",
    );
  }
  if (
    !testMode &&
    cardanoProviderUrls.some((url) => isFixtureProviderUrl(url))
  ) {
    throw new Error(
      "fixture:/file: Cardano providers require explicit CARDANO_L1_TEST_MODE=true",
    );
  }
  if (sourceMode === "local_node") {
    if (
      optionalNonEmpty(env.CARDANO_EXTERNAL_PROVIDER_IDENTITIES) !== undefined
    ) {
      throw new Error(
        "CARDANO_EXTERNAL_PROVIDER_IDENTITIES is forbidden in local_node mode",
      );
    }
    const chainSyncProviderUrl = localChainSyncUrl(
      requireEnv(env, "CARDANO_LOCAL_NODE_CHAIN_SYNC_URL"),
      testMode,
    );
    if (!testMode) {
      assertLocalQuerySurfacesShareAuthority(
        chainSyncProviderUrl,
        cardanoProviderUrls,
      );
    }
    return {
      sourceMode,
      authorityNodeId: boundedIdentity(
        requireEnv(env, "CARDANO_LOCAL_NODE_AUTHORITY_ID"),
        "CARDANO_LOCAL_NODE_AUTHORITY_ID",
      ),
      chainSyncProviderUrl,
      chainSyncCursorPath: requireEnv(
        env,
        "CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH",
      ),
      queryProviderUrls: cardanoProviderUrls,
    };
  }
  if (
    optionalNonEmpty(env.CARDANO_LOCAL_NODE_AUTHORITY_ID) !== undefined ||
    optionalNonEmpty(env.CARDANO_LOCAL_NODE_CHAIN_SYNC_URL) !== undefined ||
    optionalNonEmpty(env.CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH) !==
      undefined
  ) {
    throw new Error(
      "CARDANO_LOCAL_NODE_* configuration is forbidden in external_providers mode",
    );
  }
  if (cardanoProviderUrls.length < 2) {
    throw new Error(
      "external_providers mode requires at least two operationally independent CARDANO_PROVIDER_URLS entries",
    );
  }
  const identities = splitList(
    requireEnv(env, "CARDANO_EXTERNAL_PROVIDER_IDENTITIES"),
  ).map((identity) =>
    boundedIdentity(identity, "CARDANO_EXTERNAL_PROVIDER_IDENTITIES"),
  );
  if (identities.length !== cardanoProviderUrls.length) {
    throw new Error(
      "CARDANO_EXTERNAL_PROVIDER_IDENTITIES must contain one identity per CARDANO_PROVIDER_URLS entry",
    );
  }
  if (new Set(identities).size !== identities.length) {
    throw new Error(
      "external_providers mode requires distinct operational provider identities",
    );
  }
  const operationalIdentities = cardanoProviderUrls.map((url, index) =>
    operationalProviderIdentity(url, identities[index]!, testMode),
  );
  const endpointOwners = new Map<string, string>();
  for (const identity of operationalIdentities) {
    for (const endpoint of identity.normalizedEndpoints) {
      const existingOwner = endpointOwners.get(endpoint);
      if (existingOwner !== undefined) {
        throw new Error(
          `external_providers mode requires operationally independent backends; ${existingOwner} and ${identity.operatorId} share normalized endpoint ${endpoint}`,
        );
      }
      endpointOwners.set(endpoint, identity.operatorId);
    }
  }
  if (
    new Set(operationalIdentities.map(({ backendKey }) => backendKey)).size !==
    operationalIdentities.length
  ) {
    throw new Error(
      "external_providers mode requires distinct normalized provider backends",
    );
  }
  return {
    sourceMode,
    providers: cardanoProviderUrls.map((url, index) => ({
      identity: identities[index]!,
      url,
      operationalIdentity: operationalIdentities[index]!,
    })),
  };
};

const localChainSyncUrl = (value: string, testMode: boolean): string => {
  if (!value.startsWith("chain-sync:") || value === "chain-sync:") {
    throw new Error(
      "CARDANO_LOCAL_NODE_CHAIN_SYNC_URL must use the chain-sync:<provider> form",
    );
  }
  const provider = value.slice("chain-sync:".length);
  if (!testMode && isFixtureProviderUrl(provider)) {
    throw new Error(
      "fixture:/file: local chain-sync sources require explicit CARDANO_L1_TEST_MODE=true",
    );
  }
  if (
    !provider.startsWith("kupmios:") &&
    !provider.startsWith("ogmios:") &&
    !provider.startsWith("fixture:") &&
    !provider.startsWith("file:")
  ) {
    throw new Error(
      "CARDANO_LOCAL_NODE_CHAIN_SYNC_URL authority must be a local ogmios: or kupmios: surface (fixture:/file: only in tests)",
    );
  }
  return value;
};

const assertLocalQuerySurfacesShareAuthority = (
  chainSyncProviderUrl: string,
  queryProviderUrls: readonly string[],
): void => {
  const authorityProvider = chainSyncProviderUrl.slice("chain-sync:".length);
  const authorityOgmiosUrl = authorityProvider.startsWith("ogmios:")
    ? authorityProvider.slice("ogmios:".length)
    : authorityProvider.startsWith("kupmios:")
      ? authorityProvider.slice("kupmios:".length).split("|")[1]
      : undefined;
  if (authorityOgmiosUrl === undefined) {
    throw new Error(
      "production local_node chain sync requires an Ogmios authority endpoint",
    );
  }
  const normalizedAuthority = normalizeOperationalEndpoint(
    authorityOgmiosUrl,
    "local authority Ogmios",
  );
  for (const [index, providerUrl] of queryProviderUrls.entries()) {
    if (!providerUrl.startsWith("kupmios:")) {
      throw new Error(
        `production local_node query surface ${index.toString()} must be kupmios: backed by the local authority`,
      );
    }
    const [, queryOgmiosUrl, extra] = providerUrl
      .slice("kupmios:".length)
      .split("|");
    if (queryOgmiosUrl === undefined || extra !== undefined) {
      throw new Error(
        "kupmios provider URL must be kupmios:<kupo-url>|<ogmios-url>",
      );
    }
    const normalizedQueryAuthority = normalizeOperationalEndpoint(
      queryOgmiosUrl,
      "query Ogmios",
    );
    if (normalizedQueryAuthority !== normalizedAuthority) {
      throw new Error(
        `production local_node query surface ${index.toString()} is not backed by the configured chain-sync authority`,
      );
    }
  }
};

const isFixtureProviderUrl = (value: string): boolean =>
  value.startsWith("fixture:") || value.startsWith("file:");

const operationalProviderIdentity = (
  value: string,
  operatorId: string,
  testMode: boolean,
): {
  readonly operatorId: string;
  readonly transport: "blockfrost_https" | "kupmios" | "fixture";
  readonly normalizedEndpoints: readonly string[];
  readonly backendKey: string;
} => {
  if (value.startsWith("blockfrost:")) {
    const raw = value.slice("blockfrost:".length);
    const projectSeparator = raw.lastIndexOf("#");
    if (projectSeparator <= 0 || projectSeparator === raw.length - 1) {
      throw new Error(
        "blockfrost provider URL must be blockfrost:<api-url>#<project-id>",
      );
    }
    const endpoint = normalizeOperationalEndpoint(
      raw.slice(0, projectSeparator),
      "blockfrost",
    );
    if (!endpoint.startsWith("https://")) {
      throw new Error(
        "external Blockfrost providers require HTTPS transport evidence",
      );
    }
    return {
      operatorId,
      transport: "blockfrost_https",
      normalizedEndpoints: [endpoint],
      backendKey: `blockfrost:${endpoint}`,
    };
  }
  if (value.startsWith("kupmios:")) {
    const [kupoUrl, ogmiosUrl, extra] = value
      .slice("kupmios:".length)
      .split("|");
    if (
      kupoUrl === undefined ||
      ogmiosUrl === undefined ||
      extra !== undefined
    ) {
      throw new Error(
        "kupmios provider URL must be kupmios:<kupo-url>|<ogmios-url>",
      );
    }
    const normalizedKupo = normalizeOperationalEndpoint(kupoUrl, "Kupo");
    const normalizedOgmios = normalizeOperationalEndpoint(ogmiosUrl, "Ogmios");
    if (
      !testMode &&
      (!normalizedKupo.startsWith("https://") ||
        !normalizedOgmios.startsWith("https://"))
    ) {
      throw new Error(
        "external Kupmios providers require HTTPS Kupo and TLS-protected WSS/HTTPS Ogmios transport evidence",
      );
    }
    const endpoints = [normalizedKupo, normalizedOgmios].sort();
    return {
      operatorId,
      transport: "kupmios",
      normalizedEndpoints: endpoints,
      backendKey: `kupmios:${endpoints.join("|")}`,
    };
  }
  if (testMode && isFixtureProviderUrl(value)) {
    const endpoint = value.startsWith("file:")
      ? new URL(value).pathname
      : value.slice("fixture:".length);
    const normalized = `fixture:${endpoint}`;
    return {
      operatorId,
      transport: "fixture",
      normalizedEndpoints: [normalized],
      backendKey: normalized,
    };
  }
  throw new Error(
    `unsupported external Cardano provider ${value}; operational identity evidence requires blockfrost: or kupmios:`,
  );
};

const normalizeOperationalEndpoint = (value: string, label: string): string => {
  let parsed: URL;
  try {
    parsed = new URL(value);
  } catch {
    throw new Error(`${label} operational endpoint must be an absolute URL`);
  }
  if (
    parsed.protocol !== "https:" &&
    parsed.protocol !== "http:" &&
    parsed.protocol !== "wss:" &&
    parsed.protocol !== "ws:"
  ) {
    throw new Error(`${label} operational endpoint uses unsupported transport`);
  }
  if (parsed.username !== "" || parsed.password !== "") {
    throw new Error(
      `${label} operational endpoint must not embed credentials in its identity`,
    );
  }
  const canonicalProtocol =
    parsed.protocol === "wss:"
      ? "https:"
      : parsed.protocol === "ws:"
        ? "http:"
        : parsed.protocol;
  const defaultPort =
    canonicalProtocol === "https:" && parsed.port === "443"
      ? ""
      : canonicalProtocol === "http:" && parsed.port === "80"
        ? ""
        : parsed.port;
  const hostname = parsed.hostname.toLowerCase().replace(/\.$/u, "");
  return `${canonicalProtocol}//${hostname}${defaultPort === "" ? "" : `:${defaultPort}`}`;
};

const boundedIdentity = (value: string, name: string): string => {
  if (!/^[a-z][a-z0-9-]{2,63}$/u.test(value)) {
    throw new Error(`${name} entries must be lowercase operational identities`);
  }
  return value;
};

const isLiveLucidProviderUrl = (value: string): boolean =>
  value.startsWith("blockfrost:") || value.startsWith("kupmios:");

const CARDANO_NAMED_NETWORK_MAGIC = {
  Mainnet: 764_824_073,
  Preprod: 1,
  Preview: 2,
} as const;
const CARDANO_NETWORK_MAGIC_MAX = 4_294_967_295;
const CARDANO_AUTHORITY_ID = /^[a-zA-Z0-9][a-zA-Z0-9._-]{0,127}$/u;
const LOWER_HEX_32 = /^[0-9a-f]{64}$/u;

const cardanoL1SourceConfig = ({
  env,
  network,
  cardanoProviderUrls,
}: {
  readonly env: Env;
  readonly network: string;
  readonly cardanoProviderUrls: readonly string[];
}): CardanoL1SourceConfig => {
  const sourceMode = requireEnv(env, "CARDANO_L1_SOURCE_MODE");
  if (sourceMode !== "local_node" && sourceMode !== "external_providers") {
    throw new Error(
      "CARDANO_L1_SOURCE_MODE must be local_node or external_providers",
    );
  }
  const networkMagic = cardanoNetworkMagic(env, network);
  if (sourceMode === "local_node") {
    if (
      cardanoProviderUrls.some(
        (url) =>
          !url.startsWith("kupmios:") &&
          !url.startsWith("fixture:") &&
          !url.startsWith("file:"),
      )
    ) {
      throw new Error(
        "local_node mode permits only same-node kupmios query surfaces or deterministic fixtures",
      );
    }
    const authorityNodeId = requireEnv(env, "CARDANO_LOCAL_NODE_AUTHORITY_ID");
    if (!CARDANO_AUTHORITY_ID.test(authorityNodeId)) {
      throw new Error(
        "CARDANO_LOCAL_NODE_AUTHORITY_ID must be a stable public identifier",
      );
    }
    if (optionalNonEmpty(env.CARDANO_PROVIDER_AUTHORITY_IDS) !== undefined) {
      throw new Error(
        "CARDANO_PROVIDER_AUTHORITY_IDS must be omitted in local_node mode",
      );
    }
    const authorityDigest = cardanoAuthorityDigest({
      sourceMode,
      network,
      networkMagic,
      authorityNodeId,
      querySurfaces: cardanoProviderUrls.map(providerPublicIdentity).sort(),
    });
    return {
      sourceMode,
      authorityNodeId,
      authorityDigest,
      networkMagic,
    };
  }

  if (optionalNonEmpty(env.CARDANO_LOCAL_NODE_AUTHORITY_ID) !== undefined) {
    throw new Error(
      "CARDANO_LOCAL_NODE_AUTHORITY_ID must be omitted in external_providers mode",
    );
  }
  if (cardanoProviderUrls.length < 2) {
    throw new Error(
      "external_providers mode requires at least two Cardano provider URLs",
    );
  }
  if (cardanoProviderUrls.some((url) => !isLiveLucidProviderUrl(url))) {
    throw new Error(
      "external_providers mode requires live blockfrost or kupmios providers",
    );
  }
  const providerAuthorityIds = splitList(
    requireEnv(env, "CARDANO_PROVIDER_AUTHORITY_IDS"),
  );
  if (providerAuthorityIds.length !== cardanoProviderUrls.length) {
    throw new Error(
      "CARDANO_PROVIDER_AUTHORITY_IDS must contain exactly one identity per CARDANO_PROVIDER_URLS entry",
    );
  }
  if (providerAuthorityIds.some((identity) => !LOWER_HEX_32.test(identity))) {
    throw new Error(
      "CARDANO_PROVIDER_AUTHORITY_IDS entries must be lowercase SHA-256 identities",
    );
  }
  if (
    new Set(providerAuthorityIds).size !== providerAuthorityIds.length ||
    new Set(cardanoProviderUrls.map(providerPublicIdentity)).size !==
      cardanoProviderUrls.length
  ) {
    throw new Error(
      "external_providers mode requires operationally independent provider authorities and endpoints",
    );
  }
  const providers = cardanoProviderUrls
    .map((url, index) => ({
      authorityId: providerAuthorityIds[index]!,
      endpoint: providerPublicIdentity(url),
    }))
    .sort((left, right) => left.authorityId.localeCompare(right.authorityId));
  const authorityDigest = cardanoAuthorityDigest({
    sourceMode,
    network,
    networkMagic,
    providers,
  });
  return {
    sourceMode,
    providerAuthorityIds,
    authorityDigest,
    networkMagic,
  };
};

const cardanoNetworkMagic = (env: Env, network: string): number => {
  const configured = optionalNonEmpty(env.CARDANO_NETWORK_MAGIC);
  if (network === "Custom") {
    if (configured === undefined) {
      throw new Error("CARDANO_NETWORK_MAGIC is required for Custom network");
    }
    return networkMagicInteger(configured);
  }
  if (network === "Mainnet" || network === "Preprod" || network === "Preview") {
    if (configured !== undefined) {
      throw new Error(
        "CARDANO_NETWORK_MAGIC must be omitted for named Cardano networks",
      );
    }
    return CARDANO_NAMED_NETWORK_MAGIC[network];
  }
  throw new Error(
    "Cardano network must be Mainnet, Preprod, Preview, or Custom",
  );
};

const networkMagicInteger = (value: string): number => {
  if (!/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(
      "CARDANO_NETWORK_MAGIC must be a canonical unsigned 32-bit integer",
    );
  }
  const parsed = Number(value);
  if (
    !Number.isSafeInteger(parsed) ||
    parsed < 0 ||
    parsed > CARDANO_NETWORK_MAGIC_MAX
  ) {
    throw new Error(
      "CARDANO_NETWORK_MAGIC must be a canonical unsigned 32-bit integer",
    );
  }
  return parsed;
};

const cardanoAuthorityDigest = (identity: object): string =>
  createHash("sha256")
    .update(
      JSON.stringify({
        schemaVersion: "midgard-da-cardano-l1-authority-v1",
        ...identity,
      }),
    )
    .digest("hex");

const providerPublicIdentity = (url: string): string => {
  if (url.startsWith("blockfrost:")) {
    const raw = url.slice("blockfrost:".length);
    const projectSeparator = raw.lastIndexOf("#");
    return `blockfrost:${projectSeparator < 0 ? raw : raw.slice(0, projectSeparator)}`;
  }
  return url;
};

const booleanEnv = (
  value: string | undefined,
  defaultValue: boolean,
): boolean => {
  const normalized = value?.trim().toLowerCase();
  if (normalized === undefined || normalized === "") {
    return defaultValue;
  }
  if (["1", "true", "yes", "on"].includes(normalized)) {
    return true;
  }
  if (["0", "false", "no", "off"].includes(normalized)) {
    return false;
  }
  throw new Error("boolean environment values must be true or false");
};

const nonNegativeInt = (value: string, name: string): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${name} must be a non-negative integer`);
  }
  return parsed;
};

const positiveInt = (value: string, name: string): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${name} must be a positive integer`);
  }
  return parsed;
};

const positiveLovelace = (value: string, name: string): bigint => {
  const normalized = value.trim().replaceAll("_", "");
  if (!/^[0-9]+$/.test(normalized)) {
    throw new Error(`${name} must be a positive lovelace integer`);
  }
  const parsed = BigInt(normalized);
  if (parsed <= 0n) {
    throw new Error(`${name} must be a positive lovelace integer`);
  }
  return parsed;
};

const l1SubmitterPreflightConfig = ({
  env,
  l1SubmissionEnabled,
  l1SubmitterKeySource,
}: {
  readonly env: Env;
  readonly l1SubmissionEnabled: boolean;
  readonly l1SubmitterKeySource?: string;
}): L1SubmitterPreflightConfig => {
  const autoFundKeySource = optionalKeySource(
    env.DA_L1_AUTO_FUND_KEY_SOURCE,
    "DA_L1_AUTO_FUND_KEY_SOURCE",
  );
  if (
    autoFundKeySource !== undefined &&
    l1SubmitterKeySource !== undefined &&
    autoFundKeySource === l1SubmitterKeySource
  ) {
    throw new Error(
      "DA_L1_AUTO_FUND_KEY_SOURCE must not equal L1_SUBMITTER_KEY_SOURCE",
    );
  }
  return {
    enabled:
      l1SubmissionEnabled && booleanEnv(env.DA_L1_PREFLIGHT_ENABLED, true),
    minPlainAdaLovelace: positiveLovelace(
      env.DA_L1_MIN_PLAIN_ADA_LOVELACE ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.minPlainAdaLovelace.toString(),
      "DA_L1_MIN_PLAIN_ADA_LOVELACE",
    ),
    minCollateralLovelace: positiveLovelace(
      env.DA_L1_MIN_COLLATERAL_LOVELACE ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.minCollateralLovelace.toString(),
      "DA_L1_MIN_COLLATERAL_LOVELACE",
    ),
    minSpendableUtxoCount: positiveInt(
      env.DA_L1_MIN_SPENDABLE_UTXO_COUNT ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.minSpendableUtxoCount.toString(),
      "DA_L1_MIN_SPENDABLE_UTXO_COUNT",
    ),
    ...(autoFundKeySource === undefined ? {} : { autoFundKeySource }),
    autoFundBufferLovelace: positiveLovelace(
      env.DA_L1_AUTO_FUND_BUFFER_LOVELACE ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.autoFundBufferLovelace.toString(),
      "DA_L1_AUTO_FUND_BUFFER_LOVELACE",
    ),
    retryCount: positiveInt(
      env.DA_L1_PREFLIGHT_RETRY_COUNT ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.retryCount.toString(),
      "DA_L1_PREFLIGHT_RETRY_COUNT",
    ),
    retryDelayMs: positiveInt(
      env.DA_L1_PREFLIGHT_RETRY_DELAY_MS ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.retryDelayMs.toString(),
      "DA_L1_PREFLIGHT_RETRY_DELAY_MS",
    ),
  };
};

const signerIndex = (value: string): number => {
  const parsed = nonNegativeInt(value, "DA_SIGNER_INDEX");
  if (parsed > 255) {
    throw new Error("DA_SIGNER_INDEX must fit in one byte");
  }
  return parsed;
};

const optionalSignerConfig = (
  env: Env,
): Pick<WatcherConfig, "signerIndex" | "signerKeySource"> => {
  const configuredMode = optionalNonEmpty(env.DA_MODE);
  if (configuredMode !== undefined) {
    throw new Error("DA_MODE has been removed and must be omitted");
  }
  const index = optionalNonEmpty(env.DA_SIGNER_INDEX);
  const keySource = optionalNonEmpty(env.DA_SIGNER_KEY_SOURCE);
  if (index === undefined && keySource === undefined) {
    return {};
  }
  if (index === undefined || keySource === undefined) {
    throw new Error(
      "DA_SIGNER_INDEX and DA_SIGNER_KEY_SOURCE must be set together",
    );
  }
  return {
    signerIndex: signerIndex(index),
    signerKeySource: keySource,
  };
};

const localState = (env: Env): LocalStateConfig => {
  const dbPath = optionalNonEmpty(env.WATCHER_DB_PATH);
  const databaseUrl = optionalNonEmpty(env.WATCHER_DATABASE_URL);
  if (dbPath !== undefined && databaseUrl !== undefined) {
    throw new Error("set only one of WATCHER_DB_PATH or WATCHER_DATABASE_URL");
  }
  if (dbPath !== undefined) {
    return { kind: "file", path: dbPath };
  }
  if (databaseUrl !== undefined) {
    return { kind: "database", url: databaseUrl };
  }
  throw new Error("WATCHER_DB_PATH or WATCHER_DATABASE_URL is required");
};

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

const contractDeploymentManifestConfig = (
  contractDeploymentInfo: Record<string, unknown>,
  path: string,
): {
  readonly manifestId: string;
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly network: string;
} => {
  const verified = verifyFinalizedDeploymentManifestV1(contractDeploymentInfo);
  const exactProfile = verified.consensusProfile;
  if (!isMidgardConsensusProfileV1(exactProfile)) {
    throw new Error(`${path} does not contain the exact V1 consensus profile`);
  }
  assertMidgardConsensusV1ReleaseReady();
  const manifestId = verified.manifestId as string;
  const network = verified.network;
  if (typeof network !== "string" || network.length === 0) {
    throw new Error(`${path} does not contain a deployment network`);
  }
  return {
    manifestId: normalizeHex(manifestId, {
      fieldName: "contract deployment manifestId",
      byteLength: 32,
    }),
    consensusProfile: exactProfile,
    network,
  };
};

const deploymentFingerprintConfig = (
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

const libp2pDaTransportConfig = ({
  env,
  runtimeManifest,
  deploymentFingerprint,
}: {
  readonly env: Env;
  readonly runtimeManifest: DaLibp2pRuntimeManifest;
  readonly deploymentFingerprint: string;
}): Libp2pDaTransportConfig => {
  rejectLibp2pDaUrlEnvOverrides(env);
  if (runtimeManifest.runtime_topology.target !== "watcher") {
    throw new Error("runtime_topology.target must be watcher");
  }
  const { da_committee: daCommittee, da_transport: daTransport } =
    runtimeManifest;
  rejectUrlShapedLibp2pDaConfig(daTransport, "da_transport");
  rejectUrlShapedLibp2pDaConfig(daCommittee, "da_committee");
  const threshold = daCommittee.threshold;
  const peers = parseLibp2pDaCommitteePeers(daCommittee);
  return {
    kind: "libp2p",
    deploymentFingerprint,
    noHttpDaTransport: true,
    threshold,
    listenMultiaddrs: requiredMultiaddrList(
      daTransport.listen_multiaddrs,
      "da_transport.listen_multiaddrs",
      { requirePeerId: false },
    ),
    announceMultiaddrs: requiredMultiaddrList(
      daTransport.announce_multiaddrs,
      "da_transport.announce_multiaddrs",
      { requirePeerId: true },
    ),
    bootstrapMultiaddrs: requiredMultiaddrList(
      daTransport.bootstrap_multiaddrs,
      "da_transport.bootstrap_multiaddrs",
      { requirePeerId: true },
    ),
    gossip: {
      strictSign: true,
      emitSelf: false,
      allowedTopicsOnly: true,
      maxGossipMessageBytes: daTransport.gossip.max_gossip_message_bytes,
    },
    limits: {
      maxPayloadBytes: daTransport.limits.max_payload_bytes,
      maxInlineResponseBytes: daTransport.limits.max_inline_response_bytes,
      maxChunkBytes: daTransport.limits.max_chunk_bytes,
      maxStreamsPerPeer: daTransport.limits.max_streams_per_peer,
      requestTimeoutMs: daTransport.limits.request_timeout_ms,
    },
    retentionDays: daTransport.retention_days,
    peers,
  };
};

const rejectLibp2pDaUrlEnvOverrides = (env: Env): void => {
  for (const name of LIBP2P_DA_URL_ENV_OVERRIDES) {
    if (optionalNonEmpty(env[name]) !== undefined) {
      throw new Error(`${name} is not allowed in libp2p DA mode`);
    }
  }
};

const rejectUrlShapedLibp2pDaConfig = (value: unknown, path: string): void => {
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

const libp2pPrivateKeySourceConfig = (env: Env): string => {
  const source = optionalNonEmpty(env.DA_LIBP2P_PRIVATE_KEY_SOURCE);
  if (source === undefined) {
    throw new Error(
      "DA_LIBP2P_PRIVATE_KEY_SOURCE is required in libp2p DA mode",
    );
  }
  validateLibp2pPrivateKeySource(source);
  return source;
};

const validateLibp2pPrivateKeySource = (source: string): void => {
  if (source.startsWith("seed:")) {
    normalizeHex(source.slice("seed:".length), {
      fieldName: "DA_LIBP2P_PRIVATE_KEY_SOURCE seed",
      byteLength: 32,
    });
    return;
  }
  if (source.startsWith("hex:")) {
    const encoded = source.slice("hex:".length);
    if (encoded.length === 0) {
      throw new Error("DA_LIBP2P_PRIVATE_KEY_SOURCE must include a hex key");
    }
    normalizeHex(encoded, {
      fieldName: "DA_LIBP2P_PRIVATE_KEY_SOURCE protobuf key",
    });
    return;
  }
  if (source.startsWith("file:")) {
    if (source.slice("file:".length).trim() === "") {
      throw new Error("DA_LIBP2P_PRIVATE_KEY_SOURCE must include a file path");
    }
    return;
  }
  throw new Error(
    "DA_LIBP2P_PRIVATE_KEY_SOURCE must use seed:, hex:, or file:",
  );
};

const parseLibp2pDaCommitteePeers = (
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

const requiredMultiaddrList = (
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

const validateLibp2pCommitteeMatchesDaParams = (
  transport: Libp2pDaTransportConfig,
  daParams: DaParamsConfig,
): void => {
  const committeeKeys = hexToBytes(daParams.committeeHex, "DA committee");
  const signerCount = committeeKeys.length / 32;
  for (const peer of transport.peers) {
    if (peer.signerIndex >= signerCount) {
      throw new Error(
        `da_committee member signer_index ${peer.signerIndex.toString()} is outside DA committee`,
      );
    }
    const expectedKey = bytesToHex(
      committeeKeys.subarray(peer.signerIndex * 32, peer.signerIndex * 32 + 32),
    );
    if (peer.daVkey !== expectedKey) {
      throw new Error(
        `da_committee member signer_index ${peer.signerIndex.toString()} da_vkey does not match DA committee`,
      );
    }
  }
  if (transport.threshold !== daParams.threshold) {
    throw new Error("da_committee.threshold must match DA params threshold");
  }
};
// @midgard-no-http-da-transport:end

const daParamsConfig = (
  env: Env,
  runtimeManifest: DaLibp2pRuntimeManifest,
  daCommitteeMembers: readonly DaCommitteeMember[],
): DaParamsConfig => {
  const memberKeys = daCommitteeMembers.map((member) => member.vkey);
  const committeeHex = normalizeHex(memberKeys.join(""), {
    fieldName: "DA committee",
  });
  if (committeeHex.length === 0 || committeeHex.length % 64 !== 0) {
    throw new Error("DA committee must be packed 32-byte verification keys");
  }
  if (
    env.DA_COMMITTEE_HEX !== undefined &&
    normalizeHex(env.DA_COMMITTEE_HEX, {
      fieldName: "DA_COMMITTEE_HEX",
    }) !== committeeHex
  ) {
    throw new Error("DA_COMMITTEE_HEX must exactly match da_committee.members");
  }
  const computedCommitteeHash = bytesToHex(
    blake2b(hexToBytes(committeeHex, "DA committee"), { dkLen: 32 }),
  );
  if (
    env.DA_COMMITTEE_SIGNERS_HASH !== undefined &&
    normalizeHex(env.DA_COMMITTEE_SIGNERS_HASH, {
      fieldName: "DA_COMMITTEE_SIGNERS_HASH",
      byteLength: 32,
    }) !== computedCommitteeHash
  ) {
    throw new Error(
      "DA_COMMITTEE_SIGNERS_HASH must exactly match da_committee.members",
    );
  }
  const committeeSignersHash = computedCommitteeHash;
  const threshold = runtimeManifest.da_committee.threshold;
  if (
    env.DA_THRESHOLD !== undefined &&
    positiveInt(env.DA_THRESHOLD, "DA_THRESHOLD") !== threshold
  ) {
    throw new Error("DA_THRESHOLD must exactly match da_committee.threshold");
  }
  return { committeeHex, committeeSignersHash, threshold };
};

const parseL1SubmitterSignerIndexes = (
  env: Env,
  members: readonly DaCommitteeMember[],
): readonly number[] => {
  const configured = optionalNonEmpty(env.DA_L1_SUBMITTER_SIGNER_INDEXES);
  if (configured !== undefined) {
    return splitList(configured).map(signerIndex);
  }
  const submitterMembers = members
    .filter((member) => member.canSubmitL1)
    .map((member) => member.index);
  if (submitterMembers.length > 0) {
    return submitterMembers;
  }
  return [];
};

const parseJsonObject = (
  raw: string,
  path: string,
): Record<string, unknown> => {
  const parsed = JSON.parse(raw) as unknown;
  if (!isRecord(parsed)) {
    throw new Error(`${path} must contain a JSON object`);
  }
  return parsed;
};

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);
