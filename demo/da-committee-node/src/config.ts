import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { blake2b } from "@noble/hashes/blake2.js";

import {
  parseMidgardNodeDeploymentInfo,
  type MidgardNodeDeployment,
} from "./l1/deployment.js";
import type { DaCommitteeMember, DaPeerConfig } from "./domain.js";
import { bytesToHex, hexToBytes, normalizeHex } from "./utils/hex.js";

export type DaParamsConfig = {
  readonly committeeHex: string;
  readonly committeeSignersHash: string;
  readonly threshold: number;
};

export type LocalStateConfig =
  | { readonly kind: "file"; readonly path: string }
  | { readonly kind: "database"; readonly url: string };

export type WatcherConfig = {
  readonly network: string;
  readonly deploymentManifestPath: string;
  readonly contractDeploymentInfoPath: string;
  readonly deploymentFingerprint: string;
  readonly deploymentManifestSha256: string;
  readonly deploymentManifestRaw: string;
  readonly deploymentManifest: Record<string, unknown>;
  readonly contractDeploymentInfo: Record<string, unknown>;
  readonly midgardNodeDeployment?: MidgardNodeDeployment;
  readonly cardanoProviderUrls: readonly string[];
  readonly finalityDepth: number;
  readonly daPayloadEndpoints: readonly string[];
  readonly publicBaseUrl?: string;
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
  readonly peerEndpoints: readonly DaPeerConfig[];
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

export const loadWatcherConfig = async (
  env: Env = process.env,
): Promise<WatcherConfig> => {
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
  const contractDeploymentInfo = parseJsonObject(
    contractDeploymentInfoRaw,
    contractDeploymentInfoPath,
  );
  const deploymentManifestSha256 = createHash("sha256")
    .update(deploymentManifestRaw)
    .digest("hex");
  const deploymentFingerprint =
    optionalString(deploymentManifest, "deploymentFingerprint") ??
    optionalString(deploymentManifest, "fingerprint") ??
    deploymentManifestSha256;
  const network =
    env.MIDGARD_NETWORK ??
    optionalString(deploymentManifest, "network") ??
    optionalString(deploymentManifest, "networkId");
  if (network === undefined || network.trim() === "") {
    throw new Error("MIDGARD_NETWORK or manifest network is required");
  }
  const midgardNodeDeployment = parseMidgardNodeDeploymentInfo(
    contractDeploymentInfo,
    network,
  );

  const daAttestationPolicyId = requireDeploymentString(
    deploymentManifest,
    contractDeploymentInfo,
    [
      ["contracts", "daAttestation", "policyId"],
      ["daAttestation", "policyId"],
      ["da_attestation", "policy_id"],
    ],
    "DA attestation policy id",
    () => midgardNodeDeployment?.daAttestation.policyId,
  );
  const daAttestationAddress = requireDeploymentString(
    deploymentManifest,
    contractDeploymentInfo,
    [
      ["contracts", "daAttestation", "spendingScriptAddress"],
      ["contracts", "daAttestation", "address"],
      ["daAttestation", "spendingScriptAddress"],
      ["da_attestation", "address"],
    ],
    "DA attestation address",
    () => midgardNodeDeployment?.daAttestation.spendingScriptAddress,
  );
  const daParamsGovernorPolicyId = requireDeploymentString(
    deploymentManifest,
    contractDeploymentInfo,
    [
      ["contracts", "daParamsGovernor", "policyId"],
      ["daParamsGovernor", "policyId"],
      ["da_params_governor", "policy_id"],
    ],
    "DA params governor policy id",
    () => midgardNodeDeployment?.daParamsGovernor.policyId,
  );
  const daParamsGovernorAddress = requireDeploymentString(
    deploymentManifest,
    contractDeploymentInfo,
    [
      ["contracts", "daParamsGovernor", "spendingScriptAddress"],
      ["contracts", "daParamsGovernor", "address"],
      ["daParamsGovernor", "spendingScriptAddress"],
      ["da_params_governor", "address"],
    ],
    "DA params governor address",
    () => midgardNodeDeployment?.daParamsGovernor.spendingScriptAddress,
  );
  const stateQueuePolicyId = requireDeploymentString(
    deploymentManifest,
    contractDeploymentInfo,
    [
      ["contracts", "stateQueue", "policyId"],
      ["stateQueue", "policyId"],
      ["state_queue", "policy_id"],
    ],
    "state queue policy id",
    () => midgardNodeDeployment?.stateQueue.policyId,
  );
  const stateQueueAddress = requireDeploymentString(
    deploymentManifest,
    contractDeploymentInfo,
    [
      ["contracts", "stateQueue", "spendingScriptAddress"],
      ["contracts", "stateQueue", "address"],
      ["stateQueue", "spendingScriptAddress"],
      ["state_queue", "address"],
    ],
    "state queue address",
    () => midgardNodeDeployment?.stateQueue.spendingScriptAddress,
  );
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
    throw new Error("DA_L1_SUBMITTER_ID must be present in DA_L1_SUBMITTER_IDS");
  }
  const publicBaseUrl = normalizeOptionalBaseUrl(env.DA_PUBLIC_BASE_URL);
  const cardanoProviderUrls = splitList(requireEnv(env, "CARDANO_PROVIDER_URLS"));
  const daCommitteeMembers = committeeMembersFromManifest(
    objectAt(deploymentManifest, ["da"]),
  );
  const peerEndpoints = peerEndpointsConfig({
    env,
    manifestMembers: daCommitteeMembers,
    publicBaseUrl,
  });
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
    if (midgardNodeDeployment === undefined) {
      throw new Error(
        "L1 submission requires Midgard node deployment-info with script CBOR and reference-script UTxOs",
      );
    }
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
  const daParams = daParamsConfig(env, deploymentManifest);
  const l1SubmitterSignerIndexes = parseL1SubmitterSignerIndexes(
    env,
    daCommitteeMembers,
    daParams,
  );

  return {
    network,
    deploymentManifestPath,
    contractDeploymentInfoPath,
    deploymentFingerprint,
    deploymentManifestSha256,
    deploymentManifestRaw,
    deploymentManifest,
    contractDeploymentInfo,
    midgardNodeDeployment,
    cardanoProviderUrls,
    finalityDepth: nonNegativeInt(
      requireEnv(env, "CARDANO_FINALITY_DEPTH"),
      "CARDANO_FINALITY_DEPTH",
    ),
    daPayloadEndpoints: splitList(requireEnv(env, "DA_PAYLOAD_ENDPOINTS")),
    ...(publicBaseUrl === undefined ? {} : { publicBaseUrl }),
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
    peerEndpoints,
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

const isLiveLucidProviderUrl = (value: string): boolean =>
  value.startsWith("blockfrost:") || value.startsWith("kupmios:");

const booleanEnv = (value: string | undefined, defaultValue: boolean): boolean => {
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
      l1SubmissionEnabled &&
      booleanEnv(env.DA_L1_PREFLIGHT_ENABLED, true),
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
  if (configuredMode !== undefined && configuredMode !== "unified") {
    throw new Error(
      "DA_MODE has been removed; omit it or set DA_MODE=unified while migrating",
    );
  }
  const index = optionalNonEmpty(env.DA_SIGNER_INDEX);
  const keySource = optionalNonEmpty(env.DA_SIGNER_KEY_SOURCE);
  if (index === undefined && keySource === undefined) {
    return {};
  }
  if (index === undefined || keySource === undefined) {
    throw new Error("DA_SIGNER_INDEX and DA_SIGNER_KEY_SOURCE must be set together");
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

const daParamsConfig = (
  env: Env,
  deploymentManifest: Record<string, unknown>,
): DaParamsConfig => {
  const manifestDa = objectAt(deploymentManifest, ["da"]);
  const memberKeys =
    manifestDa === undefined ? [] : daMemberKeysFromManifest(manifestDa);
  const committeeHex = normalizeHex(
    env.DA_COMMITTEE_HEX ?? memberKeys.join(""),
    { fieldName: "DA committee" },
  );
  if (committeeHex.length === 0 || committeeHex.length % 64 !== 0) {
    throw new Error("DA committee must be packed 32-byte verification keys");
  }
  const computedCommitteeHash = bytesToHex(
    blake2b(hexToBytes(committeeHex, "DA committee"), { dkLen: 32 }),
  );
  const committeeSignersHash = normalizeHex(
    env.DA_COMMITTEE_SIGNERS_HASH ??
      stringAt(deploymentManifest, ["da", "committeeSignersHash"]) ??
      computedCommitteeHash,
    { fieldName: "DA committee signers hash", byteLength: 32 },
  );
  const threshold = positiveInt(
    env.DA_THRESHOLD ??
      numberAt(deploymentManifest, ["da", "threshold"])?.toString() ??
      "",
    "DA_THRESHOLD",
  );
  return { committeeHex, committeeSignersHash, threshold };
};

const committeeMembersFromManifest = (
  manifestDa: Record<string, unknown> | undefined,
): readonly DaCommitteeMember[] => {
  if (manifestDa === undefined || !Array.isArray(manifestDa.members)) {
    return [];
  }
  return manifestDa.members
    .map((member) => {
      if (!isRecord(member)) {
        throw new Error("manifest da.members entries must be objects");
      }
      const index = member.index;
      const vkey = member.vkey;
      if (!Number.isSafeInteger(index) || typeof vkey !== "string") {
        throw new Error("manifest da.members entries require index and vkey");
      }
      const memberIndex = index as number;
      if (memberIndex < 0 || memberIndex > 255) {
        throw new Error("manifest da.members index must fit in one byte");
      }
      const baseUrls =
        Array.isArray(member.baseUrls) || Array.isArray(member.base_urls)
          ? [...((member.baseUrls ?? member.base_urls) as unknown[])]
          : typeof member.baseUrl === "string"
            ? [member.baseUrl]
            : [];
      return {
        index: memberIndex,
        vkey: normalizeHex(vkey, {
          fieldName: `DA member ${memberIndex.toString()} vkey`,
          byteLength: 32,
        }),
        baseUrls: baseUrls.map((entry) => {
          if (typeof entry !== "string") {
            throw new Error("manifest da.members baseUrls entries must be strings");
          }
          return normalizeBaseUrl(entry);
        }),
        canSubmitL1:
          typeof member.canSubmitL1 === "boolean" ? member.canSubmitL1 : true,
      };
    })
    .sort((left, right) => left.index - right.index);
};

const daMemberKeysFromManifest = (
  manifestDa: Record<string, unknown>,
): readonly string[] => {
  return committeeMembersFromManifest(manifestDa).map((member) => member.vkey);
};

const parseL1SubmitterSignerIndexes = (
  env: Env,
  members: readonly DaCommitteeMember[],
  daParams: DaParamsConfig,
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
  return Array.from(
    { length: hexToBytes(daParams.committeeHex, "DA committee").length / 32 },
    (_value, index) => index,
  );
};

const peerEndpointsConfig = ({
  env,
  manifestMembers,
  publicBaseUrl,
}: {
  readonly env: Env;
  readonly manifestMembers: readonly DaCommitteeMember[];
  readonly publicBaseUrl?: string;
}): readonly DaPeerConfig[] => {
  const configured = [
    ...optionalSplitList(env.DA_PEER_ENDPOINTS),
    ...optionalSplitList(env.DA_COORDINATOR_ENDPOINT),
  ].map(parsePeerEndpoint);
  const fromManifest = manifestMembers.flatMap((member) =>
    member.baseUrls.map((baseUrl) => ({
      signerIndex: member.index,
      baseUrl,
    })),
  );
  const ownBaseUrl = publicBaseUrl?.toLowerCase();
  const deduped = new Map<string, DaPeerConfig>();
  for (const peer of [...fromManifest, ...configured]) {
    if (ownBaseUrl !== undefined && peer.baseUrl.toLowerCase() === ownBaseUrl) {
      continue;
    }
    deduped.set(
      `${peer.signerIndex?.toString() ?? "unknown"}@${peer.baseUrl}`,
      peer,
    );
  }
  return [...deduped.values()].sort(
    (left, right) =>
      (left.signerIndex ?? Number.MAX_SAFE_INTEGER) -
        (right.signerIndex ?? Number.MAX_SAFE_INTEGER) ||
      left.baseUrl.localeCompare(right.baseUrl),
  );
};

const parsePeerEndpoint = (value: string): DaPeerConfig => {
  const atIndex = value.indexOf("@");
  if (atIndex > 0 && /^[0-9]+$/.test(value.slice(0, atIndex))) {
    return {
      signerIndex: signerIndex(value.slice(0, atIndex)),
      baseUrl: normalizeBaseUrl(value.slice(atIndex + 1)),
    };
  }
  return { baseUrl: normalizeBaseUrl(value) };
};

const normalizeOptionalBaseUrl = (
  value: string | undefined,
): string | undefined => {
  const trimmed = optionalNonEmpty(value);
  return trimmed === undefined ? undefined : normalizeBaseUrl(trimmed);
};

const normalizeBaseUrl = (value: string): string => {
  const parsed = new URL(value.trim());
  if (parsed.protocol !== "http:" && parsed.protocol !== "https:") {
    throw new Error("DA peer base URLs must use http or https");
  }
  parsed.hash = "";
  parsed.search = "";
  return parsed.toString().replace(/\/$/, "");
};

const parseJsonObject = (raw: string, path: string): Record<string, unknown> => {
  const parsed = JSON.parse(raw) as unknown;
  if (!isRecord(parsed)) {
    throw new Error(`${path} must contain a JSON object`);
  }
  return parsed;
};

const requireDeploymentString = (
  manifest: Record<string, unknown>,
  deploymentInfo: Record<string, unknown>,
  paths: readonly (readonly string[])[],
  label: string,
  fallback?: () => string | undefined,
): string => {
  for (const path of paths) {
    const value = stringAt(manifest, path) ?? stringAt(deploymentInfo, path);
    if (value !== undefined && value.trim() !== "") {
      return value;
    }
  }
  const fallbackValue = fallback?.();
  if (fallbackValue !== undefined && fallbackValue.trim() !== "") {
    return fallbackValue;
  }
  throw new Error(`missing ${label} in deployment manifest/deployment info`);
};

const optionalString = (
  object: Record<string, unknown>,
  key: string,
): string | undefined => {
  const value = object[key];
  return typeof value === "string" ? value : undefined;
};

const stringAt = (
  root: Record<string, unknown>,
  path: readonly string[],
): string | undefined => {
  const value = valueAt(root, path);
  return typeof value === "string" ? value : undefined;
};

const numberAt = (
  root: Record<string, unknown>,
  path: readonly string[],
): number | undefined => {
  const value = valueAt(root, path);
  return typeof value === "number" ? value : undefined;
};

const objectAt = (
  root: Record<string, unknown>,
  path: readonly string[],
): Record<string, unknown> | undefined => {
  const value = valueAt(root, path);
  return isRecord(value) ? value : undefined;
};

const valueAt = (
  root: Record<string, unknown>,
  path: readonly string[],
): unknown =>
  path.reduce<unknown>(
    (current, key) => (isRecord(current) ? current[key] : undefined),
    root,
  );

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);
