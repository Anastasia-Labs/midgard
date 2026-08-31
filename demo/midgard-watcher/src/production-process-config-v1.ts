import { readFile, realpath } from "node:fs/promises";
import { isAbsolute, normalize } from "node:path";

import {
  parseWatcherConfig,
  parseWatcherStrictJsonValueV1,
  type WatcherConfig,
  type WatcherWalletKeySource,
} from "./config.js";
import {
  parseWatcherFinalityPolicyV1,
  type WatcherFinalityPolicyV1,
} from "./finality-engine.js";

export const WATCHER_PRODUCTION_PROCESS_CONFIG_V1_SCHEMA_VERSION =
  "midgard-watcher-production-process-config-v1" as const;
export const WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_V1_SCHEMA_VERSION =
  "midgard-watcher-trusted-head-authority-process-config-v1" as const;

const ENVIRONMENT_VARIABLE = /^[A-Z][A-Z0-9_]{0,127}$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const LOOPBACK_HOSTS = new Set(["127.0.0.1", "localhost", "::1", "[::1]"]);

const exactRecord = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} is not an exact plain object`);
  }
  const record = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
  return record;
};

const canonicalPath = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    !isAbsolute(value) ||
    normalize(value) !== value ||
    value === "/" ||
    value === "/tmp" ||
    value.startsWith("/tmp/")
  ) {
    throw new Error(`${label} is not a canonical production path`);
  }
  return value;
};

const loopbackEndpoint = (value: unknown, label: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${label} is invalid`);
  }
  let endpoint: URL;
  try {
    endpoint = new URL(value);
  } catch {
    throw new Error(`${label} is invalid`);
  }
  if (
    endpoint.protocol !== "http:" ||
    !LOOPBACK_HOSTS.has(endpoint.hostname.toLowerCase()) ||
    endpoint.port.length === 0 ||
    endpoint.port === "0" ||
    endpoint.username.length !== 0 ||
    endpoint.password.length !== 0 ||
    endpoint.search.length !== 0 ||
    endpoint.hash.length !== 0 ||
    !["", "/"].includes(endpoint.pathname)
  ) {
    throw new Error(`${label} must be fixed loopback HTTP`);
  }
  return endpoint.toString().replace(/\/$/u, "");
};

const secretSource = (
  value: unknown,
  label: string,
): WatcherWalletKeySource => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} is invalid`);
  }
  const kind = (value as { kind?: unknown }).kind;
  if (kind === "environment") {
    const source = exactRecord(value, ["kind", "variable"], label);
    if (
      typeof source.variable !== "string" ||
      !ENVIRONMENT_VARIABLE.test(source.variable)
    ) {
      throw new Error(`${label} environment variable is invalid`);
    }
    return Object.freeze({ kind, variable: source.variable });
  }
  if (kind === "file") {
    const source = exactRecord(value, ["kind", "path"], label);
    return Object.freeze({
      kind,
      path: canonicalPath(source.path, `${label} file`),
    });
  }
  throw new Error(`${label} kind is invalid`);
};

export const watcherSecretSourceIdentityV1 = (
  source: WatcherWalletKeySource,
): string =>
  source.kind === "environment"
    ? `environment:${source.variable}`
    : `file:${source.path}`;

const assertDistinctSources = (
  sources: readonly WatcherWalletKeySource[],
): void => {
  const identities = sources.map(watcherSecretSourceIdentityV1);
  if (new Set(identities).size !== identities.length) {
    throw new Error("production secret sources must be pairwise distinct");
  }
};

export type WatcherProductionProcessConfigV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_PROCESS_CONFIG_V1_SCHEMA_VERSION;
  watcherConfig: WatcherConfig;
  watcherRuntimeConfigPath: string;
  deploymentAuthorityPath: string;
  fundingProfileBundlePath: string;
  nativeChainSyncBinaryPath: string;
  trustedHeadAuthorityEndpoint: string;
  operationsEndpoint: string;
  httpBearerSecretSource: WatcherWalletKeySource;
  workflowJournalDirectory: string;
  readinessHeaderHash: string;
  faultProofInfrastructure: Readonly<{
    manifestPath: string;
    blueprintPath: string;
    deploymentInfoPath: string;
    midgardNodeUrl: string;
    midgardNodeAdminKeySource: WatcherWalletKeySource;
    historicalNativeScriptHistory: Readonly<{
      sourceMode: "external_provider_quorum";
      consistencyPolicy: "exact_bytes_all_providers_v1";
      providers: readonly Readonly<{
        sourceId: string;
        operatorIdentitySha256: string;
        authorityEndpoint: string;
      }>[];
    }>;
    stateQueueLeaseTtlMs?: number;
  }>;
}>;

const externalHistoryEndpoint = (value: unknown): string => {
  if (typeof value !== "string") {
    throw new Error("historical native-script provider endpoint is invalid");
  }
  let endpoint: URL;
  try {
    endpoint = new URL(value);
  } catch {
    throw new Error("historical native-script provider endpoint is invalid");
  }
  endpoint.pathname = endpoint.pathname.replace(/\/+$/u, "") || "/";
  if (
    endpoint.protocol !== "https:" ||
    endpoint.username.length !== 0 ||
    endpoint.password.length !== 0 ||
    endpoint.search.length !== 0 ||
    endpoint.hash.length !== 0 ||
    LOOPBACK_HOSTS.has(endpoint.hostname.toLowerCase())
  ) {
    throw new Error(
      "historical native-script provider endpoint must be fixed external HTTPS",
    );
  }
  return endpoint.toString().replace(/\/$/u, "");
};

const historicalNativeScriptHistory = (
  value: unknown,
): WatcherProductionProcessConfigV1["faultProofInfrastructure"]["historicalNativeScriptHistory"] => {
  const input = exactRecord(
    value,
    ["sourceMode", "consistencyPolicy", "providers"],
    "historical native-script history overlay",
  );
  if (
    input.sourceMode !== "external_provider_quorum" ||
    input.consistencyPolicy !== "exact_bytes_all_providers_v1" ||
    !Array.isArray(input.providers) ||
    input.providers.length < 2 ||
    input.providers.length > 4
  ) {
    throw new Error("historical native-script history overlay is invalid");
  }
  const sourceIds = new Set<string>();
  const operators = new Set<string>();
  const endpoints = new Set<string>();
  const providers = input.providers.map((value, index) => {
    const provider = exactRecord(
      value,
      ["sourceId", "operatorIdentitySha256", "authorityEndpoint"],
      `historical native-script provider ${index.toString()}`,
    );
    const endpoint = externalHistoryEndpoint(provider.authorityEndpoint);
    if (
      typeof provider.sourceId !== "string" ||
      provider.sourceId.length === 0 ||
      provider.sourceId.trim() !== provider.sourceId ||
      typeof provider.operatorIdentitySha256 !== "string" ||
      !HEX_32.test(provider.operatorIdentitySha256) ||
      sourceIds.has(provider.sourceId) ||
      operators.has(provider.operatorIdentitySha256) ||
      endpoints.has(endpoint)
    ) {
      throw new Error(
        "historical native-script provider identities are invalid or not independent",
      );
    }
    sourceIds.add(provider.sourceId);
    operators.add(provider.operatorIdentitySha256);
    endpoints.add(endpoint);
    return Object.freeze({
      sourceId: provider.sourceId,
      operatorIdentitySha256: provider.operatorIdentitySha256,
      authorityEndpoint: endpoint,
    });
  });
  return Object.freeze({
    sourceMode: "external_provider_quorum",
    consistencyPolicy: "exact_bytes_all_providers_v1",
    providers: Object.freeze(providers),
  });
};

const loopbackServiceUrl = (value: unknown, label: string): string => {
  if (typeof value !== "string") throw new Error(`${label} is invalid`);
  let url: URL;
  try {
    url = new URL(value);
  } catch {
    throw new Error(`${label} is invalid`);
  }
  if (
    !["http:", "https:"].includes(url.protocol) ||
    !LOOPBACK_HOSTS.has(url.hostname.toLowerCase()) ||
    url.username.length !== 0 ||
    url.password.length !== 0 ||
    url.search.length !== 0 ||
    url.hash.length !== 0
  ) {
    throw new Error(`${label} must be fixed loopback HTTP`);
  }
  return url.toString().replace(/\/$/u, "");
};

const faultProofInfrastructure = (
  value: unknown,
): WatcherProductionProcessConfigV1["faultProofInfrastructure"] => {
  const candidate = value as { readonly stateQueueLeaseTtlMs?: unknown };
  const input = exactRecord(
    value,
    [
      "manifestPath",
      "blueprintPath",
      "deploymentInfoPath",
      "midgardNodeUrl",
      "midgardNodeAdminKeySource",
      "historicalNativeScriptHistory",
      ...(candidate?.stateQueueLeaseTtlMs === undefined
        ? []
        : ["stateQueueLeaseTtlMs"]),
    ],
    "watcher fault-proof infrastructure",
  );
  if (
    input.stateQueueLeaseTtlMs !== undefined &&
    (!Number.isSafeInteger(input.stateQueueLeaseTtlMs) ||
      (input.stateQueueLeaseTtlMs as number) <= 0)
  ) {
    throw new Error("watcher state-queue lease TTL is invalid");
  }
  return Object.freeze({
    manifestPath: canonicalPath(input.manifestPath, "deployment manifest"),
    blueprintPath: canonicalPath(input.blueprintPath, "Aiken blueprint"),
    deploymentInfoPath: canonicalPath(
      input.deploymentInfoPath,
      "contract deployment information",
    ),
    midgardNodeUrl: loopbackServiceUrl(
      input.midgardNodeUrl,
      "Midgard node endpoint",
    ),
    midgardNodeAdminKeySource: secretSource(
      input.midgardNodeAdminKeySource,
      "Midgard node admin key source",
    ),
    historicalNativeScriptHistory: historicalNativeScriptHistory(
      input.historicalNativeScriptHistory,
    ),
    ...(input.stateQueueLeaseTtlMs === undefined
      ? {}
      : { stateQueueLeaseTtlMs: input.stateQueueLeaseTtlMs as number }),
  });
};

export const parseWatcherProductionProcessConfigV1 = (
  value: unknown,
): WatcherProductionProcessConfigV1 => {
  const input = exactRecord(
    value,
    [
      "schemaVersion",
      "watcherConfig",
      "watcherRuntimeConfigPath",
      "deploymentAuthorityPath",
      "fundingProfileBundlePath",
      "nativeChainSyncBinaryPath",
      "trustedHeadAuthorityEndpoint",
      "operationsEndpoint",
      "httpBearerSecretSource",
      "workflowJournalDirectory",
      "readinessHeaderHash",
      "faultProofInfrastructure",
    ],
    "watcher production process config",
  );
  if (
    input.schemaVersion !== WATCHER_PRODUCTION_PROCESS_CONFIG_V1_SCHEMA_VERSION
  ) {
    throw new Error("watcher production process config schema changed");
  }
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  if (
    watcherConfig.mode !== "acceptance" ||
    watcherConfig.targetNetwork !== "Preprod" ||
    watcherConfig.l1.source.sourceMode !== "local_node" ||
    watcherConfig.l1.finality.depth !== 30 ||
    watcherConfig.l1.finality.rollback.maxDepth !== 30 ||
    watcherConfig.l1.finality.rollback.postFinalityRecoveryMaxDepth !== 2160
  ) {
    throw new Error(
      "watcher production process requires acceptance Preprod local_node authority",
    );
  }
  const httpBearerSecretSource = secretSource(
    input.httpBearerSecretSource,
    "watcher HTTP bearer secret source",
  );
  const infrastructure = faultProofInfrastructure(
    input.faultProofInfrastructure,
  );
  assertDistinctSources([
    watcherConfig.storage.rollbackAuthorityKeySource,
    watcherConfig.proverWallet.keySource,
    httpBearerSecretSource,
    infrastructure.midgardNodeAdminKeySource,
  ]);
  if (
    typeof input.readinessHeaderHash !== "string" ||
    !HEX_28.test(input.readinessHeaderHash)
  ) {
    throw new Error("watcher fault-proof readiness header hash is invalid");
  }
  const trustedHeadAuthorityEndpoint = loopbackEndpoint(
    input.trustedHeadAuthorityEndpoint,
    "trusted-head endpoint",
  );
  const operationsEndpoint = loopbackEndpoint(
    input.operationsEndpoint,
    "watcher operations endpoint",
  );
  if (operationsEndpoint === trustedHeadAuthorityEndpoint) {
    throw new Error(
      "watcher operations and trusted-head endpoints must be distinct",
    );
  }
  return Object.freeze({
    schemaVersion: WATCHER_PRODUCTION_PROCESS_CONFIG_V1_SCHEMA_VERSION,
    watcherConfig,
    watcherRuntimeConfigPath: canonicalPath(
      input.watcherRuntimeConfigPath,
      "watcher workflow runtime config",
    ),
    deploymentAuthorityPath: canonicalPath(
      input.deploymentAuthorityPath,
      "watcher deployment authority",
    ),
    fundingProfileBundlePath: canonicalPath(
      input.fundingProfileBundlePath,
      "watcher funding profile bundle",
    ),
    nativeChainSyncBinaryPath: canonicalPath(
      input.nativeChainSyncBinaryPath,
      "native chain-sync binary",
    ),
    trustedHeadAuthorityEndpoint,
    operationsEndpoint,
    httpBearerSecretSource,
    workflowJournalDirectory: canonicalPath(
      input.workflowJournalDirectory,
      "workflow journal directory",
    ),
    readinessHeaderHash: input.readinessHeaderHash,
    faultProofInfrastructure: infrastructure,
  });
};

export type WatcherTrustedHeadAuthorityProcessConfigV1 = Readonly<{
  schemaVersion: typeof WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_V1_SCHEMA_VERSION;
  policy: WatcherFinalityPolicyV1;
  directory: string;
  endpoint: string;
  recordAuthenticationKeySource: WatcherWalletKeySource;
  httpBearerSecretSource: WatcherWalletKeySource;
}>;

export const parseWatcherTrustedHeadAuthorityProcessConfigV1 = (
  value: unknown,
): WatcherTrustedHeadAuthorityProcessConfigV1 => {
  const input = exactRecord(
    value,
    [
      "schemaVersion",
      "policy",
      "directory",
      "endpoint",
      "recordAuthenticationKeySource",
      "httpBearerSecretSource",
    ],
    "trusted-head authority process config",
  );
  if (
    input.schemaVersion !==
    WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_V1_SCHEMA_VERSION
  ) {
    throw new Error("trusted-head authority process config schema changed");
  }
  const policy = parseWatcherFinalityPolicyV1(input.policy);
  if (policy === null)
    throw new Error("trusted-head authority policy is invalid");
  if (
    policy.network !== "Preprod" ||
    policy.sourceMode !== "local_node" ||
    policy.authorityNodeId === null ||
    policy.authorityGenesisIdentitySha256 === null ||
    policy.authorityChainSyncSocketPath === null ||
    policy.confirmationDepth !== "30" ||
    policy.maximumPreFinalityRollbackDepth !== "30" ||
    policy.maximumPostFinalityRecoveryDepth !== "2160"
  ) {
    throw new Error(
      "trusted-head authority policy requires Preprod local_node authority",
    );
  }
  const recordAuthenticationKeySource = secretSource(
    input.recordAuthenticationKeySource,
    "sidecar record authentication key source",
  );
  const httpBearerSecretSource = secretSource(
    input.httpBearerSecretSource,
    "sidecar HTTP bearer secret source",
  );
  assertDistinctSources([
    recordAuthenticationKeySource,
    httpBearerSecretSource,
  ]);
  return Object.freeze({
    schemaVersion:
      WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_V1_SCHEMA_VERSION,
    policy,
    directory: canonicalPath(input.directory, "trusted-head durable directory"),
    endpoint: loopbackEndpoint(input.endpoint, "trusted-head endpoint"),
    recordAuthenticationKeySource,
    httpBearerSecretSource,
  });
};

export const loadWatcherSecretTextV1 = async (
  source: WatcherWalletKeySource,
  unsafeEnvironmentForTest?: Readonly<Record<string, string | undefined>>,
): Promise<string> => {
  let value: string;
  if (source.kind === "environment") {
    const candidate = (unsafeEnvironmentForTest ?? process.env)[
      source.variable
    ];
    if (candidate === undefined) {
      throw new Error("production secret environment source is absent");
    }
    value = candidate;
  } else {
    if ((await realpath(source.path)) !== source.path) {
      throw new Error("production secret file traverses a symlink");
    }
    const bytes = await readFile(source.path);
    if (bytes.byteLength === 0 || bytes.byteLength > 4_096) {
      throw new Error("production secret file size is invalid");
    }
    value = new TextDecoder("utf-8", { fatal: true }).decode(bytes);
  }
  if (value !== value.trim() || value.length < 32 || value.length > 4_096) {
    throw new Error("production secret text is non-canonical or out of bounds");
  }
  return value;
};

export const decodeWatcherAuthenticationKey32V1 = (
  value: string,
): Uint8Array => {
  if (!HEX_32.test(value)) {
    throw new Error(
      "production authentication key must be 32-byte lowercase hex",
    );
  }
  return Uint8Array.from(Buffer.from(value, "hex"));
};

export const decodeWatcherHttpBearerSecretV1 = (value: string): string => {
  if (value.length < 32 || value.length > 256) {
    throw new Error("production HTTP bearer secret length is invalid");
  }
  return value;
};

const productionConfigFile = async (path: string): Promise<unknown> => {
  const admitted = canonicalPath(path, "production process config");
  if ((await realpath(admitted)) !== admitted) {
    throw new Error("production process config path traverses a symlink");
  }
  const bytes = await readFile(admitted);
  if (bytes.byteLength === 0 || bytes.byteLength > 16 * 1024 * 1024) {
    throw new Error("production process config file size is invalid");
  }
  return parseWatcherStrictJsonValueV1(
    new TextDecoder("utf-8", { fatal: true }).decode(bytes),
  );
};

export const loadWatcherProductionProcessConfigFileV1 = async (
  path: string,
): Promise<WatcherProductionProcessConfigV1> =>
  parseWatcherProductionProcessConfigV1(await productionConfigFile(path));

export const loadWatcherTrustedHeadAuthorityProcessConfigFileV1 = async (
  path: string,
): Promise<WatcherTrustedHeadAuthorityProcessConfigV1> =>
  parseWatcherTrustedHeadAuthorityProcessConfigV1(
    await productionConfigFile(path),
  );
