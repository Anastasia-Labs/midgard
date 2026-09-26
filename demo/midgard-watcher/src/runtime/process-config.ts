import { readFile, realpath } from "node:fs/promises";
import { isAbsolute, normalize } from "node:path";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  parseWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import {
  parseWatcherConfig,
  parseWatcherStrictJsonValue,
  type WatcherConfig,
  type WatcherWalletKeySource,
} from "./config.js";

export const WATCHER_PROCESS_CONFIG_SCHEMA_VERSION =
  "midgard-watcher-production-process-config-v1" as const;
export const WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION =
  "midgard-watcher-trusted-head-authority-process-config-v1" as const;

const ENVIRONMENT_VARIABLE = /^[A-Z][A-Z0-9_]{0,127}$/u;
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
    (Object.getPrototypeOf(value) !== Object.prototype &&
      Object.getPrototypeOf(value) !== null) ||
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

export const watcherSecretSourceIdentity = (
  source: WatcherWalletKeySource,
): string =>
  source.kind === "environment"
    ? `environment:${source.variable}`
    : `file:${source.path}`;

const assertDistinctSources = (
  sources: readonly WatcherWalletKeySource[],
): void => {
  const identities = sources.map(watcherSecretSourceIdentity);
  if (new Set(identities).size !== identities.length) {
    throw new Error("production secret sources must be pairwise distinct");
  }
};

export type WatcherProcessConfig = Readonly<{
  schemaVersion: typeof WATCHER_PROCESS_CONFIG_SCHEMA_VERSION;
  watcherConfig: WatcherConfig;
  watcherRuntimeConfigPath: string;
  deploymentAuthorityPath: string;
  ruleBundlePath: string;
  fundingProfileBundlePath: string;
  nativeChainSyncBinaryPath: string;
  trustedHeadAuthorityEndpoint: string;
  operationsEndpoint: string;
  httpBearerSecretSource: WatcherWalletKeySource;
  workflowJournalDirectory: string;
  availability: Readonly<{
    keySource: WatcherWalletKeySource;
    journalPath: string;
    minimumFundingLovelace: string;
  }>;
  faultProofInfrastructure: Readonly<{
    manifestPath: string;
    blueprintPath: string;
    deploymentInfoPath: string;
    historicalNativeScriptHistory: Readonly<{
      sourceMode: "external_provider_quorum";
      consistencyPolicy: "exact_bytes_all_providers_v1";
      providers: readonly Readonly<{
        sourceId: string;
        operatorIdentitySha256: string;
        authorityEndpoint: string;
      }>[];
    }>;
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
): WatcherProcessConfig["faultProofInfrastructure"]["historicalNativeScriptHistory"] => {
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

const faultProofInfrastructure = (
  value: unknown,
): WatcherProcessConfig["faultProofInfrastructure"] => {
  const input = exactRecord(
    value,
    [
      "manifestPath",
      "blueprintPath",
      "deploymentInfoPath",
      "historicalNativeScriptHistory",
    ],
    "watcher fault-proof infrastructure",
  );
  return Object.freeze({
    manifestPath: canonicalPath(input.manifestPath, "deployment manifest"),
    blueprintPath: canonicalPath(input.blueprintPath, "Aiken blueprint"),
    deploymentInfoPath: canonicalPath(
      input.deploymentInfoPath,
      "contract deployment information",
    ),
    historicalNativeScriptHistory: historicalNativeScriptHistory(
      input.historicalNativeScriptHistory,
    ),
  });
};

export const parseWatcherProcessConfig = (
  value: unknown,
): WatcherProcessConfig => {
  const input = exactRecord(
    value,
    [
      "schemaVersion",
      "watcherConfig",
      "watcherRuntimeConfigPath",
      "deploymentAuthorityPath",
      "ruleBundlePath",
      "fundingProfileBundlePath",
      "nativeChainSyncBinaryPath",
      "trustedHeadAuthorityEndpoint",
      "operationsEndpoint",
      "httpBearerSecretSource",
      "workflowJournalDirectory",
      "availability",
      "faultProofInfrastructure",
    ],
    "watcher production process config",
  );
  if (input.schemaVersion !== WATCHER_PROCESS_CONFIG_SCHEMA_VERSION) {
    throw new Error("watcher production process config schema changed");
  }
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  if (
    watcherConfig.mode !== "acceptance" ||
    (watcherConfig.targetNetwork !== "Preprod" &&
      watcherConfig.targetNetwork !== "Custom") ||
    watcherConfig.l1.source.sourceMode !== "local_node"
  ) {
    throw new Error(
      "watcher production process requires acceptance Preprod or Custom local_node authority",
    );
  }
  // Parsed before the signed deployment is loaded, so this binds to the
  // compiled profile; startup later re-checks it against the verified release.
  const releaseDepth = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;
  if (
    watcherConfig.l1.finality.depth !== releaseDepth ||
    watcherConfig.l1.finality.rollback.maxDepth !== releaseDepth ||
    watcherConfig.l1.finality.rollback.postFinalityRecoveryMaxDepth !== 2160
  ) {
    throw new Error(
      `watcher production process requires finality depth and pre-finality rollback depth ${releaseDepth.toString()} from the deployment profile, with post-finality recovery depth 2160`,
    );
  }
  const httpBearerSecretSource = secretSource(
    input.httpBearerSecretSource,
    "watcher HTTP bearer secret source",
  );
  const infrastructure = faultProofInfrastructure(
    input.faultProofInfrastructure,
  );
  const availabilityInput = exactRecord(
    input.availability,
    ["keySource", "journalPath", "minimumFundingLovelace"],
    "watcher availability actor",
  );
  if (
    typeof availabilityInput.minimumFundingLovelace !== "string" ||
    !/^[1-9][0-9]*$/u.test(availabilityInput.minimumFundingLovelace)
  ) {
    throw new Error(
      "watcher availability minimum funding must be positive lovelace",
    );
  }
  const availability = Object.freeze({
    keySource: secretSource(
      availabilityInput.keySource,
      "watcher availability wallet",
    ),
    journalPath: canonicalPath(
      availabilityInput.journalPath,
      "watcher availability journal",
    ),
    minimumFundingLovelace: availabilityInput.minimumFundingLovelace,
  });
  assertDistinctSources([
    watcherConfig.storage.rollbackAuthorityKeySource,
    watcherConfig.proverWallet.keySource,
    httpBearerSecretSource,
    availability.keySource,
  ]);
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
    schemaVersion: WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
    watcherConfig,
    watcherRuntimeConfigPath: canonicalPath(
      input.watcherRuntimeConfigPath,
      "watcher workflow runtime config",
    ),
    deploymentAuthorityPath: canonicalPath(
      input.deploymentAuthorityPath,
      "watcher deployment authority",
    ),
    ruleBundlePath: canonicalPath(
      input.ruleBundlePath,
      "watcher release rule bundle",
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
    faultProofInfrastructure: infrastructure,
    availability,
  });
};

export type WatcherTrustedHeadAuthorityProcessConfig = Readonly<{
  schemaVersion: typeof WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION;
  policy: WatcherFinalityPolicy;
  directory: string;
  endpoint: string;
  recordAuthenticationKeySource: WatcherWalletKeySource;
  httpBearerSecretSource: WatcherWalletKeySource;
}>;

export const parseWatcherTrustedHeadAuthorityProcessConfig = (
  value: unknown,
): WatcherTrustedHeadAuthorityProcessConfig => {
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
    WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION
  ) {
    throw new Error("trusted-head authority process config schema changed");
  }
  const policy = parseWatcherFinalityPolicy(input.policy);
  if (policy === null)
    throw new Error("trusted-head authority policy is invalid");
  if (
    (policy.network !== "Preprod" && policy.network !== "Custom") ||
    policy.sourceMode !== "local_node" ||
    policy.authorityNodeId === null ||
    policy.authorityGenesisIdentitySha256 === null ||
    policy.authorityChainSyncSocketPath === null
  ) {
    throw new Error(
      "trusted-head authority policy requires Preprod or Custom local_node authority",
    );
  }
  const releaseDepth = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;
  if (
    policy.confirmationDepth !== releaseDepth.toString() ||
    policy.maximumPreFinalityRollbackDepth !== releaseDepth.toString() ||
    policy.maximumPostFinalityRecoveryDepth !== "2160"
  ) {
    throw new Error(
      `trusted-head authority policy requires confirmation and pre-finality rollback depth ${releaseDepth.toString()} from the deployment profile, with post-finality recovery depth 2160`,
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
    schemaVersion: WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
    policy,
    directory: canonicalPath(input.directory, "trusted-head durable directory"),
    endpoint: loopbackEndpoint(input.endpoint, "trusted-head endpoint"),
    recordAuthenticationKeySource,
    httpBearerSecretSource,
  });
};

export const loadWatcherSecretText = async (
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

export const decodeWatcherAuthenticationKey32 = (value: string): Uint8Array => {
  if (!HEX_32.test(value)) {
    throw new Error(
      "production authentication key must be 32-byte lowercase hex",
    );
  }
  return Uint8Array.from(Buffer.from(value, "hex"));
};

export const decodeWatcherHttpBearerSecret = (value: string): string => {
  if (value.length < 32 || value.length > 256) {
    throw new Error("production HTTP bearer secret length is invalid");
  }
  return value;
};

const configFile = async (path: string): Promise<unknown> => {
  const admitted = canonicalPath(path, "production process config");
  if ((await realpath(admitted)) !== admitted) {
    throw new Error("production process config path traverses a symlink");
  }
  const bytes = await readFile(admitted);
  if (bytes.byteLength === 0 || bytes.byteLength > 16 * 1024 * 1024) {
    throw new Error("production process config file size is invalid");
  }
  return parseWatcherStrictJsonValue(
    new TextDecoder("utf-8", { fatal: true }).decode(bytes),
  );
};

export const loadWatcherProcessConfigFile = async (
  path: string,
): Promise<WatcherProcessConfig> =>
  parseWatcherProcessConfig(await configFile(path));

export const loadWatcherTrustedHeadAuthorityProcessConfigFile = async (
  path: string,
): Promise<WatcherTrustedHeadAuthorityProcessConfig> =>
  parseWatcherTrustedHeadAuthorityProcessConfig(await configFile(path));
