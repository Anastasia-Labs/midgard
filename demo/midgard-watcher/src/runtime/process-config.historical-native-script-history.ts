import { isAbsolute, normalize } from "node:path";

import { type WatcherConfig, type WatcherWalletKeySource } from "./config.js";

export const WATCHER_PROCESS_CONFIG_SCHEMA_VERSION =
  "midgard-watcher-production-process-config-v1" as const;

export const WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION =
  "midgard-watcher-trusted-head-authority-process-config-v1" as const;

const ENVIRONMENT_VARIABLE = /^[A-Z][A-Z0-9_]{0,127}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

const LOOPBACK_HOSTS = new Set(["127.0.0.1", "localhost", "::1", "[::1]"]);

export const exactRecord = (
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

export const canonicalPath = (value: unknown, label: string): string => {
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

export const loopbackEndpoint = (value: unknown, label: string): string => {
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

export const secretSource = (
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

export const assertDistinctSources = (
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
  l1NodeTransportBinaryPath: string;
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

export const faultProofInfrastructure = (
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
