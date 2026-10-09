import { isAbsolute, normalize } from "node:path";

import { type WatcherConfig, type WatcherWalletKeySource } from "./config.js";

export const WATCHER_PROCESS_CONFIG_SCHEMA_VERSION =
  "midgard-watcher-production-process-config-v1" as const;

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
    const missing = expected.filter((key) => !actual.includes(key));
    const unknown = actual.filter((key) => !expected.includes(key));
    throw new Error(
      `${label} has unknown or missing fields: missing=${JSON.stringify(missing)} unknown=${JSON.stringify(unknown)}`,
    );
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
  operationsEndpoint: string;
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
  }>;
}>;

export const faultProofInfrastructure = (
  value: unknown,
): WatcherProcessConfig["faultProofInfrastructure"] => {
  const input = exactRecord(
    value,
    ["manifestPath", "blueprintPath", "deploymentInfoPath"],
    "watcher fault-proof infrastructure",
  );
  return Object.freeze({
    manifestPath: canonicalPath(input.manifestPath, "deployment manifest"),
    blueprintPath: canonicalPath(input.blueprintPath, "Aiken blueprint"),
    deploymentInfoPath: canonicalPath(
      input.deploymentInfoPath,
      "contract deployment information",
    ),
  });
};
