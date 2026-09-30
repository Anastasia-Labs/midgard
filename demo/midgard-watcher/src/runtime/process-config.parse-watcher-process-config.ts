import { readFile, realpath } from "node:fs/promises";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  parseWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import {
  parseWatcherConfig,
  parseWatcherStrictJsonValue,
  type WatcherWalletKeySource,
} from "./config.js";
import {
  assertDistinctSources,
  canonicalPath,
  exactRecord,
  faultProofInfrastructure,
  HEX_32,
  loopbackEndpoint,
  secretSource,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
  type WatcherProcessConfig,
} from "./process-config.historical-native-script-history.js";

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
