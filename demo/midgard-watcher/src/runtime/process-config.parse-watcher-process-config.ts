import { readFile, realpath, stat } from "node:fs/promises";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  parseWatcherConfig,
  parseWatcherStrictJsonValue,
  type WatcherWalletKeySource,
} from "./config.js";
import { WatcherPermanentRefusalError } from "./permanent-refusal.js";
import {
  assertDistinctSources,
  canonicalPath,
  exactRecord,
  faultProofInfrastructure,
  HEX_32,
  loopbackEndpoint,
  secretSource,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
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
      "l1NodeTransportBinaryPath",
      "operationsEndpoint",
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
      watcherConfig.targetNetwork !== "Custom")
  ) {
    throw new Error(
      "watcher production process requires acceptance Preprod or Custom",
    );
  }
  // Parsed before the signed deployment is loaded, so this binds to the
  // compiled profile; startup later re-checks it against the verified release.
  const releaseDepth = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;
  if (watcherConfig.l1.finality.depth !== releaseDepth) {
    throw new Error(
      `watcher production process requires finality depth ${releaseDepth.toString()} from the deployment profile`,
    );
  }
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
    availability.keySource,
  ]);
  const operationsEndpoint = loopbackEndpoint(
    input.operationsEndpoint,
    "watcher operations endpoint",
  );
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
    l1NodeTransportBinaryPath: canonicalPath(
      input.l1NodeTransportBinaryPath,
      "native chain-sync binary",
    ),
    operationsEndpoint,
    workflowJournalDirectory: canonicalPath(
      input.workflowJournalDirectory,
      "workflow journal directory",
    ),
    faultProofInfrastructure: infrastructure,
    availability,
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

/**
 * The node socket, when the path exists, must be a socket. A missing or
 * unreadable path is left to the transport, which holds the watcher unready
 * by name until the node creates it; a file or directory there is a
 * configuration refusal no restart clears.
 */
const assertWatcherL1SocketPath = async (socketPath: string): Promise<void> => {
  let entry: Awaited<ReturnType<typeof stat>>;
  try {
    entry = await stat(socketPath);
  } catch {
    return;
  }
  if (entry.isSocket()) return;
  throw new WatcherPermanentRefusalError(
    "l1_socket",
    new Error(
      `$.l1.source.chainSync.socketPath ${socketPath} is ${
        entry.isDirectory()
          ? "a directory"
          : entry.isFile()
            ? "a regular file"
            : "a special file"
      }, not a socket`,
    ),
  );
};

export const loadWatcherProcessConfigFile = async (
  path: string,
): Promise<WatcherProcessConfig> => {
  const config = parseWatcherProcessConfig(await configFile(path));
  await assertWatcherL1SocketPath(
    config.watcherConfig.l1.source.chainSync.socketPath,
  );
  return config;
};
