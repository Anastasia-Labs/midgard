import {
  parseKeySource,
  parseL1Source,
  requireAbsoluteFilePath,
} from "./config.parse-l1-source.js";
import { parseDaPeers } from "./config.parse-providers.js";
import {
  admittedWatcherConfigs,
  boundedInteger,
  enumValue,
  exactRecord,
  exactString,
  fail,
  HEX_32_PATTERN,
  plainRecord,
  WATCHER_CARDANO_SECURITY_PARAMETER_K,
  WATCHER_CONFIG_BOUNDS,
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
  type WatcherWalletKeySource,
} from "./config.watcher-config.js";
import { parseWatcherCustomNetwork } from "./custom-network.js";

const parseWatcherL1Origin = (value: unknown) => {
  const origin = exactRecord(value, "$.l1.origin", ["slot", "blockHash"]);
  return Object.freeze({
    slot: boundedInteger(
      origin.slot,
      "$.l1.origin.slot",
      WATCHER_CONFIG_BOUNDS.l1OriginSlot,
    ),
    blockHash: exactString(origin.blockHash, "$.l1.origin.blockHash", {
      minLength: 64,
      maxLength: 64,
      pattern: HEX_32_PATTERN,
    }),
  });
};

export const parseWatcherConfig = (value: unknown): WatcherConfig => {
  if (
    typeof value === "object" &&
    value !== null &&
    admittedWatcherConfigs.has(value)
  ) {
    return value as WatcherConfig;
  }
  const preliminary = plainRecord(value, "$");
  const root = exactRecord(preliminary, "$", [
    "schemaVersion",
    "mode",
    "targetNetwork",
    ...(preliminary.targetNetwork === "Custom" ? ["customNetwork"] : []),
    "l1",
    "da",
    "storage",
    "proverWallet",
    "deadlines",
  ]);
  if (root.schemaVersion !== WATCHER_CONFIG_SCHEMA_VERSION) {
    fail("invalid_value", "$.schemaVersion");
  }
  const mode = enumValue(root.mode, "$.mode", [
    "development",
    "acceptance",
  ] as const);
  const targetNetwork = enumValue(root.targetNetwork, "$.targetNetwork", [
    "Mainnet",
    "Preprod",
    "Preview",
    "Custom",
  ] as const);

  const hasL1Origin = Object.prototype.hasOwnProperty.call(
    plainRecord(root.l1, "$.l1"),
    "origin",
  );
  const l1 = exactRecord(root.l1, "$.l1", [
    "source",
    "requestTimeoutMs",
    "maxConcurrency",
    "finality",
    ...(hasL1Origin ? ["origin"] : []),
  ]);
  const origin = hasL1Origin ? parseWatcherL1Origin(l1.origin) : undefined;
  const finality = exactRecord(l1.finality, "$.l1.finality", [
    "depth",
    "rollback",
  ]);
  const rollback = exactRecord(finality.rollback, "$.l1.finality.rollback", [
    "beforeFinality",
    "afterFinality",
    "maxDepth",
  ]);
  const finalityDepth = boundedInteger(
    finality.depth,
    "$.l1.finality.depth",
    WATCHER_CONFIG_BOUNDS.finalityDepth,
  );
  const rollbackMaxDepth = boundedInteger(
    rollback.maxDepth,
    "$.l1.finality.rollback.maxDepth",
    WATCHER_CONFIG_BOUNDS.rollbackDepth,
  );
  if (rollbackMaxDepth > finalityDepth) {
    fail("out_of_bounds", "$.l1.finality.rollback.maxDepth");
  }
  const l1RequestTimeoutMs = boundedInteger(
    l1.requestTimeoutMs,
    "$.l1.requestTimeoutMs",
    WATCHER_CONFIG_BOUNDS.requestTimeoutMs,
  );

  const da = exactRecord(root.da, "$.da", [
    "peers",
    "requestTimeoutMs",
    "maxConcurrency",
  ]);
  const daRequestTimeoutMs = boundedInteger(
    da.requestTimeoutMs,
    "$.da.requestTimeoutMs",
    WATCHER_CONFIG_BOUNDS.requestTimeoutMs,
  );

  const storage = exactRecord(root.storage, "$.storage", [
    "driver",
    "path",
    "rollbackAuthorityKeySource",
  ]);
  if (storage.driver !== "sqlite") {
    fail("invalid_value", "$.storage.driver");
  }

  const proverWallet = exactRecord(root.proverWallet, "$.proverWallet", [
    "keySource",
  ]);

  const deadlines = exactRecord(root.deadlines, "$.deadlines", [
    "daFetchMs",
    "daPublishMs",
    "proofConstructMs",
    "proofSubmitMs",
  ]);
  const daFetchMs = boundedInteger(
    deadlines.daFetchMs,
    "$.deadlines.daFetchMs",
    WATCHER_CONFIG_BOUNDS.deadlineMs,
  );
  const daPublishMs = boundedInteger(
    deadlines.daPublishMs,
    "$.deadlines.daPublishMs",
    WATCHER_CONFIG_BOUNDS.deadlineMs,
  );
  const proofConstructMs = boundedInteger(
    deadlines.proofConstructMs,
    "$.deadlines.proofConstructMs",
    WATCHER_CONFIG_BOUNDS.deadlineMs,
  );
  const proofSubmitMs = boundedInteger(
    deadlines.proofSubmitMs,
    "$.deadlines.proofSubmitMs",
    WATCHER_CONFIG_BOUNDS.deadlineMs,
  );
  if (daFetchMs < daRequestTimeoutMs) {
    fail("out_of_bounds", "$.deadlines.daFetchMs");
  }
  if (daPublishMs < daRequestTimeoutMs) {
    fail("out_of_bounds", "$.deadlines.daPublishMs");
  }
  if (proofSubmitMs < l1RequestTimeoutMs) {
    fail("out_of_bounds", "$.deadlines.proofSubmitMs");
  }
  const rollbackAuthorityKeySource = parseKeySource(
    storage.rollbackAuthorityKeySource,
    "$.storage.rollbackAuthorityKeySource",
  );
  const proverWalletKeySource = parseKeySource(
    proverWallet.keySource,
    "$.proverWallet.keySource",
  );
  if (
    rollbackAuthorityKeySource.kind === proverWalletKeySource.kind &&
    (rollbackAuthorityKeySource.kind === "environment"
      ? rollbackAuthorityKeySource.variable ===
        (
          proverWalletKeySource as Extract<
            WatcherWalletKeySource,
            { readonly kind: "environment" }
          >
        ).variable
      : rollbackAuthorityKeySource.path ===
        (
          proverWalletKeySource as Extract<
            WatcherWalletKeySource,
            { readonly kind: "file" }
          >
        ).path)
  ) {
    fail("secret_source_alias", "$.storage.rollbackAuthorityKeySource");
  }

  const source = parseL1Source(l1.source, mode);
  const customNetwork =
    targetNetwork === "Custom"
      ? parseWatcherCustomNetwork(root.customNetwork)
      : undefined;
  if (customNetwork !== undefined && source.sourceMode !== "local_node")
    fail("invalid_configuration", "$.l1.source");
  const admitted = Object.freeze({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode,
    targetNetwork,
    ...(customNetwork === undefined ? {} : { customNetwork }),
    l1: Object.freeze({
      source,
      ...(origin === undefined ? {} : { origin }),
      requestTimeoutMs: l1RequestTimeoutMs,
      maxConcurrency: boundedInteger(
        l1.maxConcurrency,
        "$.l1.maxConcurrency",
        WATCHER_CONFIG_BOUNDS.concurrency,
      ),
      finality: Object.freeze({
        depth: finalityDepth,
        rollback: Object.freeze({
          beforeFinality: enumValue(
            rollback.beforeFinality,
            "$.l1.finality.rollback.beforeFinality",
            ["rewind"] as const,
          ),
          afterFinality: enumValue(
            rollback.afterFinality,
            "$.l1.finality.rollback.afterFinality",
            ["quarantine"] as const,
          ),
          maxDepth: rollbackMaxDepth,
          postFinalityRecoveryMaxDepth: WATCHER_CARDANO_SECURITY_PARAMETER_K,
        }),
      }),
    }),
    da: Object.freeze({
      peers: parseDaPeers(da.peers, targetNetwork),
      requestTimeoutMs: daRequestTimeoutMs,
      maxConcurrency: boundedInteger(
        da.maxConcurrency,
        "$.da.maxConcurrency",
        WATCHER_CONFIG_BOUNDS.concurrency,
      ),
    }),
    storage: Object.freeze({
      driver: "sqlite",
      path: requireAbsoluteFilePath(storage.path, "$.storage.path", true),
      rollbackAuthorityKeySource,
    }),
    proverWallet: Object.freeze({
      keySource: proverWalletKeySource,
    }),
    deadlines: Object.freeze({
      daFetchMs,
      daPublishMs,
      proofConstructMs,
      proofSubmitMs,
    }),
  });
  admittedWatcherConfigs.add(admitted);
  return admitted;
};
