import { isAbsolute, normalize } from "node:path";

import {
  ENVIRONMENT_VARIABLE_PATTERN,
  exactRecord,
  exactString,
  fail,
  HEX_32_PATTERN,
  IDENTITY_PATTERN,
  plainRecord,
  type WatcherL1SourceConfig,
  type WatcherWalletKeySource,
} from "./config.watcher-config.js";

export const requireAbsoluteFilePath = (
  value: unknown,
  path: string,
  durable: boolean,
): string => {
  const filePath = exactString(value, path, { maxLength: 4_096 });
  const normalized = normalize(filePath);
  if (
    !isAbsolute(filePath) ||
    normalized !== filePath ||
    normalized === "/" ||
    normalized.includes("\0") ||
    normalized === "/tmp" ||
    normalized.startsWith("/tmp/")
  ) {
    fail("unsafe_path", path);
  }
  if (
    durable &&
    (normalized === "/dev" ||
      normalized.startsWith("/dev/") ||
      normalized === "/proc" ||
      normalized.startsWith("/proc/") ||
      normalized === "/run" ||
      normalized.startsWith("/run/") ||
      normalized === "/sys" ||
      normalized.startsWith("/sys/"))
  ) {
    fail("unsafe_path", path);
  }
  return filePath;
};

export const parseKeySource = (
  value: unknown,
  path: string,
): WatcherWalletKeySource => {
  if (typeof value === "string") {
    fail("inline_secret_forbidden", path);
  }
  const preliminary = plainRecord(value, path);
  const kind = preliminary.kind;
  if (kind === "environment") {
    const record = exactRecord(value, path, ["kind", "variable"]);
    return Object.freeze({
      kind,
      variable: exactString(record.variable, `${path}.variable`, {
        maxLength: 128,
        pattern: ENVIRONMENT_VARIABLE_PATTERN,
      }),
    });
  }
  if (kind === "file") {
    const record = exactRecord(value, path, ["kind", "path"]);
    return Object.freeze({
      kind,
      path: requireAbsoluteFilePath(record.path, `${path}.path`, false),
    });
  }
  fail("invalid_value", `${path}.kind`);
};

export const parseL1Source = (value: unknown): WatcherL1SourceConfig => {
  const preliminary = plainRecord(value, "$.l1.source");
  if (preliminary.sourceMode === "local_node") {
    const source = exactRecord(value, "$.l1.source", [
      "sourceMode",
      "authorityNodeId",
      "chainSync",
    ]);
    const chainSync = exactRecord(source.chainSync, "$.l1.source.chainSync", [
      "kind",
      "socketPath",
      "nodeConfigPath",
      "genesisConfigPath",
      "genesisIdentitySha256",
    ]);
    if (chainSync.kind !== "cardano_node_socket") {
      fail("invalid_value", "$.l1.source.chainSync.kind");
    }
    return Object.freeze({
      sourceMode: "local_node",
      authorityNodeId: exactString(
        source.authorityNodeId,
        "$.l1.source.authorityNodeId",
        { maxLength: 32, pattern: IDENTITY_PATTERN },
      ),
      chainSync: Object.freeze({
        kind: "cardano_node_socket",
        socketPath: requireAbsoluteFilePath(
          chainSync.socketPath,
          "$.l1.source.chainSync.socketPath",
          false,
        ),
        nodeConfigPath: requireAbsoluteFilePath(
          chainSync.nodeConfigPath,
          "$.l1.source.chainSync.nodeConfigPath",
          false,
        ),
        genesisConfigPath: requireAbsoluteFilePath(
          chainSync.genesisConfigPath,
          "$.l1.source.chainSync.genesisConfigPath",
          false,
        ),
        genesisIdentitySha256: exactString(
          chainSync.genesisIdentitySha256,
          "$.l1.source.chainSync.genesisIdentitySha256",
          { maxLength: 64, pattern: HEX_32_PATTERN },
        ),
      }),
    });
  }
  fail("invalid_value", "$.l1.source.sourceMode");
};
