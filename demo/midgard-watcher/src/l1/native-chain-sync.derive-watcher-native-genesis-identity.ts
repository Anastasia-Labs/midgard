import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { readFile, realpath } from "node:fs/promises";
import { dirname, isAbsolute, normalize, resolve } from "node:path";

import {
  parseWatcherStrictJsonValue,
  type WatcherConfig,
} from "../runtime/config.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  exactRecord,
  HEX_32,
  MAX_BLOCK_CBOR_HEX,
  MAX_IDENTITY_FILE_BYTES,
  NATURAL,
  NETWORK_MAGIC,
  parsePoint,
  parseTip,
  string,
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncPoint,
} from "./native-chain-sync.exact-record.js";

export const parseWatcherNativeChainSyncEvent = (
  value: unknown,
): WatcherNativeChainSyncEvent => {
  const base = exactRecord(
    value,
    typeof value === "object" &&
      value !== null &&
      (value as { kind?: unknown }).kind === "roll_forward"
      ? [
          "blockHash",
          "blockNo",
          "blockType",
          "kind",
          "prevHash",
          "rawBlockCbor",
          "schemaVersion",
          "slot",
          "tip",
        ]
      : ["kind", "point", "schemaVersion", "tip"],
    "native chain-sync event",
  );
  if (base.schemaVersion !== WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION) {
    throw new Error("native chain-sync event schema changed");
  }
  const tip = parseTip(base.tip);
  if (base.kind === "roll_forward") {
    const rawBlockCbor = string(
      base.rawBlockCbor,
      /^(?:[0-9a-f]{2})+$/u,
      "native raw block CBOR",
    );
    if (rawBlockCbor.length > MAX_BLOCK_CBOR_HEX) {
      throw new Error("native raw block CBOR exceeds the supervisor bound");
    }
    return Object.freeze({
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
      kind: "roll_forward",
      blockHash: string(base.blockHash, HEX_32, "native block hash"),
      blockType: string(base.blockType, NATURAL, "native block type"),
      prevHash:
        base.blockNo === "0" && base.prevHash === ""
          ? ""
          : string(base.prevHash, HEX_32, "native previous block hash"),
      slot: string(base.slot, NATURAL, "native chain-sync slot"),
      blockNo: string(base.blockNo, NATURAL, "native block number"),
      rawBlockCbor,
      tip,
    });
  }
  if (base.kind !== "roll_backward") {
    throw new Error("native chain-sync event kind is unsupported");
  }
  return Object.freeze({
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    kind: "roll_backward",
    point: parsePoint(base.point, "native rollback point"),
    tip,
  });
};

export const parseJsonLine = (line: string): unknown => {
  try {
    const value = JSON.parse(line) as unknown;
    if (watcherCanonicalJson(value) !== line) {
      throw new Error("non-canonical JSON");
    }
    return value;
  } catch {
    throw new Error(
      "native chain-sync emitted malformed or non-canonical JSON",
    );
  }
};

export const lines = async function* (
  stream: AsyncIterable<Uint8Array>,
  maxTotalBytes?: number,
): AsyncGenerator<string> {
  let totalBytes = 0;
  const decoder = new TextDecoder("utf-8", { fatal: true });
  let pending = "";
  for await (const chunk of stream) {
    totalBytes += chunk.byteLength;
    if (maxTotalBytes !== undefined && totalBytes > maxTotalBytes) {
      throw new Error("native exact-point query stdout exceeded its bound");
    }
    pending += decoder.decode(chunk, { stream: true });
    if (pending.length > MAX_BLOCK_CBOR_HEX + 4_096) {
      throw new Error("native chain-sync output line exceeds its bound");
    }
    let newline = pending.indexOf("\n");
    while (newline >= 0) {
      const line = pending.slice(0, newline);
      pending = pending.slice(newline + 1);
      if (line.length === 0)
        throw new Error("native chain-sync emitted an empty line");
      yield line;
      newline = pending.indexOf("\n");
    }
  }
  pending += decoder.decode();
  if (pending.length !== 0) {
    throw new Error("native chain-sync terminated with a partial line");
  }
};

export const sha256 = (value: string): string =>
  createHash("sha256").update(value, "utf8").digest("hex");

export type ReadIdentityFile = (path: string) => Promise<Uint8Array>;

const readIdentityFile: ReadIdentityFile = async (path) => {
  if ((await realpath(path)) !== path) {
    throw new Error("native chain-sync identity path traverses a symlink");
  }
  return await readFile(path);
};

export type WatcherNativeNodeConfig = Pick<
  WatcherConfig,
  "targetNetwork" | "customNetwork"
> & {
  readonly l1: Pick<WatcherConfig["l1"], "source">;
};

export const deriveWatcherNativeGenesisIdentity = async (input: {
  readonly watcherConfig: WatcherNativeNodeConfig;
  readonly unsafeReadIdentityFileForTest?: ReadIdentityFile;
}): Promise<{ genesisIdentitySha256: string; networkMagic: number }> => {
  if (input.watcherConfig.l1.source.sourceMode !== "local_node") {
    throw new Error("native genesis identity requires local-node source");
  }
  const source = input.watcherConfig.l1.source;
  const read = input.unsafeReadIdentityFileForTest ?? readIdentityFile;
  const [nodeConfigBytes, genesisBytes] = await Promise.all([
    read(source.chainSync.nodeConfigPath),
    read(source.chainSync.genesisConfigPath),
  ]);
  if (
    nodeConfigBytes.byteLength === 0 ||
    nodeConfigBytes.byteLength > MAX_IDENTITY_FILE_BYTES ||
    genesisBytes.byteLength === 0 ||
    genesisBytes.byteLength > MAX_IDENTITY_FILE_BYTES
  ) {
    throw new Error("native chain-sync identity file size is invalid");
  }
  const decoder = new TextDecoder("utf-8", { fatal: true });
  const nodeConfig = parseWatcherStrictJsonValue(
    decoder.decode(nodeConfigBytes),
  );
  const genesis = parseWatcherStrictJsonValue(decoder.decode(genesisBytes));
  if (
    typeof nodeConfig !== "object" ||
    nodeConfig === null ||
    Array.isArray(nodeConfig) ||
    typeof genesis !== "object" ||
    genesis === null ||
    Array.isArray(genesis)
  ) {
    throw new Error("native chain-sync identity file is not an object");
  }
  const declaredGenesis = (nodeConfig as Record<string, unknown>)
    .ShelleyGenesisFile;
  if (typeof declaredGenesis !== "string" || declaredGenesis.length === 0) {
    throw new Error("node config does not declare ShelleyGenesisFile");
  }
  const resolvedGenesis = normalize(
    isAbsolute(declaredGenesis)
      ? declaredGenesis
      : resolve(dirname(source.chainSync.nodeConfigPath), declaredGenesis),
  );
  if (resolvedGenesis !== source.chainSync.genesisConfigPath) {
    throw new Error("node config genesis path differs from watcher authority");
  }
  const configuredMagic = (genesis as Record<string, unknown>).networkMagic;
  const custom = input.watcherConfig.customNetwork;
  const expectedMagic =
    input.watcherConfig.targetNetwork === "Custom"
      ? custom?.networkMagic
      : NETWORK_MAGIC[input.watcherConfig.targetNetwork];
  if (expectedMagic === undefined || configuredMagic !== expectedMagic) {
    throw new Error("node genesis network magic differs from watcher network");
  }
  if (input.watcherConfig.targetNetwork === "Custom") {
    const clock = custom?.slotConfig;
    const genesisRecord = genesis as Record<string, unknown>;
    const configRecord = nodeConfig as Record<string, unknown>;
    if (
      clock === undefined ||
      typeof genesisRecord.systemStart !== "string" ||
      Date.parse(genesisRecord.systemStart) !== clock.zeroTime ||
      typeof genesisRecord.slotLength !== "number" ||
      genesisRecord.slotLength * 1000 !== clock.slotLength ||
      clock.zeroSlot !== 0 ||
      genesisRecord.networkId !== "Testnet" ||
      configRecord.TestShelleyHardForkAtEpoch !== 0 ||
      configRecord.TestConwayHardForkAtEpoch !== 0
    )
      throw new Error(
        "Custom network slot clock differs from epoch-zero testnet genesis",
      );
  }
  const derived = createHash("sha256").update(genesisBytes).digest("hex");
  if (derived !== source.chainSync.genesisIdentitySha256) {
    throw new Error("node genesis identity differs from watcher configuration");
  }
  return { genesisIdentitySha256: derived, networkMagic: expectedMagic };
};

export type SpawnProcess = (
  binaryPath: string,
) => ChildProcessWithoutNullStreams;

export const productionSpawn: SpawnProcess = (binaryPath) =>
  spawn(binaryPath, [], {
    stdio: ["pipe", "pipe", "pipe"],
    env: Object.freeze({ PATH: process.env.PATH ?? "/usr/bin:/bin" }),
  });

export type NativeStreamInput = {
  readonly binaryPath: string;
  readonly watcherConfig: WatcherConfig;
  readonly signal?: AbortSignal;
  readonly intersection: WatcherNativeChainSyncPoint;
  readonly startupTimeoutMs: number;
  readonly onEvent: (event: WatcherNativeChainSyncEvent) => Promise<void>;
  readonly unsafeSpawnForTest?: SpawnProcess;
  readonly unsafeReadIdentityFileForTest?: ReadIdentityFile;
};
