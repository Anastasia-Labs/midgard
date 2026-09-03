import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { constants } from "node:fs";
import { access, readFile, realpath } from "node:fs/promises";
import { dirname, isAbsolute, normalize, resolve } from "node:path";

import {
  parseWatcherStrictJsonValue,
  type WatcherConfig,
} from "../runtime/config.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";

export const WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION =
  "midgard-watcher-native-chain-sync-v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const MAX_BLOCK_CBOR_HEX = 8 * 1024 * 1024;
const MAX_STDERR_BYTES = 1024 * 1024;
const MAX_INTERSECTIONS = 128;
const MAX_IDENTITY_FILE_BYTES = 4 * 1024 * 1024;
const NETWORK_MAGIC = Object.freeze({
  Mainnet: 764_824_073,
  Preprod: 1,
  Preview: 2,
} as const);

class NativeChainSyncStartupFailure extends Error {
  readonly code: string;

  constructor(code: string) {
    super(`native chain-sync startup failed: ${code}`);
    this.name = "NativeChainSyncStartupFailure";
    this.code = code;
  }
}

export type WatcherNativeChainSyncPoint =
  | Readonly<{ kind: "origin" }>
  | Readonly<{ kind: "point"; blockHash: string; slot: string }>;
type NativeTip =
  | Readonly<{ kind: "origin" }>
  | Readonly<{
      kind: "point";
      blockHash: string;
      blockNo: string;
      slot: string;
    }>;

export type WatcherNativeChainSyncRollForward = Readonly<{
  schemaVersion: typeof WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION;
  kind: "roll_forward";
  blockHash: string;
  blockType: string;
  prevHash: string;
  slot: string;
  blockNo: string;
  rawBlockCbor: string;
  tip: NativeTip;
}>;

export type WatcherNativeChainSyncRollBackward = Readonly<{
  schemaVersion: typeof WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION;
  kind: "roll_backward";
  point: WatcherNativeChainSyncPoint;
  tip: NativeTip;
}>;

export type WatcherNativeChainSyncEvent =
  | WatcherNativeChainSyncRollForward
  | WatcherNativeChainSyncRollBackward;

export type WatcherNativeChainSyncAuthority = Readonly<{
  schemaVersion: "midgard-watcher-native-chain-sync-authority-v1";
  authorityDigest: string;
}>;

export type WatcherNativeChainSyncAuthorityDetails = Readonly<{
  network: WatcherConfig["targetNetwork"];
  authorityNodeId: string;
  genesisIdentitySha256: string;
  socketPath: string;
  startupDigest: string;
  selectedIntersection: WatcherNativeChainSyncPoint;
  currentTip: NativeTip;
}>;

export type WatcherNativeChainSyncRuntime = Readonly<{
  authority: WatcherNativeChainSyncAuthority;
  done: Promise<void>;
  close(): Promise<void>;
}>;

const authorityDetails = new WeakMap<
  WatcherNativeChainSyncAuthority,
  WatcherNativeChainSyncAuthorityDetails
>();
const authorityLiveness = new WeakMap<
  WatcherNativeChainSyncAuthority,
  { active: boolean }
>();

export const watcherNativeChainSyncAuthorityDetails = (
  authority: WatcherNativeChainSyncAuthority,
): WatcherNativeChainSyncAuthorityDetails | null =>
  authorityLiveness.get(authority)?.active === true
    ? (authorityDetails.get(authority) ?? null)
    : null;

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

const string = (value: unknown, pattern: RegExp, label: string): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is invalid`);
  }
  return value;
};

const parseTip = (value: unknown): NativeTip => {
  if (
    typeof value === "object" &&
    value !== null &&
    (value as { kind?: unknown }).kind === "origin"
  ) {
    exactRecord(value, ["kind"], "native tip");
    return Object.freeze({ kind: "origin" });
  }
  const tip = exactRecord(
    value,
    ["blockHash", "blockNo", "kind", "slot"],
    "native tip",
  );
  if (tip.kind !== "point") throw new Error("native tip kind is invalid");
  return Object.freeze({
    kind: "point" as const,
    blockHash: string(tip.blockHash, HEX_32, "native tip hash"),
    blockNo: string(tip.blockNo, NATURAL, "native tip block number"),
    slot: string(tip.slot, NATURAL, "native tip slot"),
  });
};

const parsePoint = (
  value: unknown,
  label: string,
): WatcherNativeChainSyncPoint => {
  if (
    typeof value === "object" &&
    value !== null &&
    (value as { kind?: unknown }).kind === "origin"
  ) {
    exactRecord(value, ["kind"], label);
    return Object.freeze({ kind: "origin" });
  }
  const point = exactRecord(value, ["blockHash", "kind", "slot"], label);
  if (point.kind !== "point") throw new Error(`${label} kind is invalid`);
  return Object.freeze({
    kind: "point" as const,
    blockHash: string(point.blockHash, HEX_32, `${label} hash`),
    slot: string(point.slot, NATURAL, `${label} slot`),
  });
};

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
      prevHash: string(base.prevHash, HEX_32, "native previous block hash"),
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

const parseJsonLine = (line: string): unknown => {
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

const lines = async function* (
  stream: AsyncIterable<Uint8Array>,
): AsyncGenerator<string> {
  const decoder = new TextDecoder("utf-8", { fatal: true });
  let pending = "";
  for await (const chunk of stream) {
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

const sha256 = (value: string): string =>
  createHash("sha256").update(value, "utf8").digest("hex");

type ReadIdentityFile = (path: string) => Promise<Uint8Array>;

const readIdentityFile: ReadIdentityFile = async (path) => {
  if ((await realpath(path)) !== path) {
    throw new Error("native chain-sync identity path traverses a symlink");
  }
  return await readFile(path);
};

const deriveGenesisIdentity = async (input: {
  readonly watcherConfig: WatcherConfig;
  readonly unsafeReadIdentityFileForTest?: ReadIdentityFile;
}): Promise<string> => {
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
  if (configuredMagic !== NETWORK_MAGIC[input.watcherConfig.targetNetwork]) {
    throw new Error("node genesis network magic differs from watcher network");
  }
  const derived = createHash("sha256").update(genesisBytes).digest("hex");
  if (derived !== source.chainSync.genesisIdentitySha256) {
    throw new Error("node genesis identity differs from watcher configuration");
  }
  return derived;
};

type SpawnProcess = (binaryPath: string) => ChildProcessWithoutNullStreams;

const productionSpawn: SpawnProcess = (binaryPath) =>
  spawn(binaryPath, [], {
    stdio: ["pipe", "pipe", "pipe"],
    env: Object.freeze({ PATH: process.env.PATH ?? "/usr/bin:/bin" }),
  });

export const startWatcherNativeChainSync = async (input: {
  readonly binaryPath: string;
  readonly watcherConfig: WatcherConfig;
  readonly intersection: WatcherNativeChainSyncPoint;
  readonly startupTimeoutMs: number;
  readonly onEvent: (event: WatcherNativeChainSyncEvent) => Promise<void>;
  readonly unsafeSpawnForTest?: SpawnProcess;
  readonly unsafeReadIdentityFileForTest?: ReadIdentityFile;
}): Promise<WatcherNativeChainSyncRuntime> => {
  if (input.watcherConfig.l1.source.sourceMode !== "local_node") {
    throw new Error(
      "native chain-sync requires the admitted local-node source",
    );
  }
  if (
    !Number.isSafeInteger(input.startupTimeoutMs) ||
    input.startupTimeoutMs < 100 ||
    input.startupTimeoutMs > 120_000
  ) {
    throw new Error("native chain-sync startup bounds are invalid");
  }
  const binaryPath = input.binaryPath;
  if (!isAbsolute(binaryPath) || normalize(binaryPath) !== binaryPath) {
    throw new Error("native chain-sync binary path is not canonical");
  }
  if (input.unsafeSpawnForTest === undefined) {
    if ((await realpath(binaryPath)) !== binaryPath) {
      throw new Error("native chain-sync binary path traverses a symlink");
    }
    await access(binaryPath, constants.X_OK);
  }
  const intersection = parsePoint(
    input.intersection,
    "native startup intersection",
  );
  const source = input.watcherConfig.l1.source;
  const genesisIdentitySha256 = await deriveGenesisIdentity({
    watcherConfig: input.watcherConfig,
    ...(input.unsafeReadIdentityFileForTest === undefined
      ? {}
      : {
          unsafeReadIdentityFileForTest: input.unsafeReadIdentityFileForTest,
        }),
  });
  const startup = Object.freeze({
    authorityNodeId: source.authorityNodeId,
    genesisIdentitySha256,
    intersection,
    network: input.watcherConfig.targetNetwork,
    networkMagic: NETWORK_MAGIC[input.watcherConfig.targetNetwork],
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    socketPath: source.chainSync.socketPath,
  });
  const startupJson = watcherCanonicalJson(startup);
  const startupDigest = sha256(startupJson);
  const child = (input.unsafeSpawnForTest ?? productionSpawn)(binaryPath);
  child.stdin.end(`${startupJson}\n`, "utf8");

  let closing = false;
  let resolveReady!: (authority: WatcherNativeChainSyncAuthority) => void;
  let rejectReady!: (error: Error) => void;
  const ready = new Promise<WatcherNativeChainSyncAuthority>(
    (resolve, reject) => {
      resolveReady = resolve;
      rejectReady = reject;
    },
  );
  const knownPoints = new Map<
    string,
    Readonly<{ slot: bigint; blockNo: bigint }>
  >();
  if (intersection.kind === "point") {
    knownPoints.set(intersection.blockHash, {
      slot: BigInt(intersection.slot),
      blockNo: -1n,
    });
  }
  knownPoints.set("", { slot: 0n, blockNo: -1n });
  let current: Readonly<{
    hash: string;
    slot: bigint;
    blockNo: bigint;
  }> | null =
    intersection.kind === "point"
      ? Object.freeze({
          hash: intersection.blockHash,
          slot: BigInt(intersection.slot),
          blockNo: -1n,
        })
      : null;
  let sawReady = false;
  let mintedAuthority: WatcherNativeChainSyncAuthority | undefined;

  const stderrDrain = (async () => {
    let total = 0;
    for await (const chunk of child.stderr) {
      total += chunk.byteLength;
      if (total > MAX_STDERR_BYTES) {
        child.kill("SIGKILL");
        throw new Error("native chain-sync stderr exceeded its bound");
      }
    }
  })();

  const done = (async () => {
    try {
      for await (const line of lines(child.stdout)) {
        const value = parseJsonLine(line);
        if (!sawReady) {
          if (
            typeof value === "object" &&
            value !== null &&
            (value as { kind?: unknown }).kind === "error"
          ) {
            const failure = exactRecord(
              value,
              ["code", "kind", "schemaVersion"],
              "native chain-sync startup failure",
            );
            if (
              failure.schemaVersion !==
                WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION ||
              typeof failure.code !== "string" ||
              !/^[a-z][a-z0-9_]{0,62}$/u.test(failure.code)
            ) {
              throw new Error("native chain-sync emitted an invalid failure");
            }
            throw new NativeChainSyncStartupFailure(failure.code);
          }
          const record = exactRecord(
            value,
            [
              "authorityNodeId",
              "currentTip",
              "genesisIdentitySha256",
              "kind",
              "network",
              "networkMagic",
              "schemaVersion",
              "selectedIntersection",
              "socketPath",
              "startupDigest",
            ],
            "native chain-sync ready event",
          );
          const selectedIntersection = parsePoint(
            record.selectedIntersection,
            "native selected intersection",
          );
          if (
            record.kind !== "ready" ||
            record.schemaVersion !== WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION ||
            record.authorityNodeId !== startup.authorityNodeId ||
            record.genesisIdentitySha256 !== startup.genesisIdentitySha256 ||
            record.network !== startup.network ||
            record.networkMagic !== startup.networkMagic ||
            watcherCanonicalJson(selectedIntersection) !==
              watcherCanonicalJson(intersection) ||
            record.socketPath !== startup.socketPath ||
            record.startupDigest !== startupDigest
          ) {
            throw new Error(
              "native chain-sync ready identity differs from startup authority",
            );
          }
          const currentTip = parseTip(record.currentTip);
          const details = Object.freeze({
            network: startup.network,
            authorityNodeId: startup.authorityNodeId,
            genesisIdentitySha256: startup.genesisIdentitySha256,
            socketPath: startup.socketPath,
            startupDigest,
            selectedIntersection,
            currentTip,
          });
          const authority = Object.freeze({
            schemaVersion:
              "midgard-watcher-native-chain-sync-authority-v1" as const,
            authorityDigest: sha256(watcherCanonicalJson(details)),
          });
          authorityDetails.set(authority, details);
          authorityLiveness.set(authority, { active: true });
          mintedAuthority = authority;
          sawReady = true;
          resolveReady(authority);
          continue;
        }
        const event = parseWatcherNativeChainSyncEvent(value);
        if (event.kind === "roll_forward") {
          const slot = BigInt(event.slot);
          const blockNo = BigInt(event.blockNo);
          if (
            (current !== null &&
              (event.prevHash !== current.hash ||
                slot <= current.slot ||
                blockNo <= current.blockNo)) ||
            (current === null && !knownPoints.has(event.prevHash))
          ) {
            throw new Error("native chain-sync roll-forward is out of order");
          }
          current = Object.freeze({ hash: event.blockHash, slot, blockNo });
          knownPoints.set(event.blockHash, { slot, blockNo });
        } else {
          const rollbackHash =
            event.point.kind === "origin" ? "" : event.point.blockHash;
          const rollbackSlot =
            event.point.kind === "origin" ? 0n : BigInt(event.point.slot);
          const rollback = knownPoints.get(rollbackHash);
          if (rollback === undefined || rollback.slot !== rollbackSlot) {
            throw new Error(
              "native chain-sync rollback target is not durable history",
            );
          }
          current = Object.freeze({
            hash: rollbackHash,
            slot: rollback.slot,
            blockNo: rollback.blockNo,
          });
          for (const [hash, point] of knownPoints) {
            if (point.slot > rollback.slot) knownPoints.delete(hash);
          }
        }
        await input.onEvent(event);
      }
      await stderrDrain;
      if (!closing)
        throw new Error("native chain-sync process exited unexpectedly");
    } catch (error) {
      const failure = error instanceof Error ? error : new Error(String(error));
      rejectReady(failure);
      if (!closing) child.kill("SIGKILL");
      throw failure;
    } finally {
      const liveness =
        mintedAuthority === undefined
          ? undefined
          : authorityLiveness.get(mintedAuthority);
      if (liveness !== undefined) liveness.active = false;
    }
  })();
  void done.catch(() => undefined);

  let startupTimer: NodeJS.Timeout | undefined;
  const authority = await Promise.race([
    ready,
    new Promise<never>((_, reject) => {
      startupTimer = setTimeout(
        () => reject(new Error("native chain-sync startup timed out")),
        input.startupTimeoutMs,
      );
    }),
  ])
    .catch((error) => {
      closing = true;
      child.kill("SIGKILL");
      throw error;
    })
    .finally(() => {
      if (startupTimer !== undefined) clearTimeout(startupTimer);
    });

  return Object.freeze({
    authority,
    done,
    close: async () => {
      if (closing) return;
      closing = true;
      child.kill("SIGTERM");
      let forceKillTimer: NodeJS.Timeout | undefined;
      try {
        await Promise.race([
          done.catch(() => undefined),
          new Promise<void>((resolve) => {
            forceKillTimer = setTimeout(() => {
              child.kill("SIGKILL");
              resolve();
            }, 5_000);
          }),
        ]);
      } finally {
        if (forceKillTimer !== undefined) clearTimeout(forceKillTimer);
      }
    },
  });
};

export const startWatcherNativeChainSyncWithRetry = async (input: {
  readonly binaryPath: string;
  readonly watcherConfig: WatcherConfig;
  readonly intersectionCandidates: readonly WatcherNativeChainSyncPoint[];
  readonly startupTimeoutMs: number;
  readonly onEvent: (event: WatcherNativeChainSyncEvent) => Promise<void>;
  readonly unsafeSpawnForTest?: SpawnProcess;
  readonly unsafeReadIdentityFileForTest?: ReadIdentityFile;
}): Promise<WatcherNativeChainSyncRuntime> => {
  if (
    input.intersectionCandidates.length === 0 ||
    input.intersectionCandidates.length > MAX_INTERSECTIONS
  ) {
    throw new Error(
      "native chain-sync intersection candidate bounds are invalid",
    );
  }
  const seen = new Set<string>();
  const candidates = input.intersectionCandidates.map((candidate, index) => {
    const parsed = parsePoint(candidate, "native intersection candidate");
    const key = watcherCanonicalJson(parsed);
    if (seen.has(key))
      throw new Error("native intersection candidate is duplicated");
    if (
      parsed.kind === "origin" &&
      index !== input.intersectionCandidates.length - 1
    ) {
      throw new Error("native Origin candidate must be the final fallback");
    }
    seen.add(key);
    return parsed;
  });
  let lastIntersectionFailure: NativeChainSyncStartupFailure | undefined;
  for (const intersection of candidates) {
    try {
      return await startWatcherNativeChainSync({
        binaryPath: input.binaryPath,
        watcherConfig: input.watcherConfig,
        intersection,
        startupTimeoutMs: input.startupTimeoutMs,
        onEvent: input.onEvent,
        ...(input.unsafeSpawnForTest === undefined
          ? {}
          : { unsafeSpawnForTest: input.unsafeSpawnForTest }),
        ...(input.unsafeReadIdentityFileForTest === undefined
          ? {}
          : {
              unsafeReadIdentityFileForTest:
                input.unsafeReadIdentityFileForTest,
            }),
      });
    } catch (error) {
      if (
        !(error instanceof NativeChainSyncStartupFailure) ||
        error.code !== "intersection_failed"
      ) {
        throw error;
      }
      lastIntersectionFailure = error;
    }
  }
  throw (
    lastIntersectionFailure ??
    new Error("native chain-sync did not admit an intersection")
  );
};
