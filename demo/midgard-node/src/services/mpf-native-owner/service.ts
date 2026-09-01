import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";
import { createHash, timingSafeEqual } from "node:crypto";
import { open, readFile, rename, rm } from "node:fs/promises";
import { resolve } from "node:path";
import { getHeapStatistics } from "node:v8";
import { MessageChannel, type MessagePort } from "node:worker_threads";

import { Level } from "level";

import { positiveSafeInteger } from "@/artifact-schema.js";

import {
  createEventFlatDigest,
  prepareEventFlatDigest,
} from "../../workers/utils/mpf-event-flat-digest.js";
import { encodeNativeMpfRpcFrame, NativeMpfRpcFrameDecoder } from "./codec.js";
import {
  assertNativeMpfGenerationHandle,
  assertNativeMpfHashHex,
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  NATIVE_MPF_RPC_SCHEMA,
  type NativeMpfApplyResult,
  type NativeMpfGenerationHandle,
  type NativeMpfOwnerDiagnostics,
  type NativeMpfOwnerService,
  type NativeMpfRpcFrame,
  NativeMpfRpcKind,
  type PersistedNativeMpfReplay,
} from "./protocol.js";

const HASH_BYTES = 32;
const FULL_INDEX_HEADER_BYTES = 72;
const FULL_INDEX_MAX_BYTES = 512 * 1024 * 1024;
const FULL_INDEX_MAX_RECORDS = 2_000_000;
const EVENT_LOG_HEADER_BYTES = 92;
const LOAD_DIGEST_DOMAIN = Buffer.from("MIDGARD-MPF-OWNER-LOAD-V1");
const EVENT_LOG_DIGEST_DOMAIN = Buffer.from("MIDGARD-MPF-ARCH-G-EVENT-LOG-V1");
const EVENT_STREAM_DIGEST_DOMAIN = Buffer.from("MIDGARD-MPF-ARCH-G-EVENTS-V1");
const PROMOTION_DIGEST_DOMAIN = Buffer.from("MIDGARD-MPF-OWNER-PROMOTION-V1");
const SELF_TEST_DOMAIN = Buffer.from("MIDGARD-MPF-OWNER-BLAKE2B-SELFTEST-V1");
const ZERO_EPOCH = Buffer.alloc(16);
const SIDECAR_MAGIC = "MGNS";
const SIDECAR_HEADER_BYTES = 144;
const SIDECAR_DIGEST_DOMAIN = Buffer.from("MIDGARD-MPF-OWNER-NODE-SIDECAR-V1");
const NATIVE_OWNER_MIN_RUNTIME_HEADROOM_BYTES = 1024 * 1024 * 1024;

type StoredLeaf = {
  readonly __kind: "Leaf";
  readonly prefix: string;
  readonly key: string;
  readonly value: string;
};

type StoredBranch = {
  readonly __kind: "Branch";
  readonly prefix: string;
  readonly children: readonly (string | null)[];
  readonly size: number;
};

type StoredNode = StoredLeaf | StoredBranch;
type StoredValue = StoredNode | string;

type DecodedPromotionRecord = {
  readonly hash: Buffer;
  readonly hashHex: string;
  readonly encoded: Buffer;
  readonly stored: StoredNode;
};

type PendingRequest = {
  readonly expected: ReadonlySet<NativeMpfRpcKind>;
  readonly resolve: (frame: NativeMpfRpcFrame) => void;
  readonly reject: (error: Error) => void;
  readonly timer: NodeJS.Timeout;
};

type PendingPromotion = {
  readonly chunks: Buffer[];
  readonly resolve: (value: {
    frame: NativeMpfRpcFrame;
    bytes: Buffer;
  }) => void;
  readonly reject: (error: Error) => void;
  readonly timer: NodeJS.Timeout;
};

type WorkerGenerationLease = {
  readonly port: MessagePort;
  readonly handle: NativeMpfGenerationHandle;
  journalOwned: boolean;
};

export type NativeMpfEventOp =
  | {
      readonly type: "insert";
      readonly key: Uint8Array;
      readonly value: Uint8Array;
    }
  | { readonly type: "delete"; readonly key: Uint8Array };

export type NativeMpfOwnerServiceOptions = {
  readonly levelPath: string;
  readonly binaryPath: string;
  readonly binarySha256: string;
  readonly maxFrameBytes?: number;
  readonly maxChunkBytes?: number;
  readonly requestTimeoutMs?: number;
  readonly restartLimit?: number;
  readonly sidecarPath?: string;
  /** Process-crash test seam; production callers must leave this undefined. */
  readonly faultInjectionForTests?: (
    point: "before_promotion_batch" | "after_promotion_batch_before_ack",
  ) => void | Promise<void>;
  /** Process-lifecycle test seam; production callers must leave this undefined. */
  readonly onChildSpawnForTests?: (pid: number) => void;
};

type NormalizedNativeMpfOwnerServiceOptions = NativeMpfOwnerServiceOptions & {
  readonly maxFrameBytes: number;
  readonly maxChunkBytes: number;
  readonly requestTimeoutMs: number;
  readonly restartLimit: number;
};

const digest = (...parts: readonly Uint8Array[]): Buffer => {
  const state = createEventFlatDigest();
  for (const part of parts) state.update(part);
  return state.digest();
};

const normalizeOwnerOptions = (
  options: NativeMpfOwnerServiceOptions,
): NormalizedNativeMpfOwnerServiceOptions => {
  const maxFrameBytes =
    options.maxFrameBytes ?? NATIVE_MPF_OWNER_DEFAULT_CAPS.maxFrameBytes;
  const maxChunkBytes =
    options.maxChunkBytes ?? NATIVE_MPF_OWNER_DEFAULT_CAPS.maxChunkBytes;
  const requestTimeoutMs =
    options.requestTimeoutMs ?? NATIVE_MPF_OWNER_DEFAULT_CAPS.loadTimeoutMs;
  const restartLimit = options.restartLimit ?? 3;
  positiveSafeInteger(maxFrameBytes, "maxFrameBytes");
  positiveSafeInteger(maxChunkBytes, "maxChunkBytes");
  positiveSafeInteger(requestTimeoutMs, "requestTimeoutMs");
  if (!Number.isSafeInteger(restartLimit) || restartLimit < 0) {
    throw new Error("restartLimit must be a non-negative safe integer");
  }
  if (maxFrameBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxFrameBytes) {
    throw new Error("maxFrameBytes exceeds the compiled native owner cap");
  }
  if (maxChunkBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxChunkBytes) {
    throw new Error("maxChunkBytes exceeds the compiled native owner cap");
  }
  if (maxChunkBytes > maxFrameBytes - 68) {
    throw new Error("maxChunkBytes must fit inside maxFrameBytes");
  }
  return {
    ...options,
    maxFrameBytes,
    maxChunkBytes,
    requestTimeoutMs,
    restartLimit,
  };
};

export type NativeOwnerCgroupMemoryBudget =
  | { readonly kind: "finite"; readonly limitBytes: number }
  | { readonly kind: "unlimited" }
  | { readonly kind: "unavailable"; readonly containerized: boolean };

export const parseNativeOwnerCgroupMemoryLimit = (
  value: string,
):
  | Extract<
      NativeOwnerCgroupMemoryBudget,
      { readonly kind: "finite" | "unlimited" }
    >
  | undefined => {
  const normalized = value.trim();
  if (normalized === "max") return { kind: "unlimited" };
  if (!/^[0-9]+$/.test(normalized)) return undefined;
  const parsed = BigInt(normalized);
  if (parsed >= 1n << 60n) return { kind: "unlimited" };
  return parsed > 0n && parsed <= BigInt(Number.MAX_SAFE_INTEGER)
    ? { kind: "finite", limitBytes: Number(parsed) }
    : undefined;
};

export const assertNativeOwnerRuntimeMemoryBudget = ({
  cgroup,
  v8HeapLimitBytes,
}: {
  readonly cgroup: NativeOwnerCgroupMemoryBudget;
  readonly v8HeapLimitBytes: number;
}): void => {
  if (cgroup.kind === "unlimited") return;
  if (cgroup.kind === "unavailable") {
    if (cgroup.containerized) {
      throw new Error(
        "Architecture G cannot prove an enforceable container memory budget",
      );
    }
    return;
  }
  const cgroupLimitBytes = cgroup.limitBytes;
  const requiredBytes =
    v8HeapLimitBytes +
    NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes +
    NATIVE_OWNER_MIN_RUNTIME_HEADROOM_BYTES;
  if (
    !Number.isSafeInteger(cgroupLimitBytes) ||
    cgroupLimitBytes <= 0 ||
    !Number.isSafeInteger(v8HeapLimitBytes) ||
    v8HeapLimitBytes <= 0
  ) {
    throw new Error("Architecture G runtime memory budget is invalid");
  }
  if (cgroupLimitBytes < requiredBytes) {
    throw new Error(
      `Architecture G runtime memory budget is insufficient: cgroup_limit_bytes=${cgroupLimitBytes.toString()},v8_heap_limit_bytes=${v8HeapLimitBytes.toString()},owner_cap_bytes=${NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes.toString()},required_headroom_bytes=${NATIVE_OWNER_MIN_RUNTIME_HEADROOM_BYTES.toString()},required_total_bytes=${requiredBytes.toString()}`,
    );
  }
};

const readCgroupMemoryBudget =
  async (): Promise<NativeOwnerCgroupMemoryBudget> => {
    const candidates = ["/sys/fs/cgroup/memory.max"];
    try {
      const membership = await readFile("/proc/self/cgroup", "utf8");
      const unified = membership
        .split("\n")
        .find((line) => line.startsWith("0::"))
        ?.slice(3);
      if (unified !== undefined) {
        candidates.unshift(
          `/sys/fs/cgroup${unified === "/" ? "" : unified}/memory.max`,
        );
      }
    } catch {
      // The fixed root candidate still covers ordinary container cgroup mounts.
    }
    candidates.push("/sys/fs/cgroup/memory/memory.limit_in_bytes");
    for (const path of candidates) {
      try {
        const value = (await readFile(path, "utf8")).trim();
        const parsed = parseNativeOwnerCgroupMemoryLimit(value);
        if (parsed !== undefined) return parsed;
      } catch {
        // Try the next supported cgroup layout.
      }
    }
    const containerized = await Promise.any(
      ["/.dockerenv", "/run/.containerenv"].map((path) =>
        readFile(path).then(() => true),
      ),
    ).catch(() => false);
    return { kind: "unavailable", containerized };
  };

const assertBufferLength = (
  value: Uint8Array,
  length: number,
  field: string,
): Buffer => {
  const buffer = Buffer.from(value);
  if (buffer.byteLength !== length) {
    throw new Error(`${field} must contain exactly ${length.toString()} bytes`);
  }
  return buffer;
};

const assertStoredHash = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/.test(value)) {
    throw new Error(`${field} must be canonical 32-byte lowercase hex`);
  }
  return value;
};

const assertStoredNode = (value: unknown, hashHex: string): StoredNode => {
  if (typeof value !== "object" || value === null || !("__kind" in value)) {
    throw new Error(`MPF record ${hashHex} is not a node`);
  }
  const candidate = value as Record<string, unknown>;
  if (
    typeof candidate.prefix !== "string" ||
    !/^[0-9a-f]*$/.test(candidate.prefix)
  ) {
    throw new Error(`MPF record ${hashHex} has a non-canonical prefix`);
  }
  if (candidate.__kind === "Leaf") {
    if (
      typeof candidate.key !== "string" ||
      !/^(?:[0-9a-f]{2})*$/.test(candidate.key) ||
      typeof candidate.value !== "string" ||
      !/^(?:[0-9a-f]{2})*$/.test(candidate.value)
    ) {
      throw new Error(`MPF leaf ${hashHex} has invalid key/value hex`);
    }
    return {
      __kind: "Leaf",
      prefix: candidate.prefix,
      key: candidate.key,
      value: candidate.value,
    };
  }
  if (
    candidate.__kind !== "Branch" ||
    !Array.isArray(candidate.children) ||
    candidate.children.length !== 16 ||
    candidate.children.some(
      (child) => child !== null && !/^[0-9a-f]{64}$/.test(String(child)),
    ) ||
    !Number.isSafeInteger(candidate.size) ||
    Number(candidate.size) < 0
  ) {
    throw new Error(`MPF branch ${hashHex} is invalid`);
  }
  return {
    __kind: "Branch",
    prefix: candidate.prefix,
    children: candidate.children.map((child) =>
      child === null ? null : String(child),
    ),
    size: Number(candidate.size),
  };
};

const encodeStoredNode = (hashHex: string, value: unknown): Buffer => {
  const node = assertStoredNode(value, hashHex);
  const prefix = Buffer.from(
    [...node.prefix].map((nibble) => Number.parseInt(nibble, 16)),
  );
  if (node.__kind === "Leaf") {
    const key = Buffer.from(node.key, "hex");
    const leafValue = Buffer.from(node.value, "hex");
    const output = Buffer.allocUnsafe(
      1 +
        HASH_BYTES +
        1 +
        prefix.length +
        2 +
        4 +
        key.length +
        leafValue.length,
    );
    let offset = 0;
    output.writeUInt8(1, offset++);
    Buffer.from(hashHex, "hex").copy(output, offset);
    offset += HASH_BYTES;
    output.writeUInt8(prefix.length, offset++);
    prefix.copy(output, offset);
    offset += prefix.length;
    output.writeUInt16LE(key.length, offset);
    offset += 2;
    output.writeUInt32LE(leafValue.length, offset);
    offset += 4;
    key.copy(output, offset);
    offset += key.length;
    leafValue.copy(output, offset);
    return output;
  }
  const children = node.children.filter(
    (child): child is string => child !== null,
  );
  const output = Buffer.allocUnsafe(
    1 + HASH_BYTES + 1 + prefix.length + 8 + 2 + children.length * HASH_BYTES,
  );
  let offset = 0;
  output.writeUInt8(2, offset++);
  Buffer.from(hashHex, "hex").copy(output, offset);
  offset += HASH_BYTES;
  output.writeUInt8(prefix.length, offset++);
  prefix.copy(output, offset);
  offset += prefix.length;
  output.writeBigUInt64LE(BigInt(node.size), offset);
  offset += 8;
  const bitmap = node.children.reduce(
    (bits, child, index) => bits | (child === null ? 0 : 1 << index),
    0,
  );
  output.writeUInt16LE(bitmap, offset);
  offset += 2;
  for (const child of children) {
    Buffer.from(child, "hex").copy(output, offset);
    offset += HASH_BYTES;
  }
  return output;
};

const makeFullIndexHeader = (marker: string, recordCount: number): Buffer => {
  const output = Buffer.alloc(FULL_INDEX_HEADER_BYTES);
  output.write("MEF6", 0, "ascii");
  output.writeUInt16LE(1, 4);
  output.writeUInt16LE(0, 6);
  output.writeUInt32LE(FULL_INDEX_MAX_RECORDS, 8);
  output.writeUInt32LE(NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents, 12);
  output.writeUInt32LE(NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps, 16);
  output.writeUInt32LE(FULL_INDEX_MAX_BYTES, 20);
  output.writeUInt32LE(FULL_INDEX_MAX_BYTES, 24);
  output.writeUInt32LE(recordCount, 28);
  output.writeUInt32LE(0, 32);
  output.writeUInt32LE(0, 36);
  Buffer.from(marker, "hex").copy(output, 40);
  return output;
};

const sidecarPathDigest = (levelPath: string): Buffer =>
  digest(
    Buffer.from("MIDGARD-MPF-OWNER-LEVEL-PATH-V1"),
    Buffer.from(resolve(levelPath)),
  );

const encodeSidecar = ({
  levelPath,
  marker,
  binarySha256,
  payload,
}: {
  readonly levelPath: string;
  readonly marker: string;
  readonly binarySha256: string;
  readonly payload: Buffer;
}): Buffer => {
  const pathDigest = sidecarPathDigest(levelPath);
  const binarySha = Buffer.from(binarySha256, "hex");
  const markerBytes = Buffer.from(marker, "hex");
  const header = Buffer.alloc(SIDECAR_HEADER_BYTES);
  header.write(SIDECAR_MAGIC, 0, "ascii");
  header.writeUInt16LE(1, 4);
  header.writeUInt16LE(0, 6);
  markerBytes.copy(header, 8);
  binarySha.copy(header, 40);
  pathDigest.copy(header, 72);
  header.writeBigUInt64LE(BigInt(payload.length), 104);
  digest(
    SIDECAR_DIGEST_DOMAIN,
    markerBytes,
    binarySha,
    pathDigest,
    payload,
  ).copy(header, 112);
  return Buffer.concat([header, payload]);
};

const decodeSidecar = ({
  bytes,
  levelPath,
  marker,
  binarySha256,
}: {
  readonly bytes: Buffer;
  readonly levelPath: string;
  readonly marker: string;
  readonly binarySha256: string;
}): Buffer => {
  if (
    bytes.length < SIDECAR_HEADER_BYTES + FULL_INDEX_HEADER_BYTES ||
    bytes.subarray(0, 4).toString("ascii") !== SIDECAR_MAGIC ||
    bytes.readUInt16LE(4) !== 1 ||
    bytes.readUInt16LE(6) !== 0
  ) {
    throw new Error("Native MPF sidecar header is invalid");
  }
  const markerBytes = Buffer.from(marker, "hex");
  const binarySha = Buffer.from(binarySha256, "hex");
  const pathDigest = sidecarPathDigest(levelPath);
  if (
    !timingSafeEqual(bytes.subarray(8, 40), markerBytes) ||
    !timingSafeEqual(bytes.subarray(40, 72), binarySha) ||
    !timingSafeEqual(bytes.subarray(72, 104), pathDigest)
  ) {
    throw new Error("Native MPF sidecar binding is stale");
  }
  const payloadLength = bytes.readBigUInt64LE(104);
  if (
    payloadLength > BigInt(FULL_INDEX_MAX_BYTES) ||
    payloadLength !== BigInt(bytes.length - SIDECAR_HEADER_BYTES)
  ) {
    throw new Error("Native MPF sidecar payload length is invalid");
  }
  const payload = Buffer.from(bytes.subarray(SIDECAR_HEADER_BYTES));
  const expected = digest(
    SIDECAR_DIGEST_DOMAIN,
    markerBytes,
    binarySha,
    pathDigest,
    payload,
  );
  if (!timingSafeEqual(bytes.subarray(112, 144), expected)) {
    throw new Error("Native MPF sidecar digest mismatch");
  }
  if (
    payload.subarray(0, 4).toString("ascii") !== "MEF6" ||
    payload.subarray(40, 72).toString("hex") !== marker
  ) {
    throw new Error("Native MPF sidecar full-index marker is invalid");
  }
  return payload;
};

const writeSidecarAtomic = async (
  path: string,
  bytes: Buffer,
): Promise<void> => {
  const temporary = `${path}.tmp-${process.pid.toString()}`;
  const file = await open(temporary, "w", 0o600);
  try {
    await file.writeFile(bytes);
    await file.sync();
  } finally {
    await file.close();
  }
  try {
    await rename(temporary, path);
  } catch (error) {
    await rm(temporary, { force: true }).catch(() => undefined);
    throw error;
  }
};

const walkReachableRecords = async (
  db: Level<string, StoredValue>,
  marker: string,
  visit: (hash: string, node: StoredNode) => Promise<void> | void,
): Promise<number> => {
  const seen = new Set<string>();
  const pending = [marker];
  let count = 0;
  while (pending.length > 0) {
    const batch = pending.splice(0, 4_096).filter((hash) => {
      if (seen.has(hash)) return false;
      seen.add(hash);
      return true;
    });
    if (batch.length === 0) continue;
    const values = await db.getMany(batch);
    for (let index = 0; index < batch.length; index += 1) {
      const hash = batch[index]!;
      const value = values[index];
      if (value === undefined) {
        throw new Error(`Native MPF durable closure is missing record ${hash}`);
      }
      const node = assertStoredNode(value, hash);
      await visit(hash, node);
      count += 1;
      if (node.__kind === "Branch") {
        for (const child of node.children) {
          if (child !== null && !seen.has(child)) pending.push(child);
        }
      }
    }
  }
  return count;
};

const buildOrReadFullIndex = async ({
  db,
  marker,
  options,
  binarySha256,
}: {
  readonly db: Level<string, StoredValue>;
  readonly marker: string;
  readonly options: NormalizedNativeMpfOwnerServiceOptions;
  readonly binarySha256: string;
}): Promise<Buffer> => {
  let fullIndex: Buffer | undefined;
  if (options.sidecarPath !== undefined) {
    try {
      fullIndex = decodeSidecar({
        bytes: await readFile(options.sidecarPath),
        levelPath: options.levelPath,
        marker,
        binarySha256,
      });
    } catch {
      fullIndex = undefined;
    }
  }
  let rebuiltSidecar = false;
  if (fullIndex === undefined) {
    const recordCount = await walkReachableRecords(db, marker, () => undefined);
    if (recordCount === 0 || recordCount > FULL_INDEX_MAX_RECORDS) {
      throw new Error(
        `Native MPF durable record count is invalid: ${recordCount.toString()}`,
      );
    }
    const records: Buffer[] = [];
    let totalBytes = FULL_INDEX_HEADER_BYTES;
    await walkReachableRecords(db, marker, (key, value) => {
      const encoded = encodeStoredNode(key, value);
      totalBytes += encoded.length;
      if (totalBytes > FULL_INDEX_MAX_BYTES) {
        throw new Error("Native MPF full index exceeds input cap");
      }
      records.push(encoded);
    });
    fullIndex = Buffer.concat(
      [makeFullIndexHeader(marker, recordCount), ...records],
      totalBytes,
    );
    rebuiltSidecar = options.sidecarPath !== undefined;
  }
  const recordCount = fullIndex.readUInt32LE(28);
  if (recordCount === 0 || recordCount > FULL_INDEX_MAX_RECORDS) {
    throw new Error(
      `Native MPF full-index record count is invalid: ${recordCount.toString()}`,
    );
  }
  if (rebuiltSidecar && options.sidecarPath !== undefined) {
    await writeSidecarAtomic(
      options.sidecarPath,
      encodeSidecar({
        levelPath: options.levelPath,
        marker,
        binarySha256,
        payload: fullIndex,
      }),
    );
  }
  return fullIndex;
};

const packedNibbles = (prefix: string): Buffer => {
  if (prefix.length % 2 !== 0) {
    throw new Error("MPF leaf tail must contain an even nibble count");
  }
  return Buffer.from(
    Array.from({ length: prefix.length / 2 }, (_, index) =>
      Number.parseInt(prefix.slice(index * 2, index * 2 + 2), 16),
    ),
  );
};

const hashStoredNode = (node: StoredNode): Buffer => {
  if (node.__kind === "Leaf") {
    const odd = node.prefix.length % 2 === 1;
    const head = odd
      ? Buffer.from([0, Number.parseInt(node.prefix[0]!, 16)])
      : Buffer.from([0xff]);
    const tail = packedNibbles(odd ? node.prefix.slice(1) : node.prefix);
    return digest(head, tail, digest(Buffer.from(node.value, "hex")));
  }
  let layer = node.children.map((child) =>
    child === null ? Buffer.alloc(HASH_BYTES) : Buffer.from(child, "hex"),
  );
  while (layer.length > 1) {
    layer = Array.from({ length: layer.length / 2 }, (_, index) =>
      digest(layer[index * 2]!, layer[index * 2 + 1]!),
    );
  }
  return digest(
    Buffer.from([...node.prefix].map((nibble) => Number.parseInt(nibble, 16))),
    layer[0]!,
  );
};

const parsePromotionRecords = (bytes: Buffer): DecodedPromotionRecord[] => {
  const records: DecodedPromotionRecord[] = [];
  let offset = 0;
  while (offset < bytes.length) {
    const start = offset;
    if (bytes.length - offset < 34) {
      throw new Error("Native MPF promotion record is truncated");
    }
    const kind = bytes.readUInt8(offset++);
    const hash = Buffer.from(bytes.subarray(offset, offset + HASH_BYTES));
    offset += HASH_BYTES;
    const prefixLength = bytes.readUInt8(offset++);
    if (offset + prefixLength > bytes.length) {
      throw new Error("Native MPF promotion prefix is truncated");
    }
    const prefixBytes = bytes.subarray(offset, offset + prefixLength);
    offset += prefixLength;
    if (prefixBytes.some((value) => value > 15)) {
      throw new Error("Native MPF promotion prefix is not canonical nibbles");
    }
    const prefix = [...prefixBytes].map((value) => value.toString(16)).join("");
    let stored: StoredNode;
    if (kind === 1) {
      if (offset + 6 > bytes.length) {
        throw new Error("Native MPF promotion leaf header is truncated");
      }
      const keyLength = bytes.readUInt16LE(offset);
      offset += 2;
      const valueLength = bytes.readUInt32LE(offset);
      offset += 4;
      const end = offset + keyLength + valueLength;
      if (end > bytes.length) {
        throw new Error("Native MPF promotion leaf payload is truncated");
      }
      const key = bytes.subarray(offset, offset + keyLength).toString("hex");
      offset += keyLength;
      const value = bytes.subarray(offset, end).toString("hex");
      offset = end;
      stored = { __kind: "Leaf", prefix, key, value };
    } else if (kind === 2) {
      if (offset + 10 > bytes.length) {
        throw new Error("Native MPF promotion branch header is truncated");
      }
      const size = bytes.readBigUInt64LE(offset);
      offset += 8;
      if (size > BigInt(Number.MAX_SAFE_INTEGER)) {
        throw new Error(
          "Native MPF promotion branch size exceeds safe integer",
        );
      }
      const bitmap = bytes.readUInt16LE(offset);
      offset += 2;
      const children: (string | null)[] = [];
      for (let branch = 0; branch < 16; branch += 1) {
        if ((bitmap & (1 << branch)) === 0) {
          children.push(null);
          continue;
        }
        if (offset + HASH_BYTES > bytes.length) {
          throw new Error("Native MPF promotion child hash is truncated");
        }
        children.push(
          bytes.subarray(offset, offset + HASH_BYTES).toString("hex"),
        );
        offset += HASH_BYTES;
      }
      stored = { __kind: "Branch", prefix, children, size: Number(size) };
    } else {
      throw new Error(
        `Unknown native MPF promotion record kind ${kind.toString()}`,
      );
    }
    const calculated = hashStoredNode(stored);
    if (!timingSafeEqual(hash, calculated)) {
      throw new Error(
        `Native MPF promotion record content hash mismatch: claimed=${hash.toString("hex")},calculated=${calculated.toString("hex")}`,
      );
    }
    records.push({
      hash,
      hashHex: hash.toString("hex"),
      encoded: Buffer.from(bytes.subarray(start, offset)),
      stored,
    });
  }
  return records;
};

const keyNibbles = (key: string): string =>
  [...digest(Buffer.from(key, "hex"))]
    .flatMap((byte) => [byte >> 4, byte & 0x0f])
    .map((value) => value.toString(16))
    .join("");

export const encodeNativeMpfEventLog = (
  baseRoot: string,
  events: readonly (readonly NativeMpfEventOp[])[],
): Buffer => {
  assertNativeMpfHashHex(baseRoot, "baseRoot");
  const opCount = events.reduce((total, event) => total + event.length, 0);
  if (events.length > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents) {
    throw new Error("Native MPF event count exceeds cap");
  }
  if (opCount > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps) {
    throw new Error("Native MPF operation count exceeds cap");
  }
  const bodyBytes = events.reduce(
    (total, event) =>
      total +
      4 +
      event.reduce(
        (eventTotal, op) =>
          eventTotal +
          7 +
          op.key.byteLength +
          (op.type === "insert" ? op.value.byteLength : 0),
        0,
      ),
    0,
  );
  const output = Buffer.allocUnsafe(EVENT_LOG_HEADER_BYTES + bodyBytes);
  output.write("MEGO", 0, "ascii");
  output.writeUInt16LE(1, 4);
  output.writeUInt16LE(0, 6);
  output.writeUInt32LE(events.length, 8);
  output.writeUInt32LE(opCount, 12);
  output.writeUInt32LE(NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents, 16);
  output.writeUInt32LE(NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps, 20);
  output.writeUInt32LE(FULL_INDEX_MAX_BYTES, 24);
  Buffer.from(baseRoot, "hex").copy(output, 28);
  output.fill(0, 60, EVENT_LOG_HEADER_BYTES);
  let offset = EVENT_LOG_HEADER_BYTES;
  for (const event of events) {
    output.writeUInt32LE(event.length, offset);
    offset += 4;
    for (const op of event) {
      const key = Buffer.from(op.key);
      const value =
        op.type === "insert" ? Buffer.from(op.value) : Buffer.alloc(0);
      output.writeUInt8(op.type === "insert" ? 1 : 2, offset++);
      output.writeUInt16LE(key.length, offset);
      offset += 2;
      output.writeUInt32LE(value.length, offset);
      offset += 4;
      key.copy(output, offset);
      offset += key.length;
      value.copy(output, offset);
      offset += value.length;
    }
  }
  digest(
    EVENT_STREAM_DIGEST_DOMAIN,
    output.subarray(8, 28),
    output.subarray(28, 60),
    output.subarray(EVENT_LOG_HEADER_BYTES),
  ).copy(output, 60);
  return output;
};

class NativeChildRpc {
  private readonly child: ChildProcessWithoutNullStreams;
  private readonly decoder: NativeMpfRpcFrameDecoder;
  private readonly pending = new Map<bigint, PendingRequest>();
  private readonly promotions = new Map<bigint, PendingPromotion>();
  private requestId = 0n;
  private ownerEpoch = ZERO_EPOCH;
  private stderr = "";
  private closed = false;
  private failureHandler: ((error: Error) => void) | undefined;

  public constructor(
    binaryPath: string,
    private readonly binarySha256: string,
    private readonly maxFrameBytes: number,
    private readonly timeoutMs: number,
    onChildSpawnForTests?: (pid: number) => void,
  ) {
    this.decoder = new NativeMpfRpcFrameDecoder(
      (chunks) => digest(...chunks),
      maxFrameBytes,
    );
    this.child = spawn(binaryPath, ["--rpc"], {
      stdio: ["pipe", "pipe", "pipe"],
    });
    if (this.child.pid !== undefined) onChildSpawnForTests?.(this.child.pid);
    this.child.stdout.on("data", (chunk: Buffer) => this.onData(chunk));
    this.child.stderr.on("data", (chunk: Buffer) => {
      this.stderr = (this.stderr + chunk.toString("utf8")).slice(-16_384);
    });
    this.child.once("error", (error) => this.failAll(error));
    this.child.once("exit", (code, signal) => {
      this.failAll(
        new Error(
          `Native MPF owner exited: code=${String(code)},signal=${String(signal)},stderr=${this.stderr}`,
        ),
      );
    });
  }

  public async handshake(): Promise<void> {
    const response = await this.request(
      NativeMpfRpcKind.Hello,
      Buffer.from(this.binarySha256, "hex"),
      new Set([NativeMpfRpcKind.HelloAck]),
      true,
      NATIVE_MPF_OWNER_DEFAULT_CAPS.handshakeTimeoutMs,
    );
    this.ownerEpoch = assertBufferLength(response.ownerEpoch, 16, "ownerEpoch");
    if (response.payload.byteLength !== 82) {
      throw new Error("Native MPF HelloAck payload length is invalid");
    }
    const payload = Buffer.from(response.payload);
    if (
      payload.readUInt16LE(0) !== NATIVE_MPF_RPC_SCHEMA ||
      payload.subarray(2, 34).toString("hex") !== this.binarySha256 ||
      payload.readUInt32LE(34) !== FULL_INDEX_MAX_RECORDS ||
      payload.readUInt32LE(38) !== NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents ||
      payload.readUInt32LE(42) !== NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps ||
      payload.readUInt32LE(46) !==
        NATIVE_MPF_OWNER_DEFAULT_CAPS.maxActiveGenerations ||
      !timingSafeEqual(payload.subarray(50), digest(SELF_TEST_DOMAIN))
    ) {
      throw new Error("Native MPF HelloAck capability/self-test mismatch");
    }
  }

  public get epoch(): Buffer {
    return Buffer.from(this.ownerEpoch);
  }

  public get isClosed(): boolean {
    return this.closed;
  }

  public setFailureHandler(handler: (error: Error) => void): void {
    this.failureHandler = handler;
  }

  public request(
    kind: NativeMpfRpcKind,
    payload: Uint8Array,
    expected: ReadonlySet<NativeMpfRpcKind>,
    zeroEpoch = false,
    timeoutMs = this.timeoutMs,
  ): Promise<NativeMpfRpcFrame> {
    if (this.closed)
      return Promise.reject(new Error("Native MPF owner is closed"));
    const requestId = ++this.requestId;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        const error = new Error(
          `Native MPF RPC request timed out: kind=${kind.toString()}`,
        );
        this.child.kill("SIGKILL");
        this.failAll(error);
      }, timeoutMs);
      this.pending.set(requestId, { expected, resolve, reject, timer });
      try {
        this.child.stdin.write(
          encodeNativeMpfRpcFrame(
            {
              schema: NATIVE_MPF_RPC_SCHEMA,
              kind,
              requestId,
              ownerEpoch: zeroEpoch ? ZERO_EPOCH : this.ownerEpoch,
              payload,
            },
            (chunks) => digest(...chunks),
            this.maxFrameBytes,
          ),
        );
      } catch (error) {
        clearTimeout(timer);
        this.pending.delete(requestId);
        reject(error instanceof Error ? error : new Error(String(error)));
      }
    });
  }

  public promotion(
    generationId: Uint8Array,
    timeoutMs: number,
  ): Promise<{ frame: NativeMpfRpcFrame; bytes: Buffer }> {
    if (this.closed)
      return Promise.reject(new Error("Native MPF owner is closed"));
    const requestId = ++this.requestId;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        const error = new Error("Native MPF promotion stream timed out");
        this.child.kill("SIGKILL");
        this.failAll(error);
      }, timeoutMs);
      this.promotions.set(requestId, { chunks: [], resolve, reject, timer });
      try {
        this.child.stdin.write(
          encodeNativeMpfRpcFrame(
            {
              schema: NATIVE_MPF_RPC_SCHEMA,
              kind: NativeMpfRpcKind.PreparePromotion,
              requestId,
              ownerEpoch: this.ownerEpoch,
              payload: generationId,
            },
            (chunks) => digest(...chunks),
            this.maxFrameBytes,
          ),
        );
      } catch (error) {
        clearTimeout(timer);
        this.promotions.delete(requestId);
        reject(error instanceof Error ? error : new Error(String(error)));
      }
    });
  }

  private onData(chunk: Buffer): void {
    try {
      for (const frame of this.decoder.push(chunk)) this.dispatch(frame);
    } catch (error) {
      this.child.kill("SIGKILL");
      this.failAll(error instanceof Error ? error : new Error(String(error)));
    }
  }

  private dispatch(frame: NativeMpfRpcFrame): void {
    if (!timingSafeEqual(Buffer.from(frame.ownerEpoch), this.ownerEpoch)) {
      if (
        !(
          this.ownerEpoch.equals(ZERO_EPOCH) &&
          frame.kind === NativeMpfRpcKind.HelloAck
        )
      ) {
        throw new Error("Native MPF RPC response owner epoch mismatch");
      }
    }
    const promotion = this.promotions.get(frame.requestId);
    if (promotion !== undefined) {
      if (frame.kind === NativeMpfRpcKind.PromotionChunk) {
        const total = promotion.chunks.reduce(
          (sum, value) => sum + value.length,
          0,
        );
        if (
          total + frame.payload.byteLength >
          NATIVE_MPF_OWNER_DEFAULT_CAPS.maxGeneratedBytes
        ) {
          throw new Error(
            "Native MPF promotion stream exceeds generated-byte cap",
          );
        }
        promotion.chunks.push(Buffer.from(frame.payload));
        return;
      }
      clearTimeout(promotion.timer);
      this.promotions.delete(frame.requestId);
      if (frame.kind === NativeMpfRpcKind.Error) {
        promotion.reject(
          new Error(Buffer.from(frame.payload).toString("utf8")),
        );
      } else if (frame.kind !== NativeMpfRpcKind.PromotionEnd) {
        promotion.reject(
          new Error(
            `Unexpected native MPF promotion response ${frame.kind.toString()}`,
          ),
        );
      } else {
        promotion.resolve({ frame, bytes: Buffer.concat(promotion.chunks) });
      }
      return;
    }
    const pending = this.pending.get(frame.requestId);
    if (pending === undefined) {
      throw new Error(
        `Native MPF RPC unsolicited response id=${frame.requestId.toString()}`,
      );
    }
    clearTimeout(pending.timer);
    this.pending.delete(frame.requestId);
    if (frame.kind === NativeMpfRpcKind.Error) {
      pending.reject(new Error(Buffer.from(frame.payload).toString("utf8")));
    } else if (!pending.expected.has(frame.kind)) {
      pending.reject(
        new Error(
          `Unexpected native MPF RPC response kind ${frame.kind.toString()}`,
        ),
      );
    } else {
      pending.resolve(frame);
    }
  }

  private failAll(error: Error): void {
    if (this.closed) return;
    this.closed = true;
    for (const pending of this.pending.values()) {
      clearTimeout(pending.timer);
      pending.reject(error);
    }
    this.pending.clear();
    for (const pending of this.promotions.values()) {
      clearTimeout(pending.timer);
      pending.reject(error);
    }
    this.promotions.clear();
    this.failureHandler?.(error);
  }

  public async close(): Promise<void> {
    if (this.closed) return;
    await this.request(
      NativeMpfRpcKind.Shutdown,
      Buffer.alloc(0),
      new Set([NativeMpfRpcKind.ShutdownAck]),
      false,
      NATIVE_MPF_OWNER_DEFAULT_CAPS.shutdownTimeoutMs,
    );
    this.closed = true;
    this.child.stdin.end();
  }
}

const startNativeChild = async ({
  options,
  fullIndex,
  marker,
}: {
  readonly options: NormalizedNativeMpfOwnerServiceOptions;
  readonly fullIndex: Buffer;
  readonly marker: string;
}): Promise<NativeChildRpc> => {
  const rpc = new NativeChildRpc(
    options.binaryPath,
    options.binarySha256,
    options.maxFrameBytes,
    options.requestTimeoutMs,
    options.onChildSpawnForTests,
  );
  try {
    await rpc.handshake();
    const header = fullIndex.subarray(0, FULL_INDEX_HEADER_BYTES);
    await rpc.request(
      NativeMpfRpcKind.LoadBegin,
      header,
      new Set([NativeMpfRpcKind.LoadBegin]),
    );
    for (
      let offset = FULL_INDEX_HEADER_BYTES;
      offset < fullIndex.length;
      offset += options.maxChunkBytes
    ) {
      await rpc.request(
        NativeMpfRpcKind.LoadChunk,
        fullIndex.subarray(
          offset,
          Math.min(fullIndex.length, offset + options.maxChunkBytes),
        ),
        new Set([NativeMpfRpcKind.LoadChunk]),
      );
    }
    const ready = await rpc.request(
      NativeMpfRpcKind.LoadEnd,
      digest(LOAD_DIGEST_DOMAIN, fullIndex),
      new Set([NativeMpfRpcKind.Ready]),
    );
    const readyPayload = Buffer.from(ready.payload);
    if (
      readyPayload.length !== 72 ||
      readyPayload.subarray(0, HASH_BYTES).toString("hex") !== marker
    ) {
      throw new Error("Native MPF Ready marker/diagnostics are invalid");
    }
    const readyValue = (offset: number): number => {
      const value = readyPayload.readBigUInt64LE(offset);
      if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
        throw new Error("Native MPF Ready diagnostic exceeds safe integer");
      }
      return Number(value);
    };
    const readyNodes = readyValue(32);
    const readyResidentBytes = readyValue(48);
    const readyRssBytes = readyValue(56) * 1024;
    const readyPeakRssBytes = readyValue(64) * 1024;
    if (
      readyNodes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentNodes ||
      readyResidentBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes ||
      readyRssBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes ||
      readyPeakRssBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes
    ) {
      throw new Error(
        `Native MPF Ready cap exceeded: nodes=${readyNodes.toString()},resident_bytes=${readyResidentBytes.toString()},rss_bytes=${readyRssBytes.toString()},peak_rss_bytes=${readyPeakRssBytes.toString()}`,
      );
    }
    return rpc;
  } catch (error) {
    await rpc.close().catch(() => undefined);
    throw error;
  }
};

export class ProductionNativeMpfOwnerService implements NativeMpfOwnerService {
  private readonly workerPorts = new Set<MessagePort>();
  private readonly workerPortLastRequestId = new Map<MessagePort, number>();
  private readonly workerGenerationLeases = new Map<
    string,
    WorkerGenerationLease
  >();
  private childRestarts = 0;
  private restartAttempts = 0;
  private restartPromise: Promise<void> | undefined;
  private closing = false;
  private lastChildError: Error | undefined;

  private constructor(
    private readonly db: Level<string, StoredValue>,
    private rpc: NativeChildRpc,
    private readonly binarySha256: string,
    private durableRoot: string,
    private readonly options: NormalizedNativeMpfOwnerServiceOptions,
  ) {}

  public static async create(
    options: NativeMpfOwnerServiceOptions,
  ): Promise<ProductionNativeMpfOwnerService> {
    await prepareEventFlatDigest();
    const normalized = normalizeOwnerOptions(options);
    assertNativeOwnerRuntimeMemoryBudget({
      cgroup: await readCgroupMemoryBudget(),
      v8HeapLimitBytes: getHeapStatistics().heap_size_limit,
    });
    assertNativeMpfHashHex(normalized.binarySha256, "binarySha256");
    const actualSha = createHash("sha256")
      .update(await readFile(normalized.binaryPath))
      .digest("hex");
    if (actualSha !== normalized.binarySha256) {
      throw new Error(
        `Native MPF owner binary SHA-256 mismatch: expected=${normalized.binarySha256},actual=${actualSha}`,
      );
    }
    const db = new Level<string, StoredValue>(normalized.levelPath, {
      valueEncoding: "json",
    });
    await db.open();
    let rpc: NativeChildRpc | undefined;
    try {
      const marker = assertStoredHash(await db.get("__root__"), "durableRoot");
      const fullIndex = await buildOrReadFullIndex({
        db,
        marker,
        options: normalized,
        binarySha256: actualSha,
      });
      rpc = await startNativeChild({ options: normalized, fullIndex, marker });
      const service = new ProductionNativeMpfOwnerService(
        db,
        rpc,
        actualSha,
        marker,
        normalized,
      );
      service.installFailureHandler(rpc);
      return service;
    } catch (error) {
      await rpc?.close().catch(() => undefined);
      await db.close();
      throw error;
    }
  }

  public async fork(baseRoot: string): Promise<NativeMpfGenerationHandle> {
    const rpc = await this.ensureRpc();
    assertNativeMpfHashHex(baseRoot, "baseRoot");
    if (baseRoot !== this.durableRoot) {
      throw new Error(
        `Native MPF fork base is stale: requested=${baseRoot},durable=${this.durableRoot}`,
      );
    }
    const response = await rpc.request(
      NativeMpfRpcKind.Fork,
      Buffer.from(baseRoot, "hex"),
      new Set([NativeMpfRpcKind.Forked]),
    );
    const payload = Buffer.from(response.payload);
    if (
      payload.length !== 48 ||
      payload.subarray(16).toString("hex") !== baseRoot
    ) {
      throw new Error("Native MPF Forked payload is invalid");
    }
    return {
      ownerEpoch: rpc.epoch,
      generationId: Buffer.from(payload.subarray(0, 16)),
      baseRoot,
    };
  }

  public async applyEvents(
    handle: NativeMpfGenerationHandle,
    eventLog: Uint8Array,
  ): Promise<NativeMpfApplyResult> {
    const rpc = await this.ensureRpc();
    this.assertOwnedHandle(handle, rpc);
    const log = Buffer.from(eventLog);
    const eventCount = this.validateEventLog(handle.baseRoot, log);
    const response = await rpc.request(
      NativeMpfRpcKind.ApplyEvents,
      Buffer.concat([Buffer.from(handle.generationId), log]),
      new Set([NativeMpfRpcKind.Applied]),
      false,
      NATIVE_MPF_OWNER_DEFAULT_CAPS.applyTimeoutMs,
    );
    const payload = Buffer.from(response.payload);
    const rootsEnd = 84 + eventCount * HASH_BYTES;
    const expectedBytes = rootsEnd + 16;
    if (
      payload.length !== expectedBytes ||
      !timingSafeEqual(
        payload.subarray(0, 16),
        Buffer.from(handle.generationId),
      ) ||
      payload.readUInt32LE(80) !== eventCount
    ) {
      throw new Error("Native MPF Applied payload count/handle is invalid");
    }
    const candidateRoot = payload.subarray(16, 48).toString("hex");
    assertNativeMpfHashHex(candidateRoot, "candidateRoot");
    const expectedDigest = digest(EVENT_LOG_DIGEST_DOMAIN, log);
    if (!timingSafeEqual(payload.subarray(48, 80), expectedDigest)) {
      throw new Error("Native MPF Applied event-log digest mismatch");
    }
    const readDurationNs = (offset: number, field: string): number => {
      const value = payload.readBigUInt64LE(offset);
      if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
        throw new Error(`Native MPF Applied ${field} exceeds safe integer`);
      }
      return Number(value);
    };
    return {
      handle,
      candidateRoot,
      eventLogDigest: expectedDigest.toString("hex"),
      eventRoots: Array.from({ length: eventCount }, (_, index) =>
        payload
          .subarray(84 + index * HASH_BYTES, 84 + (index + 1) * HASH_BYTES)
          .toString("hex"),
      ),
      proofArenaDurationNs: readDurationNs(rootsEnd, "proofArenaDurationNs"),
      mutationDurationNs: readDurationNs(rootsEnd + 8, "mutationDurationNs"),
    };
  }

  public async discard(handle: NativeMpfGenerationHandle): Promise<void> {
    const rpc = await this.ensureRpc();
    this.assertOwnedHandle(handle, rpc);
    const response = await rpc.request(
      NativeMpfRpcKind.Discard,
      handle.generationId,
      new Set([NativeMpfRpcKind.Discarded]),
    );
    if (
      !timingSafeEqual(
        Buffer.from(response.payload),
        Buffer.from(handle.generationId),
      )
    ) {
      throw new Error("Native MPF Discarded handle mismatch");
    }
    this.workerGenerationLeases.delete(this.generationKey(handle));
  }

  public async promote(handle: NativeMpfGenerationHandle): Promise<void> {
    const rpc = await this.ensureRpc();
    this.assertOwnedHandle(handle, rpc);
    if (handle.baseRoot !== this.durableRoot) {
      throw new Error("Native MPF promotion base is stale");
    }
    const { frame, bytes } = await rpc.promotion(
      handle.generationId,
      NATIVE_MPF_OWNER_DEFAULT_CAPS.promotionTimeoutMs,
    );
    const payload = Buffer.from(frame.payload);
    if (
      payload.length !== 116 ||
      !timingSafeEqual(
        payload.subarray(0, 16),
        Buffer.from(handle.generationId),
      ) ||
      payload.subarray(16, 48).toString("hex") !== handle.baseRoot
    ) {
      throw new Error("Native MPF PromotionEnd handle/base is invalid");
    }
    const candidateRoot = payload.subarray(48, 80).toString("hex");
    const recordCount = payload.readUInt32LE(80);
    const records = parsePromotionRecords(bytes);
    if (
      records.length !== recordCount ||
      recordCount > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxGeneratedNodes
    ) {
      throw new Error("Native MPF promotion record count mismatch/cap breach");
    }
    for (let index = 1; index < records.length; index += 1) {
      if (Buffer.compare(records[index - 1]!.hash, records[index]!.hash) >= 0) {
        throw new Error(
          "Native MPF promotion records are not uniquely hash-sorted",
        );
      }
    }
    const aggregate = createEventFlatDigest();
    aggregate
      .update(PROMOTION_DIGEST_DOMAIN)
      .update(Buffer.from(handle.baseRoot, "hex"))
      .update(Buffer.from(candidateRoot, "hex"));
    for (const record of records) aggregate.update(record.encoded);
    if (!timingSafeEqual(aggregate.digest(), payload.subarray(84, 116))) {
      throw new Error("Native MPF promotion aggregate digest mismatch");
    }
    const marker = assertStoredHash(
      await this.db.get("__root__"),
      "durableRoot",
    );
    if (marker !== handle.baseRoot) {
      throw new Error(
        `Native MPF durable marker changed before promotion: expected=${handle.baseRoot},actual=${marker}`,
      );
    }
    await this.validatePromotionClosure(
      candidateRoot,
      handle.baseRoot,
      records,
    );
    await this.options.faultInjectionForTests?.("before_promotion_batch");
    await this.db.batch([
      ...records.map((record) => ({
        type: "put" as const,
        key: record.hashHex,
        value: record.stored,
      })),
      { type: "put" as const, key: "__root__", value: candidateRoot },
    ]);
    await this.options.faultInjectionForTests?.(
      "after_promotion_batch_before_ack",
    );
    const committed = await rpc.request(
      NativeMpfRpcKind.PromotionCommitted,
      handle.generationId,
      new Set([NativeMpfRpcKind.PromotionCommitted]),
    );
    if (Buffer.from(committed.payload).toString("hex") !== candidateRoot) {
      throw new Error("Native MPF PromotionCommitted root mismatch");
    }
    this.durableRoot = candidateRoot;
    this.workerGenerationLeases.delete(this.generationKey(handle));
  }

  public async recover(replay: PersistedNativeMpfReplay): Promise<void> {
    if (replay.schema !== NATIVE_MPF_RPC_SCHEMA) {
      throw new Error("Native MPF replay schema mismatch");
    }
    if (replay.ownerBinarySha256 !== this.binarySha256) {
      throw new Error("Native MPF replay binary SHA-256 mismatch");
    }
    const log = Buffer.from(replay.eventLog);
    const digestHex = digest(EVENT_LOG_DIGEST_DOMAIN, log).toString("hex");
    if (digestHex !== replay.eventLogDigest) {
      throw new Error("Native MPF replay event-log digest mismatch");
    }
    if (replay.baseRoot !== this.durableRoot) {
      if (replay.candidateRoot === this.durableRoot) return;
      throw new Error("Native MPF replay marker is neither base nor candidate");
    }
    const handle = await this.fork(replay.baseRoot);
    try {
      const applied = await this.applyEvents(handle, log);
      if (
        applied.candidateRoot !== replay.candidateRoot ||
        applied.eventRoots.length !== replay.eventCount ||
        !timingSafeEqual(
          Buffer.from(applied.eventRoots.join(""), "hex"),
          Buffer.from(replay.eventRoots),
        )
      ) {
        throw new Error(
          "Native MPF replay roots diverged from durable journal",
        );
      }
      await this.promote(handle);
    } catch (error) {
      await this.discard(handle).catch(() => undefined);
      throw error;
    }
  }

  public async diagnostics(): Promise<NativeMpfOwnerDiagnostics> {
    const rpc = await this.ensureRpc();
    const response = await rpc.request(
      NativeMpfRpcKind.Diagnostics,
      Buffer.alloc(0),
      new Set([NativeMpfRpcKind.DiagnosticsResult]),
    );
    const payload = Buffer.from(response.payload);
    if (payload.length !== 96) {
      throw new Error("Native MPF diagnostics payload length is invalid");
    }
    const value = (offset: number): number => {
      const result = payload.readBigUInt64LE(offset);
      if (result > BigInt(Number.MAX_SAFE_INTEGER)) {
        throw new Error("Native MPF diagnostic exceeds safe integer");
      }
      return Number(result);
    };
    const diagnostics = {
      ownerEpoch: rpc.epoch,
      durableRoot: payload.subarray(0, 32).toString("hex"),
      residentNodes: value(32),
      residentEdges: value(40),
      residentBytes: value(48),
      activeGenerations: value(56),
      generatedNodes: value(64),
      generatedBytes: value(72),
      rssBytes: value(80) * 1024,
      peakRssBytes: value(88) * 1024,
      childRestarts: this.childRestarts,
    };
    if (
      diagnostics.residentNodes >
        NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentNodes ||
      diagnostics.residentBytes >
        NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes ||
      diagnostics.rssBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes ||
      diagnostics.peakRssBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes
    ) {
      throw new Error(
        `Native MPF diagnostics cap exceeded: nodes=${diagnostics.residentNodes.toString()},resident_bytes=${diagnostics.residentBytes.toString()},rss_bytes=${diagnostics.rssBytes.toString()},peak_rss_bytes=${diagnostics.peakRssBytes.toString()}`,
      );
    }
    return diagnostics;
  }

  public createWorkerPort(): MessagePort {
    const channel = new MessageChannel();
    this.workerPorts.add(channel.port1);
    this.workerPortLastRequestId.set(channel.port1, 0);
    channel.port1.on("message", (message: unknown) => {
      void this.handleWorkerMessage(channel.port1, message);
    });
    channel.port1.once("close", () => {
      this.workerPorts.delete(channel.port1);
      this.workerPortLastRequestId.delete(channel.port1);
      void this.releaseWorkerLeases(channel.port1);
    });
    channel.port1.start();
    return channel.port2;
  }

  public async close(): Promise<void> {
    this.closing = true;
    for (const port of this.workerPorts) port.close();
    this.workerPorts.clear();
    this.workerPortLastRequestId.clear();
    this.workerGenerationLeases.clear();
    await this.restartPromise?.catch(() => undefined);
    await this.rpc.close();
    await this.db.close();
  }

  private installFailureHandler(rpc: NativeChildRpc): void {
    rpc.setFailureHandler((error) => {
      this.lastChildError = error;
      this.workerGenerationLeases.clear();
      void this.scheduleRestart(error).catch(() => undefined);
    });
  }

  private scheduleRestart(error: Error): Promise<void> {
    if (this.closing) {
      return Promise.reject(new Error("Native MPF owner service is closing"));
    }
    if (this.restartPromise !== undefined) return this.restartPromise;
    if (this.restartAttempts >= this.options.restartLimit) {
      return Promise.reject(
        new Error(
          `Native MPF owner restart limit exhausted after ${this.restartAttempts.toString()} attempt(s): ${error.message}`,
        ),
      );
    }
    this.restartAttempts += 1;
    const restart = this.restartChild();
    this.restartPromise = restart;
    void restart.then(
      () => {
        if (this.restartPromise === restart) this.restartPromise = undefined;
      },
      (restartError: unknown) => {
        this.lastChildError =
          restartError instanceof Error
            ? restartError
            : new Error(String(restartError));
        if (this.restartPromise === restart) this.restartPromise = undefined;
      },
    );
    return restart;
  }

  private async restartChild(): Promise<void> {
    const marker = assertStoredHash(
      await this.db.get("__root__"),
      "durableRoot",
    );
    const fullIndex = await buildOrReadFullIndex({
      db: this.db,
      marker,
      options: this.options,
      binarySha256: this.binarySha256,
    });
    const rpc = await startNativeChild({
      options: this.options,
      fullIndex,
      marker,
    });
    if (this.closing) {
      await rpc.close().catch(() => undefined);
      throw new Error("Native MPF owner service closed during child restart");
    }
    this.rpc = rpc;
    this.durableRoot = marker;
    this.childRestarts += 1;
    this.installFailureHandler(rpc);
  }

  private async ensureRpc(): Promise<NativeChildRpc> {
    if (this.closing) throw new Error("Native MPF owner service is closed");
    if (!this.rpc.isClosed) return this.rpc;
    await this.scheduleRestart(
      this.lastChildError ?? new Error("Native MPF owner child is unavailable"),
    );
    if (this.rpc.isClosed) {
      throw this.lastChildError ?? new Error("Native MPF owner restart failed");
    }
    return this.rpc;
  }

  private assertOwnedHandle(
    handle: NativeMpfGenerationHandle,
    rpc: NativeChildRpc,
  ): void {
    assertNativeMpfGenerationHandle(handle);
    if (!timingSafeEqual(Buffer.from(handle.ownerEpoch), rpc.epoch)) {
      throw new Error(
        "Native MPF generation handle belongs to a stale owner epoch",
      );
    }
  }

  private validateEventLog(baseRoot: string, log: Buffer): number {
    if (
      log.length < EVENT_LOG_HEADER_BYTES ||
      log.subarray(0, 4).toString("ascii") !== "MEGO" ||
      log.readUInt16LE(4) !== 1 ||
      log.readUInt16LE(6) !== 0 ||
      log.subarray(28, 60).toString("hex") !== baseRoot
    ) {
      throw new Error("Native MPF event log header/base is invalid");
    }
    const eventCount = log.readUInt32LE(8);
    const opCount = log.readUInt32LE(12);
    if (
      eventCount > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents ||
      opCount > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps ||
      log.length > FULL_INDEX_MAX_BYTES
    ) {
      throw new Error("Native MPF event log cap exceeded");
    }
    const expected = digest(
      EVENT_STREAM_DIGEST_DOMAIN,
      log.subarray(8, 28),
      log.subarray(28, 60),
      log.subarray(EVENT_LOG_HEADER_BYTES),
    );
    if (!timingSafeEqual(expected, log.subarray(60, EVENT_LOG_HEADER_BYTES))) {
      throw new Error("Native MPF event log digest mismatch");
    }
    return eventCount;
  }

  private async validatePromotionClosure(
    candidateRoot: string,
    baseRoot: string,
    records: readonly DecodedPromotionRecord[],
  ): Promise<void> {
    assertNativeMpfHashHex(candidateRoot, "candidateRoot");
    const byHash = new Map(records.map((record) => [record.hashHex, record]));
    if (candidateRoot === baseRoot) {
      if (records.length !== 0) {
        throw new Error(
          "No-op native MPF promotion returned generated records",
        );
      }
      return;
    }
    if (!byHash.has(candidateRoot)) {
      throw new Error("Native MPF promotion closure is missing candidate root");
    }
    const visited = new Set<string>();
    const visit = async (hash: string, path: string): Promise<void> => {
      const record = byHash.get(hash);
      if (record === undefined) {
        try {
          assertStoredNode(await this.db.get(hash), hash);
          return;
        } catch (error) {
          throw new Error(
            `Native MPF promotion references missing durable child ${hash}: ${String(error)}`,
          );
        }
      }
      if (visited.has(hash)) return;
      visited.add(hash);
      const nodePath = path + record.stored.prefix;
      if (record.stored.__kind === "Leaf") {
        if (keyNibbles(record.stored.key) !== nodePath) {
          throw new Error(
            `Native MPF promoted leaf ${hash} is linked at the wrong path`,
          );
        }
        return;
      }
      await Promise.all(
        record.stored.children.map((child, branch) =>
          child === null
            ? undefined
            : visit(child, nodePath + branch.toString(16)),
        ),
      );
    };
    await visit(candidateRoot, "");
    if (visited.size !== records.length) {
      throw new Error(
        `Native MPF promotion contains unreachable generated records: reachable=${visited.size.toString()},records=${records.length.toString()}`,
      );
    }
  }

  private async handleWorkerMessage(
    port: MessagePort,
    message: unknown,
  ): Promise<void> {
    if (
      typeof message !== "object" ||
      message === null ||
      !("requestId" in message) ||
      !("method" in message)
    ) {
      port.close();
      return;
    }
    const request = message as Record<string, unknown>;
    if (!Number.isSafeInteger(request.requestId)) {
      port.close();
      return;
    }
    const requestId = Number(request.requestId);
    const lastRequestId = this.workerPortLastRequestId.get(port);
    if (lastRequestId === undefined || requestId <= lastRequestId) {
      port.close();
      return;
    }
    this.workerPortLastRequestId.set(port, requestId);
    try {
      let value: unknown;
      if (request.method === "fork") {
        const handle = await this.fork(String(request.baseRoot));
        if (!this.workerPorts.has(port)) {
          await this.discard(handle).catch(() => undefined);
          return;
        }
        this.workerGenerationLeases.set(this.generationKey(handle), {
          port,
          handle,
          journalOwned: false,
        });
        value = handle;
      } else if (request.method === "applyEvents") {
        this.assertWorkerLease(
          port,
          request.handle as NativeMpfGenerationHandle,
        );
        value = await this.applyEvents(
          request.handle as NativeMpfGenerationHandle,
          request.eventLog as Uint8Array,
        );
      } else if (request.method === "discard") {
        this.assertWorkerLease(
          port,
          request.handle as NativeMpfGenerationHandle,
        );
        await this.discard(request.handle as NativeMpfGenerationHandle);
        value = undefined;
      } else if (request.method === "retainForJournal") {
        const handle = request.handle as NativeMpfGenerationHandle;
        const lease = this.assertWorkerLease(port, handle);
        lease.journalOwned = true;
        value = undefined;
      } else {
        throw new Error(
          `Unknown native MPF worker method ${String(request.method)}`,
        );
      }
      port.postMessage({ requestId: request.requestId, ok: true, value });
    } catch (error) {
      port.postMessage({
        requestId: request.requestId,
        ok: false,
        error: error instanceof Error ? error.message : String(error),
      });
    }
  }

  private generationKey(handle: NativeMpfGenerationHandle): string {
    assertNativeMpfGenerationHandle(handle);
    return `${Buffer.from(handle.ownerEpoch).toString("hex")}:${Buffer.from(handle.generationId).toString("hex")}`;
  }

  private assertWorkerLease(
    port: MessagePort,
    handle: NativeMpfGenerationHandle,
  ): WorkerGenerationLease {
    const lease = this.workerGenerationLeases.get(this.generationKey(handle));
    if (lease === undefined || lease.port !== port) {
      throw new Error(
        "Native MPF generation is not leased to this worker port",
      );
    }
    return lease;
  }

  private async releaseWorkerLeases(port: MessagePort): Promise<void> {
    const abandoned: NativeMpfGenerationHandle[] = [];
    for (const [key, lease] of this.workerGenerationLeases) {
      if (lease.port !== port || lease.journalOwned) continue;
      this.workerGenerationLeases.delete(key);
      abandoned.push(lease.handle);
    }
    await Promise.all(
      abandoned.map((handle) => this.discard(handle).catch(() => undefined)),
    );
  }
}
