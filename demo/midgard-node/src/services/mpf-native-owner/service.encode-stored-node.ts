import { timingSafeEqual } from "node:crypto";
import { open, rename, rm } from "node:fs/promises";
import { resolve } from "node:path";

import { Level } from "level";

import {
  fullIndexCapMessage,
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  type NativeMpfFullIndexCap,
} from "./protocol.js";
import {
  digest,
  EMPTY_ROOT_HEX,
  FULL_INDEX_HEADER_BYTES,
  FULL_INDEX_MAX_BYTES,
  FULL_INDEX_MAX_RECORDS,
  HASH_BYTES,
  SIDECAR_DIGEST_DOMAIN,
  SIDECAR_HEADER_BYTES,
  SIDECAR_MAGIC,
  type StoredNode,
  type StoredValue,
} from "./service.normalize-owner-options.js";

export const assertStoredNode = (
  value: unknown,
  hashHex: string,
): StoredNode => {
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

export const encodeStoredNode = (hashHex: string, value: unknown): Buffer => {
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

export const makeFullIndexHeader = (
  marker: string,
  recordCount: number,
): Buffer => {
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

export const encodeSidecar = ({
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

export const decodeSidecar = ({
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

export const writeSidecarAtomic = async (
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

/** The node closure a walk started from is not all in the store: a record it
 * reaches is absent, or is not a well-formed node (`LEVEL_CORRUPTION` and
 * `LEVEL_DECODE_ERROR` reads count as not well-formed). A store that does
 * not hold a root's closure in full raises this and nothing else; every
 * other read failure keeps its own error. */
export class NativeMpfClosureIncomplete extends Error {
  readonly _tag = "NativeMpfClosureIncomplete";
}

/** The node closure of `root` is in the store, but its full index exceeds
 * `cap`, whose configured value is `limit`. */
export class NativeMpfFullIndexOverCap extends Error {
  readonly _tag = "NativeMpfFullIndexOverCap";
  constructor(
    readonly root: string,
    readonly cap: NativeMpfFullIndexCap,
    readonly limit: number,
    readonly observed: number,
  ) {
    super(fullIndexCapMessage(root, cap, limit, observed));
  }
}

const unreadableRecordCodes = new Set([
  "LEVEL_CORRUPTION",
  "LEVEL_DECODE_ERROR",
]);

const readRecords = async (
  db: Level<string, StoredValue>,
  batch: string[],
): Promise<(StoredValue | undefined)[]> => {
  try {
    return await db.getMany(batch);
  } catch (cause) {
    const code = (cause as { code?: unknown } | null)?.code;
    if (typeof code === "string" && unreadableRecordCodes.has(code))
      throw new NativeMpfClosureIncomplete(
        `Native MPF durable closure has an unreadable record (${code})`,
        { cause },
      );
    throw cause;
  }
};

/** Visits every record `marker` reaches, once each, and returns how many.
 * Throws `NativeMpfClosureIncomplete` when the closure is not all in the
 * store; a store read that fails otherwise, or a `visit` that throws,
 * propagates unchanged. */
export const walkReachableRecords = async (
  db: Level<string, StoredValue>,
  marker: string,
  visit: (hash: string, node: StoredNode) => Promise<void> | void,
): Promise<number> => {
  const seen = new Set<string>();
  const pending = marker === EMPTY_ROOT_HEX ? [] : [marker];
  let count = 0;
  while (pending.length > 0) {
    const batch = pending.splice(0, 4_096).filter((hash) => {
      if (seen.has(hash)) return false;
      seen.add(hash);
      return true;
    });
    if (batch.length === 0) continue;
    const values = await readRecords(db, batch);
    for (let index = 0; index < batch.length; index += 1) {
      const hash = batch[index]!;
      const value = values[index];
      if (value === undefined) {
        throw new NativeMpfClosureIncomplete(
          `Native MPF durable closure is missing record ${hash}`,
        );
      }
      let node: StoredNode;
      try {
        node = assertStoredNode(value, hash);
      } catch (cause) {
        throw new NativeMpfClosureIncomplete(
          `Native MPF durable closure has a malformed record ${hash}`,
          { cause },
        );
      }
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
