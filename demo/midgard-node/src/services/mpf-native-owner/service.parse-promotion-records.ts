import { timingSafeEqual } from "node:crypto";
import { readFile } from "node:fs/promises";

import { Level } from "level";

import {
  assertNativeMpfHashHex,
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  NativeMpfFullIndexCapExceeded,
  NativeMpfRestoreReadFailed,
  NativeMpfRootNotRetained,
} from "./protocol.js";
import {
  decodeSidecar,
  encodeSidecar,
  encodeStoredNode,
  makeFullIndexHeader,
  NativeMpfClosureIncomplete,
  NativeMpfFullIndexOverCap,
  walkReachableRecords,
  writeSidecarAtomic,
} from "./service.encode-stored-node.js";
import {
  type DecodedPromotionRecord,
  digest,
  EMPTY_ROOT_HEX,
  EVENT_LOG_HEADER_BYTES,
  EVENT_STREAM_DIGEST_DOMAIN,
  FULL_INDEX_HEADER_BYTES,
  FULL_INDEX_MAX_BYTES,
  FULL_INDEX_MAX_RECORDS,
  HASH_BYTES,
  type NativeMpfEventOp,
  type NormalizedNativeMpfOwnerServiceOptions,
  type StoredNode,
  type StoredValue,
} from "./service.normalize-owner-options.js";

/**
 * The full index of `marker`'s node closure: the sidecar's, when one is
 * configured and binds this marker, or else one built from the closure in
 * the store. Throws `NativeMpfClosureIncomplete` when the store does not hold
 * the closure in full, and `NativeMpfFullIndexOverCap` when it does but
 * the index is over `FULL_INDEX_MAX_RECORDS` or `FULL_INDEX_MAX_BYTES`; a
 * store read that fails otherwise propagates unchanged.
 */
export const buildOrReadFullIndex = async ({
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
    if (recordCount === 0 && marker !== EMPTY_ROOT_HEX)
      throw new NativeMpfClosureIncomplete(
        `Native MPF durable closure of ${marker} has no records`,
      );
    if (recordCount > FULL_INDEX_MAX_RECORDS)
      throw new NativeMpfFullIndexOverCap(
        marker,
        "FULL_INDEX_MAX_RECORDS",
        FULL_INDEX_MAX_RECORDS,
        recordCount,
      );
    const records: Buffer[] = [];
    let totalBytes = FULL_INDEX_HEADER_BYTES;
    await walkReachableRecords(db, marker, (key, value) => {
      const encoded = encodeStoredNode(key, value);
      totalBytes += encoded.length;
      if (totalBytes > FULL_INDEX_MAX_BYTES)
        throw new NativeMpfFullIndexOverCap(
          marker,
          "FULL_INDEX_MAX_BYTES",
          FULL_INDEX_MAX_BYTES,
          totalBytes,
        );
      records.push(encoded);
    });
    fullIndex = Buffer.concat(
      [makeFullIndexHeader(marker, recordCount), ...records],
      totalBytes,
    );
    rebuiltSidecar = options.sidecarPath !== undefined;
  }
  const recordCount = fullIndex.readUInt32LE(28);
  if (recordCount > FULL_INDEX_MAX_RECORDS)
    throw new NativeMpfFullIndexOverCap(
      marker,
      "FULL_INDEX_MAX_RECORDS",
      FULL_INDEX_MAX_RECORDS,
      recordCount,
    );
  if (recordCount === 0 && marker !== EMPTY_ROOT_HEX) {
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

/** Why a canonical restore cannot load `targetRoot`'s full index, by cause:
 * its closure is not all in the store (`NativeMpfRootNotRetained`), it is
 * but its index is over a full-index cap (`NativeMpfFullIndexCapExceeded`),
 * or a store read failed (`NativeMpfRestoreReadFailed`, which a later
 * restore retries). */
export const restoreIndexRefusal = (targetRoot: string, cause: unknown) =>
  cause instanceof NativeMpfClosureIncomplete
    ? new NativeMpfRootNotRetained(targetRoot, { cause })
    : cause instanceof NativeMpfFullIndexOverCap
      ? new NativeMpfFullIndexCapExceeded(
          targetRoot,
          cause.cap,
          cause.limit,
          cause.observed,
          { cause },
        )
      : new NativeMpfRestoreReadFailed(targetRoot, { cause });

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
      ? Buffer.from([0x10, Number.parseInt(node.prefix[0]!, 16)])
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

export const parsePromotionRecords = (
  bytes: Buffer,
): DecodedPromotionRecord[] => {
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

export const keyNibbles = (key: string): string =>
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
