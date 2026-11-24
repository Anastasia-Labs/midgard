import { LedgerNotRetained } from "@al-ft/midgard-fault-proofs/ledger-store";

import {
  assertNativeMpfHashHex,
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  NativeMpfRestoreReadFailed,
  NativeMpfRootNotRetained,
} from "./protocol.js";
import {
  type DecodedPromotionRecord,
  digest,
  EVENT_LOG_HEADER_BYTES,
  EVENT_LOG_MAX_BYTES,
  EVENT_STREAM_DIGEST_DOMAIN,
  HASH_BYTES,
  type NativeMpfEventOp,
  type StoredNode,
} from "./service.normalize-owner-options.js";

/** Why a canonical restore cannot move to `targetRoot`, by the cause the
 * store's `at` gave: the store does not retain it
 * (`NativeMpfRootNotRetained`), or the read failed
 * (`NativeMpfRestoreReadFailed`, which a later restore retries). */
export const restoreRefusal = (targetRoot: string, cause: unknown) =>
  cause instanceof LedgerNotRetained
    ? new NativeMpfRootNotRetained(targetRoot, { cause })
    : new NativeMpfRestoreReadFailed(targetRoot, { cause });

export const parsePromotionRecords = (
  bytes: Buffer,
): DecodedPromotionRecord[] => {
  const records: DecodedPromotionRecord[] = [];
  let offset = 0;
  while (offset < bytes.length) {
    if (bytes.length - offset < 34) {
      throw new Error("Native MPF promotion record is truncated");
    }
    const kind = bytes.readUInt8(offset++);
    const hashHex = bytes.subarray(offset, offset + HASH_BYTES).toString("hex");
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
    records.push({ hashHex, stored });
  }
  return records;
};

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
  output.writeUInt32LE(EVENT_LOG_MAX_BYTES, 24);
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
