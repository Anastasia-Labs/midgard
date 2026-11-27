/**
 * The payload layouts of the native owner's schema-2 messages that carry the
 * node cache: Hello, HelloAck, SetRoot, Ready, NeedNodes, ProvideNodes,
 * NodesProvided and DiagnosticsResult. Every integer is little-endian. The
 * child's side is described in `native/mpf-event-flat-wasm/README.md`.
 */
import { timingSafeEqual } from "node:crypto";

import {
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  NATIVE_MPF_RPC_SCHEMA,
} from "./protocol.js";
import {
  digest,
  HASH_BYTES,
  SELF_TEST_DOMAIN,
} from "./service.normalize-owner-options.js";

const GENERATION_ID_BYTES = 16;

const readSafeU64 = (payload: Buffer, offset: number, field: string) => {
  const value = payload.readBigUInt64LE(offset);
  if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`Native MPF ${field} exceeds safe integer`);
  }
  return Number(value);
};

/** Hello: binary SHA-256[32], cache budget u64. */
export const encodeHello = (binarySha256: string, cacheBytes: number) => {
  const payload = Buffer.alloc(HASH_BYTES + 8);
  Buffer.from(binarySha256, "hex").copy(payload, 0);
  payload.writeBigUInt64LE(BigInt(cacheBytes), HASH_BYTES);
  return payload;
};

/** HelloAck: schema u16, binary SHA-256[32], cache budget u64, max events
 * u32, max ops u32, max active generations u32, self-test digest[32]. */
export const assertHelloAck = (
  response: Uint8Array,
  binarySha256: string,
  cacheBytes: number,
): void => {
  const payload = Buffer.from(response);
  if (payload.byteLength !== 86) {
    throw new Error("Native MPF HelloAck payload length is invalid");
  }
  if (
    payload.readUInt16LE(0) !== NATIVE_MPF_RPC_SCHEMA ||
    payload.subarray(2, 34).toString("hex") !== binarySha256 ||
    payload.readBigUInt64LE(34) !== BigInt(cacheBytes) ||
    payload.readUInt32LE(42) !== NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents ||
    payload.readUInt32LE(46) !== NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps ||
    payload.readUInt32LE(50) !==
      NATIVE_MPF_OWNER_DEFAULT_CAPS.maxActiveGenerations ||
    !timingSafeEqual(payload.subarray(54), digest(SELF_TEST_DOMAIN))
  ) {
    throw new Error("Native MPF HelloAck capability/self-test mismatch");
  }
};

/** SetRoot: root[32]. */
export const encodeSetRoot = (root: string) => Buffer.from(root, "hex");

/** Ready: root[32]. Refused unless it names `root`. */
export const assertReady = (response: Uint8Array, root: string): void => {
  if (Buffer.from(response).toString("hex") !== root) {
    throw new Error("Native MPF Ready root is invalid");
  }
};

/** NeedNodes: generation id[16], count u32, count node hashes[32] sorted
 * ascending and unique. Answers the hashes as lowercase hex. */
export const decodeNeedNodes = (
  response: Uint8Array,
  generationId: Uint8Array,
): string[] => {
  const payload = Buffer.from(response);
  const headerBytes = GENERATION_ID_BYTES + 4;
  if (
    payload.byteLength < headerBytes ||
    !timingSafeEqual(
      payload.subarray(0, GENERATION_ID_BYTES),
      Buffer.from(generationId),
    )
  ) {
    throw new Error("Native MPF NeedNodes handle is invalid");
  }
  const count = payload.readUInt32LE(GENERATION_ID_BYTES);
  if (count === 0 || payload.byteLength !== headerBytes + count * HASH_BYTES) {
    throw new Error("Native MPF NeedNodes count is invalid");
  }
  const hashes: string[] = [];
  for (let index = 0; index < count; index += 1) {
    const start = headerBytes + index * HASH_BYTES;
    if (
      index > 0 &&
      Buffer.compare(
        payload.subarray(start - HASH_BYTES, start),
        payload.subarray(start, start + HASH_BYTES),
      ) >= 0
    ) {
      throw new Error("Native MPF NeedNodes hashes are not uniquely sorted");
    }
    hashes.push(payload.subarray(start, start + HASH_BYTES).toString("hex"));
  }
  return hashes;
};

/** ProvideNodes: one or more whole node records (`encodeStoredNode`) laid
 * end to end, at most `maxChunkBytes` a frame. Answers the frames with the
 * number of records in each. */
export const encodeProvideNodes = (
  records: readonly Buffer[],
  maxChunkBytes: number,
): { readonly payload: Buffer; readonly count: number }[] => {
  const frames: { payload: Buffer; count: number }[] = [];
  let pending: Buffer[] = [];
  let pendingBytes = 0;
  const flush = () => {
    if (pending.length === 0) return;
    frames.push({
      payload: Buffer.concat(pending, pendingBytes),
      count: pending.length,
    });
    pending = [];
    pendingBytes = 0;
  };
  for (const record of records) {
    if (record.byteLength > maxChunkBytes) {
      throw new Error(
        `Native MPF node record of ${record.byteLength.toString()} bytes exceeds the chunk cap ${maxChunkBytes.toString()}`,
      );
    }
    if (pendingBytes + record.byteLength > maxChunkBytes) flush();
    pending.push(record);
    pendingBytes += record.byteLength;
  }
  flush();
  return frames;
};

/** NodesProvided: count u32, the records the frame carried. */
export const decodeNodesProvided = (response: Uint8Array): number => {
  const payload = Buffer.from(response);
  if (payload.byteLength !== 4) {
    throw new Error("Native MPF NodesProvided payload length is invalid");
  }
  return payload.readUInt32LE(0);
};

/** DiagnosticsResult: marker[32], cached nodes u64, cached bytes u64, active
 * generations u64, generated nodes u64, generated bytes u64, VmRSS KiB u64,
 * VmHWM KiB u64. */
export const decodeDiagnostics = (response: Uint8Array) => {
  const payload = Buffer.from(response);
  if (payload.byteLength !== HASH_BYTES + 7 * 8) {
    throw new Error("Native MPF diagnostics payload length is invalid");
  }
  return {
    durableRoot: payload.subarray(0, HASH_BYTES).toString("hex"),
    cachedNodes: readSafeU64(payload, 32, "diagnostic cached nodes"),
    cachedBytes: readSafeU64(payload, 40, "diagnostic cached bytes"),
    activeGenerations: readSafeU64(payload, 48, "diagnostic generations"),
    generatedNodes: readSafeU64(payload, 56, "diagnostic generated nodes"),
    generatedBytes: readSafeU64(payload, 64, "diagnostic generated bytes"),
    rssBytes: readSafeU64(payload, 72, "diagnostic VmRSS") * 1024,
    peakRssBytes: readSafeU64(payload, 80, "diagnostic VmHWM") * 1024,
  };
};
