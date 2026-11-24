import { describe, expect, it } from "vitest";

import {
  encodeNativeMpfRpcFrame,
  NativeMpfRpcFrameDecoder,
} from "../src/services/mpf-native-owner/codec.js";
import {
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  NATIVE_MPF_RPC_SCHEMA,
  NativeMpfRpcKind,
} from "../src/services/mpf-native-owner/protocol.js";
import {
  digest as ownerDigest,
  SELF_TEST_DOMAIN,
} from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import {
  assertHelloAck,
  decodeDiagnostics,
  decodeNeedNodes,
  decodeNodesProvided,
  decodeReady,
  encodeProvideNodes,
} from "../src/services/mpf-native-owner/service.rpc-payloads.js";
import {
  createEventFlatDigest,
  prepareEventFlatDigest,
} from "../src/workers/utils/mpf-event-flat-digest.js";

const digest = (chunks: readonly Uint8Array[]): Buffer => {
  const state = createEventFlatDigest();
  for (const chunk of chunks) state.update(chunk);
  return state.digest();
};

describe("native MPF owner protocol", () => {
  it("round-trips fragmented frames and rejects tamper/truncation/caps", async () => {
    await prepareEventFlatDigest();
    const encoded = encodeNativeMpfRpcFrame(
      {
        schema: NATIVE_MPF_RPC_SCHEMA,
        kind: NativeMpfRpcKind.ApplyEvents,
        requestId: 42n,
        ownerEpoch: Buffer.alloc(16, 7),
        payload: Buffer.from([1, 2, 3, 4]),
      },
      digest,
      256,
    );
    const decoder = new NativeMpfRpcFrameDecoder(digest, 256);
    expect(decoder.push(encoded.subarray(0, 3))).toEqual([]);
    expect(decoder.push(encoded.subarray(3, 19))).toEqual([]);
    const [frame] = decoder.push(encoded.subarray(19));
    expect(frame).toMatchObject({
      kind: NativeMpfRpcKind.ApplyEvents,
      requestId: 42n,
      payload: Uint8Array.from([1, 2, 3, 4]),
    });
    decoder.finish();

    const tampered = Buffer.from(encoded);
    tampered[tampered.length - 33] ^= 1;
    expect(() =>
      new NativeMpfRpcFrameDecoder(digest, 256).push(tampered),
    ).toThrow(/digest mismatch/);

    const truncated = new NativeMpfRpcFrameDecoder(digest, 256);
    truncated.push(encoded.subarray(0, encoded.length - 1));
    expect(() => truncated.finish()).toThrow(/truncated frame/);

    expect(() =>
      encodeNativeMpfRpcFrame(
        {
          schema: NATIVE_MPF_RPC_SCHEMA,
          kind: NativeMpfRpcKind.ApplyEvents,
          requestId: 1n,
          ownerEpoch: Buffer.alloc(16),
          payload: Buffer.alloc(200),
        },
        digest,
        100,
      ),
    ).toThrow(/exceeds cap/);
  });

  const sha = "ab".repeat(32);
  const helloAck = (budget: bigint) => {
    const payload = Buffer.alloc(86);
    payload.writeUInt16LE(NATIVE_MPF_RPC_SCHEMA, 0);
    Buffer.from(sha, "hex").copy(payload, 2);
    payload.writeBigUInt64LE(budget, 34);
    payload.writeUInt32LE(NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents, 42);
    payload.writeUInt32LE(NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps, 46);
    payload.writeUInt32LE(
      NATIVE_MPF_OWNER_DEFAULT_CAPS.maxActiveGenerations,
      50,
    );
    ownerDigest(SELF_TEST_DOMAIN).copy(payload, 54);
    return payload;
  };

  it("accepts a HelloAck only when it echoes the schema, binary and cache budget", async () => {
    await prepareEventFlatDigest();
    expect(() => assertHelloAck(helloAck(4_096n), sha, 4_096)).not.toThrow();
    expect(() => assertHelloAck(helloAck(4_095n), sha, 4_096)).toThrow(
      /capability\/self-test mismatch/,
    );
    expect(() =>
      assertHelloAck(helloAck(4_096n).subarray(0, 85), sha, 4_096),
    ).toThrow(/length is invalid/);
    const otherSchema = helloAck(4_096n);
    otherSchema.writeUInt16LE(1, 0);
    expect(() => assertHelloAck(otherSchema, sha, 4_096)).toThrow(
      /capability\/self-test mismatch/,
    );
  });

  it("accepts a Ready only for the root it set", () => {
    const root = "cd".repeat(32);
    const ready = Buffer.alloc(64);
    Buffer.from(root, "hex").copy(ready);
    ready.writeBigUInt64LE(3n, 32);
    ready.writeBigUInt64LE(900n, 40);
    ready.writeBigUInt64LE(2n, 48);
    ready.writeBigUInt64LE(5n, 56);
    expect(decodeReady(ready, root)).toEqual({
      cachedNodes: 3,
      cachedBytes: 900,
      rssBytes: 2_048,
      peakRssBytes: 5_120,
    });
    expect(() => decodeReady(ready, "ef".repeat(32))).toThrow(
      /Ready root\/diagnostics are invalid/,
    );
    expect(() => decodeReady(ready.subarray(0, 63), root)).toThrow(
      /Ready root\/diagnostics are invalid/,
    );
  });

  it("reads a NeedNodes only for its generation, with a positive count of strictly ascending hashes", () => {
    const id = Buffer.alloc(16, 9);
    const needNodes = (generation: Buffer, ...hashes: readonly number[]) => {
      const payload = Buffer.alloc(20 + hashes.length * 32);
      generation.copy(payload);
      payload.writeUInt32LE(hashes.length, 16);
      hashes.forEach((fill, index) =>
        payload.fill(fill, 20 + index * 32, 52 + index * 32),
      );
      return payload;
    };
    expect(decodeNeedNodes(needNodes(id, 1, 2), id)).toEqual([
      "01".repeat(32),
      "02".repeat(32),
    ]);
    expect(() =>
      decodeNeedNodes(needNodes(Buffer.alloc(16, 8), 1), id),
    ).toThrow(/handle is invalid/);
    expect(() => decodeNeedNodes(needNodes(id), id)).toThrow(
      /count is invalid/,
    );
    expect(() =>
      decodeNeedNodes(needNodes(id, 1, 2).subarray(0, 83), id),
    ).toThrow(/count is invalid/);
    expect(() => decodeNeedNodes(needNodes(id, 2, 1), id)).toThrow(
      /not uniquely sorted/,
    );
    expect(() => decodeNeedNodes(needNodes(id, 1, 1), id)).toThrow(
      /not uniquely sorted/,
    );
  });

  it("packs ProvideNodes frames of whole records within the chunk cap", () => {
    const records = [60, 50, 40, 100].map((length, index) =>
      Buffer.alloc(length, index + 1),
    );
    const frames = encodeProvideNodes(records, 100);
    expect(frames.map((frame) => frame.count)).toEqual([1, 2, 1]);
    expect(frames.map((frame) => frame.payload.byteLength)).toEqual([
      60, 90, 100,
    ]);
    expect(Buffer.concat(frames.map((frame) => frame.payload))).toEqual(
      Buffer.concat(records),
    );
    expect(() => encodeProvideNodes([Buffer.alloc(101)], 100)).toThrow(
      /101 bytes exceeds the chunk cap 100/,
    );
    expect(decodeNodesProvided(Buffer.from([2, 0, 0, 0]))).toBe(2);
    expect(() => decodeNodesProvided(Buffer.alloc(5))).toThrow(
      /length is invalid/,
    );
  });

  it("reads the cache fields of a DiagnosticsResult", () => {
    const result = Buffer.alloc(88);
    Buffer.alloc(32, 7).copy(result);
    [11n, 1_000n, 1n, 4n, 800n, 3n, 6n].forEach((value, index) =>
      result.writeBigUInt64LE(value, 32 + index * 8),
    );
    expect(decodeDiagnostics(result)).toEqual({
      durableRoot: "07".repeat(32),
      cachedNodes: 11,
      cachedBytes: 1_000,
      activeGenerations: 1,
      generatedNodes: 4,
      generatedBytes: 800,
      rssBytes: 3_072,
      peakRssBytes: 6_144,
    });
    expect(() => decodeDiagnostics(result.subarray(0, 87))).toThrow(
      /length is invalid/,
    );
  });
});
