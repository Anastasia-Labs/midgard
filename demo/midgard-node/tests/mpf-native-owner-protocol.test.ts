import { describe, expect, it } from "vitest";

import {
  NativeMpfRpcFrameDecoder,
  encodeNativeMpfRpcFrame,
} from "../src/services/mpf-native-owner/codec.js";
import {
  NATIVE_MPF_RPC_SCHEMA,
  NativeMpfRpcKind,
} from "../src/services/mpf-native-owner/protocol.js";
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
});
