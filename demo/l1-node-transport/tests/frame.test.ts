import { describe, expect, it } from "vitest";

import {
  CborMap,
  CborTag,
  decodeCbor,
  encodeCbor,
  encodeFrame,
  FrameError,
  FrameReader,
  MAX_HEADER_BYTES,
  MAX_PAYLOAD_BYTES,
} from "../src/index.js";

const lengths = (header: number, payload: number): Buffer => {
  const bytes = Buffer.alloc(8);
  bytes.writeUInt32BE(header, 0);
  bytes.writeUInt32BE(payload, 4);
  return bytes;
};

describe("frame codec", () => {
  it("round-trips a header and raw payload split across chunks", () => {
    const payload = Uint8Array.from({ length: 1000 }, (_, i) => i % 251);
    const frame = encodeFrame(
      {
        type: "cs_roll_forward",
        stream: 3,
        seq: 2n ** 63n,
        skipped: undefined,
      },
      payload,
    );
    const reader = new FrameReader();
    const frames = [];
    for (let at = 0; at < frame.length; at += 7)
      frames.push(...reader.push(frame.subarray(at, at + 7)));
    expect(frames).toHaveLength(1);
    expect(frames[0]!.header).toEqual({
      type: "cs_roll_forward",
      stream: 3,
      seq: 2n ** 63n,
    });
    expect(Buffer.from(frames[0]!.payload).equals(Buffer.from(payload))).toBe(
      true,
    );
    expect(reader.pending).toBe(0);
  });

  it("refuses an empty or oversized header and a header without a type", () => {
    expect(() => new FrameReader().push(lengths(0, 0))).toThrow(FrameError);
    expect(() =>
      new FrameReader().push(lengths(MAX_HEADER_BYTES + 1, 0)),
    ).toThrow(FrameError);
    const untyped = encodeCbor({ id: 1 });
    expect(() =>
      new FrameReader().push(
        Buffer.concat([lengths(untyped.length, 0), untyped]),
      ),
    ).toThrow(FrameError);
  });
});

describe("frame bounds", () => {
  it("round-trips a maximal payload and refuses one byte more", () => {
    const payload = new Uint8Array(MAX_PAYLOAD_BYTES).fill(0xa5);
    payload[0] = 1;
    payload[MAX_PAYLOAD_BYTES - 1] = 2;
    const frames = new FrameReader().push(
      encodeFrame({ type: "cs_roll_forward" }, payload),
    );
    expect(frames).toHaveLength(1);
    expect(Buffer.from(frames[0]!.payload).equals(Buffer.from(payload))).toBe(
      true,
    );
    expect(() =>
      encodeFrame(
        { type: "cs_roll_forward" },
        new Uint8Array(MAX_PAYLOAD_BYTES + 1),
      ),
    ).toThrow(FrameError);
    expect(() =>
      new FrameReader().push(lengths(1, MAX_PAYLOAD_BYTES + 1)),
    ).toThrow(FrameError);
  });

  it("round-trips a maximal header and refuses one byte more", () => {
    // A byte string of 256..65535 bytes has a fixed 3-byte length prefix,
    // so the header's overhead is measured once and the string sized to it.
    const overhead =
      encodeCbor({ type: "x", p: new Uint8Array(256) }).length - 256;
    const maximal = {
      type: "x",
      p: new Uint8Array(MAX_HEADER_BYTES - overhead),
    };
    const frame = encodeFrame(maximal);
    expect(frame.readUInt32BE(0)).toBe(MAX_HEADER_BYTES);
    const frames = new FrameReader().push(frame);
    expect(frames).toHaveLength(1);
    expect(frames[0]!.header.type).toBe("x");
    expect(() =>
      encodeFrame({
        type: "x",
        p: new Uint8Array(MAX_HEADER_BYTES - overhead + 1),
      }),
    ).toThrow(FrameError);
    expect(() =>
      new FrameReader().push(
        Buffer.concat([
          lengths(MAX_HEADER_BYTES + 1, 0),
          Buffer.alloc(MAX_HEADER_BYTES + 1),
        ]),
      ),
    ).toThrow(FrameError);
  });
});

describe("cbor", () => {
  it("decodes non-text map keys, tags, bigints and indefinite items", () => {
    // {[0, h'aa']: 5, "k": tag 258 [1]} followed by an indefinite array
    const map = encodeCbor(
      new CborMap([
        [[0, Uint8Array.of(0xaa)], 5],
        ["k", new CborTag(258, [1])],
      ]),
    );
    const decoded = decodeCbor(map) as CborMap;
    expect(decoded.entries[0]![0]).toEqual([0, Uint8Array.of(0xaa)]);
    expect(decoded.entries[1]![1]).toEqual(new CborTag(258, [1]));
    expect(decodeCbor(Uint8Array.of(0x9f, 0x01, 0x02, 0xff))).toEqual([1, 2]);
    expect(decodeCbor(encodeCbor(2n ** 64n - 1n))).toBe(2n ** 64n - 1n);
  });

  it("refuses trailing bytes and truncated items", () => {
    expect(() => decodeCbor(Uint8Array.of(0x01, 0x02))).toThrow();
    expect(() => decodeCbor(Uint8Array.of(0x82, 0x01))).toThrow();
  });
});
