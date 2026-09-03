import { describe, expect, it } from "vitest";

import {
  advanceMidgardCekDataBytes,
  buildMidgardCekDataBytesTrace,
  commitMidgardCekBlob,
  encodeCborBytes,
  encodeMidgardCekDataBytesControl,
  finalizeMidgardCekDataBytes,
  hashMidgardCekDataNode,
  initialMidgardCekDataBytesControl,
  MIDGARD_CEK_DATA_BYTES_MAX_SOURCE_SPAN,
  MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES,
  MidgardCekDataBytesStages,
  nextMidgardCekDataBytesSpan,
  parseMidgardCekDataBytesSyntax,
} from "../src/index.js";

const encodeCardanoDataBytes = (content: Uint8Array): Buffer => {
  const bytes = Buffer.from(content);
  if (bytes.length <= 64) return encodeCborBytes(bytes);
  const chunks: Buffer[] = [];
  for (let offset = 0; offset < bytes.length; offset += 64) {
    chunks.push(encodeCborBytes(bytes.subarray(offset, offset + 64)));
  }
  return Buffer.concat([Buffer.from([0x5f]), ...chunks, Buffer.from([0xff])]);
};

describe("authenticated CEK Data bytes V1", () => {
  it.each([0, 1, 23, 24, 63, 64, 65, 127, 128, 129, 4_095, 4_096, 15_885])(
    "proves %i raw bytes through canonical Cardano framing",
    (length) => {
      const content = Buffer.alloc(length, 0x6a);
      const source = encodeCardanoDataBytes(content);
      const trace = buildMidgardCekDataBytesTrace({
        sourceStart: 73,
        source,
      });
      const summary = finalizeMidgardCekDataBytes(trace.terminal)!;
      const bytesRoot = commitMidgardCekBlob(content).root;
      const memory = 4n + (length === 0 ? 1n : BigInt(length));

      expect(summary).toStrictEqual({
        root: Buffer.from(
          hashMidgardCekDataNode({
            kind: "bytes",
            bytesRoot,
            bytesLength: BigInt(length),
            cborLength: BigInt(source.length),
            memory,
          }),
        ),
        cborLength: BigInt(source.length),
        memory,
      });
      expect(
        parseMidgardCekDataBytesSyntax({
          syntaxBytes: source.subarray(0, MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES),
          sourceLength: source.length,
        }),
      ).toBe(length);
      for (const { sourceBytes } of trace.steps) {
        if (sourceBytes !== null) {
          expect(sourceBytes.length).toBeLessThanOrEqual(
            MIDGARD_CEK_DATA_BYTES_MAX_SOURCE_SPAN,
          );
        }
      }
      expect(
        trace.steps.some(
          ({ control }) => control.stage === MidgardCekDataBytesStages.Break,
        ),
      ).toBe(length > 64);
    },
  );

  it("fits the largest canonical byte leaf in a 16,384-byte transaction", () => {
    const content = Buffer.alloc(15_885, 0x7b);
    const source = encodeCardanoDataBytes(content);
    const trace = buildMidgardCekDataBytesTrace({
      sourceStart: 17,
      source,
    });
    const maximumReveal = Math.max(
      ...trace.steps.map(({ sourceBytes }) => sourceBytes?.length ?? 0),
    );

    expect(source).toHaveLength(16_384);
    expect(maximumReveal).toBeLessThanOrEqual(
      MIDGARD_CEK_DATA_BYTES_MAX_SOURCE_SPAN,
    );
    expect(finalizeMidgardCekDataBytes(trace.terminal)).toMatchObject({
      cborLength: 16_384n,
      memory: 15_889n,
    });
  });

  it("binds an Aiken terminal vector across virtual and raw coordinates", () => {
    const content = Buffer.alloc(65, 0x6a);
    const source = encodeCardanoDataBytes(content);
    const trace = buildMidgardCekDataBytesTrace({
      sourceStart: 17,
      source,
    });

    expect(source.toString("hex")).toBe(`5f5840${"6a".repeat(64)}416aff`);
    expect(
      trace.steps
        .filter(({ next }) => next.blob !== null)
        .every(
          ({ next }) =>
            next.blob!.sourceStart === 0 && next.blob!.sourceLength === 65,
        ),
    ).toBe(true);
    expect(
      encodeMidgardCekDataBytesControl(trace.terminal).toString("hex"),
    ).toBe(
      "8601031118461841d8799f86010100184184010118418183005820ebc3448dad1500c73547d17bd6e9e93387c20bc9422b3313e45b434bf26a967c1841d87a80ff",
    );
    expect(
      finalizeMidgardCekDataBytes(trace.terminal)!.root.toString("hex"),
    ).toBe("e36bf656c2ebd1bb060fa2a5b3c8d515c595d7ddac821eab2ab29d5e97b29836");
  });

  it.each([
    Buffer.concat([Buffer.from([0x58, 0x17]), Buffer.alloc(23, 0x6a)]),
    Buffer.concat([Buffer.from([0x58, 0x41]), Buffer.alloc(65, 0x6a)]),
    Buffer.concat([
      Buffer.from([0x5f, 0x58, 0x40]),
      Buffer.alloc(64, 0x6a),
      Buffer.from([0xff]),
    ]),
    Buffer.concat([
      Buffer.from([0x5f, 0x40, 0x40]),
      Buffer.alloc(64, 0x6a),
      Buffer.from([0x41, 0x6a, 0xff]),
    ]),
    Buffer.concat([
      Buffer.from([0x5f, 0x58, 0x40]),
      Buffer.alloc(64, 0x6a),
      Buffer.from([0x42, 0x6a, 0xff]),
    ]),
    Buffer.concat([
      Buffer.from([0x5f, 0x58, 0x40]),
      Buffer.alloc(64, 0x6a),
      Buffer.from([0x41, 0x6a, 0x00]),
    ]),
    Buffer.from([0x40, 0x00]),
  ])("rejects malformed or noncanonical byte Data CBOR %#", (source) => {
    expect(() =>
      buildMidgardCekDataBytesTrace({
        sourceStart: 0,
        source,
      }),
    ).toThrow(/failed closed/u);
  });

  it("fails closed for missing, short, and surplus authenticated windows", () => {
    const initial = initialMidgardCekDataBytesControl({
      sourceStart: 9,
      sourceLength: 70,
    });
    const span = nextMidgardCekDataBytesSpan(initial)!;

    expect(span).toStrictEqual({
      absoluteStart: 9,
      length: 2,
    });
    expect(
      advanceMidgardCekDataBytes({
        control: initial,
        sourceBytes: null,
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekDataBytes({
        control: initial,
        sourceBytes: Buffer.alloc(span.length - 1),
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekDataBytes({
        control: initial,
        sourceBytes: Buffer.alloc(span.length + 1),
      }),
    ).toBeNull();
  });
});
