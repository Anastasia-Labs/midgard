import { describe, expect, it } from "vitest";

import {
  advanceMidgardCekDataIntegerV1,
  buildMidgardCekDataIntegerTraceV1,
  commitMidgardCekBlobV1,
  encodeMidgardCekDataIntegerControlV1,
  finalizeMidgardCekDataIntegerV1,
  hashMidgardCekDataNodeV1,
  initialMidgardCekDataIntegerControlV1,
  MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES,
  MIDGARD_CEK_SOURCE_BLOB_V1_VERSION,
  MidgardCekDataIntegerStagesV1,
  nextMidgardCekDataIntegerSpanV1,
  parseMidgardCekDataIntegerSyntaxV1,
  parseMidgardCekDataLargeConstructorSyntaxV1,
} from "../src/index.js";

const integerCases = [
  {
    name: "zero",
    source: Buffer.from("00", "hex"),
    memory: 5n,
  },
  {
    name: "negative one",
    source: Buffer.from("20", "hex"),
    memory: 5n,
  },
  {
    name: "uint64 maximum",
    source: Buffer.from("1bffffffffffffffff", "hex"),
    memory: 13n,
  },
  {
    name: "major-one uint64 maximum",
    source: Buffer.from("3bffffffffffffffff", "hex"),
    memory: 13n,
  },
  {
    name: "positive bignum boundary",
    source: Buffer.from("c249010000000000000000", "hex"),
    memory: 13n,
  },
  {
    name: "negative bignum boundary",
    source: Buffer.from("c349010000000000000000", "hex"),
    memory: 13n,
  },
] as const;

describe("authenticated CEK Data integer V1", () => {
  it.each(integerCases)(
    "proves canonical $name encoding",
    ({ source, memory }) => {
      const trace = buildMidgardCekDataIntegerTraceV1({
        sourceStart: 91,
        source,
      });
      const summary = finalizeMidgardCekDataIntegerV1(
        trace.terminal,
      )!;
      const cborRoot = commitMidgardCekBlobV1(source).root;

      expect(trace.terminal.stage).toBe(
        MidgardCekDataIntegerStagesV1.Terminal,
      );
      expect(summary).toStrictEqual({
        root: Buffer.from(
          hashMidgardCekDataNodeV1({
            kind: "integer",
            cborRoot,
            cborLength: BigInt(source.length),
            memory,
          }),
        ),
        cborLength: BigInt(source.length),
        memory,
      });
      expect(
        parseMidgardCekDataIntegerSyntaxV1({
          syntaxBytes: source,
          sourceLength: source.length,
        }),
      ).toBe(memory);
    },
  );

  it("streams a maximum-transaction-sized bignum through bounded reveals", () => {
    const magnitude = Buffer.alloc(16_380);
    magnitude[0] = 1;
    const source = Buffer.concat([
      Buffer.from([0xc2, 0x59, 0x3f, 0xfc]),
      magnitude,
    ]);
    const trace = buildMidgardCekDataIntegerTraceV1({
      sourceStart: 17,
      source,
    });
    const blobReveals = Buffer.concat(
      trace.steps.flatMap(({ control, sourceBytes }) =>
        control.stage === MidgardCekDataIntegerStagesV1.Blob &&
        sourceBytes !== null
          ? [sourceBytes]
          : [],
      ),
    );

    expect(source).toHaveLength(16_384);
    expect(blobReveals).toStrictEqual(source);
    expect(finalizeMidgardCekDataIntegerV1(trace.terminal)).toMatchObject({
      cborLength: 16_384n,
      memory: 16_384n,
    });
    for (const { sourceBytes } of trace.steps) {
      if (sourceBytes !== null) {
        expect(sourceBytes.length).toBeLessThanOrEqual(128);
      }
    }
  });

  it("binds the source range in every nested state", () => {
    const source = Buffer.from(
      "c349010000000000000000",
      "hex",
    );
    const trace = buildMidgardCekDataIntegerTraceV1({
      sourceStart: 17,
      source,
    });

    expect(
      trace.steps
        .filter(({ next }) => next.blob !== null)
        .every(
          ({ next }) =>
            next.blob!.version ===
              MIDGARD_CEK_SOURCE_BLOB_V1_VERSION &&
            next.blob!.sourceStart === 17 &&
            next.blob!.sourceLength === source.length,
        ),
    ).toBe(true);
    expect(
      encodeMidgardCekDataIntegerControlV1(
        trace.terminal,
      ).toString("hex"),
    ).toBe(
      "860102110b0dd8799f860101110b8401010b8183005820529618b73f1e990ed364ce58c08a76518a3f4ddaf2397ea92207a760422764840bd87a80ff",
    );
    expect(
      finalizeMidgardCekDataIntegerV1(
        trace.terminal,
      )!.root.toString("hex"),
    ).toBe(
      "720c28eb8291c0e25d860108458a13027f509d93b9c61296532fdb230063c691",
    );
  });

  it("accepts only canonical constructor alternatives above 127", () => {
    const accepted = [
      Buffer.from("1880", "hex"),
      Buffer.from("1bffffffffffffffff", "hex"),
      Buffer.from("c249010000000000000000", "hex"),
    ];
    const rejected = [
      Buffer.from("1817", "hex"),
      Buffer.from("187f", "hex"),
      Buffer.from("3880", "hex"),
      Buffer.from("c349010000000000000000", "hex"),
    ];

    for (const source of accepted) {
      expect(
        parseMidgardCekDataLargeConstructorSyntaxV1({
          syntaxBytes: source,
          sourceLength: source.length,
        }),
      ).not.toBeNull();
    }
    for (const source of rejected) {
      expect(
        parseMidgardCekDataLargeConstructorSyntaxV1({
          syntaxBytes: source,
          sourceLength: source.length,
        }),
      ).toBeNull();
    }
  });

  it.each([
    Buffer.from("1817", "hex"),
    Buffer.from("c248ffffffffffffffff", "hex"),
    Buffer.from("c249000100000000000000", "hex"),
    Buffer.from("c25809010000000000000000", "hex"),
    Buffer.from("c25f490100000000000000ff", "hex"),
    Buffer.from("40", "hex"),
  ])("rejects malformed or noncanonical integer CBOR %#", (source) => {
    expect(() =>
      buildMidgardCekDataIntegerTraceV1({
        sourceStart: 0,
        source,
      }),
    ).toThrow(/failed closed/u);
  });

  it("fails closed for missing, short, and surplus authenticated windows", () => {
    const initial = initialMidgardCekDataIntegerControlV1({
      sourceStart: 9,
      sourceLength: 11,
    });
    const span = nextMidgardCekDataIntegerSpanV1(initial)!;

    expect(span).toStrictEqual({
      absoluteStart: 9,
      length: 11,
    });
    expect(
      advanceMidgardCekDataIntegerV1({
        control: initial,
        sourceBytes: null,
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekDataIntegerV1({
        control: initial,
        sourceBytes: Buffer.alloc(span.length - 1),
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekDataIntegerV1({
        control: initial,
        sourceBytes: Buffer.alloc(span.length + 1),
      }),
    ).toBeNull();
    expect(() =>
      initialMidgardCekDataIntegerControlV1({
        sourceStart: 0,
        sourceLength: 0,
      }),
    ).toThrow(/range/u);
    expect(MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES).toBe(14);
  });
});
