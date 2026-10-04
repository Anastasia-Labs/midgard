import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardCekDataInteger,
  buildMidgardCekDataIntegerTrace,
  commitMidgardCekBlob,
  encodeMidgardCekDataIntegerControl,
  finalizeMidgardCekDataInteger,
  hashMidgardCekDataNode,
  initialMidgardCekDataIntegerControl,
  MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES,
  MIDGARD_CEK_SOURCE_BLOB_VERSION,
  midgardCekDataBytesCborLength,
  MidgardCekDataIntegerStages,
  nextMidgardCekDataIntegerSpan,
  parseMidgardCekDataIntegerSyntax,
  parseMidgardCekDataLargeConstructorSyntax,
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
      const trace = buildMidgardCekDataIntegerTrace({
        sourceStart: 91,
        source,
      });
      const summary = finalizeMidgardCekDataInteger(trace.terminal)!;
      const cborRoot = commitMidgardCekBlob(source).root;

      expect(trace.terminal.stage).toBe(MidgardCekDataIntegerStages.Terminal);
      expect(summary).toStrictEqual({
        root: Buffer.from(
          hashMidgardCekDataNode({
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
        parseMidgardCekDataIntegerSyntax({
          syntaxBytes: source,
          sourceLength: source.length,
        }),
      ).toBe(memory);
    },
  );

  it.each([64, 65, 128, 129])(
    "streams Cardano signed %i-byte magnitudes with exact summaries",
    (length) => {
      for (const highBit of [false, true]) {
        const magnitude = (highBit ? 128n : 1n) << BigInt((length - 1) * 8);
        for (const value of [magnitude, -magnitude - 1n]) {
          const source = Buffer.from(Data.to(value), "hex");
          const trace = buildMidgardCekDataIntegerTrace({
            sourceStart: 17,
            source,
          });
          expect(finalizeMidgardCekDataInteger(trace.terminal)).toMatchObject({
            cborLength: BigInt(source.length),
            memory: 4n + BigInt(length) + (highBit ? 1n : 0n),
          });
          if (length === 65 && !highBit && value === magnitude) {
            expect(
              trace.steps
                .filter(
                  ({ control }) =>
                    control.stage === MidgardCekDataIntegerStages.Measure,
                )
                .map(({ control }) =>
                  encodeMidgardCekDataIntegerControl(control).toString("hex"),
                ),
            ).toStrictEqual([
              "860103110200d87a80",
              "8601031118441844d87a80",
              "8601031118461845d87a80",
            ]);
            expect(
              encodeMidgardCekDataIntegerControl(trace.terminal).toString(
                "hex",
              ),
            ).toBe(
              "8601021118471845d8799f860101111847840101184781830058206f2bcd2c7aaabd4f57c1d3b1d0f2ba43fcbe6927f0551a73dd7daf4fdcf7e3051847d87a80ff",
            );
          }
          expect(
            trace.steps.every(
              ({ sourceBytes }) =>
                sourceBytes === null || sourceBytes.length <= 128,
            ),
          ).toBe(true);
          expect(
            trace.steps
              .filter(
                ({ control }) =>
                  control.stage === MidgardCekDataIntegerStages.Measure,
              )
              .every(
                ({ sourceBytes }) =>
                  sourceBytes !== null && sourceBytes.length <= 3,
              ),
          ).toBe(true);
        }
      }
    },
  );

  it.each([
    Buffer.concat([Buffer.from("c25841", "hex"), Buffer.alloc(65, 1)]),
    Buffer.concat([
      Buffer.from("c25f5840", "hex"),
      Buffer.alloc(64, 1),
      Buffer.from("ff", "hex"),
    ]),
    Buffer.concat([
      Buffer.from("c25f583f", "hex"),
      Buffer.alloc(63, 1),
      Buffer.from("420101ff", "hex"),
    ]),
    Buffer.concat([
      Buffer.from("c25f5840", "hex"),
      Buffer.alloc(64, 1),
      Buffer.from("41014101ff", "hex"),
    ]),
    Buffer.concat([
      Buffer.from("c25f5840", "hex"),
      Buffer.alloc(64),
      Buffer.from("4101ff", "hex"),
    ]),
    Buffer.concat([
      Buffer.from("c25f5840", "hex"),
      Buffer.alloc(64, 1),
      Buffer.from("5841", "hex"),
      Buffer.alloc(65, 1),
      Buffer.from("ff", "hex"),
    ]),
    Buffer.concat([
      Buffer.from("c25f5840", "hex"),
      Buffer.alloc(64, 1),
      Buffer.from("4101", "hex"),
    ]),
    Buffer.concat([
      Buffer.from("c25f5840", "hex"),
      Buffer.alloc(64, 1),
      Buffer.from("4101ff00", "hex"),
    ]),
  ])("rejects noncanonical Cardano magnitude layout %#", (source) => {
    expect(() =>
      buildMidgardCekDataIntegerTrace({ sourceStart: 0, source }),
    ).toThrow(/failed closed/u);
  });

  it("streams a maximum-transaction-sized bignum through bounded reveals", () => {
    let magnitudeLength = 16_384;
    while (
      1n + midgardCekDataBytesCborLength(BigInt(magnitudeLength)) >
      16_384n
    )
      magnitudeLength -= 1;
    const source = Buffer.from(
      Data.to(1n << BigInt((magnitudeLength - 1) * 8)),
      "hex",
    );
    const trace = buildMidgardCekDataIntegerTrace({
      sourceStart: 17,
      source,
    });
    const blobReveals = Buffer.concat(
      trace.steps.flatMap(({ control, sourceBytes }) =>
        control.stage === MidgardCekDataIntegerStages.Blob &&
        sourceBytes !== null
          ? [sourceBytes]
          : [],
      ),
    );

    expect(source.length).toBe(16_384);
    expect(
      1n + midgardCekDataBytesCborLength(BigInt(magnitudeLength + 1)),
    ).toBeGreaterThan(16_384n);
    expect(source.length).toBeGreaterThan(16_000);
    expect(blobReveals).toStrictEqual(source);
    expect(finalizeMidgardCekDataInteger(trace.terminal)).toMatchObject({
      cborLength: BigInt(source.length),
      memory: 4n + BigInt(magnitudeLength),
    });
    for (const { sourceBytes } of trace.steps) {
      if (sourceBytes !== null) {
        expect(sourceBytes.length).toBeLessThanOrEqual(128);
      }
    }
  });

  it("binds the source range in every nested state", () => {
    const source = Buffer.from("c349010000000000000000", "hex");
    const trace = buildMidgardCekDataIntegerTrace({
      sourceStart: 17,
      source,
    });

    expect(
      trace.steps
        .filter(({ next }) => next.blob !== null)
        .every(
          ({ next }) =>
            next.blob!.version === MIDGARD_CEK_SOURCE_BLOB_VERSION &&
            next.blob!.sourceStart === 17 &&
            next.blob!.sourceLength === source.length,
        ),
    ).toBe(true);
    expect(
      encodeMidgardCekDataIntegerControl(trace.terminal).toString("hex"),
    ).toBe(
      "860102110b0dd8799f860101110b8401010b8183005820529618b73f1e990ed364ce58c08a76518a3f4ddaf2397ea92207a760422764840bd87a80ff",
    );
    expect(
      finalizeMidgardCekDataInteger(trace.terminal)!.root.toString("hex"),
    ).toBe("720c28eb8291c0e25d860108458a13027f509d93b9c61296532fdb230063c691");
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
        parseMidgardCekDataLargeConstructorSyntax({
          syntaxBytes: source,
          sourceLength: source.length,
        }),
      ).not.toBeNull();
    }
    for (const source of rejected) {
      expect(
        parseMidgardCekDataLargeConstructorSyntax({
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
      buildMidgardCekDataIntegerTrace({
        sourceStart: 0,
        source,
      }),
    ).toThrow(/failed closed/u);
  });

  it("fails closed for missing, short, and surplus authenticated windows", () => {
    const initial = initialMidgardCekDataIntegerControl({
      sourceStart: 9,
      sourceLength: 11,
    });
    const span = nextMidgardCekDataIntegerSpan(initial, 20)!;

    expect(span).toStrictEqual({
      absoluteStart: 9,
      length: 11,
    });
    expect(
      advanceMidgardCekDataInteger({
        control: initial,
        sourceEnd: 20,
        sourceBytes: null,
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekDataInteger({
        control: initial,
        sourceEnd: 20,
        sourceBytes: Buffer.alloc(span.length - 1),
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekDataInteger({
        control: initial,
        sourceEnd: 20,
        sourceBytes: Buffer.alloc(span.length + 1),
      }),
    ).toBeNull();
    expect(() =>
      initialMidgardCekDataIntegerControl({
        sourceStart: 0,
        sourceLength: 0,
      }),
    ).toThrow(/range/u);
    expect(MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES).toBe(14);
  });
});
