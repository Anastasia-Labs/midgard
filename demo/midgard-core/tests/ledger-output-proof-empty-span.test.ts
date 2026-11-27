import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardLedgerOutputProof,
  boundMidgardLedgerOutputWindowBytes,
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardLedgerOutputProofTrace,
  decodeMidgardDatum,
  demandedMidgardLedgerOutputProofSpan,
  encodeMidgardLedgerOutputProofControl,
  encodeMidgardTxOutput,
  isExactMidgardLedgerOutputProofTerminal,
  isWellFormedMidgardLedgerOutputProofControl,
  MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
  type MidgardLedgerOutputProofControl,
  MidgardLedgerOutputProofResultKinds,
  MidgardLedgerOutputProofStages,
  type MidgardLedgerOutputProofWitness,
  midgardLedgerOutputWindowCovers,
  type MidgardTxOutput,
  nextMidgardCekDataTraverseSpan,
} from "../src/index.js";
import { authenticatedOutputSpan } from "../src/ledger-output-proof.authenticated-output-span.js";

const address = Buffer.concat([Buffer.from([0x78]), Buffer.alloc(28, 0x11)]);

/** An indefinite byte string of `length` bytes in 64-byte chunks. */
const chunkedBytes = (length: number): string => {
  const parts = ["5f"];
  for (let at = 0; at < length; at += 64) {
    const size = Math.min(64, length - at);
    const head =
      size < 24 ? (0x40 + size).toString(16) : `58${size.toString(16)}`;
    parts.push(head, "ab".repeat(size));
  }
  parts.push("ff");
  return parts.join("");
};

const outputCbor = (
  datumHex: string,
  extra: Partial<MidgardTxOutput> = {},
): Buffer =>
  encodeMidgardTxOutput({
    address,
    value: { lovelace: 2_000_000n, assets: new Map() },
    datum: decodeMidgardDatum(Buffer.from(datumHex, "hex")),
    ...extra,
  });

const encoded = (control: MidgardLedgerOutputProofControl): string =>
  encodeMidgardLedgerOutputProofControl(control).toString("hex");

const advanced = (
  control: MidgardLedgerOutputProofControl,
  witness: MidgardLedgerOutputProofWitness,
): string | null => {
  const result = advanceMidgardLedgerOutputProof({ control, witness });
  return result?.kind === MidgardLedgerOutputProofResultKinds.Advanced
    ? encoded(result.control)
    : null;
};

const proofOf = (cbor: Buffer) => {
  const trace = buildMidgardLedgerOutputProofTrace({
    outputIndex: 0,
    outputCbor: cbor,
  });
  const item = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
    itemIndex: 0,
    bytes: cbor,
  });
  return {
    trace,
    chunk: (index: number) => buildMidgardBoundedItemChunkProof(item, index),
  };
};

/** Built on first use inside a test, so a builder fault fails that test. */
const lazy = <T>(build: () => T): (() => T) => {
  let value: T | undefined;
  return () => (value ??= build());
};

const emptySpanStart = (control: MidgardLedgerOutputProofControl) => {
  if (
    control.stage !== MidgardLedgerOutputProofStages.DatumTraversal ||
    control.datum === null
  ) {
    return null;
  }
  const span = nextMidgardCekDataTraverseSpan(control.datum);
  return span?.length === 0 ? span.absoluteStart : null;
};

describe("ledger output proof of an empty span across chunks", () => {
  it.each([
    {
      label: "an empty byte string ending the first chunk",
      cbor: outputCbor(`9f${chunkedBytes(3_922)}40ff`),
      total: 4_095,
      emptyStarts: [4_094],
    },
    {
      label: "an empty byte string whose content starts the second chunk",
      cbor: outputCbor(`9f${chunkedBytes(3_923)}40ff`),
      total: 4_096,
      emptyStarts: [4_095],
    },
    {
      label: "an empty byte string inside the second chunk",
      cbor: outputCbor(`9f${chunkedBytes(3_924)}40ff`),
      total: 4_097,
      emptyStarts: [4_096],
    },
    {
      label: "an empty byte string datum ending a two-chunk output",
      cbor: outputCbor("40", {
        value: {
          lovelace: 2_000_000n,
          assets: new Map([
            [
              "55".repeat(28),
              new Map(
                Array.from({ length: 200 }, (_, index) => [
                  index.toString(16).padStart(40, "0"),
                  BigInt(index + 1),
                ]),
              ),
            ],
          ]),
        },
      }),
      total: 4_653,
      emptyStarts: [4_653],
    },
    {
      label: "an empty byte string datum before a three-chunk script",
      cbor: outputCbor("40", {
        script_ref: {
          language: "PlutusV3",
          scriptBytes: Buffer.alloc(9_000, 0x6b),
        },
      }),
      total: 9_050,
      emptyStarts: [44],
    },
  ])("proves $label", ({ cbor, total, emptyStarts }) => {
    const { trace } = proofOf(cbor);
    expect(cbor.length).toBe(total);
    expect(isExactMidgardLedgerOutputProofTerminal(trace.terminal)).toBe(true);
    const starts: number[] = [];
    for (const { control, witness, next } of trace.steps) {
      expect(advanced(control, witness)).toBe(encoded(next));
      const start = emptySpanStart(control);
      if (start === null) continue;
      starts.push(start);
      expect(demandedMidgardLedgerOutputProofSpan(control)).toBeNull();
      expect(witness?.kind).toBe("datum");
    }
    expect(starts).toEqual(emptyStarts);
  });

  it("an empty span takes no window and no span attach", () => {
    const cbor = outputCbor(`9f${chunkedBytes(3_923)}40ff`);
    const { trace, chunk } = proofOf(cbor);
    const step = trace.steps.find(
      ({ control }) => emptySpanStart(control) !== null,
    )!;
    expect(step.witness?.kind).toBe("datum");
    const datum = step.witness as Extract<
      MidgardLedgerOutputProofWitness,
      { readonly kind: "datum" }
    >;
    const recorded = step.control.spanWindow!;
    const recordedBytes = cbor.subarray(
      recorded.start,
      recorded.start + recorded.length,
    );
    expect(advanced(step.control, datum)).toBe(encoded(step.next));
    for (const window of [
      Buffer.alloc(0),
      recordedBytes,
      Buffer.from("00", "hex"),
    ]) {
      expect(advanced(step.control, { ...datum, window })).toBeNull();
    }
    for (const [first, next] of [
      [0, null],
      [1, null],
      [0, 1],
    ] as const) {
      expect(
        advanced(step.control, {
          kind: "spanAttach",
          chunkProof: chunk(first),
          nextChunkProof: next === null ? null : chunk(next),
        }),
      ).toBeNull();
    }
  });
});

describe("ledger output proof span attach over two chunks", () => {
  const cbor = outputCbor(chunkedBytes(5_000));
  const fixture = lazy(() => {
    const { trace, chunk } = proofOf(cbor);
    const attaches = trace.steps.filter(
      ({ witness }) => witness?.kind === "spanAttach",
    );
    return { chunk, attaches };
  });

  it("reads the window exactly with both chunk proofs", () => {
    const { attaches } = fixture();
    expect(cbor.length).toBe(5_204);
    expect(attaches.length).toBeGreaterThan(0);
    const [first] = attaches;
    expect(first!.next.spanWindow).toMatchObject({ start: 45, length: 4_095 });
    expect(Buffer.from(first!.next.spanWindow!.digest)).toEqual(
      Buffer.from(blake2b(cbor.subarray(45, 4_140), { dkLen: 32 })),
    );
    for (const { control, witness, next } of attaches) {
      expect(advanced(control, witness)).toBe(encoded(next));
    }
  });

  it("refuses a missing, extra or wrong chunk proof", () => {
    const { chunk, attaches } = fixture();
    const [first] = attaches;
    for (const [head, next] of [
      [0, null],
      [1, null],
      [0, 0],
      [1, 1],
      [1, 0],
    ] as const) {
      expect(
        advanced(first!.control, {
          kind: "spanAttach",
          chunkProof: chunk(head),
          nextChunkProof: next === null ? null : chunk(next),
        }),
      ).toBeNull();
    }
    const oneChunk = attaches.find(
      ({ witness }) =>
        witness?.kind === "spanAttach" && witness.nextChunkProof === null,
    )!;
    const witness = oneChunk.witness as Extract<
      MidgardLedgerOutputProofWitness,
      { readonly kind: "spanAttach" }
    >;
    expect(
      advanced(oneChunk.control, {
        ...witness,
        nextChunkProof: chunk(witness.chunkProof.chunkIndex === 0 ? 1 : 0),
      }),
    ).toBeNull();
  });
});

describe("ledger output proof authenticated output span", () => {
  const twoChunks = outputCbor(chunkedBytes(5_000));
  const threeChunks = outputCbor("40", {
    script_ref: {
      language: "PlutusV3",
      scriptBytes: Buffer.alloc(9_000, 0x6b),
    },
  });
  const twoProof = lazy(() => proofOf(twoChunks));
  const threeProof = lazy(() => proofOf(threeChunks));
  const spanOf = (
    cbor: Buffer,
    absoluteStart: number,
    length: number,
    first: number,
    next: number | null,
  ) => {
    const { trace, chunk } = cbor === twoChunks ? twoProof() : threeProof();
    return authenticatedOutputSpan({
      control: trace.steps[0]!.control,
      absoluteStart,
      length,
      witness: {
        kind: "chunks",
        chunkProof: chunk(first),
        nextChunkProof: next === null ? null : chunk(next),
      },
    });
  };

  it("reads one- and two-chunk spans exactly", () => {
    for (const [cbor, start, length, first, next] of [
      [twoChunks, 45, 4_095, 0, 1],
      [twoChunks, 4_000, 100, 0, 1],
      [twoChunks, 4_094, 2, 0, 1],
      [twoChunks, 0, 4_095, 0, null],
      [twoChunks, 4_095, 1_109, 1, null],
      [threeChunks, 4_095, 4_095, 1, null],
      [threeChunks, 4_096, 4_095, 1, 2],
      [threeChunks, 8_190, 860, 2, null],
    ] as const) {
      expect(spanOf(cbor, start, length, first, next)).toEqual(
        cbor.subarray(start, start + length),
      );
    }
  });

  it("refuses a span out of bounds or a missing, extra or wrong proof", () => {
    for (const [cbor, start, length, first, next] of [
      [twoChunks, 45, 4_095, 0, null],
      [twoChunks, 45, 4_095, 0, 0],
      [twoChunks, 45, 4_095, 1, 1],
      [twoChunks, 45, 4_095, 1, 0],
      [twoChunks, 0, 4_095, 0, 1],
      [twoChunks, 0, 4_096, 0, 1],
      [twoChunks, 5_203, 2, 1, null],
      [twoChunks, 45, 0, 0, null],
      [twoChunks, -1, 10, 0, null],
      [threeChunks, 4_094, 4_097, 0, 1],
      [threeChunks, 1, 8_190, 0, 1],
      [threeChunks, 4_096, 4_095, 1, null],
      [threeChunks, 4_096, 4_095, 0, 2],
    ] as const) {
      expect(spanOf(cbor, start, length, first, next)).toBeNull();
    }
  });
});

describe("ledger output proof span window", () => {
  const cbor = outputCbor(chunkedBytes(5_000));
  const recorded = lazy(
    () =>
      proofOf(cbor).trace.steps.find(
        ({ witness }) => witness?.kind === "spanAttach",
      )!.next,
  );
  const bytes = cbor.subarray(45, 4_140);
  const bound = (absoluteStart: number, length: number, offered = bytes) =>
    boundMidgardLedgerOutputWindowBytes({
      spanWindow: recorded().spanWindow,
      absoluteStart,
      length,
      bytes: offered,
    });

  it("reads spans inside the window exactly", () => {
    const spanWindow = recorded().spanWindow;
    expect(bound(45, 4_095)).toEqual(bytes);
    expect(bound(4_000, 100)).toEqual(cbor.subarray(4_000, 4_100));
    expect(bound(4_139, 1)).toEqual(cbor.subarray(4_139, 4_140));
    expect(
      midgardLedgerOutputWindowCovers({
        spanWindow,
        absoluteStart: 45,
        length: 4_095,
      }),
    ).toBe(true);
  });

  it("refuses spans outside the window and an empty span", () => {
    const spanWindow = recorded().spanWindow;
    for (const [start, length] of [
      [4_139, 2],
      [44, 1],
      [44, 2],
      [45, 4_096],
      [45, 0],
      [4_140, 0],
    ] as const) {
      expect(bound(start, length)).toBeNull();
      expect(
        midgardLedgerOutputWindowCovers({
          spanWindow,
          absoluteStart: start,
          length,
        }),
      ).toBe(false);
    }
    expect(
      midgardLedgerOutputWindowCovers({
        spanWindow: null,
        absoluteStart: 45,
        length: 1,
      }),
    ).toBe(false);
  });

  it("refuses bytes other than the recorded window", () => {
    const tampered = Buffer.from(bytes);
    tampered[0] ^= 0xff;
    for (const offered of [
      bytes.subarray(0, 4_094),
      Buffer.concat([bytes, Buffer.from("00", "hex")]),
      tampered,
      cbor.subarray(46, 4_141),
    ]) {
      expect(bound(45, 10, offered)).toBeNull();
    }
  });

  it("is well formed only as one chunk inside the output", () => {
    const control = recorded();
    const withWindow = (start: number, length: number, digestLength = 32) => ({
      ...control,
      spanWindow: { start, length, digest: Buffer.alloc(digestLength, 1) },
    });
    expect(isWellFormedMidgardLedgerOutputProofControl(control)).toBe(true);
    for (const [start, length] of [
      [45, 4_095],
      [0, 1],
      [5_204 - 4_095, 4_095],
    ] as const) {
      expect(
        isWellFormedMidgardLedgerOutputProofControl(withWindow(start, length)),
      ).toBe(true);
    }
    for (const window of [
      withWindow(45, 4_096),
      withWindow(45, 0),
      withWindow(5_204 - 4_094, 4_095),
      withWindow(-1, 4_095),
      withWindow(45, 4_095, 31),
    ]) {
      expect(isWellFormedMidgardLedgerOutputProofControl(window)).toBe(false);
    }
  });
});
