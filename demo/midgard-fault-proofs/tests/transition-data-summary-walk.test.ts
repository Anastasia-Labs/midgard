import { buildMidgardCekDataTraverseTrace } from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import type * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  dataSummaryWalkEntryData,
  planDataSummaryWalkTransaction,
  traceDataSummaryWalk,
} from "../src/transition-trace/data-summary-walk.js";
import {
  nextTransitionDatumPhase,
  TransitionDatumUnreadableError,
} from "../src/transition-trace/output-phases.js";

const bytes = (hex: string) => Buffer.from(hex, "hex");
const uint = (major: number, value: bigint) => {
  const m = major << 5;
  if (value < 24n) return Buffer.from([m | Number(value)]);
  const width =
    value < 256n ? 1 : value < 65536n ? 2 : value < 2n ** 32n ? 4 : 8;
  const out = Buffer.alloc(1 + width);
  out[0] = m | { 1: 24, 2: 25, 4: 26, 8: 27 }[width]!;
  for (let i = 0; i < width; i++)
    out[1 + i] = Number((value >> BigInt(8 * (width - 1 - i))) & 255n);
  return out;
};
const chunked = (major: number, content: Buffer) => {
  if (content.length <= 64)
    return Buffer.concat([uint(major, BigInt(content.length)), content]);
  const parts = [Buffer.from([(major << 5) | 31])];
  for (let at = 0; at < content.length; at += 64)
    parts.push(chunked(major, content.subarray(at, at + 64)));
  parts.push(Buffer.from([255]));
  return Buffer.concat(parts);
};
const magnitude = (value: bigint) => {
  let hex = value.toString(16);
  if (hex.length % 2 === 1) hex = `0${hex}`;
  return Buffer.from(hex, "hex");
};
const integer = (value: bigint) =>
  value >= 0n && value < 2n ** 64n
    ? uint(0, value)
    : value < 0n && value >= -(2n ** 64n)
      ? uint(1, -value - 1n)
      : Buffer.concat([
          Buffer.from([value >= 0n ? 0xc2 : 0xc3]),
          chunked(2, magnitude(value >= 0n ? value : -value - 1n)),
        ]);
const list = (items: Buffer[]) =>
  items.length === 0
    ? Buffer.from([0x80])
    : Buffer.concat([Buffer.from([0x9f]), ...items, Buffer.from([0xff])]);
const map = (pairs: [Buffer, Buffer][]) =>
  Buffer.concat([uint(5, BigInt(pairs.length)), ...pairs.flat()]);
const constr = (index: bigint, fields: Buffer[]) =>
  index <= 6n
    ? Buffer.concat([uint(6, 121n + index), list(fields)])
    : index <= 127n
      ? Buffer.concat([uint(6, 1273n + index), list(fields)])
      : Buffer.concat([
          uint(6, 102n),
          Buffer.from([0x82]),
          uint(0, index),
          list(fields),
        ]);
const nested = (nodes: number): Buffer =>
  nodes === 1 ? integer(0n) : list([nested(nodes - 1)]);
const flat = (nodes: number) =>
  list(Array.from({ length: nodes - 1 }, () => integer(0n)));

/** The deployed traverse twin's summary of a canonical span. */
const oracle = (source: Buffer) => {
  const result = buildMidgardCekDataTraverseTrace({ sourceStart: 0, source })
    .terminal.result!;
  return {
    root: Buffer.from(result.root).toString("hex"),
    cborLength: result.cborLength,
    memory: result.memory,
  };
};
const walked = (source: Buffer, offset = 0) => {
  const embedded = Buffer.concat([
    Buffer.alloc(offset, 0x5a),
    source,
    bytes("a5"),
  ]);
  const { outcome } = traceDataSummaryWalk(
    embedded,
    offset,
    offset + source.length,
  );
  return outcome.kind === "unreadable"
    ? "unreadable"
    : {
        root: Buffer.from(outcome.summary.root).toString("hex"),
        cborLength: outcome.summary.cborLength,
        memory: outcome.summary.memory,
      };
};

const canonical: [string, Buffer][] = [
  ...[
    0n,
    23n,
    24n,
    255n,
    256n,
    65535n,
    65536n,
    2n ** 32n,
    2n ** 64n - 1n,
    -1n,
    -25n,
    -(2n ** 64n),
  ].map((value): [string, Buffer] => [`integer ${value}`, integer(value)]),
  ...[2n ** 64n, -(2n ** 64n) - 1n, 2n ** 512n, -(2n ** 512n), 2n ** 2000n].map(
    (value): [string, Buffer] => [`bignum ${value}`, integer(value)],
  ),
  ...[0, 1, 64, 65, 128, 129, 5000].map((length): [string, Buffer] => [
    `${length} bytes`,
    chunked(2, Buffer.alloc(length, 7)),
  ]),
  ["an empty map", map([])],
  [
    "a map of mixed entries",
    map([
      [integer(1n), list([integer(2n)])],
      [chunked(2, Buffer.alloc(70, 1)), map([[integer(0n), constr(0n, [])]])],
    ]),
  ],
  ["constructor 0", constr(0n, [integer(1n), chunked(2, Buffer.alloc(3, 9))])],
  ["constructor 7", constr(7n, [])],
  ["constructor 1280's alternative 127", constr(127n, [integer(5n)])],
  ["constructor 128", constr(128n, [])],
  ["constructor 2^64 - 1", constr(2n ** 64n - 1n, [integer(2n ** 64n)])],
  ["512 nested nodes", nested(512)],
  ["512 flat nodes", flat(512)],
  ["1,024 nested nodes", nested(1024)],
  ["1,024 flat nodes", flat(1024)],
];

const nonCanonical: [string, string][] = [
  ["an empty indefinite list", "9fff"],
  ["a constructor over an empty indefinite list", "d8799fff"],
  ["a definite non-empty list", "8100"],
  ["an indefinite map", "bfff"],
  ["a non-minimal integer head", "1817"],
  ["a small tagged bignum", "c24105"],
  ["short indefinite bytes", "5f416aff"],
  ["a general-form constructor below 128", "d866820580"],
  ["trailing bytes", "0000"],
  ["a missing break", "9f00"],
  ["a text string", "6161"],
];

describe("the transition datum walk", () => {
  it.each(canonical)(
    "summarises %s as the traverse twin does",
    (_name, source) => {
      const expected = oracle(source);
      expect(walked(source)).toEqual(expected);
      expect(walked(source, 3)).toEqual(expected);
    },
  );

  it.each(nonCanonical)("refuses %s", (_name, hex) => {
    expect(walked(bytes(hex))).toBe("unreadable");
  });

  it.each([
    ["512 nested nodes", nested(512)],
    ["1,024 flat nodes", flat(1024)],
    [
      "a mixed datum",
      constr(1280n - 1273n, [
        map([[chunked(2, Buffer.alloc(70, 2)), nested(40)]]),
        integer(-(2n ** 512n)),
        constr(2n ** 64n - 1n, [flat(60)]),
      ]),
    ],
  ])(
    "cuts %s into transactions whose witnesses chain to each checkpoint",
    (_name, source) => {
      const trace = traceDataSummaryWalk(source, 0, source.length);
      let from = 0;
      let transactions = 0;
      for (;;) {
        const transaction = planDataSummaryWalkTransaction(trace, from);
        const point = trace.points[from]!;
        const chained = [...transaction.restore]
          .reverse()
          .reduce(
            (below, entry) =>
              computeHash32(
                Buffer.concat([
                  Buffer.from(Data.to(dataSummaryWalkEntryData(entry)), "hex"),
                  below,
                ]),
              ),
            transaction.base,
          );
        expect(chained.toString("hex")).toBe(
          (point.stack?.chain ?? Buffer.alloc(0)).toString("hex"),
        );
        expect(transaction.steps).toBeGreaterThan(0);
        transactions += 1;
        if (transaction.final) break;
        from += transaction.steps;
      }
      expect(from).toBeLessThan(trace.steps.length);
      expect(transactions).toBeGreaterThan(1);
    },
  );
});

/**
 * An output whose inline datum is exactly `datum`. The output encoder refuses
 * a non-canonical datum, so a short one replaces a byte string of its length.
 */
const outputWithDatum = (datum: Buffer) => {
  const placeholder =
    datum.length > 24
      ? datum
      : Buffer.concat([
          Buffer.from([0x40 + datum.length - 1]),
          Buffer.alloc(datum.length - 1, 0xee),
        ]);
  const output = encodeMidgardTxOutput({
    address: Buffer.from("60" + "aa".repeat(28), "hex"),
    value: { lovelace: 2_000_000n, assets: new Map() },
    datum: { kind: "inline", cbor: placeholder },
  });
  datum.copy(output, output.lastIndexOf(placeholder));
  return output.toString("hex");
};
const phaseEight: SDK.TransitionTraceFinalState = {
  kind: 0n,
  phase: 8n,
  proof_commitment: { hash: "ff".repeat(32) },
  opened: { spend_input_keys: [], output_hashes: [] },
  input_index: 0n,
  output_index: 0n,
  current_root: "",
  summaries: { summaries: [] },
  scan_cbor: "",
  value_cbor: "",
  value_start: 0n,
  value_end: 0n,
  value_summary: null,
  descriptor_cbor: "",
  deposit_index: 0n,
  deposit_source_cbor: "",
  deposit_asset_count: 0n,
  datum_walk: null,
};
const summary = (byte: string) => ({
  root: byte.repeat(32),
  cbor_length: 1n,
  memory: 4n,
});
const summaries: SDK.TransitionTraceOutputSummaries["summaries"][number] = [
  summary("01"),
  summary("02"),
  summary("03"),
];

describe("the phase-8 datum step", () => {
  it("walks 512 nested nodes over several checkpoints, then commits the summaries", () => {
    const output = outputWithDatum(nested(512));
    let state = phaseEight;
    let transactions = 0;
    while (state.phase === 8n) {
      state = nextTransitionDatumPhase(state, output, summaries).state;
      transactions += 1;
    }
    expect(transactions).toBeGreaterThan(1);
    expect(state.phase).toBe(3n);
    expect(state.datum_walk).toBeNull();
    expect(state.summaries).toEqual({ summaries: [summaries] });
  });

  it("refuses a checkpoint that is not on the output's walk", () => {
    const output = outputWithDatum(nested(512));
    const first = nextTransitionDatumPhase(phaseEight, output, summaries).state;
    const elsewhere = nextTransitionDatumPhase(
      phaseEight,
      outputWithDatum(flat(512)),
      summaries,
    ).state;
    expect(() =>
      nextTransitionDatumPhase(
        { ...first, datum_walk: elsewhere.datum_walk },
        output,
        summaries,
      ),
    ).toThrow();
  });

  it.each(nonCanonical.filter(([, hex]) => hex !== "0000" && hex !== "9f00"))(
    "refuses to step an output whose datum is %s",
    (_name, hex) => {
      expect(() =>
        nextTransitionDatumPhase(
          phaseEight,
          outputWithDatum(bytes(hex)),
          summaries,
        ),
      ).toThrow(TransitionDatumUnreadableError);
    },
  );
});
