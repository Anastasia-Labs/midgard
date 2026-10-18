/**
 * The ticks a script decode takes over constant lists nested deep.
 *
 * A tick whose bulk decode stops inside a list keeps everything it decoded
 * before the stop and resumes at the innermost list it was in; only the value
 * it stopped in is read again. So each Flat bit is read by at most two
 * decodes that are discarded, besides the scan, the fold and the one summary
 * decode of a pair's first component that are kept, however deep the lists
 * nest. At the same size, a list nested many levels deep then takes no more
 * ticks per bit than one nested two levels deep.
 */
import { describe, expect, it } from "vitest";

import { SCRIPT_DECODE_LANGUAGE_PLUTUS_V3 } from "../src/script-decode.step.js";
import {
  planScriptDecodeWalk,
  scriptDecodeRecordOfScript,
} from "../src/script-decode.step-walk.js";
import { flatOfTermBits } from "./cek-program.cardano-flat.vectors.js";

const SIZE = 2_048;

const tag = (value: number): string => "1" + value.toString(2).padStart(4, "0");

/** The Flat program wrapped in one definite CBOR byte string. */
const script = (termBits: string): Buffer => {
  const flat = Buffer.from(flatOfTermBits(termBits), "hex");
  const n = flat.length;
  const head =
    n < 24
      ? Buffer.from([0x40 + n])
      : n < 256
        ? Buffer.from([0x58, n])
        : Buffer.from([0x59, n >> 8, n & 255]);
  return Buffer.concat([head, flat]);
};

/**
 * A constant list of `count` values, each `levels` lists deep around
 * `inner` pair chains of `depth` unit-first pairs over `[()]`; the lists
 * nest `levels + 1` deep.
 */
const nested = (
  levels: number,
  depth: number,
  inner: number,
  count: number,
): Buffer => {
  const types =
    tag(7) +
    tag(5) +
    (tag(7) + tag(5)).repeat(levels) +
    (tag(7) + tag(7) + tag(6) + tag(3)).repeat(depth) +
    tag(7) +
    tag(5) +
    tag(3);
  const element =
    "1".repeat(levels - 1) + "110".repeat(inner) + "0" + "0".repeat(levels - 1);
  return script("0100" + types + "0" + ("1" + element).repeat(count) + "0");
};

/** The most repetitions that fit `SIZE` bytes. */
const largest = (make: (count: number) => Buffer): Buffer => {
  let count = 1;
  while (make(count + 1).length <= SIZE) count += 1;
  return make(count);
};

/** Ticks per Flat bit of the whole walk, one tick per step. */
const ticksPerBit = (bytes: Buffer): number => {
  const walk = planScriptDecodeWalk(SCRIPT_DECODE_LANGUAGE_PLUTUS_V3, bytes, 1);
  expect(walk.record).toEqual(
    scriptDecodeRecordOfScript(SCRIPT_DECODE_LANGUAGE_PLUTUS_V3, bytes),
  );
  expect(walk.record.k).toBe("decoded");
  return walk.steps.length / (8 * bytes.length);
};

describe("script-decode ticks over nested lists", () => {
  it.each([
    ["lists of units", 14, 0, 103],
    ["lists of depth-2 unit pair chains", 10, 2, 47],
    ["lists of depth-4 unit pair chains", 6, 4, 31],
  ] as const)(
    "decodes %s nested %i deep in no more ticks per bit than two deep",
    (_label, nesting, depth, inner) => {
      const shallow = ticksPerBit(
        largest((count) => nested(1, depth, inner, count)),
      );
      const deepest = ticksPerBit(
        largest((count) => nested(nesting - 1, depth, inner, count)),
      );
      expect(deepest).toBeLessThanOrEqual(1.1 * shallow);
    },
  );
});
