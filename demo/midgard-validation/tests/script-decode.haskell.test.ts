import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { scriptDecodeRecordOfScript } from "../src/script-decode.step-walk.js";
import { scriptDecodeStepScripts } from "./fixtures/script-decode-step-v1.vectors.mjs";

/**
 * The step machine against Cardano's own decoder.
 *
 * `fixtures/script-decode-haskell-v1.vectors.json` holds the verdict of
 * `deserialiseScript` and `mkTermToEvaluate` at protocol version 11 on every
 * PlutusV3 script of the step golden vectors and on seeded one-bit flips of
 * the small ones, as the record it stands for. Regenerate it with
 * `pnpm run fixtures:script-decode-haskell-v1:sync` and the built
 * `scripts/haskell/ScriptDecodeProbe.hs`.
 *
 * Where a byte-string chunk's length byte asks for more bytes than are left,
 * Cardano's decoder state is just past that length byte, and the record's
 * `bit_offset` is the length byte itself, 8 bits earlier. Those rows are
 * compared under that rule, and every other row exactly.
 */

type HaskellRecord =
  | {
      readonly k: "decoded";
      readonly programVersion: number;
      readonly phase2Class: number;
    }
  | {
      readonly k: "refused";
      readonly class: number;
      readonly bitOffset: number;
    };

type HaskellRow = {
  readonly label: string;
  readonly flipBit?: number;
  readonly scriptBytes: number;
  readonly record: HaskellRecord;
};

const PLUTUS_V3 = 3;

const { rows } = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/script-decode-haskell-v1.vectors.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as { readonly rows: readonly HaskellRow[] };

const scripts = new Map(
  scriptDecodeStepScripts()
    .filter(([, language]) => language === PLUTUS_V3)
    .map(([label, , script]) => [label, script] as const),
);

const flipScriptBit = (script: Buffer, bit: number): Buffer => {
  const flipped = Buffer.from(script);
  flipped[bit >> 3]! ^= 0x80 >> (bit & 7);
  return flipped;
};

const scriptOf = (row: HaskellRow): Buffer => {
  const script = scripts.get(row.label);
  if (script === undefined) throw new Error(`no script ${row.label}`);
  return row.flipBit === undefined
    ? script
    : flipScriptBit(script, row.flipBit);
};

const recordOf = (script: Buffer): HaskellRecord => {
  const record = scriptDecodeRecordOfScript(PLUTUS_V3, script);
  return record.k === "decoded"
    ? {
        k: "decoded",
        programVersion: record.programVersion,
        phase2Class: record.phase2Class,
      }
    : { k: "refused", class: record.class, bitOffset: record.bitOffset };
};

/** Whether a byte-string chunk length byte sits at `bitOffset` in `script`
 * and asks for more bytes than are left. */
const overrunningChunkHeadAt = (script: Buffer, bitOffset: number): boolean => {
  if (bitOffset % 8 !== 0) return false;
  const head = bitOffset / 8;
  const length = script[head] ?? 0;
  return length > 0 && head + 1 + length > script.length;
};

/**
 * The record Cardano's verdict stands for: Cardano's own, except that a
 * refusal at an overrunning chunk is recorded at the chunk's length byte.
 */
const expectedOf = (
  script: Buffer,
  cardano: HaskellRecord,
  actual: HaskellRecord,
): HaskellRecord =>
  cardano.k === "refused" &&
  cardano.class === 3 &&
  actual.k === "refused" &&
  actual.bitOffset === cardano.bitOffset - 8 &&
  overrunningChunkHeadAt(script, actual.bitOffset)
    ? { ...cardano, bitOffset: actual.bitOffset }
    : cardano;

const atChunkHead = (row: HaskellRow): boolean => {
  const script = scriptOf(row);
  return expectedOf(script, row.record, recordOf(script)) !== row.record;
};

const labelOf = (row: HaskellRow): string =>
  row.flipBit === undefined
    ? row.label
    : `${row.label}, bit ${row.flipBit.toString()} flipped`;

describe("script decode against Cardano's decoder", () => {
  it("covers every golden PlutusV3 script and both verdicts", () => {
    expect(
      new Set(
        rows.filter((row) => row.flipBit === undefined).map((row) => row.label),
      ),
    ).toEqual(new Set(scripts.keys()));
    const kinds = new Set(rows.map((row) => row.record.k));
    expect(kinds).toEqual(new Set(["decoded", "refused"]));
    const overruns = rows.filter(atChunkHead);
    expect(overruns.length).toBeGreaterThan(0);
    expect(overruns.length).toBeLessThan(rows.length / 4);
  });

  it.each(rows.map((row) => [labelOf(row), row] as const))(
    "%s: the same record as Cardano",
    (_, row) => {
      const script = scriptOf(row);
      expect(script.length).toBe(row.scriptBytes);
      const actual = recordOf(script);
      expect(actual).toEqual(expectedOf(script, row.record, actual));
    },
  );
});
