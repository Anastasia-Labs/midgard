import { readFileSync } from "node:fs";

import { encodeCbor } from "@al-ft/midgard-core";
import {
  ErrorUPLC,
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { storedFlatProgramBytes } from "../src/cek-program.unwrap-canonical-cbor-byte-string.js";
import { evaluateStoredMidgardPlutusScript } from "../src/plutus-script-evaluator.js";
import { scriptDecodeRecordOfScript } from "../src/script-decode.step-walk.js";
import { scriptDecodeStepScripts } from "./fixtures/script-decode-step-v1.vectors.mjs";

/**
 * The PV11 evaluator runs a carried script's stored Flat program, applied to
 * its script context, with the ledger's budget semantics.
 */

const UNIT_CONTEXT = Buffer.from("d87980", "hex");

const script = (
  term: UPLCTerm,
  version: [number, number, number] = [1, 1, 0],
): Buffer =>
  encodeCbor(Buffer.from(UPLCEncoder.compile(new UPLCProgram(version, term))));

const evaluate = (
  scriptBytes: Buffer,
  executionBudget?: { cpu: bigint; memory: bigint },
) =>
  evaluateStoredMidgardPlutusScript({
    scriptBytes,
    scriptLanguage: "PlutusV3",
    contextCbor: UNIT_CONTEXT,
    ...(executionBudget === undefined ? {} : { executionBudget }),
    executionIndex: 0n,
    maxSteps: 1,
  });

/** Fails `verdict` unless it refuses the script with `detail` in its detail. */
const expectInvalid = (
  verdict: ReturnType<typeof evaluate>,
  detail: string,
) => {
  expect(verdict.kind).toBe("script_invalid");
  if (verdict.kind === "script_invalid")
    expect(verdict.detail).toContain(detail);
};

describe("the PV11 evaluator", () => {
  it("accepts a script returning unit, with the budget it spent", () => {
    const verdict = evaluate(script(new Lambda(UPLCConst.unit)));
    expect(verdict.kind).toBe("accepted");
    if (verdict.kind === "accepted") {
      expect(verdict.budget.cpu).toBeGreaterThan(0n);
      expect(verdict.budget.memory).toBeGreaterThan(0n);
    }
  });

  it("fails a script that errors", () => {
    expectInvalid(
      evaluate(script(new Lambda(new ErrorUPLC()))),
      "execution failed (2)",
    );
  });

  it("fails a script that runs out of its budget", () => {
    expectInvalid(
      evaluate(script(new Lambda(UPLCConst.unit)), { cpu: 1n, memory: 1n }),
      "execution failed (1)",
    );
  });

  it("fails a PlutusV3 script returning something other than unit", () => {
    expectInvalid(
      evaluate(script(new Lambda(UPLCConst.int(1)))),
      "execution failed (3)",
    );
  });

  it.each([
    ["an unavailable program version", script(new UPLCVar(0), [2, 0, 0])],
    ["an open term", script(new UPLCVar(0))],
  ])("fails %s before running it", (_, scriptBytes) => {
    expectInvalid(evaluate(scriptBytes), "phase-two class");
  });
});

// Flat bits, most significant first.
const bitsOf = (bytes: Uint8Array): number[] =>
  [...bytes].flatMap((byte) =>
    Array.from({ length: 8 }, (_, bit) => (byte >> (7 - bit)) & 1),
  );
const bytesOf = (bits: readonly number[]): Buffer =>
  Buffer.from(
    Array.from({ length: bits.length / 8 }, (_, index) =>
      bits
        .slice(index * 8, index * 8 + 8)
        .reduce((byte, bit) => (byte << 1) | bit, 0),
    ),
  );
const bitsFrom = (text: string): number[] => [...text].map(Number);

/** The bit length of a Flat program's three version naturals. */
const versionBitLength = (bits: readonly number[]): number => {
  let offset = 0;
  for (let natural = 0; natural < 3; natural += 1) {
    for (;;) {
      const more = bits[offset] === 1;
      offset += 8;
      if (!more) break;
    }
  }
  return offset;
};

const BUILTIN_ADD_INTEGER = "0111" + "0000000";
/**
 * `[(lam _ (lam _ [(lam _ [(lam _ ()) b]) b])) (delay T)]`, with `b` the
 * builtin addInteger: 64 bits, so T keeps its byte alignment and its byte
 * strings their fillers.
 */
const HOLDING_PREFIX = bitsFrom(
  ["0011", "0010", "0010"].join("") +
    ["0011", "0010", "0011", "0010", "0100" + "1" + "0011" + "0"].join("") +
    BUILTIN_ADD_INTEGER +
    BUILTIN_ADD_INTEGER +
    "0001",
);

/**
 * T held unevaluated under its own version, by a program that succeeds on one
 * argument: the evaluator's verdict on it depends on whether it decodes T,
 * never on what T does. T ends where the old filler's zeros begin; the decode
 * record finds that point, as a filler of eight or more zeros is refused.
 */
const holdingUnevaluated = (scriptBytes: Buffer): Buffer => {
  const bits = bitsOf(storedFlatProgramBytes(scriptBytes));
  const version = versionBitLength(bits);
  for (let end = bits.length - 1; end >= bits.length - 8; end -= 1) {
    const wrapped = [
      ...bits.slice(0, version),
      ...HOLDING_PREFIX,
      ...bits.slice(version, end),
    ];
    while ((wrapped.length + 1) % 8 !== 0) wrapped.push(0);
    wrapped.push(1);
    const held = encodeCbor(bytesOf(wrapped));
    if (scriptDecodeRecordOfScript(3, held).k === "decoded") return held;
    if (bits[end - 1] !== 0) break;
  }
  throw new Error("no decodable holding program");
};

type HaskellRow = {
  readonly label: string;
  readonly flipBit?: number;
  readonly record: { readonly k: string; readonly phase2Class?: number };
};

const { rows } = JSON.parse(
  readFileSync(
    new URL(
      "./fixtures/script-decode-haskell-v1.vectors.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as { readonly rows: readonly HaskellRow[] };
const corpus = new Map(
  scriptDecodeStepScripts()
    .filter(([, language]) => language === 3)
    .map(([label, , bytes]) => [label, bytes] as const),
);
const scriptOf = (row: HaskellRow): Buffer => {
  const bytes = Buffer.from(corpus.get(row.label)!);
  if (row.flipBit !== undefined)
    bytes[row.flipBit >> 3]! ^= 0x80 >> (row.flipBit & 7);
  return bytes;
};
const runnable = rows.filter(
  (row) => row.record.k === "decoded" && row.record.phase2Class === 0,
);

describe("the evaluator decodes what the decode record decodes", () => {
  it("covers the runnable programs of Cardano's own decoder verdicts", () => {
    expect(HOLDING_PREFIX).toHaveLength(64);
    expect(runnable.length).toBeGreaterThan(50);
  });

  it.each(
    runnable.map(
      (row) =>
        [
          row.flipBit === undefined
            ? row.label
            : `${row.label}, bit ${row.flipBit.toString()} flipped`,
          row,
        ] as const,
    ),
  )("%s", (_, row) => {
    const held = holdingUnevaluated(scriptOf(row));
    expect(scriptDecodeRecordOfScript(3, held)).toMatchObject({
      k: "decoded",
      phase2Class: 0,
    });
    expect(evaluate(held).kind).toBe("accepted");
  });
});
