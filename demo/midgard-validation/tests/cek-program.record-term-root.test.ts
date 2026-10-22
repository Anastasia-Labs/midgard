import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import { CardanoFlatRefusal } from "../src/cek-program.cardano-flat.reader.js";
import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";
import { scriptDecodeRecordOfScript } from "../src/script-decode.step-walk.js";
import { scriptDecodeStepScripts } from "./fixtures/script-decode-step-v1.vectors.mjs";

/**
 * The dispute program built from a stored script is the program its decode
 * record names: the same term root, for every program Cardano's decoder
 * accepts and can run. The V1 dispute profile cannot represent array and
 * Value constants, an always-unbound variable index, or a constant beyond the
 * one-transaction proof envelope; the builder refuses exactly those, and never
 * builds a different program.
 */

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
// A program whose version or scope fails it before evaluation has no run.
const decoded = rows.filter(
  (row) => row.record.k === "decoded" && row.record.phase2Class === 0,
);

const builtRoot = (script: Buffer): Buffer | "unrepresentable" => {
  try {
    return Buffer.from(buildMidgardCanonicalCekProgram(script).termRoot);
  } catch (error) {
    if (
      (error instanceof CardanoFlatRefusal &&
        error.refusal === "outsideProfile") ||
      (error instanceof Error &&
        error.message.includes("exceeds the 9,215-byte L1 proof envelope"))
    )
      return "unrepresentable";
    throw error;
  }
};

describe("the dispute program is the decode record's program", () => {
  it("builds most of Cardano's decoded programs", () => {
    const built = decoded.filter(
      (row) => builtRoot(scriptOf(row)) !== "unrepresentable",
    );
    expect(built.length).toBeGreaterThan(decoded.length * 0.8);
  });

  it.each(
    decoded.map(
      (row) =>
        [
          row.flipBit === undefined
            ? row.label
            : `${row.label}, bit ${row.flipBit.toString()} flipped`,
          row,
        ] as const,
    ),
  )("%s", (_, row) => {
    const script = scriptOf(row);
    const record = scriptDecodeRecordOfScript(3, script);
    expect(record.k).toBe("decoded");
    const root = builtRoot(script);
    if (root !== "unrepresentable" && record.k === "decoded")
      expect(root.toString("hex")).toBe(record.termRoot.toString("hex"));
  });
});
