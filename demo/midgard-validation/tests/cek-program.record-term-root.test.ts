import { readFileSync } from "node:fs";

import { encodeCbor } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { CardanoFlatRefusal } from "../src/cek-program.cardano-flat.reader.js";
import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";
import { scriptDecodeRecordOfScript } from "../src/script-decode.step-walk.js";
import { constT, UPLCConst } from "../src/uplc-constant.js";
import { type Data, DataB, DataI, DataList } from "../src/uplc-data.js";
import { Application, Lambda, UPLCProgram } from "../src/uplc-term.js";
import { scriptDecodeStepScripts } from "./fixtures/script-decode-step-v1.vectors.mjs";

/**
 * The dispute program built from a stored script is the program its decode
 * record names: the same term root, for every program Cardano's decoder
 * accepts and can run, whatever the size of its constants. The V1 dispute
 * profile cannot represent array and Value constants or an always-unbound
 * variable index; the builder refuses exactly those, and never builds a
 * different program.
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
      error instanceof CardanoFlatRefusal &&
      error.refusal === "outsideProfile"
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

/** `(program 1.1.0 (lam ctx [(lam _ (con unit ())) constant]))`, stored. */
const holding = (constant: UPLCConst): Buffer =>
  encodeCbor(
    Buffer.from(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram(
          [1, 1, 0],
          new Lambda(new Application(new Lambda(UPLCConst.unit), constant)),
        ),
      ),
    ),
  );

const wide = 2n ** BigInt(8 * 9_300) + 12_345n;
const integers = (count: number): Data[] =>
  Array.from(
    { length: count },
    (_, index) => new DataI(2n ** 60n + BigInt(index)),
  );

/** Constants whose payload is past the 9,215-byte direct witness bound. */
const PAST_THE_BOUND = [
  [
    "a bytestring of 9,216 bytes",
    UPLCConst.byteString(new DataB(Buffer.alloc(9_216, 0x5a)).bytes),
  ],
  [
    "a bytestring of 15,000 bytes",
    UPLCConst.byteString(new DataB(Buffer.alloc(15_000, 0xa5)).bytes),
  ],
  ["a string of 9,600 bytes", UPLCConst.str("\u20ac".repeat(3_200))],
  ["an integer above 2^74,400", UPLCConst.int(wide)],
  ["an integer below -2^74,400", UPLCConst.int(-wide)],
  [
    "a Data list of 1,200 integers",
    UPLCConst.data(new DataList(integers(1_200))),
  ],
  [
    "a list of 1,100 integers",
    UPLCConst.listOf(constT.int)(
      Array.from(
        { length: 1_100 },
        (_, index) => 2n ** 60n + BigInt(index),
      ) as never,
    ),
  ],
  [
    "a list of 1,100 Data integers",
    UPLCConst.listOf(constT.data)(integers(1_100)),
  ],
] as const;

describe("the dispute program of a constant past the direct witness bound is the decode record's program", () => {
  it.each(PAST_THE_BOUND)("%s", (_, constant) => {
    const script = holding(constant);
    const record = scriptDecodeRecordOfScript(3, script);
    if (record.k !== "decoded") throw new Error("expected decoded record");
    expect(record.phase2Class).toBe(0);
    const program = buildMidgardCanonicalCekProgram(script);
    expect(Buffer.from(program.termRoot).toString("hex")).toBe(
      record.termRoot.toString("hex"),
    );
    expect(
      [...program.constantWitnesses.values()].map(({ kind }) => kind),
    ).toContain("semanticConstant");
  });
});
