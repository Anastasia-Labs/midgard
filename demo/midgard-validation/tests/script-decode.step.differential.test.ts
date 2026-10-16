import { describe, expect, it } from "vitest";

import {
  type CardanoFlatRefusalClass,
  decodeCardanoFlatProgram,
} from "../src/cek-program.cardano-flat.js";
import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";
import {
  SCRIPT_DECODE_LANGUAGE_PLUTUS_V3,
  ScriptDecodeClass,
} from "../src/script-decode.step.js";
import {
  planScriptDecodeWalk,
  scriptDecodeRecordOfScript,
} from "../src/script-decode.step-walk.js";
import {
  CARDANO_ACCEPTED,
  CARDANO_REFUSED,
  CONSTR_1024_UNITS,
  CONSTR_1025_UNITS,
  flatOfTermBits,
  OUTSIDE_PROFILE,
} from "./cek-program.cardano-flat.vectors.js";
import { scriptDecodeStepScripts } from "./fixtures/script-decode-step-v1.vectors.mjs";

const bytes = (value: string): Buffer => Buffer.from(value, "hex");

/** The Flat program wrapped in one definite CBOR byte string. */
const wrap = (flat: Buffer): Buffer => {
  const n = flat.length;
  const head =
    n < 24
      ? Buffer.from([0x40 + n])
      : n < 256
        ? Buffer.from([0x58, n])
        : Buffer.from([0x59, n >> 8, n & 255]);
  return Buffer.concat([head, flat]);
};

/** The Flat inside a script's CBOR byte-string wrapper. */
const unwrap = (script: Buffer): Buffer =>
  script.subarray(script[0]! < 0x58 ? 1 : script[0] === 0x58 ? 2 : 3);

/** The constant lists of the golden vectors, decoded in bulk. */
const BULK_ROWS = scriptDecodeStepScripts()
  .filter(([label]) => label.startsWith("bulk") && !label.includes(","))
  .map(([label, , script]) => [label, unwrap(script).toString("hex")] as const);

/**
 * Each reader verdict and the script-decode verdict it projects to: a class
 * code, or `decoded` for a program the reader puts outside the profile but
 * Cardano decodes.
 */
const PROJECTION: Readonly<
  Record<CardanoFlatRefusalClass, number | "decoded">
> = {
  notEnoughSpace: ScriptDecodeClass.flatTruncated,
  tooMuchSpace: ScriptDecodeClass.flatBadPadding,
  fillerNotByteAligned: ScriptDecodeClass.flatBadPadding,
  unknownTermTag: ScriptDecodeClass.unknownTermTag,
  constrOrCaseBeforeV110: ScriptDecodeClass.constrCaseBeforeV110,
  unknownBuiltin: ScriptDecodeClass.unavailableBuiltin,
  builtinNotAvailable: ScriptDecodeClass.unavailableBuiltin,
  badConstantType: ScriptDecodeClass.unknownConstantType,
  constantTypeTooLarge: ScriptDecodeClass.constantTypeTooLarge,
  constantNotFlatDecodable: ScriptDecodeClass.blsConstant,
  word64Overflow: ScriptDecodeClass.wordOverflow,
  constrTooManyFields: ScriptDecodeClass.constrFieldsOverLimit,
  invalidUtf8: ScriptDecodeClass.invalidUtf8,
  invalidData: ScriptDecodeClass.invalidData,
  invalidValue: ScriptDecodeClass.nonCanonicalValue,
  outsideProfile: "decoded",
};

/**
 * Data constants with bytes after the first item. Cardano reads the first
 * item and steps over the rest, as the step machine does; the reader refuses
 * them.
 */
const TRAILING_DATA = new Set([
  "0101004c01020000000001",
  "0101004c010200ff0001",
]);

const walkVerdict = (flat: Buffer) => {
  const { record } = planScriptDecodeWalk(
    SCRIPT_DECODE_LANGUAGE_PLUTUS_V3,
    wrap(flat),
    64,
  );
  return record;
};

const readerVerdict = (flat: Buffer): number | "decoded" | "accepted" => {
  const read = decodeCardanoFlatProgram(flat);
  return read.ok ? "accepted" : PROJECTION[read.refusal];
};

const RAW_ROWS: readonly (readonly [string, string])[] = [
  ...CARDANO_ACCEPTED.flatMap(
    ([label, raw, , twin]) =>
      [
        [label, raw],
        [`${label} (twin)`, twin],
      ] as const,
  ),
  ...CARDANO_REFUSED.map(([label, raw]) => [label, raw] as const),
  ...OUTSIDE_PROFILE.map(([label, raw]) => [label, raw] as const),
  ["a constr of 1,024 units", CONSTR_1024_UNITS],
  ["a constr of 1,025 units", CONSTR_1025_UNITS],
  ["Data with a trailing byte string", "0101004c01020000000001"],
  ["Data with a trailing break", "0101004c010200ff0001"],
  ["deep delays", flatOfTermBits("0001".repeat(2_000) + "0110")],
  ...BULK_ROWS,
];

describe("the script-decode step port against the Flat reader", () => {
  it.each(RAW_ROWS)("%s gets the reader's class", (_label, raw) => {
    const flat = bytes(raw);
    const record = walkVerdict(flat);
    const reader = readerVerdict(flat);
    if (TRAILING_DATA.has(raw)) {
      expect(reader).toBe(ScriptDecodeClass.invalidData);
      expect(record).toMatchObject({ k: "decoded", phase2Class: 0 });
      return;
    }
    if (reader === "accepted") {
      expect(record).toMatchObject({ k: "decoded", phase2Class: 0 });
    } else if (reader === "decoded") {
      expect(record.k).toBe("decoded");
    } else {
      expect(record).toMatchObject({ k: "refused", class: reader });
    }
  });

  it.each(RAW_ROWS)("%s has the builder's term root", (_label, raw) => {
    const flat = bytes(raw);
    let built: Uint8Array | undefined;
    try {
      built = buildMidgardCanonicalCekProgram(flat).envelope.termRoot;
    } catch {
      built = undefined;
    }
    const record = walkVerdict(flat);
    if (built === undefined) return;
    expect(record.k).toBe("decoded");
    if (record.k === "decoded") {
      expect(record.termRoot.toString("hex")).toBe(
        Buffer.from(built).toString("hex"),
      );
    }
  });

  it.each(RAW_ROWS)("%s ends one-shot where it ends walked", (_label, raw) => {
    const script = wrap(bytes(raw));
    expect(
      scriptDecodeRecordOfScript(SCRIPT_DECODE_LANGUAGE_PLUTUS_V3, script),
    ).toEqual(
      planScriptDecodeWalk(SCRIPT_DECODE_LANGUAGE_PLUTUS_V3, script, 3).record,
    );
  });

  it("refuses bytes that are not one CBOR byte string", () => {
    expect(
      planScriptDecodeWalk(SCRIPT_DECODE_LANGUAGE_PLUTUS_V3, bytes("8001"), 8),
    ).toMatchObject({
      steps: [],
      record: {
        k: "refused",
        class: ScriptDecodeClass.cborNotBytes,
        bitOffset: 0,
      },
    });
  });

  it("refuses bytes after the CBOR byte string", () => {
    const flat = bytes(CARDANO_ACCEPTED[0][3]);
    expect(
      planScriptDecodeWalk(
        SCRIPT_DECODE_LANGUAGE_PLUTUS_V3,
        Buffer.concat([wrap(flat), bytes("00")]),
        64,
      ).record,
    ).toMatchObject({
      k: "refused",
      class: ScriptDecodeClass.cborTrailingBytes,
      bitOffset: 8 * (1 + flat.length),
    });
  });
});
