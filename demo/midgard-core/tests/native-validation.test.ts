import { describe, expect, it } from "vitest";

import { decodeSingleCbor } from "../src/codec/cbor.js";
import {
  decodeMidgardNativeTxCanonical,
  decodeMidgardNativeTxCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
} from "../src/codec/native.js";
import {
  decodeValidityCode,
  encodeValidityCode,
  type MidgardTxValidity,
  MidgardTxValidityCodes,
} from "../src/codec/native-validation.js";

const VALIDITY_VECTORS = [
  ["TxIsValid", 0n, "d87980"],
  ["TxIsInvalid", 1n, "d87a80"],
] as const satisfies readonly [MidgardTxValidity, bigint, string][];

const makeCanonical = (
  validity: MidgardTxValidity,
): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
  validity,
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: EMPTY_CBOR_LIST,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  },
});

const replaceFinalValidityCode = (
  encoded: Uint8Array,
  replacement: Uint8Array,
): Buffer => Buffer.concat([Buffer.from(encoded).subarray(0, -1), replacement]);

describe("Midgard native V1 validity language", () => {
  it("is the exact total code and Plutus-constructor bijection", () => {
    expect(Object.keys(MidgardTxValidityCodes)).toHaveLength(2);
    expect(new Set(Object.values(MidgardTxValidityCodes))).toEqual(
      new Set([0n, 1n]),
    );
    expect(new Set(VALIDITY_VECTORS.map(([, , cbor]) => cbor))).toEqual(
      new Set(["d87980", "d87a80"]),
    );

    for (const [meaning, code, plutusDataCbor] of VALIDITY_VECTORS) {
      const full = materializeMidgardNativeTxFromCanonical(
        makeCanonical(meaning),
      );
      const compactCbor = encodeMidgardNativeTxCompact(full.compact);
      const canonicalCbor = encodeMidgardNativeTxCanonical(full);

      expect(MidgardTxValidityCodes[meaning]).toBe(code);
      expect(encodeValidityCode(meaning)).toBe(code);
      expect(decodeValidityCode(code, "validity")).toBe(meaning);
      expect(compactCbor.subarray(-1).toString("hex")).toBe(
        code.toString(16).padStart(2, "0"),
      );
      expect(canonicalCbor.subarray(-1).toString("hex")).toBe(
        code.toString(16).padStart(2, "0"),
      );
      expect(decodeMidgardNativeTxCompact(compactCbor).validity).toBe(meaning);
      expect(decodeMidgardNativeTxCanonical(canonicalCbor).validity).toBe(
        meaning,
      );
      expect(plutusDataCbor).toBe(`d8${(0x79 + Number(code)).toString(16)}80`);
    }
  });

  it("rejects adjacent, negative, fractional, and unknown meanings", () => {
    // `2n` is the first code past the two-arm frontier — the constructor index
    // that `d87b80` would carry, and the code the retired `InvalidSignature`
    // arm used to own.
    for (const value of [-1n, 2n, 3n, 255n, -1, 2, 1.5, "1"]) {
      expect(() => decodeValidityCode(value, "validity")).toThrow();
    }
    expect(() =>
      encodeValidityCode("UnknownValidity" as MidgardTxValidity),
    ).toThrow(/Unsupported Midgard tx validity variant/u);
    for (const retired of [
      "NonExistentInputUtxo",
      "InvalidSignature",
      "FailedScript",
      "FeeTooLow",
      "UnbalancedTx",
    ]) {
      expect(() => encodeValidityCode(retired as MidgardTxValidity)).toThrow(
        /Unsupported Midgard tx validity variant/u,
      );
    }
  });

  it("rejects non-minimal and out-of-range compact and canonical codes", () => {
    const txIsInvalid = materializeMidgardNativeTxFromCanonical(
      makeCanonical("TxIsInvalid"),
    );
    const compact = encodeMidgardNativeTxCompact(txIsInvalid.compact);
    const canonical = encodeMidgardNativeTxCanonical(txIsInvalid);

    expect(() => decodeSingleCbor(Buffer.from("1801", "hex"))).toThrow(
      /Non-minimal CBOR integer/u,
    );
    expect(() =>
      decodeMidgardNativeTxCompact(
        replaceFinalValidityCode(compact, Buffer.from("1801", "hex")),
      ),
    ).toThrow(/Non-minimal CBOR integer/u);
    expect(() =>
      decodeMidgardNativeTxCanonical(
        replaceFinalValidityCode(canonical, Buffer.from("1801", "hex")),
      ),
    ).toThrow(/Non-minimal CBOR integer/u);
    expect(() =>
      decodeMidgardNativeTxCompact(
        replaceFinalValidityCode(compact, Buffer.from([2])),
      ),
    ).toThrow(/Unsupported Midgard tx validity code/u);
    expect(() =>
      decodeMidgardNativeTxCanonical(
        replaceFinalValidityCode(canonical, Buffer.from([2])),
      ),
    ).toThrow(/Unsupported Midgard tx validity code/u);
  });
});
