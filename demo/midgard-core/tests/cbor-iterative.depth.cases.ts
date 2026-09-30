/**
 * Depth cases at the protocol maximum for every site that walks Plutus Data
 * or generic CBOR. Plutus Data has no depth bound, only byte caps, so each
 * site must handle the deepest value the carrier's byte cap admits without
 * recursion. The same cases run on a process main thread (about 1 MB of
 * stack) and on a worker thread (4 MB).
 */
import { isMainThread } from "node:worker_threads";

import {
  type DeepDataShape,
  deepPlutusDataCbor,
  PLUTUS_DATA_PROTOCOL_MAX_DEPTHS,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";
import { describe, expect, it } from "vitest";

import { commitSemanticData } from "../src/cek-proof.commit-semantic-data.js";
import {
  assertSemanticDataEncodable,
  encodeSemanticData,
} from "../src/cek-proof.encode-semantic-data.js";
import { verifyMidgardCekProgramMaterial } from "../src/cek-proof.js";
import { type SemanticDataValue } from "../src/cek-proof.program-material-task.js";
import {
  assertCanonicalCbor,
  decodeSingleCbor,
  encodeCbor,
  skipCborItem,
} from "../src/codec/cbor.js";
import {
  lucidDataFromCborIterative,
  lucidDataToCborIterative,
} from "../src/plutus-data-lucid-iterative.js";
import {
  deepDataConstantProgramMaterial,
  deepSemanticCbor,
  deepSemanticValue,
  type SemanticChainShape,
} from "./cbor-iterative.depth.material.js";

export type DepthThread = "main" | "worker";

const REDEEMER = PLUTUS_DATA_PROTOCOL_MAX_DEPTHS.redeemer;

/** The CEK constant cap is 9,215 bytes of semantic (indefinite) CBOR. */
const CEK_SEMANTIC_MAX_DEPTHS: Record<SemanticChainShape, number> = {
  list: 4_607,
  map: 4_607,
  constr: 2_303,
};

/** Generous: the iterative sites finish each case in well under a second. */
const CASE_TIMEOUT_MS = 20_000;

/** A nested JS value one container per level around the number 0. */
const deepHostValue = (shape: "array" | "map", depth: number): unknown => {
  let value: unknown = 0;
  for (let level = 0; level < depth; level += 1) {
    value = shape === "array" ? [value] : new Map([[0, value]]);
  }
  return value;
};

/** Lucid's own encoding of a one-path value: indefinite lists and maps. */
const lucidDeepCbor = (shape: DeepDataShape, depth: number): string => {
  const open = shape === "map" ? "bf00" : shape === "constr" ? "d8799f" : "9f";
  return `${open.repeat(depth)}00${"ff".repeat(depth)}`;
};

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

export const registerDeepDataDepthCases = (thread: DepthThread): void => {
  it("runs on the intended thread", () => {
    expect(isMainThread).toBe(thread === "main");
  });

  describe("generic CBOR codec", () => {
    for (const shape of ["definite-list", "map"] as const) {
      const depth = REDEEMER[shape];
      it(
        `skips, checks, decodes and re-encodes a ${shape} ${depth} deep`,
        () => {
          const bytes = Buffer.from(deepPlutusDataCbor(shape, depth));
          expect(skipCborItem(bytes, 0).end).toBe(bytes.length);
          expect(() => assertCanonicalCbor(bytes, "deep")).not.toThrow();
          const decoded = decodeSingleCbor(bytes);
          expect(encodeCbor(decoded).equals(bytes)).toBe(true);
        },
        CASE_TIMEOUT_MS,
      );
    }

    for (const [shape, depth] of [
      ["array", REDEEMER["definite-list"]],
      ["map", REDEEMER.map],
    ] as const) {
      it(
        `encodes a nested ${shape} ${depth} deep`,
        () => {
          const encoded = encodeCbor(deepHostValue(shape, depth));
          const expected =
            shape === "array"
              ? deepPlutusDataCbor("definite-list", depth)
              : deepPlutusDataCbor("map", depth);
          expect(hex(encoded)).toBe(hex(expected));
        },
        CASE_TIMEOUT_MS,
      );
    }
  });

  describe("semantic Data encoder and commitment", () => {
    for (const [shape, depth] of [
      ["list", REDEEMER["definite-list"]],
      ["map", REDEEMER.map],
      ["constr", REDEEMER.constr],
    ] as const) {
      it(
        `encodes and commits a semantic ${shape} ${depth} deep`,
        () => {
          const value = deepSemanticValue(shape, depth) as SemanticDataValue;
          const expected = deepSemanticCbor(shape, depth);
          expect(() => assertSemanticDataEncodable(value)).not.toThrow();
          expect(encodeSemanticData(value).equals(expected)).toBe(true);
          const committed = commitSemanticData(value);
          expect(committed.cborLength).toBe(BigInt(expected.length));
        },
        CASE_TIMEOUT_MS,
      );
    }
  });

  describe("CEK program material with a deep Data constant", () => {
    for (const shape of ["list", "map", "constr"] as const) {
      const depth = CEK_SEMANTIC_MAX_DEPTHS[shape];
      it(
        `verifies a ${shape} constant ${depth} deep and refuses one level more`,
        () => {
          const fixture = deepDataConstantProgramMaterial(shape, depth);
          expect(fixture.payloadCbor.length).toBeLessThanOrEqual(9_215);
          const verified = verifyMidgardCekProgramMaterial(
            fixture.envelope,
            fixture.material,
          );
          expect(verified.constants).toHaveLength(1);
          expect(
            verified.constants[0]!.payloadCbor.equals(fixture.payloadCbor),
          ).toBe(true);
          const over = deepDataConstantProgramMaterial(shape, depth + 1);
          expect(over.payloadCbor.length).toBeGreaterThan(9_215);
          expect(() =>
            verifyMidgardCekProgramMaterial(over.envelope, over.material),
          ).toThrow(/source constant payload exceeds the 9215-byte/u);
        },
        CASE_TIMEOUT_MS,
      );
    }
  });

  describe("Lucid Plutus Data reader and writer", () => {
    for (const shape of [
      "definite-list",
      "indefinite-list",
      "map",
      "constr",
    ] as const) {
      const depth = REDEEMER[shape];
      it(
        `reads and re-writes a ${shape} ${depth} deep`,
        () => {
          const data = lucidDataFromCborIterative(
            deepPlutusDataCbor(shape, depth),
          );
          const written = lucidDataToCborIterative(data);
          expect(hex(written)).toBe(lucidDeepCbor(shape, depth));
          const again = lucidDataToCborIterative(
            lucidDataFromCborIterative(written),
          );
          expect(again.equals(written)).toBe(true);
        },
        CASE_TIMEOUT_MS,
      );
    }
  });
};
