import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { runMaxDepthCmlOperation } from "./helpers/cml-max-depth-runner.js";

/**
 * C26 Step 2 — the CML that Lucid resolves reads maximum-depth Plutus Data.
 *
 * History: `@anastasia-labs/cardano-multiplatform-lib-*@6.2.0-1` shipped a
 * 1 MiB wasm shadow stack, trapping the recursive CBOR decoders at Plutus Data
 * depth 1,523 — below the depth-4,043 maximum that a 16,384-byte signed
 * transaction admits. That was bridged by a hash-pinned install-time binary
 * patch, then by CML `6.2.0-2`, then by `@lucid-evolution/cml` 0.1.0 (Lucid
 * Evolution 0.8.0), whose 16 MiB shadow stack still needed V8's machine stack
 * raised past 1,400 KB at the maximum. Lucid Evolution 0.8.3 vendors
 * `@lucid-evolution/cml` 0.1.2, which needs neither.
 *
 * This suite establishes, on every run:
 *
 *   1. lucid-evolution resolves the published `@lucid-evolution/cml` artifact;
 *   2. the binary's `__stack_pointer` global is 16 MiB;
 *   3. the old depth ceiling is gone: the first previously-trapping depth
 *      (1,523), the derived maximum (4,043), the adjacent beyond-maximum
 *      control (4,044) and a depth far past it parse and round-trip exactly
 *      in this process, on its default stack, so the admitted bound keeps
 *      deriving from transaction capacity, not from the library;
 *   4. the maximum parses and round-trips on a machine stack below the
 *      1,400 KB that `@lucid-evolution/cml` 0.1.0 needed there.
 */

const HERE = dirname(fileURLToPath(import.meta.url));

const PINNED_PACKAGE = "@lucid-evolution/cml";
const PINNED_VERSION = "0.1.2";
const SHADOW_STACK_POINTER = 16_777_216;
const MAXIMUM_UNARY_DEPTH = 4_043;
const OLD_STOCK_TRAP_DEPTH = 1_523;
const BEYOND_MAXIMUM_CONTROL_DEPTH = 4_044;
const FAR_BEYOND_MAXIMUM_DEPTH = 100_000;
// Below the 1,400 KB machine stack `@lucid-evolution/cml` 0.1.0 needed at
// depth 4,043.
const BELOW_OLD_FLOOR_V8_STACK_SIZE_KB = 600;

const unaryConstructorDataCborHex = (depth: number): string =>
  "d8799f".repeat(depth) + "00" + "ff".repeat(depth);

const require_ = createRequire(join(HERE, "placeholder.cjs"));
const requireFromLucid = createRequire(
  require_.resolve("@lucid-evolution/lucid"),
);
const installedCmlMainPath = requireFromLucid.resolve(PINNED_PACKAGE);
const installedCmlManifestPath = requireFromLucid.resolve(
  `${PINNED_PACKAGE}/package.json`,
);
const installedCmlDir = dirname(installedCmlMainPath);

/**
 * Reads the init value of `global[0]` (wasm-bindgen's `__stack_pointer`,
 * `mut i32` initialised by `i32.const`) straight from the binary's global
 * section (id 6). Sections are length-prefixed, so the scan is exact.
 */
const readStackPointerGlobal = (wasm: Buffer): number => {
  expect(wasm.readUInt32LE(0)).toBe(0x6d73_6100); // "\0asm"
  let offset = 8;
  const readVarU32 = (): number => {
    let value = 0;
    let shift = 0;
    let byte: number;
    do {
      byte = wasm[offset++];
      value |= (byte & 0x7f) << shift;
      shift += 7;
    } while (byte & 0x80);
    return value >>> 0;
  };
  while (offset < wasm.length) {
    const sectionId = wasm[offset++];
    const sectionLength = readVarU32();
    if (sectionId !== 6) {
      offset += sectionLength;
      continue;
    }
    const globalCount = readVarU32();
    expect(globalCount).toBeGreaterThanOrEqual(1);
    const valueType = wasm[offset++];
    const mutability = wasm[offset++];
    const opcode = wasm[offset++];
    expect(valueType).toBe(0x7f); // i32
    expect(mutability).toBe(1); // mut
    expect(opcode).toBe(0x41); // i32.const
    return readVarU32();
  }
  throw new Error("no global section found in wasm binary");
};

describe("C26 CML wasm shadow stack (@lucid-evolution/cml)", () => {
  it("resolves the pinned published artifact through lucid-evolution", () => {
    const manifest = JSON.parse(
      readFileSync(installedCmlManifestPath, "utf8"),
    ) as { name: string; version: string };
    expect(manifest.name).toBe(PINNED_PACKAGE);
    expect(manifest.version).toBe(PINNED_VERSION);
  });

  it("carries the 16 MiB shadow stack structurally in the binary", () => {
    const wasm = readFileSync(
      join(installedCmlDir, "cardano_multiplatform_lib_bg.wasm"),
    );
    expect(readStackPointerGlobal(wasm)).toBe(SHADOW_STACK_POINTER);
  });

  it("parses and round-trips the old ceiling, the derived maximum and beyond in process", () => {
    for (const depth of [
      OLD_STOCK_TRAP_DEPTH,
      MAXIMUM_UNARY_DEPTH,
      BEYOND_MAXIMUM_CONTROL_DEPTH,
      FAR_BEYOND_MAXIMUM_DEPTH,
    ]) {
      const input = unaryConstructorDataCborHex(depth);
      expect(
        CML.PlutusData.from_cbor_hex(input).to_cbor_hex(),
        `depth ${depth} must parse and round-trip`,
      ).toBe(input);
    }
  }, 300_000);

  it("parses and round-trips the maximum on a machine stack below the old floor", () => {
    const input = unaryConstructorDataCborHex(MAXIMUM_UNARY_DEPTH);
    const inProcessHash = Buffer.from(
      CML.hash_plutus_data(CML.PlutusData.from_cbor_hex(input)).to_raw_bytes(),
    ).toString("hex");
    expect(
      runMaxDepthCmlOperation(
        {
          operation: "plutusDataParse",
          cmlMainPath: installedCmlMainPath,
          depth: MAXIMUM_UNARY_DEPTH,
        },
        { stackSizeKb: BELOW_OLD_FLOOR_V8_STACK_SIZE_KB },
      ),
    ).toMatchObject({
      ok: true,
      depth: MAXIMUM_UNARY_DEPTH,
      roundTripIsInput: true,
      hashHex: inProcessHash,
    });
  }, 300_000);
});
