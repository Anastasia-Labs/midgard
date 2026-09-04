import { createHash } from "node:crypto";
import { readdirSync, readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  MAX_DEPTH_V8_STACK_SIZE_KB,
  runMaxDepthCmlOperation,
} from "./helpers/cml-max-depth-runner.js";

/**
 * C26 Step 2 — verification of the source-fixed CML shadow stack.
 *
 * History: `@anastasia-labs/cardano-multiplatform-lib-*@6.2.0-1` shipped a
 * 1 MiB wasm shadow stack, trapping the recursive CBOR decoders at Plutus Data
 * depth 1,523 — below the depth-4,043 maximum that a 16,384-byte signed
 * transaction admits. That was bridged by a hash-pinned install-time binary
 * patch (`demo/scripts/patch-cml-wasm-stack.mjs`, retired with this suite's
 * predecessor `cml-wasm-stack-patch-v1.test.ts`). The defect is now fixed at
 * source: CML `6.2.0-2` is built with `-C link-arg=-zstack-size=16777216`
 * (Anastasia-Labs/cardano-multiplatform-lib PR #6), and the workspace pins it
 * via the `pnpm.overrides` entries in `demo/package.json`.
 *
 * This suite establishes, on every run:
 *
 *   1. lucid-evolution resolves the published `6.2.0-2` artifact, and every
 *      `6.2.0-x` CML copy in the store is `6.2.0-2` with the pinned bytes —
 *      a lockfile regression back to `6.2.0-1` fails loudly here;
 *   2. the binary's `__stack_pointer` global is 16 MiB, i.e. the fix is
 *      structural in the artifact, not an environment accident;
 *   3. the old depth ceiling is gone: the first previously-trapping depth
 *      (1,523), the derived maximum (4,043), and the adjacent beyond-maximum
 *      control (4,044) all parse and round-trip — no cliff sits at the
 *      maximum, so the admitted bound keeps deriving from transaction
 *      capacity, not from the library;
 *   4. V8's machine-stack limit still applies past the shadow stack, which is
 *      why max-depth work runs out of process with `--stack-size` raised.
 */

const HERE = dirname(fileURLToPath(import.meta.url));
const PNPM_STORE = join(HERE, "..", "..", "node_modules", ".pnpm");

const PINNED_VERSION = "6.2.0-2";
const PUBLISHED_WASM_SHA256 =
  "47e566383ca7b8f945377b149af83eb32c6d185e5e9e1b58eea19f85043d2b3c";
const PUBLISHED_WASM_BYTES = 2_904_467;
const SHADOW_STACK_POINTER = 16_777_216;
const MAXIMUM_UNARY_DEPTH = 4_043;
const OLD_STOCK_TRAP_DEPTH = 1_523;
const BEYOND_MAXIMUM_CONTROL_DEPTH = 4_044;
// Below the measured 1,400 KB machine-stack floor at depth 4,043, so the
// RangeError control fails deterministically on every Node version.
const BELOW_FLOOR_V8_STACK_SIZE_KB = 600;

const sha256Hex = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const require_ = createRequire(join(HERE, "placeholder.cjs"));
const installedCmlMainPath = createRequire(
  require_.resolve("@lucid-evolution/lucid"),
).resolve("@anastasia-labs/cardano-multiplatform-lib-nodejs");
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

describe("C26 CML wasm shadow stack (source-fixed 6.2.0-2)", () => {
  it("resolves the pinned published artifact through lucid-evolution", () => {
    const manifest = JSON.parse(
      readFileSync(join(installedCmlDir, "package.json"), "utf8"),
    ) as { name: string; version: string };
    expect(manifest.name).toBe(
      "@anastasia-labs/cardano-multiplatform-lib-nodejs",
    );
    expect(manifest.version).toBe(PINNED_VERSION);

    const wasm = readFileSync(
      join(installedCmlDir, "cardano_multiplatform_lib_bg.wasm"),
    );
    expect(wasm.length).toBe(PUBLISHED_WASM_BYTES);
    expect(sha256Hex(wasm)).toBe(PUBLISHED_WASM_SHA256);
  });

  it("has no 6.2.0-x CML copy in the store other than pinned 6.2.0-2", () => {
    const entries = readdirSync(PNPM_STORE).filter((entry) =>
      /^@anastasia-labs\+cardano-multiplatform-lib-(nodejs|browser)@/.test(
        entry,
      ),
    );
    const series = entries.filter((entry) => entry.includes("@6.2.0"));
    // Both wasm-bindgen targets are shipped by lucid-evolution; both must be
    // present and pinned. Other majors (e.g. 6.0.2-x for core-utils) are a
    // different consumer contract and are deliberately left alone.
    expect(
      series.filter((entry) => entry.includes("-nodejs@")).length,
    ).toBeGreaterThanOrEqual(1);
    expect(
      series.filter((entry) => entry.includes("-browser@")).length,
    ).toBeGreaterThanOrEqual(1);
    for (const entry of series) {
      expect(entry, `stale/overridden CML copy in store: ${entry}`).toContain(
        `@${PINNED_VERSION}`,
      );
      const variant = entry.includes("-browser@") ? "browser" : "nodejs";
      const wasm = readFileSync(
        join(
          PNPM_STORE,
          entry,
          "node_modules",
          "@anastasia-labs",
          `cardano-multiplatform-lib-${variant}`,
          "cardano_multiplatform_lib_bg.wasm",
        ),
      );
      expect(wasm.length).toBe(PUBLISHED_WASM_BYTES);
      expect(sha256Hex(wasm)).toBe(PUBLISHED_WASM_SHA256);
    }
  });

  it("carries the 16 MiB shadow stack structurally in the binary", () => {
    const wasm = readFileSync(
      join(installedCmlDir, "cardano_multiplatform_lib_bg.wasm"),
    );
    expect(readStackPointerGlobal(wasm)).toBe(SHADOW_STACK_POINTER);
  });

  it("parses through the old ceiling, the derived maximum, and beyond", () => {
    for (const depth of [
      OLD_STOCK_TRAP_DEPTH,
      MAXIMUM_UNARY_DEPTH,
      BEYOND_MAXIMUM_CONTROL_DEPTH,
    ]) {
      expect(
        runMaxDepthCmlOperation({
          operation: "plutusDataParse",
          cmlMainPath: installedCmlMainPath,
          depth,
        }),
        `depth ${depth} must parse and round-trip`,
      ).toMatchObject({ ok: true, depth, roundTripIsInput: true });
    }
  }, 300_000);

  it("still needs a raised V8 machine stack at the maximum", () => {
    // The wasm shadow stack is only one of the two budgets the C26
    // investigation isolated; V8 executes wasm frames on the machine stack
    // (measured floor 1,400 KB at depth 4,043 on the pinned Node; the runner
    // uses MAX_DEPTH_V8_STACK_SIZE_KB = 2,000 for headroom). Whether the
    // *default* stack suffices varies by Node version, so the control forces
    // a below-floor stack, which must fail on every Node. This is why
    // max-depth operations run in short-lived child processes.
    const withBelowFloorStack = runMaxDepthCmlOperation(
      {
        operation: "plutusDataParse",
        cmlMainPath: installedCmlMainPath,
        depth: MAXIMUM_UNARY_DEPTH,
      },
      { stackSizeKb: BELOW_FLOOR_V8_STACK_SIZE_KB },
    );
    expect(withBelowFloorStack.ok).toBe(false);
    expect(withBelowFloorStack.errorName).toBe("RangeError");
    expect(withBelowFloorStack.message).toContain(
      "Maximum call stack size exceeded",
    );
    expect(MAX_DEPTH_V8_STACK_SIZE_KB).toBeGreaterThanOrEqual(1_400);
  }, 300_000);
});
