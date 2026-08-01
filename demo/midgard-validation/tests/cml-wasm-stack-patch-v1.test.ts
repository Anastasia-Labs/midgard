import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdirSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  MAX_DEPTH_V8_STACK_SIZE_KB_V1,
  runMaxDepthCmlOperationV1,
} from "./helpers/cml-max-depth-runner-v1.js";

/**
 * C26 Step 2 — verification of the hash-pinned CML wasm shadow-stack patch.
 *
 * The patch itself lives in `demo/scripts/patch-cml-wasm-stack.mjs` and is
 * applied by the `postinstall` script of `demo/package.json`. It is documented
 * in `demo/scripts/cml-wasm-stack-patch.md`.
 *
 * This suite establishes, on every run:
 *
 *   1. the installed artifact is exactly the pinned patched binary, for both
 *      the `-nodejs` and `-browser` variants and in every store in the tree;
 *   2. the rewrite is exactly invertible — reverting the installed binary
 *      reproduces the stock sha256 bit for bit;
 *   3. the patched library is BYTE-IDENTICAL to the stock library across a
 *      broad deterministic exercise (owner condition #2);
 *   4. the patch is what lifts the depth ceiling: the stock binary still traps
 *      at the derived maximum, the patched one does not;
 *   5. the pin FAILS CLOSED — an unrecognised binary at the pinned version, or
 *      a missing pinned dependency, aborts loudly and writes nothing
 *      (owner condition #1).
 */

const HERE = dirname(fileURLToPath(import.meta.url));
const PATCHER = join(HERE, "..", "..", "scripts", "patch-cml-wasm-stack.mjs");
const DIFFERENTIAL_SCENARIOS = join(
  HERE,
  "helpers",
  "cml-wasm-differential-scenarios.cjs",
);

const STOCK_SHA256 =
  "91b38c8e0ad609862186620e2fc07a1919740112c819d13884d029a0a0481b6e";
const PATCHED_SHA256 =
  "cd96b005edaaabc4239f61857f92c322b04cc967363917aaf2ff17ea20313435";
const STOCK_BYTES = 3_191_423;
const PATCHED_BYTES = 3_191_424;
const PINNED_VERSION = "6.2.0-1";
const MAXIMUM_UNARY_DEPTH_V1 = 4_043;
const STOCK_PLUTUS_DATA_DEPTH_CEILING_V1 = 1_522;

const sha256Hex = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const require_ = createRequire(join(HERE, "placeholder.cjs"));
const installedCmlMainPath = createRequire(
  require_.resolve("@lucid-evolution/lucid"),
).resolve("@anastasia-labs/cardano-multiplatform-lib-nodejs");
const installedCmlWasmPath = join(
  dirname(installedCmlMainPath),
  "cardano_multiplatform_lib_bg.wasm",
);

type PatcherTarget = {
  readonly wasmPath: string;
  readonly version: string | null;
  readonly sha256?: string;
  readonly action: string;
};
type PatcherOutcome = {
  readonly mode: string;
  readonly targets: readonly PatcherTarget[];
  readonly failures: readonly string[];
  readonly pinnedTargetCount: number;
};

const runPatcher = (
  args: readonly string[],
): {
  readonly status: number;
  readonly stdout: string;
  readonly stderr: string;
} => {
  try {
    const stdout = execFileSync(process.execPath, [PATCHER, ...args], {
      encoding: "utf8",
      timeout: 120_000,
      maxBuffer: 16 * 1024 * 1024,
      stdio: ["ignore", "pipe", "pipe"],
    });
    return { status: 0, stdout, stderr: "" };
  } catch (cause) {
    const error = cause as {
      status?: number;
      stdout?: string;
      stderr?: string;
    };
    return {
      status: error.status ?? -1,
      stdout: error.stdout ?? "",
      stderr: error.stderr ?? "",
    };
  }
};

const runPatcherJson = (args: readonly string[]): PatcherOutcome => {
  const { status, stdout, stderr } = runPatcher([...args, "--json"]);
  expect(
    status,
    `patcher exited ${status}\nstdout:\n${stdout}\nstderr:\n${stderr}`,
  ).toBe(0);
  return JSON.parse(stdout) as PatcherOutcome;
};

const runDifferentialScenarios = (cmlMainPath: string): string =>
  execFileSync(
    process.execPath,
    [
      `--stack-size=${MAX_DEPTH_V8_STACK_SIZE_KB_V1}`,
      DIFFERENTIAL_SCENARIOS,
      cmlMainPath,
    ],
    { encoding: "utf8", timeout: 300_000, maxBuffer: 64 * 1024 * 1024 },
  );

const emitStockPackage = (): string => {
  const scratch = mkdtempSync(join(tmpdir(), "midgard-c26-stock-cml-"));
  const { status, stdout, stderr } = runPatcher([
    "--emit-stock-package",
    scratch,
  ]);
  expect(status, `--emit-stock-package failed:\n${stderr}`).toBe(0);
  const emitted = JSON.parse(stdout) as {
    readonly installedSha256: string;
    readonly stockSha256: string;
    readonly mainPath: string;
  };
  expect(emitted.installedSha256).toBe(PATCHED_SHA256);
  expect(emitted.stockSha256).toBe(STOCK_SHA256);
  return emitted.mainPath;
};

describe("C26 CML wasm shadow-stack patch", () => {
  it("has patched every pinned CML copy in the tree, for both variants", () => {
    const outcome = runPatcherJson(["--check"]);
    const pinned = outcome.targets.filter(
      (target) => target.version === PINNED_VERSION,
    );
    expect(outcome.failures).toEqual([]);
    expect(pinned.length).toBeGreaterThanOrEqual(2);
    expect(outcome.pinnedTargetCount).toBe(pinned.length);
    for (const target of pinned) {
      expect(
        target.action,
        `${target.wasmPath} is not patched — run \`pnpm --dir demo run patch:cml-wasm\``,
      ).toBe("verified-patched");
      expect(target.sha256).toBe(PATCHED_SHA256);
    }
    // Both wasm-bindgen targets are shipped by lucid-evolution and both carry
    // the same 1 MiB shadow stack, so both must be covered.
    expect(
      pinned.some((target) =>
        target.wasmPath.includes("cardano-multiplatform-lib-nodejs"),
      ),
    ).toBe(true);
    expect(
      pinned.some((target) =>
        target.wasmPath.includes("cardano-multiplatform-lib-browser"),
      ),
    ).toBe(true);
    // Other CML majors in the tree are reported and deliberately left alone.
    for (const target of outcome.targets) {
      if (target.version !== PINNED_VERSION) {
        expect(target.action).toBe("skipped-unpinned-version");
      }
    }
  }, 120_000);

  it("is idempotent: re-running the patcher writes nothing", () => {
    const before = readFileSync(installedCmlWasmPath);
    const outcome = runPatcherJson([]);
    expect(outcome.failures).toEqual([]);
    for (const target of outcome.targets) {
      expect(["unchanged", "skipped-unpinned-version"]).toContain(
        target.action,
      );
    }
    expect(readFileSync(installedCmlWasmPath).equals(before)).toBe(true);
  }, 120_000);

  it("pins both artifacts and reverts the patch bit-for-bit", () => {
    const installed = readFileSync(installedCmlWasmPath);
    expect(sha256Hex(installed)).toBe(PATCHED_SHA256);
    expect(installed.length).toBe(PATCHED_BYTES);

    const stockMainPath = emitStockPackage();
    const stockWasm = readFileSync(
      join(dirname(stockMainPath), "cardano_multiplatform_lib_bg.wasm"),
    );
    expect(sha256Hex(stockWasm)).toBe(STOCK_SHA256);
    expect(stockWasm.length).toBe(STOCK_BYTES);
    // The patch adds exactly one byte: `initial` 21 -> 277 pages widens the
    // memory-limits LEB128 from one byte to two. The `i32.const` operand of
    // the __stack_pointer global stays four bytes wide.
    expect(installed.length - stockWasm.length).toBe(1);
  }, 120_000);

  it("produces byte-identical results to the stock library", () => {
    const stockMainPath = emitStockPackage();
    const stockOutput = runDifferentialScenarios(stockMainPath);
    const patchedOutput = runDifferentialScenarios(installedCmlMainPath);

    // Guard against a vacuous comparison of two empty/errored runs.
    const parsed = JSON.parse(stockOutput) as Record<string, unknown>;
    expect(Object.keys(parsed).sort()).toEqual([
      "addresses",
      "allocationChurn",
      "bigInteger",
      "ed25519",
      "nativeScript",
      "plutusData",
      "transaction",
      "value",
    ]);
    expect((parsed.plutusData as unknown[]).length).toBe(13);
    expect(stockOutput.length).toBeGreaterThan(40_000);

    expect(sha256Hex(Buffer.from(patchedOutput, "utf8"))).toBe(
      sha256Hex(Buffer.from(stockOutput, "utf8")),
    );
    expect(patchedOutput).toBe(stockOutput);
  }, 300_000);

  it("is the reason the derived maximum depth is reachable at all", () => {
    const stockMainPath = emitStockPackage();

    // Control: the stock binary still parses right up to its documented
    // ceiling, so the deep failure below is a depth limit and not a broken
    // copy of the library.
    expect(
      runMaxDepthCmlOperationV1({
        operation: "plutusDataParse",
        cmlMainPath: stockMainPath,
        depth: STOCK_PLUTUS_DATA_DEPTH_CEILING_V1,
      }),
    ).toMatchObject({ ok: true, roundTripIsInput: true });

    // Control: the stock binary traps at the derived maximum, with the same
    // extra machine stack the patched run gets. Raising only `--stack-size`
    // does not help — this is the wasm shadow stack.
    const stockAtMaximum = runMaxDepthCmlOperationV1({
      operation: "plutusDataParse",
      cmlMainPath: stockMainPath,
      depth: MAXIMUM_UNARY_DEPTH_V1,
    });
    expect(stockAtMaximum.ok).toBe(false);
    expect(stockAtMaximum.errorName).toBe("RuntimeError");
    expect(stockAtMaximum.message).toContain("memory access out of bounds");

    // The patched binary reaches the maximum.
    const patchedAtMaximum = runMaxDepthCmlOperationV1({
      operation: "plutusDataParse",
      cmlMainPath: installedCmlMainPath,
      depth: MAXIMUM_UNARY_DEPTH_V1,
    });
    expect(patchedAtMaximum).toMatchObject({
      ok: true,
      depth: MAXIMUM_UNARY_DEPTH_V1,
      roundTripIsInput: true,
    });

    // ...but only together with the raised V8 machine stack. This is the
    // second limit the C26 investigation isolated; it is why the max-depth
    // work runs out of process rather than inline.
    const patchedWithDefaultStack = runMaxDepthCmlOperationV1(
      {
        operation: "plutusDataParse",
        cmlMainPath: installedCmlMainPath,
        depth: MAXIMUM_UNARY_DEPTH_V1,
      },
      { stackSizeKb: null },
    );
    expect(patchedWithDefaultStack.ok).toBe(false);
    expect(patchedWithDefaultStack.errorName).toBe("RangeError");
    expect(patchedWithDefaultStack.message).toContain(
      "Maximum call stack size exceeded",
    );
  }, 300_000);

  it("refuses to patch a binary that does not match the pin", () => {
    const root = mkdtempSync(join(tmpdir(), "midgard-c26-badpin-"));
    const packageDir = join(
      root,
      "node_modules",
      ".pnpm",
      `@anastasia-labs+cardano-multiplatform-lib-nodejs@${PINNED_VERSION}`,
      "node_modules",
      "@anastasia-labs",
      "cardano-multiplatform-lib-nodejs",
    );
    mkdirSync(packageDir, { recursive: true });
    writeFileSync(
      join(packageDir, "package.json"),
      JSON.stringify({
        name: "@anastasia-labs/cardano-multiplatform-lib-nodejs",
        version: PINNED_VERSION,
      }),
    );
    // A plausible-looking but unrecognised wasm module: correct magic, wrong
    // contents. This stands in for a CML republish or a tampered artifact.
    const impostor = Buffer.from([
      0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0xde, 0xad, 0xbe, 0xef,
    ]);
    const wasmPath = join(packageDir, "cardano_multiplatform_lib_bg.wasm");
    writeFileSync(wasmPath, impostor);

    const { status, stderr } = runPatcher(["--root", root]);
    expect(status).not.toBe(0);
    expect(stderr).toContain("CML WASM PATCH REFUSED");
    expect(stderr).toContain(sha256Hex(impostor));
    expect(stderr).toContain(STOCK_SHA256);
    // Nothing was written.
    expect(readFileSync(wasmPath).equals(impostor)).toBe(true);
  }, 120_000);

  it("refuses to report success when the pinned dependency is absent", () => {
    const root = mkdtempSync(join(tmpdir(), "midgard-c26-nopin-"));
    mkdirSync(join(root, "node_modules"), { recursive: true });
    const { status, stderr } = runPatcher(["--root", root]);
    expect(status).not.toBe(0);
    expect(stderr).toContain("CML WASM PATCH REFUSED");
    expect(stderr).toContain(
      `no @anastasia-labs/cardano-multiplatform-lib-* @ ${PINNED_VERSION} found`,
    );
  }, 120_000);
});
