#!/usr/bin/env node
/**
 * C26 Step 2 — CML wasm shadow-stack relocation (hash-pinned, reversible).
 *
 * WHAT THIS DOES
 * --------------
 * `@anastasia-labs/cardano-multiplatform-lib-nodejs@6.2.0-1` and its sibling
 * `-browser@6.2.0-1` ship a `cardano_multiplatform_lib_bg.wasm` built with the
 * wasm-bindgen default 1 MiB shadow stack. CML's Plutus Data / Transaction CBOR
 * decoders are recursive descent (~688 bytes of shadow stack per Plutus Data
 * nesting level), so the stack pointer walks below zero at roughly:
 *
 *   PlutusData.from_cbor_hex        depth 1,523
 *   TransactionOutput.from_cbor_hex depth 1,518
 *   Transaction.from_cbor_hex       depth 1,503
 *
 * and V8 reports the resulting out-of-range linear-memory access as
 * `RuntimeError: memory access out of bounds`. The trap permanently poisons the
 * `WebAssembly.Instance` for the rest of the process.
 *
 * Canonical Midgard V1 admits a unary Plutus Data depth of 4,043 inside an
 * exactly-16,384-byte signed transaction (that number derives from Cardano
 * transaction capacity, not from any Midgard cap), so genuine emulator
 * admission at the derived maximum is unreachable on the stock binary.
 *
 * This script rewrites exactly two numbers in the binary. It changes no code,
 * relocates no data, and adds/removes no sections:
 *
 *   1. memory section (id 5) limits: `initial` 21 pages -> 277 pages.
 *      Encoded `01 00 15` -> `01 00 95 02` (count=1, flags=0x00 "no maximum",
 *      initial as unsigned LEB128). The section payload therefore grows by one
 *      byte and the whole file grows by one byte; no other offset semantics
 *      change, because wasm sections are length-prefixed and self-locating.
 *
 *   2. global section (id 6), `global[0]` — the wasm-bindgen `__stack_pointer`
 *      (`mut i32`, initialised by an `i32.const`): 1048576 -> 18153472.
 *      Encoded `01 7f 01 41 80 80 c0 00 0b` -> `01 7f 01 41 80 80 d4 08 0b`
 *      (both `i32.const` operands are 4-byte signed LEB128, so the global
 *      section length is unchanged).
 *
 * 21 pages = 1,376,256 bytes is the stock initial memory; 277 pages =
 * 18,153,472 bytes. The extra 256 pages (16 MiB) sit entirely ABOVE the stock
 * initial memory end, and the new stack pointer is the top of the new initial
 * memory, so the shadow stack grows downward through 16 MiB of freshly added,
 * never-otherwise-reachable address space.
 *
 * WHY THIS IS SAFE
 * ----------------
 * - The stock data section spans 0x100000..0x141d0d, i.e. all static data lives
 *   ABOVE the old 1 MiB shadow stack and BELOW the old initial memory end. The
 *   relocation neither overlaps static data nor moves it.
 * - Rust's dlmalloc on wasm32 only ever hands out pages it obtained from
 *   `memory.grow`. Pages that were already present at instantiation (which is
 *   what raising `initial` produces) are never returned by the allocator, so
 *   the relocated stack cannot collide with the heap.
 * - The old `[0, 0x100000)` region simply becomes dead space.
 * - Cost: ~16.6 MiB of initial linear memory per CML instance.
 *
 * A SECOND LIMIT HIDES BEHIND THIS ONE
 * ------------------------------------
 * V8 runs wasm frames on the real machine stack, so deep recursion consumes
 * both budgets. With this patch applied but the default V8 stack, depth 4,043
 * fails with `RangeError: Maximum call stack size exceeded`. The measured floor
 * is `--stack-size=1400`; the repo's max-depth suites use `--stack-size=2000`
 * for headroom. See `demo/scripts/cml-wasm-stack-patch.md`.
 *
 * HASH PIN (owner condition #1)
 * -----------------------------
 * Every candidate file is identified by sha256 before anything is written:
 *
 *   stock   91b38c8e0ad609862186620e2fc07a1919740112c819d13884d029a0a0481b6e
 *   patched cd96b005edaaabc4239f61857f92c322b04cc967363917aaf2ff17ea20313435
 *
 * - stock   -> patched, and the OUTPUT hash is re-verified before the file is
 *             swapped in. A mismatch aborts without touching the target.
 * - patched -> left alone (idempotent; safe to re-run).
 * - anything else at version 6.2.0-1 -> HARD FAILURE, nothing is written.
 * - no 6.2.0-1 target found at all -> HARD FAILURE (the pinned dependency
 *   moved; the max-depth suites would otherwise silently lose their premise).
 *
 * Other CML versions present in the tree (6.0.2-x) are reported and skipped;
 * they are not on the deep-Plutus-Data path.
 *
 * HARDLINK SAFETY
 * ---------------
 * pnpm hardlinks package files from the global content-addressable store, so
 * the target may have a link count > 1. Editing in place would silently corrupt
 * the store and every other project on the machine. This script always writes a
 * fresh temp file in the same directory and `rename()`s it over the target,
 * which breaks the hardlink and leaves the store byte-identical.
 *
 * USAGE
 * -----
 *   node demo/scripts/patch-cml-wasm-stack.mjs            # patch (postinstall)
 *   node demo/scripts/patch-cml-wasm-stack.mjs --check    # report only
 *   node demo/scripts/patch-cml-wasm-stack.mjs --revert   # restore stock bytes
 *   node demo/scripts/patch-cml-wasm-stack.mjs --json     # machine readable
 *
 *   node demo/scripts/patch-cml-wasm-stack.mjs --emit-stock-package <dir>
 *       Materialises a standalone, loadable copy of the STOCK nodejs package
 *       (glue + reverted wasm) into <dir>. Used by the byte-identity
 *       differential suite, and by anyone auditing the patch by hand.
 *
 *   --root <dir>   Scan <dir> instead of `demo/`. Used by the negative
 *                  controls that prove the pin fails closed.
 */
import { createHash } from "node:crypto";
import {
  chmodSync,
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  renameSync,
  statSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const SCRIPT_DIR = dirname(fileURLToPath(import.meta.url));
const DEMO_ROOT = resolve(SCRIPT_DIR, "..");

export const CML_WASM_PATCH_PIN_V1 = Object.freeze({
  pinnedVersion: "6.2.0-1",
  packageNames: Object.freeze([
    "@anastasia-labs/cardano-multiplatform-lib-nodejs",
    "@anastasia-labs/cardano-multiplatform-lib-browser",
  ]),
  wasmFileName: "cardano_multiplatform_lib_bg.wasm",
  stockSha256:
    "91b38c8e0ad609862186620e2fc07a1919740112c819d13884d029a0a0481b6e",
  patchedSha256:
    "cd96b005edaaabc4239f61857f92c322b04cc967363917aaf2ff17ea20313435",
  stockBytes: 3_191_423,
  patchedBytes: 3_191_424,
  stockMemoryInitialPages: 21,
  patchedMemoryInitialPages: 277,
  stockStackPointer: 1_048_576,
  patchedStackPointer: 18_153_472,
  requiredV8StackSizeKb: 2_000,
  measuredV8StackSizeFloorKb: 1_400,
  receiptFileName: ".midgard-cml-wasm-stack-patch-v1.json",
});

const sha256Hex = (bytes) => createHash("sha256").update(bytes).digest("hex");

/* ------------------------------------------------------------------ *
 * Minimal wasm section walker. Only the memory (5) and global (6)
 * sections are inspected; everything else is copied verbatim.
 * ------------------------------------------------------------------ */

const readUnsignedLeb128 = (bytes, offset) => {
  let result = 0;
  let shift = 0;
  let cursor = offset;
  for (;;) {
    if (cursor >= bytes.length) {
      throw new Error("Truncated unsigned LEB128");
    }
    const byte = bytes[cursor];
    cursor += 1;
    result += (byte & 0x7f) * 2 ** shift;
    if ((byte & 0x80) === 0) {
      return { value: result, length: cursor - offset };
    }
    shift += 7;
    if (shift > 35) {
      throw new Error("Unsigned LEB128 exceeds u32");
    }
  }
};

const writeUnsignedLeb128 = (value) => {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff_ffff) {
    throw new Error(`Unsigned LEB128 value out of range: ${value}`);
  }
  const out = [];
  let remaining = value;
  do {
    let byte = remaining & 0x7f;
    remaining = Math.floor(remaining / 128);
    if (remaining !== 0) {
      byte |= 0x80;
    }
    out.push(byte);
  } while (remaining !== 0);
  return Buffer.from(out);
};

const readSignedLeb128 = (bytes, offset) => {
  let result = 0n;
  let shift = 0n;
  let cursor = offset;
  let byte;
  do {
    if (cursor >= bytes.length) {
      throw new Error("Truncated signed LEB128");
    }
    byte = bytes[cursor];
    cursor += 1;
    result |= BigInt(byte & 0x7f) << shift;
    shift += 7n;
  } while (byte & 0x80);
  if (byte & 0x40 && shift < 64n) {
    result |= -(1n << shift);
  }
  return { value: Number(result), length: cursor - offset };
};

const writeSignedLeb128 = (value) => {
  let remaining = BigInt(value);
  const out = [];
  for (;;) {
    const byte = Number(remaining & 0x7fn);
    remaining >>= 7n;
    const signBit = (byte & 0x40) !== 0;
    if (
      (remaining === 0n && !signBit) ||
      (remaining === -1n && signBit)
    ) {
      out.push(byte);
      return Buffer.from(out);
    }
    out.push(byte | 0x80);
  }
};

const parseWasmSections = (bytes) => {
  if (
    bytes.length < 8 ||
    bytes.readUInt32LE(0) !== 0x6d73_6100 ||
    bytes.readUInt32LE(4) !== 1
  ) {
    throw new Error("Not a version-1 wasm module");
  }
  const sections = [];
  let cursor = 8;
  while (cursor < bytes.length) {
    const id = bytes[cursor];
    cursor += 1;
    const size = readUnsignedLeb128(bytes, cursor);
    cursor += size.length;
    const payloadStart = cursor;
    const payloadEnd = payloadStart + size.value;
    if (payloadEnd > bytes.length) {
      throw new Error(`wasm section ${id} runs past end of file`);
    }
    sections.push({ id, payloadStart, payloadEnd });
    cursor = payloadEnd;
  }
  return sections;
};

const encodeSection = (id, payload) =>
  Buffer.concat([Buffer.from([id]), writeUnsignedLeb128(payload.length), payload]);

/**
 * Rewrites the memory `initial` page count and the `__stack_pointer` global.
 * `expect` / `next` make the function usable in both directions, so `--revert`
 * exercises exactly the same code path as the forward patch.
 */
const rewriteWasmStackLayout = (bytes, { expect, next }) => {
  const sections = parseWasmSections(bytes);
  const memory = sections.find((section) => section.id === 5);
  const global = sections.find((section) => section.id === 6);
  if (!memory) throw new Error("wasm module has no memory section");
  if (!global) throw new Error("wasm module has no global section");

  /* ---- memory section: count, limits{flags, initial[, maximum]} ---- */
  const memoryPayload = bytes.subarray(memory.payloadStart, memory.payloadEnd);
  const memoryCount = readUnsignedLeb128(memoryPayload, 0);
  if (memoryCount.value !== 1) {
    throw new Error(
      `expected exactly 1 memory, found ${memoryCount.value}`,
    );
  }
  const flagsOffset = memoryCount.length;
  const flags = memoryPayload[flagsOffset];
  if (flags !== 0x00) {
    throw new Error(
      `expected memory limits flags 0x00 (no maximum), found 0x${flags
        .toString(16)
        .padStart(2, "0")}`,
    );
  }
  const initial = readUnsignedLeb128(memoryPayload, flagsOffset + 1);
  if (initial.value !== expect.memoryInitialPages) {
    throw new Error(
      `expected memory initial ${expect.memoryInitialPages} pages, found ${initial.value}`,
    );
  }
  if (flagsOffset + 1 + initial.length !== memoryPayload.length) {
    throw new Error("memory section carries unexpected trailing bytes");
  }
  const nextMemoryPayload = Buffer.concat([
    memoryPayload.subarray(0, flagsOffset + 1),
    writeUnsignedLeb128(next.memoryInitialPages),
  ]);

  /* ---- global section: count, globaltype{valtype, mut}, init expr ---- */
  const globalPayload = bytes.subarray(global.payloadStart, global.payloadEnd);
  const globalCount = readUnsignedLeb128(globalPayload, 0);
  if (globalCount.value < 1) {
    throw new Error("wasm module declares no globals");
  }
  let offset = globalCount.length;
  const valueType = globalPayload[offset];
  const mutability = globalPayload[offset + 1];
  if (valueType !== 0x7f || mutability !== 0x01) {
    throw new Error(
      "global[0] is not a mutable i32; refusing to treat it as __stack_pointer",
    );
  }
  const opcode = globalPayload[offset + 2];
  if (opcode !== 0x41) {
    throw new Error("global[0] initialiser is not i32.const");
  }
  const constant = readSignedLeb128(globalPayload, offset + 3);
  if (constant.value !== expect.stackPointer) {
    throw new Error(
      `expected __stack_pointer ${expect.stackPointer}, found ${constant.value}`,
    );
  }
  const endOffset = offset + 3 + constant.length;
  if (globalPayload[endOffset] !== 0x0b) {
    throw new Error("global[0] initialiser is not terminated by `end`");
  }
  const nextConstant = writeSignedLeb128(next.stackPointer);
  const nextGlobalPayload = Buffer.concat([
    globalPayload.subarray(0, offset + 3),
    nextConstant,
    globalPayload.subarray(endOffset),
  ]);

  /* ---- reassemble, copying every other section verbatim ---- */
  const parts = [bytes.subarray(0, 8)];
  for (const section of sections) {
    if (section.id === 5) {
      parts.push(encodeSection(5, nextMemoryPayload));
    } else if (section.id === 6) {
      parts.push(encodeSection(6, nextGlobalPayload));
    } else {
      parts.push(
        encodeSection(
          section.id,
          bytes.subarray(section.payloadStart, section.payloadEnd),
        ),
      );
    }
  }
  return Buffer.concat(parts);
};

const STOCK_LAYOUT = {
  memoryInitialPages: CML_WASM_PATCH_PIN_V1.stockMemoryInitialPages,
  stackPointer: CML_WASM_PATCH_PIN_V1.stockStackPointer,
};
const PATCHED_LAYOUT = {
  memoryInitialPages: CML_WASM_PATCH_PIN_V1.patchedMemoryInitialPages,
  stackPointer: CML_WASM_PATCH_PIN_V1.patchedStackPointer,
};

/** Stock bytes -> patched bytes. Exported so tests can re-derive the pin. */
export const patchCmlWasmBytes = (bytes) =>
  rewriteWasmStackLayout(bytes, { expect: STOCK_LAYOUT, next: PATCHED_LAYOUT });

/** Patched bytes -> stock bytes. Proves the rewrite is exactly invertible. */
export const revertCmlWasmBytes = (bytes) =>
  rewriteWasmStackLayout(bytes, { expect: PATCHED_LAYOUT, next: STOCK_LAYOUT });

/* ------------------------------------------------------------------ *
 * Target discovery
 * ------------------------------------------------------------------ */

const shortPackageNames = CML_WASM_PATCH_PIN_V1.packageNames.map((name) =>
  name.slice("@anastasia-labs/".length),
);

const readPackageVersion = (packageDir) => {
  try {
    return JSON.parse(
      readFileSync(join(packageDir, "package.json"), "utf8"),
    ).version;
  } catch {
    return null;
  }
};

const collectFromNodeModules = (nodeModulesDir, found) => {
  if (!existsSync(nodeModulesDir)) return;

  // pnpm virtual store: node_modules/.pnpm/@anastasia-labs+<pkg>@<ver>/node_modules/@anastasia-labs/<pkg>
  const virtualStore = join(nodeModulesDir, ".pnpm");
  if (existsSync(virtualStore)) {
    for (const entry of readdirSync(virtualStore)) {
      for (const shortName of shortPackageNames) {
        if (!entry.startsWith(`@anastasia-labs+${shortName}@`)) continue;
        const packageDir = join(
          virtualStore,
          entry,
          "node_modules",
          "@anastasia-labs",
          shortName,
        );
        const wasmPath = join(packageDir, CML_WASM_PATCH_PIN_V1.wasmFileName);
        if (existsSync(wasmPath)) {
          found.set(wasmPath, {
            wasmPath,
            packageDir,
            version: readPackageVersion(packageDir),
          });
        }
      }
    }
  }

  // hoisted / npm-style layout: node_modules/@anastasia-labs/<pkg>
  for (const shortName of shortPackageNames) {
    const packageDir = join(nodeModulesDir, "@anastasia-labs", shortName);
    const wasmPath = join(packageDir, CML_WASM_PATCH_PIN_V1.wasmFileName);
    if (existsSync(wasmPath)) {
      // resolve symlinks so pnpm links are deduped against the virtual store
      const realPath = statSync(wasmPath, { throwIfNoEntry: false })
        ? resolve(wasmPath)
        : null;
      if (realPath && !found.has(realPath)) {
        found.set(realPath, {
          wasmPath: realPath,
          packageDir,
          version: readPackageVersion(packageDir),
        });
      }
    }
  }
};

/**
 * Every `node_modules` reachable from the demo root or from one directory
 * below it. This covers the workspace store (`demo/node_modules`) plus the
 * private stores some sub-projects carry (`demo/da-committee-node`,
 * `demo/midgard-manager`).
 */
export const discoverCmlWasmTargets = (demoRoot = DEMO_ROOT) => {
  const found = new Map();
  collectFromNodeModules(join(demoRoot, "node_modules"), found);
  let children = [];
  try {
    children = readdirSync(demoRoot, { withFileTypes: true });
  } catch {
    children = [];
  }
  for (const child of children) {
    if (!child.isDirectory() || child.name === "node_modules") continue;
    collectFromNodeModules(join(demoRoot, child.name, "node_modules"), found);
  }
  return [...found.values()].sort((left, right) =>
    left.wasmPath.localeCompare(right.wasmPath),
  );
};

/* ------------------------------------------------------------------ *
 * Apply
 * ------------------------------------------------------------------ */

const writeThroughRename = (wasmPath, bytes) => {
  // NEVER write in place: pnpm hardlinks package files into the global
  // content-addressable store (link counts of 40+ observed here), so an
  // in-place write would corrupt the store for every project on the machine.
  const temporaryPath = `${wasmPath}.midgard-patch-${process.pid}.tmp`;
  const mode = statSync(wasmPath).mode & 0o777;
  try {
    writeFileSync(temporaryPath, bytes, { flag: "wx" });
    chmodSync(temporaryPath, mode);
    renameSync(temporaryPath, wasmPath);
  } catch (cause) {
    try {
      if (existsSync(temporaryPath)) unlinkSync(temporaryPath);
    } catch {
      /* best effort */
    }
    throw cause;
  }
};

const writeReceipt = (target, action) => {
  try {
    writeFileSync(
      join(target.packageDir, CML_WASM_PATCH_PIN_V1.receiptFileName),
      `${JSON.stringify(
        {
          patch: "c26-cml-wasm-shadow-stack-v1",
          appliedAtIso: new Date().toISOString(),
          action,
          packageVersion: target.version,
          stockSha256: CML_WASM_PATCH_PIN_V1.stockSha256,
          patchedSha256: CML_WASM_PATCH_PIN_V1.patchedSha256,
          memoryInitialPages: {
            stock: CML_WASM_PATCH_PIN_V1.stockMemoryInitialPages,
            patched: CML_WASM_PATCH_PIN_V1.patchedMemoryInitialPages,
          },
          stackPointer: {
            stock: CML_WASM_PATCH_PIN_V1.stockStackPointer,
            patched: CML_WASM_PATCH_PIN_V1.patchedStackPointer,
          },
          requiredV8StackSizeKb: CML_WASM_PATCH_PIN_V1.requiredV8StackSizeKb,
          documentation: "demo/scripts/cml-wasm-stack-patch.md",
          revert: "node demo/scripts/patch-cml-wasm-stack.mjs --revert",
        },
        null,
        2,
      )}\n`,
      "utf8",
    );
  } catch {
    // A read-only store is not a reason to fail the install; the sha256 of the
    // wasm itself is the authoritative record.
  }
};

const classify = (target) => {
  const bytes = readFileSync(target.wasmPath);
  const digest = sha256Hex(bytes);
  if (digest === CML_WASM_PATCH_PIN_V1.stockSha256) return { state: "stock", bytes, digest };
  if (digest === CML_WASM_PATCH_PIN_V1.patchedSha256) return { state: "patched", bytes, digest };
  return { state: "unknown", bytes, digest };
};

export const runCmlWasmStackPatch = ({
  mode = "patch",
  demoRoot = DEMO_ROOT,
  log = () => {},
} = {}) => {
  const targets = discoverCmlWasmTargets(demoRoot);
  const pinned = targets.filter(
    (target) => target.version === CML_WASM_PATCH_PIN_V1.pinnedVersion,
  );
  const others = targets.filter(
    (target) => target.version !== CML_WASM_PATCH_PIN_V1.pinnedVersion,
  );
  const results = [];
  const failures = [];

  for (const target of others) {
    results.push({
      wasmPath: target.wasmPath,
      version: target.version,
      action: "skipped-unpinned-version",
    });
    log(
      `  skip   ${target.version ?? "unknown version"}  (not the pinned ${CML_WASM_PATCH_PIN_V1.pinnedVersion})  ${target.wasmPath}`,
    );
  }

  for (const target of pinned) {
    const { state, bytes, digest } = classify(target);

    if (state === "unknown") {
      failures.push(
        `${target.wasmPath}\n` +
          `      version   ${target.version}\n` +
          `      sha256    ${digest}\n` +
          `      expected  ${CML_WASM_PATCH_PIN_V1.stockSha256} (stock)\n` +
          `           or   ${CML_WASM_PATCH_PIN_V1.patchedSha256} (already patched)`,
      );
      results.push({
        wasmPath: target.wasmPath,
        version: target.version,
        sha256: digest,
        action: "refused-unrecognised-artifact",
      });
      continue;
    }

    const wantPatched = mode !== "revert";
    const alreadyThere = wantPatched ? state === "patched" : state === "stock";

    if (mode === "check" || alreadyThere) {
      results.push({
        wasmPath: target.wasmPath,
        version: target.version,
        sha256: digest,
        action: mode === "check" ? `verified-${state}` : "unchanged",
      });
      log(`  ${mode === "check" ? "verify" : "ok    "} ${state.padEnd(7)} ${target.wasmPath}`);
      continue;
    }

    const rewritten = wantPatched
      ? patchCmlWasmBytes(bytes)
      : revertCmlWasmBytes(bytes);
    const expectedDigest = wantPatched
      ? CML_WASM_PATCH_PIN_V1.patchedSha256
      : CML_WASM_PATCH_PIN_V1.stockSha256;
    const producedDigest = sha256Hex(rewritten);
    if (producedDigest !== expectedDigest) {
      failures.push(
        `${target.wasmPath}\n` +
          `      rewrite produced sha256 ${producedDigest}\n` +
          `      but the pin requires     ${expectedDigest}\n` +
          `      NOTHING WAS WRITTEN.`,
      );
      results.push({
        wasmPath: target.wasmPath,
        version: target.version,
        action: "refused-output-hash-mismatch",
      });
      continue;
    }

    writeThroughRename(target.wasmPath, rewritten);
    const verified = sha256Hex(readFileSync(target.wasmPath));
    if (verified !== expectedDigest) {
      failures.push(
        `${target.wasmPath}\n` +
          `      post-write sha256 ${verified} != ${expectedDigest}`,
      );
      results.push({
        wasmPath: target.wasmPath,
        version: target.version,
        action: "failed-post-write-verification",
      });
      continue;
    }
    writeReceipt(target, wantPatched ? "patched" : "reverted");
    results.push({
      wasmPath: target.wasmPath,
      version: target.version,
      sha256: verified,
      action: wantPatched ? "patched" : "reverted",
    });
    log(
      `  ${wantPatched ? "patch " : "revert"} ${
        wantPatched ? "stock -> patched" : "patched -> stock"
      }  ${target.wasmPath}`,
    );
  }

  if (pinned.length === 0) {
    failures.push(
      `no @anastasia-labs/cardano-multiplatform-lib-* @ ${CML_WASM_PATCH_PIN_V1.pinnedVersion} found under ${demoRoot}.\n` +
        "      The pinned dependency moved, or dependencies are not installed.\n" +
        "      Refusing to report success: the max-depth Plutus Data suites\n" +
        "      depend on this exact artifact.",
    );
  }

  return { mode, demoRoot, targets: results, failures, pinnedTargetCount: pinned.length };
};

/**
 * Writes a standalone, `require()`-able copy of the STOCK nodejs CML package
 * into `outputDir`. The wasm is reverted from whatever is installed and then
 * hash-checked against the stock pin, so the differential suite compares the
 * patched library against bytes it has *proved* are the original ones.
 */
export const emitStockCmlPackage = (outputDir, { demoRoot = DEMO_ROOT } = {}) => {
  const nodejsTargets = discoverCmlWasmTargets(demoRoot).filter(
    (candidate) =>
      candidate.version === CML_WASM_PATCH_PIN_V1.pinnedVersion &&
      candidate.packageDir.includes("cardano-multiplatform-lib-nodejs"),
  );
  // Prefer the workspace store, i.e. the copy the demo packages actually load.
  const workspaceStorePrefix = join(demoRoot, "node_modules");
  const target =
    nodejsTargets.find((candidate) =>
      candidate.wasmPath.startsWith(workspaceStorePrefix),
    ) ?? nodejsTargets[0];
  if (!target) {
    throw new Error(
      `no @anastasia-labs/cardano-multiplatform-lib-nodejs@${CML_WASM_PATCH_PIN_V1.pinnedVersion} found under ${demoRoot}`,
    );
  }
  const installed = readFileSync(target.wasmPath);
  const installedDigest = sha256Hex(installed);
  let stock;
  if (installedDigest === CML_WASM_PATCH_PIN_V1.stockSha256) {
    stock = installed;
  } else if (installedDigest === CML_WASM_PATCH_PIN_V1.patchedSha256) {
    stock = revertCmlWasmBytes(installed);
  } else {
    throw new Error(
      `installed wasm sha256 ${installedDigest} matches neither the stock nor the patched pin`,
    );
  }
  const stockDigest = sha256Hex(stock);
  if (stockDigest !== CML_WASM_PATCH_PIN_V1.stockSha256) {
    throw new Error(
      `reverted wasm sha256 ${stockDigest} != stock pin ${CML_WASM_PATCH_PIN_V1.stockSha256}`,
    );
  }
  mkdirSync(outputDir, { recursive: true });
  for (const fileName of [
    "cardano_multiplatform_lib.js",
    "cardano_multiplatform_lib.d.ts",
    "package.json",
  ]) {
    const from = join(target.packageDir, fileName);
    if (existsSync(from)) {
      writeFileSync(join(outputDir, fileName), readFileSync(from));
    }
  }
  writeFileSync(
    join(outputDir, CML_WASM_PATCH_PIN_V1.wasmFileName),
    stock,
  );
  return {
    sourcePackageDir: target.packageDir,
    installedSha256: installedDigest,
    stockSha256: stockDigest,
    mainPath: join(outputDir, "cardano_multiplatform_lib.js"),
  };
};

/* ------------------------------------------------------------------ *
 * CLI
 * ------------------------------------------------------------------ */

const isMain =
  process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url));

if (isMain) {
  const rawArgv = process.argv.slice(2);
  const rootIndex = rawArgv.indexOf("--root");
  const demoRoot =
    rootIndex === -1 ? DEMO_ROOT : resolve(rawArgv[rootIndex + 1] ?? ".");
  const emitIndex = rawArgv.indexOf("--emit-stock-package");
  if (emitIndex !== -1) {
    const outputDir = rawArgv[emitIndex + 1];
    if (!outputDir) {
      process.stderr.write("--emit-stock-package requires a target directory\n");
      process.exit(2);
    }
    try {
      process.stdout.write(
        `${JSON.stringify(
          emitStockCmlPackage(resolve(outputDir), { demoRoot }),
          null,
          2,
        )}\n`,
      );
      process.exit(0);
    } catch (cause) {
      process.stderr.write(
        `FATAL: could not materialise the stock CML package.\n  ${String(
          cause && cause.stack ? cause.stack : cause,
        )}\n`,
      );
      process.exit(1);
    }
  }

  const argv = new Set(rawArgv);
  const mode = argv.has("--revert")
    ? "revert"
    : argv.has("--check")
      ? "check"
      : "patch";
  const json = argv.has("--json");
  const quiet = json || argv.has("--quiet");

  if (!quiet) {
    process.stdout.write(
      `c26 CML wasm shadow-stack patch (${mode}) — pin ${CML_WASM_PATCH_PIN_V1.stockSha256.slice(0, 12)}… -> ${CML_WASM_PATCH_PIN_V1.patchedSha256.slice(0, 12)}…\n`,
    );
  }

  let outcome;
  try {
    outcome = runCmlWasmStackPatch({
      mode,
      demoRoot,
      log: quiet ? () => {} : (line) => process.stdout.write(`${line}\n`),
    });
  } catch (cause) {
    process.stderr.write(
      `\nFATAL: CML wasm patch aborted before completing.\n  ${String(
        cause && cause.stack ? cause.stack : cause,
      )}\n`,
    );
    process.exit(1);
  }

  if (json) {
    process.stdout.write(`${JSON.stringify(outcome, null, 2)}\n`);
  }

  if (outcome.failures.length > 0) {
    process.stderr.write(
      "\n==================== CML WASM PATCH REFUSED ====================\n" +
        "The vendored cardano-multiplatform-lib wasm did not match its pin.\n" +
        "No unrecognised binary is ever patched — that is the whole point of\n" +
        "the pin. Nothing was written for the entries below.\n\n" +
        outcome.failures.map((failure) => `  !! ${failure}`).join("\n\n") +
        "\n\nIf CML was intentionally upgraded, re-run the C26 measurements and\n" +
        "update the pin in demo/scripts/patch-cml-wasm-stack.mjs together with\n" +
        "demo/scripts/cml-wasm-stack-patch.md. Do not loosen the check.\n" +
        "===============================================================\n",
    );
    process.exit(1);
  }

  if (!quiet) {
    const patched = outcome.targets.filter((t) => t.action === "patched").length;
    const reverted = outcome.targets.filter((t) => t.action === "reverted").length;
    const unchanged = outcome.targets.filter((t) => t.action === "unchanged").length;
    const verified = outcome.targets.filter((t) =>
      String(t.action).startsWith("verified-"),
    ).length;
    process.stdout.write(
      `  done: ${outcome.pinnedTargetCount} pinned target(s); patched=${patched} reverted=${reverted} unchanged=${unchanged} verified=${verified}\n` +
        `  max-depth suites additionally require --stack-size=${CML_WASM_PATCH_PIN_V1.requiredV8StackSizeKb} (V8 machine stack)\n`,
    );
  }
}
