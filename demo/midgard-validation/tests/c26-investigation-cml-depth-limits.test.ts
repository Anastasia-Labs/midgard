/**
 * C26 INVESTIGATION SCAFFOLDING — throwaway, not evidence.
 *
 * Reproduces the two independent recursion limits behind the C26 PARTIAL
 * status and pins the depths measured in
 * `docs/exec-plans/evidence/c26-cml-investigation.md`.
 *
 * Every case is skipped unless `MIDGARD_C26_INVESTIGATION=1` so the default
 * suite stays green: the deep cases deliberately trap CML/WASM, and a trapped
 * `WebAssembly.Instance` is poisoned for the rest of the process, which would
 * corrupt any test sharing the worker.
 *
 * Run with:
 *   MIDGARD_C26_INVESTIGATION=1 pnpm --dir demo/midgard-validation test -- \
 *     c26-investigation-cml-depth-limits
 *
 * The parent may delete this file; it is scaffolding for the memo, and it
 * intentionally forks a child process per probe so traps stay contained.
 */
import { execFileSync } from "node:child_process";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

const investigationEnabled =
  process.env.MIDGARD_C26_INVESTIGATION === "1";
const describeInvestigation = investigationEnabled
  ? describe
  : describe.skip;

const unaryConstructorDataCborHex = (depth: number): string =>
  "d8799f".repeat(depth) + "00" + "ff".repeat(depth);

type ProbeResult = {
  readonly ok: boolean;
  readonly errType?: string;
  readonly message?: string;
};

/**
 * Runs one CML operation at one depth in a fresh child process. A fresh
 * process per probe is required: `memory access out of bounds` leaves the
 * shared CML instance unusable, so in-process probing would make every later
 * assertion meaningless.
 */
const probeInChildProcess = (
  operation: "plutusDataParse" | "transactionParse",
  depth: number,
): ProbeResult => {
  const scratch = mkdtempSync(join(tmpdir(), "c26-probe-"));
  const scriptPath = join(scratch, "probe.cjs");
  writeFileSync(
    scriptPath,
    `const { CML } = require(${JSON.stringify(
      require.resolve("@lucid-evolution/lucid"),
    )});
const depth = ${depth.toString()};
const unary = "d8799f".repeat(depth) + "00" + "ff".repeat(depth);
const encodeHead = (major, value) => {
  if (value < 24n) return Buffer.from([(major << 5) | Number(value)]);
  const widths = [[0xffn, 24, 1], [0xffffn, 25, 2], [0xffffffffn, 26, 4], [0xffffffffffffffffn, 27, 8]];
  const width = widths.find(([limit]) => value <= limit);
  const out = Buffer.alloc(1 + width[2]);
  out[0] = (major << 5) | width[1];
  let rest = value;
  for (let index = width[2]; index > 0; index -= 1) { out[index] = Number(rest & 0xffn); rest >>= 8n; }
  return out;
};
const uint = (value) => encodeHead(0, value);
const bytes = (buf) => Buffer.concat([encodeHead(2, BigInt(buf.length)), buf]);
const array = (items) => Buffer.concat([encodeHead(4, BigInt(items.length)), ...items]);
const map = (entries) => Buffer.concat([encodeHead(5, BigInt(entries.length)), ...entries.flatMap(([k, v]) => [k, v])]);
const tag = (value, item) => Buffer.concat([encodeHead(6, value), item]);
const transactionHex = () => {
  const inputs = tag(258n, array([array([bytes(Buffer.alloc(32)), uint(0n)])]));
  const address = Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 7)]);
  const output = map([
    [uint(0n), bytes(address)],
    [uint(1n), uint(39999000000n)],
    [uint(2n), array([uint(1n), tag(24n, bytes(Buffer.from(unary, "hex")))])],
  ]);
  const body = map([[uint(0n), inputs], [uint(1n), array([output])], [uint(2n), uint(1000000n)]]);
  const witnessSet = map([[uint(0n), tag(258n, array([array([bytes(Buffer.alloc(32, 1)), bytes(Buffer.alloc(64, 2))])]))]]);
  return array([body, witnessSet, Buffer.from([0xf5]), Buffer.from([0xf6])]).toString("hex");
};
try {
  if (${JSON.stringify(operation)} === "plutusDataParse") CML.PlutusData.from_cbor_hex(unary);
  else CML.Transaction.from_cbor_hex(transactionHex());
  process.stdout.write(JSON.stringify({ ok: true }));
} catch (cause) {
  process.stdout.write(JSON.stringify({
    ok: false,
    errType: cause && cause.constructor ? cause.constructor.name : typeof cause,
    message: String(cause && cause.message ? cause.message : cause).slice(0, 120),
  }));
}
`,
    "utf8",
  );
  return JSON.parse(
    execFileSync(process.execPath, [scriptPath], {
      encoding: "utf8",
      timeout: 120_000,
    }),
  ) as ProbeResult;
};

describeInvestigation("C26 investigation: CML/WASM unary-depth limits", () => {
  it("shows the repo's own Plutus Data CBOR shape helper is depth-independent", () => {
    for (const depth of [1, 1_024, 2_048, 4_043, 4_044, 16_000]) {
      const hex = unaryConstructorDataCborHex(depth);
      expect(hex.length / 2).toBe(depth * 4 + 1);
    }
  });

  it("reproduces the wasm shadow-stack trap and pins the exact maximum depth", () => {
    expect(probeInChildProcess("plutusDataParse", 1_024).ok).toBe(true);
    expect(probeInChildProcess("plutusDataParse", 1_522).ok).toBe(true);

    const trapped = probeInChildProcess("plutusDataParse", 1_523);
    expect(trapped.ok).toBe(false);
    expect(trapped.errType).toBe("RuntimeError");
    expect(trapped.message).toContain("memory access out of bounds");

    for (const depth of [2_048, 4_043, 4_044]) {
      const deep = probeInChildProcess("plutusDataParse", depth);
      expect(deep.ok).toBe(false);
      expect(deep.message).toContain("memory access out of bounds");
    }
  }, 300_000);

  it("reproduces the same trap through Transaction.from_cbor_hex", () => {
    expect(probeInChildProcess("transactionParse", 1_502).ok).toBe(true);

    const trapped = probeInChildProcess("transactionParse", 1_503);
    expect(trapped.ok).toBe(false);
    expect(trapped.message).toContain("memory access out of bounds");

    const maximum = probeInChildProcess("transactionParse", 4_043);
    expect(maximum.ok).toBe(false);
    expect(maximum.message).toContain("memory access out of bounds");
  }, 300_000);

  it("confirms one trap poisons the shared CML instance for later calls", () => {
    const scratch = mkdtempSync(join(tmpdir(), "c26-poison-"));
    const scriptPath = join(scratch, "poison.cjs");
    writeFileSync(
      scriptPath,
      `const { CML } = require(${JSON.stringify(
        require.resolve("@lucid-evolution/lucid"),
      )});
const deep = "d8799f".repeat(4043) + "00" + "ff".repeat(4043);
let firstTrap = null;
try { CML.PlutusData.from_cbor_hex(deep); } catch (cause) { firstTrap = String(cause.message).slice(0, 60); }
let shallowAfterTrap = null;
try { shallowAfterTrap = CML.PlutusData.from_cbor_hex("d87980").to_cbor_hex(); }
catch (cause) { shallowAfterTrap = "ERROR: " + String(cause.message).slice(0, 60); }
process.stdout.write(JSON.stringify({ firstTrap, shallowAfterTrap }));
`,
      "utf8",
    );
    const result = JSON.parse(
      execFileSync(process.execPath, [scriptPath], {
        encoding: "utf8",
        timeout: 120_000,
      }),
    ) as { readonly firstTrap: string; readonly shallowAfterTrap: string };

    expect(result.firstTrap).toContain("memory access out of bounds");
    // A depth-3 datum needs no recursion budget at all, yet still fails:
    // the instance itself is unusable after the trap.
    expect(result.shallowAfterTrap).toContain("ERROR");
  }, 300_000);
});
