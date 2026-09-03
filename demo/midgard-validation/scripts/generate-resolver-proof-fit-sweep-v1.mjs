#!/usr/bin/env node

/**
 * #606/§8.3 C53 "Resolver proof-fit sweep" — generator.
 *
 * This is a thin, deterministic wrapper. All of the real work — driving
 * each resolver's transaction through the real emulator harness
 * (`tx.complete({ localUPLCEval: true })` + sign + submit + `awaitTx()`) and
 * measuring the genuinely evaluated ExUnits/bytes off the signed
 * transaction — happens in
 * `demo/midgard-fault-proofs/tests/resolver-proof-fit-sweep-generate.test.ts`,
 * a vitest test file living in the same package as the pinned
 * emulator-harness support helpers it imports verbatim
 * (`tests/support/submit-init-emulator-shared.ts`,
 * `tests/support/submit-init-emulator-fixtures.ts`,
 * `tests/support/legacy-submit-emulator.ts`, and `../src/index.js`'s
 * `submitValidationDispute*` family). Those helpers are plain TypeScript
 * with no compiled `dist` entry point and are not resolvable from a plain
 * `node`-run script (Node's built-in type-stripping does not elide
 * value-position imports of type-only exports the way `tsc`/`esbuild` do,
 * and there is no `.js`-to-`.ts` sibling-resolution fallback) — running the
 * generation logic as a vitest test inside
 * `demo/midgard-fault-proofs` is the sanctioned resolution for that
 * constraint. That test file is gated behind
 * `MIDGARD_REGENERATE_RESOLVER_SWEEP=1` so routine `vitest run`/CI passes
 * over `demo/midgard-fault-proofs` skip it at ~zero cost; this script is the
 * only intended caller.
 *
 * usage: node scripts/generate-resolver-proof-fit-sweep-v1.mjs [--check]
 */

import { execFileSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const scriptPath = fileURLToPath(import.meta.url);
const checkOnly = process.argv.includes("--check");

const fail = (message) => {
  throw new Error(`resolver proof-fit sweep generation failed: ${message}`);
};

const repoRoot = fileURLToPath(new URL("../../../", import.meta.url));
const faultProofsDir = join(repoRoot, "demo", "midgard-fault-proofs");
const fixturePath = fileURLToPath(
  new URL(
    "../tests/fixtures/resolver-proof-fit-sweep-v1.generated.json",
    import.meta.url,
  ),
);

/**
 * Runs the harness-based generation worker
 * (`resolver-proof-fit-sweep-generate.test.ts`) via `vitest run` inside
 * `demo/midgard-fault-proofs`, writing the fixture JSON to `outputPath`.
 * Never swallows the worker's own output — a real emulator lifecycle
 * failure surfaces exactly as vitest reports it.
 */
const runGenerator = (outputPath) => {
  try {
    execFileSync(
      "pnpm",
      [
        "--dir",
        faultProofsDir,
        "exec",
        "vitest",
        "run",
        "tests/resolver-proof-fit-sweep-generate.test.ts",
      ],
      {
        cwd: faultProofsDir,
        env: {
          ...process.env,
          MIDGARD_REGENERATE_RESOLVER_SWEEP: "1",
          MIDGARD_RESOLVER_SWEEP_OUTPUT_PATH: outputPath,
        },
        stdio: "inherit",
      },
    );
  } catch (error) {
    fail(
      `the harness generation worker (resolver-proof-fit-sweep-generate.test.ts) failed: ${String(error)}`,
    );
  }
};

if (checkOnly) {
  const tmpDir = mkdtempSync(join(tmpdir(), "resolver-proof-fit-sweep-"));
  const tmpOutputPath = join(
    tmpDir,
    "resolver-proof-fit-sweep-v1.generated.json",
  );
  try {
    runGenerator(tmpOutputPath);
    let committed;
    try {
      committed = readFileSync(fixturePath, "utf8");
    } catch (error) {
      fail(`missing generated artifact ${fixturePath}: ${String(error)}`);
    }
    const actual = readFileSync(tmpOutputPath, "utf8");
    if (actual !== committed) {
      fail(`generated artifact is stale: ${fixturePath}`);
    }
    process.stdout.write(
      `${JSON.stringify({ script: scriptPath, check: "byte-identical", fixturePath }, null, 2)}\n`,
    );
  } finally {
    rmSync(tmpDir, { recursive: true, force: true });
  }
} else {
  runGenerator(fixturePath);
}
