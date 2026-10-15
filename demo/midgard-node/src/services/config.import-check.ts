import { Config } from "effect";

import { boundedValidationInteger } from "./config.node-config-dep.js";

/**
 * The import checker's bounds (`../import-check/`).
 *
 * One script run is killed `floor + a * declared cpu + b * bytes` after it
 * starts on a started worker, where bytes are the script, its context and
 * the transaction's program-material sidecar:
 * - `killFloorMs`: the floor, at least the p99.9 of a minimal run including
 *   a worker GC pause;
 * - `killMsPerGigaCpu`: a, in milliseconds per 10^9 declared cpu units;
 * - `killUsPerKib`: b, in microseconds per KiB.
 *
 * `blockCapMs` is the checker's wall time per block: the transactions not
 * started within it are recorded as skipped (`import_check_cap`).
 *
 * The defaults are measured on the pool's import-check lane with the
 * TypeScript verdict evaluator (`tests/benchmarks/import-check-kill.bench.ts`,
 * 2026-10-15, Node 22):
 * - floor 6 ms: the p99.9 of 20,000 minimal runs (p50 1.2 ms, p99 2.4 ms,
 *   max 11.5 ms);
 * - 210 ms per 10^9 cpu: the slope of a machine-step loop (207 ms; a chain
 *   of hashing builtins measures 95 ms);
 * - 110 us per KiB: the slope over the run's bytes.
 * `blockCapMs` covers a block at twice the per-transaction cpu limit
 * (2 * 10^10 cpu, 4.2 s) plus 10,000 minimal runs at their median (12 s).
 */
export type ImportCheckConfig = Readonly<{
  killFloorMs: number;
  killMsPerGigaCpu: number;
  killUsPerKib: number;
  blockCapMs: number;
}>;

export const DEFAULT_IMPORT_CHECK_CONFIG: ImportCheckConfig = {
  killFloorMs: 6,
  killMsPerGigaCpu: 210,
  killUsPerKib: 110,
  blockCapMs: 20_000,
};

export const importCheckConfig: Config.Config<ImportCheckConfig> = Config.all({
  killFloorMs: boundedValidationInteger(
    "IMPORT_CHECK_KILL_FLOOR_MS",
    DEFAULT_IMPORT_CHECK_CONFIG.killFloorMs,
  ),
  killMsPerGigaCpu: boundedValidationInteger(
    "IMPORT_CHECK_KILL_MS_PER_GIGA_CPU",
    DEFAULT_IMPORT_CHECK_CONFIG.killMsPerGigaCpu,
    true,
  ),
  killUsPerKib: boundedValidationInteger(
    "IMPORT_CHECK_KILL_US_PER_KIB",
    DEFAULT_IMPORT_CHECK_CONFIG.killUsPerKib,
    true,
  ),
  blockCapMs: boundedValidationInteger(
    "IMPORT_CHECK_BLOCK_CAP_MS",
    DEFAULT_IMPORT_CHECK_CONFIG.blockCapMs,
  ),
});
