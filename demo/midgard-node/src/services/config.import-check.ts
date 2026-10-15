import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
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
 * The defaults are four times the values measured on the pool's
 * import-check lane with the TypeScript verdict evaluator
 * (`tests/benchmarks/import-check-kill.bench.ts`, 2026-10-15, Node 22):
 * - floor: p99.9 of 20,000 minimal runs 6.0 ms (p50 1.2 ms, max 11.5 ms);
 * - a: the slope of a machine-step loop, 207 ms per 10^9 cpu (a chain of
 *   hashing builtins measures 95 ms); the slowest loop sample, 1,153 ms at
 *   3 * 10^9 cpu, sits under half its kill;
 * - b: the slope over the run's bytes, 110 us per KiB.
 *
 * `blockCapMs` is the checker's wall time per block: the transactions not
 * started within it are recorded as skipped (`import_check_cap`). It covers
 * a block at twice the per-transaction cpu limit (2 * 10^10 cpu, 4.2 s) plus
 * 10,000 minimal runs at their median (12 s).
 *
 * `negativeCacheTtlMs` is how long a script whose run faulted twice skips
 * its import-time runs; at most half the block maturity M (the default).
 *
 * `backlogWorkers` is the validation pool's share reserved for the
 * backlog lane (at least one worker, at most the pool).
 *
 * `openerMarginMs` is the time a challenge opener needs before an entry's
 * deadline: its two transactions, each within the dispute response window
 * (the default). `unverified_near_deadline` fires when the backlog's
 * projected completion runs past a deadline minus this margin.
 */
export type ImportCheckConfig = Readonly<{
  killFloorMs: number;
  killMsPerGigaCpu: number;
  killUsPerKib: number;
  blockCapMs: number;
  negativeCacheTtlMs: number;
  backlogWorkers: number;
  openerMarginMs: number;
}>;

const HALF_MATURITY_MS = Math.floor(
  MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs / 2,
);

export const DEFAULT_IMPORT_CHECK_CONFIG: ImportCheckConfig = {
  killFloorMs: 24,
  killMsPerGigaCpu: 840,
  killUsPerKib: 440,
  blockCapMs: 20_000,
  negativeCacheTtlMs: HALF_MATURITY_MS,
  backlogWorkers: 1,
  openerMarginMs:
    2 * MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
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
  negativeCacheTtlMs: boundedValidationInteger(
    "IMPORT_CHECK_NEGATIVE_CACHE_TTL_MS",
    DEFAULT_IMPORT_CHECK_CONFIG.negativeCacheTtlMs,
  ).pipe(
    Config.mapAttempt((value) => {
      if (value > HALF_MATURITY_MS)
        throw new Error(
          `IMPORT_CHECK_NEGATIVE_CACHE_TTL_MS must be at most half the block maturity (${HALF_MATURITY_MS.toString()})`,
        );
      return value;
    }),
  ),
  backlogWorkers: boundedValidationInteger(
    "IMPORT_CHECK_BACKLOG_WORKERS",
    DEFAULT_IMPORT_CHECK_CONFIG.backlogWorkers,
  ),
  openerMarginMs: boundedValidationInteger(
    "IMPORT_CHECK_OPENER_MARGIN_MS",
    DEFAULT_IMPORT_CHECK_CONFIG.openerMarginMs,
    true,
  ),
});
