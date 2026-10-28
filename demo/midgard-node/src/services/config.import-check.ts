import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  type DeploymentProfileName,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import { Config } from "effect";

import { boundedValidationInteger } from "./config.node-config-dep.js";

/**
 * The import checker's bounds (`../import-check/`).
 *
 * One script run is killed `floor + a * declared cpu + b * bytes` after it
 * starts on a started worker, where bytes are the script and its context:
 * - `killFloorMs`: the floor, at least the p99.9 of a minimal run including
 *   a worker GC pause;
 * - `killMsPerGigaCpu`: a, in milliseconds per 10^9 declared cpu units;
 * - `killUsPerKib`: b, in microseconds per KiB.
 * The kill is wall-clock time, so a killed run is no verdict: its entry is
 * neither faulted nor cached, and it is run again later
 * (`../import-check/checker.ts`).
 *
 * The defaults are the ruled values 24 ms, 840 ms per 10^9 cpu and 440 us
 * per KiB. Against the pool's import-check lane with the TypeScript verdict
 * evaluator (`tests/benchmarks/import-check-kill.bench.ts`, 2026-10-15,
 * Node 22, a machine shared with other work) they are:
 * - floor: 1.7 times the slowest of 20,000 minimal runs (13.9 ms; p99.9
 *   4.9 ms, p50 1.5 ms);
 * - a: 3.1 times the slope of a machine-step loop (273 ms per 10^9 cpu; a
 *   chain of hashing builtins measures 117 ms). At 3 * 10^9 cpu the kill,
 *   2,545 ms, is 2.2 times the slowest sample seen (1,153 ms);
 * - b: 2.8 times the slope over the run's bytes (155 us per KiB).
 *
 * `blockCapMs` bounds the import pass's time on one block: no run starts
 * after it (a run still waiting for a worker then is cancelled), and the
 * block's entries not yet run wait as skipped (`import_check_cap`) for the
 * backlog lane. A run started within the cap keeps its whole kill, so the
 * pass holds a block for at most `blockCapMs` plus the longest kill of a
 * run it started. A fault's fresh retry is a run like any other: one not
 * started within the cap is cancelled, and its entry waits as skipped (on
 * a two-worker pool the retry waits for the killed worker's replacement,
 * the lanes' one worker). For example, at
 * the defaults a run at the per-transaction cpu limit (10^10, 8.4 s) with
 * 16 MiB of bytes (7.2 s) is killed 15.6 s after its start, so a block
 * started with it is held 35.6 s at most. The cap never makes an entry
 * unrunnable: the backlog lane runs it under the same kill.
 *
 * `negativeCacheTtlMs` is how long a script whose run faulted twice skips
 * its import-time runs; at most half the block maturity M (the default).
 *
 * `backlogWorkers` is the share of the validation pool reserved for the
 * backlog lane, clamped to 1 to `size - 1` workers: the import and backlog
 * lanes together hold at most `size - 1`, so admission always keeps a
 * worker. A single-worker pool reserves none: both lanes run only while
 * admission is idle (a warning is logged).
 *
 * `openerMarginMs` is the time a challenge opener needs before an entry's
 * deadline, the on-chain case open deadline `block_end + M/2`
 * (`../import-check/deadline.ts`), to land its Init, Open and VerifySource
 * transactions (default `OPENER_MARGIN_MS_BY_PROFILE` for the compiled
 * profile; no profile parameter bounds it). `unverified_near_deadline` fires when the backlog's
 * projected completion of the unrun entries runs past a deadline minus this
 * margin, over the pool's clamped backlog share (with none, at every
 * deadline). Faulted entries are not projected: each already raised
 * `uplc_import_fault`.
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

// Room before the case open deadline (block_end + M/2) for an opener's Init,
// Open and VerifySource to land; the values are the pre-removal per-profile
// margins (twice the former dispute response window).
const OPENER_MARGIN_MS_BY_PROFILE = {
  mainnet: 600_000,
  "preprod-public": 600_000,
  "preprod-testing": 120_000,
  "local-devnet-testing": 120_000,
  "preprod-emulator-testing": 120_000,
  "preprod-emulator-curated": 120_000,
} as const satisfies Record<DeploymentProfileName, number>;

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
  openerMarginMs: OPENER_MARGIN_MS_BY_PROFILE[SELECTED_DEPLOYMENT_PROFILE.name],
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
