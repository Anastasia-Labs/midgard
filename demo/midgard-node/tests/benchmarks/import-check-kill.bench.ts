/**
 * Measures the import checker's kill parameters (`config.import-check.ts`)
 * on the evaluator it runs: the validation pool's import-check lane, on the
 * production worker.
 *
 * - floor: the p99.9 latency of a minimal transaction's run (an honest
 *   one-input spend), over enough runs that worker GC pauses fall inside it;
 * - ms per 10^9 cpu: the slope of run latency over spent cpu, for a loop of
 *   machine steps and for a chain of hashing builtins (the larger one sets
 *   the default);
 * - us per KiB: the slope of run latency over the run's bytes (script,
 *   context and program material), for an accepting script carrying an
 *   unused constant.
 *
 * It also reports each cpu point's coverage under the default kill: the kill
 * for that run with its declared cpu set to the cpu it spent (an honest
 * transaction's tightest budget), over its measured latency.
 *
 * Run from demo/midgard-node after `pnpm run build`:
 *   NODE_ENV=emulator npx vitest run --config vitest.bench.config.ts \
 *     tests/benchmarks/import-check-kill.bench.ts --disableConsoleIntercept
 * `BENCH_IMPORT_CHECK_MINIMAL_RUNS` sets the minimal-run sample (20,000).
 */
import { encodeCbor } from "@al-ft/midgard-core";
import {
  encodeMidgardCekCardanoFlatProgram,
  type MidgardCekVerdictRequest,
} from "@al-ft/midgard-validation";
import {
  Application,
  Builtin,
  Lambda,
  midgardCekBuiltinTagOfName,
  UPLCConst,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@al-ft/midgard-validation/uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  importCheckKillMs,
  importCheckRunBytes,
} from "../../src/import-check/kill.js";
import { DEFAULT_IMPORT_CHECK_CONFIG } from "../../src/services/config.import-check.js";
import {
  evaluateImportCheckOn,
  FixedValidationWorkerPool,
} from "../../src/services/validation-pool.js";
import { committedBlock, fundedBy } from "../import-check.fixture.js";
import {
  init,
  workerEntry,
} from "../validation-worker-pool.build-adversarial-corpus.js";

const MINIMAL_RUNS = Number(
  process.env.BENCH_IMPORT_CHECK_MINIMAL_RUNS ?? 20_000,
);

const quantile = (sorted: readonly number[], q: number) =>
  sorted[Math.min(sorted.length - 1, Math.ceil(q * sorted.length) - 1)]!;

/** Least-squares slope of y over x. */
const slope = (points: readonly (readonly [number, number])[]) => {
  const n = points.length;
  const mx = points.reduce((sum, [x]) => sum + x, 0) / n;
  const my = points.reduce((sum, [, y]) => sum + y, 0) / n;
  const sxy = points.reduce((sum, [x, y]) => sum + (x - mx) * (y - my), 0);
  const sxx = points.reduce((sum, [x]) => sum + (x - mx) ** 2, 0);
  return sxy / sxx;
};

const withProgram = (
  base: MidgardCekVerdictRequest,
  body: UPLCTerm,
  cpu: bigint,
): MidgardCekVerdictRequest => {
  return {
    ...base,
    scriptBytes: encodeCbor(
      encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], body)),
    ),
    scriptLanguage: "PlutusV3",
    executionBudget: { cpu, memory: 10n ** 12n },
  };
};

const omega = new Lambda(new Application(new UPLCVar(0), new UPLCVar(0)));
const loop = new Lambda(new Application(omega, omega));

/** `\ctx -> (\_ -> ctx) (sha2_256 (... (sha2_256 #00)))`, `depth` deep. */
const hashChain = (depth: number) => {
  let term: UPLCTerm = UPLCConst.byteString(new Uint8Array(32));
  for (let index = 0; index < depth; index += 1)
    term = new Application(
      new Builtin(midgardCekBuiltinTagOfName("sha2_256")),
      term,
    );
  return new Lambda(
    new Application(new Lambda(new UPLCVar(1)), term),
  ) as UPLCTerm;
};

/** `\ctx -> (\_ -> ... ctx) #c1 ... #ck`: `bytes` of unused constants, in
 * 8 KiB constants (a constant's payload is bounded). */
const carrying = (bytes: number) => {
  const sizes: number[] = [];
  for (let left = bytes; left > 0; left -= 8_192)
    sizes.push(Math.min(8_192, left));
  let term: UPLCTerm = new UPLCVar(sizes.length);
  for (const [index, size] of sizes.entries())
    term = new Application(
      new Lambda(term),
      UPLCConst.byteString(new Uint8Array(size).fill(index % 251)),
    );
  return new Lambda(term) as UPLCTerm;
};

describe("import checker kill parameters", () => {
  it("measures the floor and the slopes", async () => {
    const parent = [fundedBy(0x21, "accept")];
    const block = await committedBlock(parent, [
      { input: parent[0]!.outRef, body: "accept", next: "accept" },
    ]);
    const minimal = block.runs[0]![0]!;

    const pool = new FixedValidationWorkerPool(
      2,
      8,
      600_000,
      workerEntry,
      init,
    );
    await pool.start();
    const evaluate = evaluateImportCheckOn(pool);
    const timed = async (run: MidgardCekVerdictRequest) => {
      const startedAt = performance.now();
      const result = await Effect.runPromise(
        evaluate(run, { killMs: 600_000, fresh: false, lane: "import" }),
      );
      return { ms: performance.now() - startedAt, result };
    };
    try {
      for (let index = 0; index < 200; index += 1) await timed(minimal);
      const latencies: number[] = [];
      for (let index = 0; index < MINIMAL_RUNS; index += 1) {
        const { ms, result } = await timed(minimal);
        expect(result.kind).toBe("accepted");
        latencies.push(ms);
      }
      latencies.sort((a, b) => a - b);

      const coverage: {
        family: string;
        cpu: number;
        ms: number;
        killMs: number;
      }[] = [];
      const cpuPoints = async (
        family: string,
        make: (step: number) => UPLCTerm,
        steps: number[],
        cpu: (step: number) => bigint,
      ) => {
        const points: [number, number][] = [];
        for (const step of steps)
          for (let rep = 0; rep < 3; rep += 1) {
            const run = withProgram(minimal, make(step), cpu(step));
            const { ms, result } = await timed(run);
            if (result.kind !== "accepted") throw new Error(result.detail);
            points.push([Number(result.budget.cpu) / 1e9, ms]);
            const killMs = importCheckKillMs(
              {
                ...run,
                executionBudget: { cpu: result.budget.cpu, memory: 10n ** 12n },
              },
              DEFAULT_IMPORT_CHECK_CONFIG,
            );
            coverage.push({
              family,
              cpu: Number(result.budget.cpu),
              ms,
              killMs,
            });
          }
        return points;
      };
      const loopPoints = await cpuPoints(
        "loop",
        () => loop,
        [1e8, 1e9, 3e9, 1e10],
        (step) => BigInt(step),
      );
      const hashPoints = await cpuPoints(
        "hashChain",
        hashChain,
        [100, 400, 1_600, 3_200],
        () => 10n ** 11n,
      );

      const bytePoints: [number, number][] = [];
      for (const size of [1_024, 16_384, 65_536, 131_072])
        for (let rep = 0; rep < 5; rep += 1) {
          const run = withProgram(minimal, carrying(size), 10n ** 10n);
          const { ms, result } = await timed(run);
          if (result.kind !== "accepted") throw new Error(result.detail);
          bytePoints.push([importCheckRunBytes(run) / 1024, ms]);
        }

      const report = {
        minimal: {
          runs: latencies.length,
          bytes: importCheckRunBytes(minimal),
          p50: quantile(latencies, 0.5),
          p99: quantile(latencies, 0.99),
          p999: quantile(latencies, 0.999),
          max: latencies.at(-1),
        },
        msPerGigaCpu: {
          loop: slope(loopPoints),
          hashChain: slope(hashPoints),
          loopPoints,
          hashPoints,
        },
        usPerKib: { slope: slope(bytePoints) * 1000, bytePoints },
        defaultKillCoverage: {
          min: Math.min(...coverage.map((point) => point.killMs / point.ms)),
          points: coverage.map((point) => ({
            ...point,
            ratio: point.killMs / point.ms,
          })),
        },
      };
      console.log(JSON.stringify(report, null, 2));
    } finally {
      await pool.close();
    }
  });
});
