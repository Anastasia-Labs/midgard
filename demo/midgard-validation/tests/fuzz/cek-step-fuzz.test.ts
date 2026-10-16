import { randomInt } from "node:crypto";

import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  coverageOf,
  programOf,
  readCorpus,
  readCoverageReport,
  recordCorpusVectors,
  vectorOf,
  writeCoverageReport,
} from "./cek-step-fuzz.corpus.js";
import {
  adjudicateCases,
  type CaseResult,
  prepareCase,
} from "./cek-step-fuzz.harness.js";
import {
  type AikenStepOracle,
  makeAikenStepOracle,
} from "./cek-step-fuzz.oracle.js";
import {
  type ClassCounts,
  STEP_CORPUS_PINS,
  STEP_FIXED_SET_PINS,
} from "./cek-step-fuzz.pins.js";
import {
  type CekStepFuzzProgram,
  cekStepFuzzProgram,
  FUZZ_PRIMARY_TAGS,
} from "./cek-step-fuzz.programs.js";

/**
 * The step differential: for generated UPLC programs over every builtin, the
 * TypeScript trace builder's steps are adjudicated by the on-chain machine,
 * `cek_machine_v1.verify_core_step_v1` under the pinned Aiken evaluator, on
 * the same pre-state and witness, and the post-state hashes are compared
 * with `cek_machine_v1.hash_state_v1`. A perturbed step must be refused by
 * both. Any disagreement is a divergence, except a genuine step in a
 * catalogued class (`cek-step-fuzz.catalogue.ts`) that the Aiken machine
 * does not accept; each run logs how many steps each class took.
 *
 * - CI: the fixed seed set and the checked-in corpus, with zero divergences.
 *   Each catalogued step is pinned to its vector: the steps of each class per
 *   vector, and per class over the row, must equal `cek-step-fuzz.pins.ts`.
 *   The per-builtin coverage of the fixed set must equal the checked-in
 *   report. `MIDGARD_WRITE_CEK_STEP_FUZZ_COVERAGE=1` rewrites the report.
 * - Release: `MIDGARD_CEK_STEP_FUZZ_SECONDS=<n>` adds a random run of about
 *   n seconds. `MIDGARD_CEK_STEP_FUZZ_RECORD=1` appends every diverging
 *   program to the corpus.
 */

const SEEDS_PER_TAG = 6;
const SEED_SET = `0xf022_0000 + tag * 16 + k, k < ${SEEDS_PER_TAG.toString()}`;
const fixedSeed = (tag: number, k: number): number =>
  0xf022_0000 + tag * 16 + k;

/** Cases prepared and adjudicated together, to batch the evaluator. */
const CHUNK = 24;

const releaseSeconds = Number(process.env.MIDGARD_CEK_STEP_FUZZ_SECONDS ?? "0");
const recordDivergences = process.env.MIDGARD_CEK_STEP_FUZZ_RECORD === "1";

/** A run's case and step counts, and its steps per catalogued class. */
type RunSummary = {
  cases: number;
  steps: number;
  readonly catalogued: Record<string, number>;
};

const emptySummary = (): RunSummary => ({ cases: 0, steps: 0, catalogued: {} });

const addToSummary = (
  summary: RunSummary,
  results: readonly CaseResult[],
): RunSummary => {
  for (const result of results) {
    summary.cases += 1;
    summary.steps += result.steps.length;
    for (const { catalogued } of result.steps) {
      if (catalogued === null) continue;
      summary.catalogued[catalogued] =
        (summary.catalogued[catalogued] ?? 0) + 1;
    }
  }
  return summary;
};

const logRun = (name: string, summary: RunSummary): void =>
  console.info(
    `cek step fuzz ${name}: ${summary.cases.toString()} cases, ${summary.steps.toString()} adjudicated steps, catalogued ${JSON.stringify(summary.catalogued)}`,
  );

/** Per vector label, its steps in each catalogued class; vectors with any. */
const cataloguedByVector = (
  results: readonly CaseResult[],
): Record<string, ClassCounts> =>
  Object.fromEntries(
    results.flatMap((result) => {
      const counts: Record<string, number> = {};
      for (const { catalogued } of result.steps) {
        if (catalogued !== null)
          counts[catalogued] = (counts[catalogued] ?? 0) + 1;
      }
      return Object.keys(counts).length === 0
        ? []
        : [[result.prepared.program.label, counts]];
    }),
  );

const describeDivergences = (results: readonly CaseResult[]): string[] =>
  results.flatMap((result) =>
    result.divergences.map(
      (detail) => `${result.prepared.program.label}: ${detail}`,
    ),
  );

describe("CEK step differential against the Aiken machine", () => {
  let oracle: AikenStepOracle | undefined;

  beforeAll(() => {
    oracle = makeAikenStepOracle();
  }, 180_000);

  afterAll(() => oracle?.close());

  const run = (
    programs: readonly { program: CekStepFuzzProgram; sampleSeed: number }[],
  ): readonly CaseResult[] => {
    const results: CaseResult[] = [];
    for (let start = 0; start < programs.length; start += CHUNK) {
      const prepared = programs
        .slice(start, start + CHUNK)
        .map(({ program, sampleSeed }) => prepareCase(program, sampleSeed));
      results.push(...adjudicateCases(oracle!, prepared));
    }
    if (recordDivergences) {
      recordCorpusVectors(
        results
          .filter((result) => result.divergences.length > 0)
          .map((result) =>
            vectorOf(result.prepared.program, result.prepared.sampleSeed),
          ),
      );
    }
    return results;
  };

  it(
    "agrees on every sampled step of the fixed seed set",
    { timeout: 1_800_000 },
    () => {
      const results = run(
        FUZZ_PRIMARY_TAGS.flatMap((tag) =>
          Array.from({ length: SEEDS_PER_TAG }, (_, k) => ({
            program: cekStepFuzzProgram(fixedSeed(tag, k), tag),
            sampleSeed: fixedSeed(tag, k),
          })),
        ),
      );
      const summary = addToSummary(emptySummary(), results);
      logRun("fixed seed set", summary);
      const coverage = coverageOf(results);
      if (process.env.MIDGARD_WRITE_CEK_STEP_FUZZ_COVERAGE === "1") {
        writeCoverageReport({
          note: "Written by tests/fuzz/cek-step-fuzz.test.ts with MIDGARD_WRITE_CEK_STEP_FUZZ_COVERAGE=1. Per builtin tag, the builtin-executing arms on which the TypeScript builder and the Aiken machine agreed over the fixed seed set; and every machine arm they agreed on.",
          seedSet: SEED_SET,
          coverage,
        });
      }
      expect(describeDivergences(results)).toStrictEqual([]);
      expect(cataloguedByVector(results)).toStrictEqual(
        STEP_FIXED_SET_PINS.vectors,
      );
      expect(summary.catalogued).toStrictEqual(STEP_FIXED_SET_PINS.totals);
      expect(readCoverageReport()).toStrictEqual(
        expect.objectContaining({ seedSet: SEED_SET, coverage }),
      );
    },
  );

  it("agrees on every corpus vector", { timeout: 600_000 }, () => {
    const corpus = readCorpus();
    expect(corpus.length).toBeGreaterThan(0);
    const results = run(
      corpus.map((vector, index) => ({
        program: programOf(vector),
        sampleSeed: vector.sampleSeed ?? index,
      })),
    );
    const summary = addToSummary(emptySummary(), results);
    logRun("corpus", summary);
    expect(describeDivergences(results)).toStrictEqual([]);
    expect(cataloguedByVector(results)).toStrictEqual(STEP_CORPUS_PINS.vectors);
    expect(summary.catalogued).toStrictEqual(STEP_CORPUS_PINS.totals);
    for (const result of results) {
      expect(result.prepared.totalSteps).toBeGreaterThan(0);
    }
  });

  it.runIf(releaseSeconds > 0)(
    "agrees over a time-boxed random run",
    { timeout: (releaseSeconds + 600) * 1000 },
    () => {
      const deadline = Date.now() + releaseSeconds * 1000;
      const divergences: string[] = [];
      const summary = emptySummary();
      while (Date.now() < deadline) {
        const results = run(
          Array.from({ length: CHUNK }, () => {
            const seed = randomInt(2 ** 31);
            const tag = FUZZ_PRIMARY_TAGS[randomInt(FUZZ_PRIMARY_TAGS.length)]!;
            return { program: cekStepFuzzProgram(seed, tag), sampleSeed: seed };
          }),
        );
        addToSummary(summary, results);
        divergences.push(...describeDivergences(results));
      }
      logRun("random run", summary);
      expect(divergences).toStrictEqual([]);
    },
  );
});
