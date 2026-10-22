import { randomInt } from "node:crypto";

import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { adjudicateCases } from "./cek-step-fuzz.harness.js";
import {
  type AikenStepOracle,
  makeAikenStepOracle,
} from "./cek-step-fuzz.oracle.js";
import { runTxCase, type TxCaseResult } from "./tx-fuzz.legs.js";
import {
  pinsWithoutReferenceLeg,
  TX_CORPUS_PINS,
  TX_FIXED_SET_PINS,
  type TxRowPins,
} from "./tx-fuzz.pins.js";
import {
  makeReference,
  type Reference,
  referenceBinary,
} from "./tx-fuzz.reference.js";
import { txFuzzScenario } from "./tx-fuzz.scenario.js";

/**
 * The transaction differential: generated PlutusV3 spends over generated
 * ledger fields, each through the verdict leg, the full trace builder in
 * both directions, the cost check across modes and, when
 * `MIDGARD_TX_FUZZ_UPLC_BIN` names an evaluator (`pinned`: the pinned
 * aiken), the reference leg (`tx-fuzz.legs.ts`). The proof run's sampled steps go to the on-chain
 * machine through the step oracle, as in the step differential.
 *
 * - CI: the fixed seed set and the corpus seeds, with zero divergences. Each
 *   known disagreement and catalogued step is pinned to its seed: the counts
 *   of each class per seed, and per class over the row, must equal
 *   `tx-fuzz.pins.ts` (less the reference leg's classes when it is off).
 *   The package suite runs it with the leg off; the `tx-fuzz-reference`
 *   check runs it again with the leg on, against the pinned aiken.
 * - Release: `MIDGARD_TX_FUZZ_SECONDS=<n>` adds a random run of about n
 *   seconds.
 */

const FIXED_SEEDS = 64;
const fixedSeed = (index: number): number => 0xe3_0000 + index;

/**
 * Seeds found by random runs, each kept as a regression row; their classes
 * are pinned in `tx-fuzz.pins.ts`.
 */
const CORPUS_SEEDS = [
  330_470_803, 590_911_258, 1_479_182_247, 1_405_241_274, 322_290_577,
  1_088_032_817, 345_237_037,
] as const;
/** Cases whose sampled steps are adjudicated together. */
const CHUNK = 16;

const releaseSeconds = Number(process.env.MIDGARD_TX_FUZZ_SECONDS ?? "0");

type TxRunSummary = {
  cases: number;
  referenced: number;
  adjudicatedSteps: number;
  readonly verdicts: Record<string, number>;
  readonly known: Record<string, number>;
  readonly catalogued: Record<string, number>;
  readonly divergences: string[];
  /** Per seed, its known disagreements and catalogued steps, by class. */
  readonly perSeed: Record<string, SeedCounts>;
};

type SeedCounts = {
  known?: Record<string, number>;
  catalogued?: Record<string, number>;
};

const emptySummary = (): TxRunSummary => ({
  cases: 0,
  referenced: 0,
  adjudicatedSteps: 0,
  verdicts: {},
  known: {},
  catalogued: {},
  divergences: [],
  perSeed: {},
});

const bump = (counts: Record<string, number>, key: string): void => {
  counts[key] = (counts[key] ?? 0) + 1;
};

const bumpSeed = (
  summary: TxRunSummary,
  seed: number,
  part: keyof SeedCounts,
  key: string,
): void => {
  const counts = (summary.perSeed[seed.toString()] ??= {});
  bump((counts[part] ??= {}), key);
};

describe("transaction differential", () => {
  let oracle: AikenStepOracle | undefined;
  let reference: Reference | undefined;

  // The oracle compiles the on-chain project twice per run, shared by every
  // file that uses it (about 45 s on a developer machine, a few minutes on a
  // loaded CI runner); a file that starts meanwhile waits for it.
  beforeAll(async () => {
    oracle = await makeAikenStepOracle();
    const binary = referenceBinary();
    reference = binary === undefined ? undefined : makeReference(binary);
  }, 300_000);

  afterAll(() => {
    oracle?.close();
    reference?.close();
  });

  const run = async (
    seeds: readonly number[],
    summary: TxRunSummary,
  ): Promise<void> => {
    for (let start = 0; start < seeds.length; start += CHUNK) {
      const results: TxCaseResult[] = [];
      for (const seed of seeds.slice(start, start + CHUNK)) {
        results.push(
          await runTxCase(await txFuzzScenario(seed), seed, reference),
        );
      }
      const prepared = results.flatMap((result) =>
        result.steps === null ? [] : [result.steps],
      );
      for (const adjudicated of adjudicateCases(oracle!, prepared)) {
        summary.adjudicatedSteps += adjudicated.steps.length;
        for (const { catalogued } of adjudicated.steps) {
          if (catalogued === null) continue;
          bump(summary.catalogued, catalogued);
          bumpSeed(
            summary,
            adjudicated.prepared.sampleSeed,
            "catalogued",
            catalogued,
          );
        }
        summary.divergences.push(
          ...adjudicated.divergences.map(
            (detail) => `${adjudicated.prepared.program.label}: step ${detail}`,
          ),
        );
      }
      results.forEach((result, index) => {
        const seed = seeds[start + index]!;
        summary.cases += 1;
        if (result.referenced) summary.referenced += 1;
        bump(
          summary.verdicts,
          `${result.verdict}${result.rejectionCode === null ? "" : `/${result.rejectionCode}`}`,
        );
        for (const known of result.known) {
          bump(summary.known, known);
          bumpSeed(summary, seed, "known", known);
        }
        summary.divergences.push(
          ...result.divergences.map((detail) => `${result.label}: ${detail}`),
        );
      });
    }
  };

  const log = (name: string, summary: TxRunSummary): void =>
    console.info(
      `tx fuzz ${name}: ${JSON.stringify({ ...summary, divergences: summary.divergences.length, perSeed: undefined })}`,
    );

  /**
   * The row's pins, less the reference leg's classes when it is off. A row
   * run with the leg configured must have referenced some spend: otherwise a
   * leg that never ran would match the leg-off pins.
   */
  const expectPinned = (summary: TxRunSummary, pins: TxRowPins): void => {
    if (process.env.MIDGARD_TX_FUZZ_UPLC_BIN !== undefined) {
      expect(summary.referenced).toBeGreaterThan(0);
    }
    const expected =
      reference === undefined ? pinsWithoutReferenceLeg(pins) : pins;
    expect(summary.perSeed).toStrictEqual(expected.seeds);
    expect(summary.known).toStrictEqual(expected.known);
    expect(summary.catalogued).toStrictEqual(expected.catalogued);
  };

  it(
    "agrees on every leg of the fixed seed set",
    { timeout: 1_800_000 },
    async () => {
      const summary = emptySummary();
      await run(
        Array.from({ length: FIXED_SEEDS }, (_, index) => fixedSeed(index)),
        summary,
      );
      log("fixed seed set", summary);
      expect(summary.divergences).toStrictEqual([]);
      expectPinned(summary, TX_FIXED_SET_PINS);
      expect(Object.keys(summary.verdicts).length).toBeGreaterThan(1);
    },
  );

  it(
    "agrees on every leg of the corpus seeds",
    { timeout: 600_000 },
    async () => {
      const summary = emptySummary();
      await run(CORPUS_SEEDS, summary);
      log("corpus seeds", summary);
      expect(summary.divergences).toStrictEqual([]);
      expectPinned(summary, TX_CORPUS_PINS);
    },
  );

  it.runIf(releaseSeconds > 0)(
    "agrees over a time-boxed random run",
    { timeout: (releaseSeconds + 600) * 1000 },
    async () => {
      const deadline = Date.now() + releaseSeconds * 1000;
      const summary = emptySummary();
      while (Date.now() < deadline) {
        await run(
          Array.from({ length: CHUNK }, () => randomInt(2 ** 31)),
          summary,
        );
      }
      log("random run", summary);
      expect(summary.divergences).toStrictEqual([]);
    },
  );
});
