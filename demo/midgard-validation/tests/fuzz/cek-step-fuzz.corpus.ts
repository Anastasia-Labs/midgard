import { readFileSync, writeFileSync } from "node:fs";

import { dataFromCbor } from "@harmoniclabs/plutus-data";
import { parseUPLC, UPLCProgram } from "@harmoniclabs/uplc";

import { encodeMidgardCekPlutusData } from "../../src/cek-constant.js";
import { encodeMidgardCekCardanoFlatProgram } from "../../src/cek-program.cardano-flat.js";
import type { CaseResult } from "./cek-step-fuzz.harness.js";
import type { CekStepFuzzProgram } from "./cek-step-fuzz.programs.js";

/**
 * The checked-in vectors of the step differential, and its per-builtin
 * coverage report. A vector holds the program and context bytes themselves,
 * not the seed that generated them, so it still replays the same program
 * after the generator changes.
 */

export type CekStepFuzzVector = {
  readonly label: string;
  /** The script as `(program 1.1.0 term)` Flat, hex. */
  readonly programFlat: string;
  /** The script context's Plutus Data CBOR, hex. */
  readonly contextCbor: string;
  readonly tightBudget: boolean;
  /**
   * The seed of the step sample that found the vector; absent, the vector's
   * index in the corpus.
   */
  readonly sampleSeed?: number;
};

const CORPUS_URL = new URL("./cek-step-fuzz.corpus.json", import.meta.url);
const COVERAGE_URL = new URL("./cek-step-fuzz.coverage.json", import.meta.url);

const writeJson = (url: URL, value: unknown): void =>
  writeFileSync(url, `${JSON.stringify(value, null, 2)}\n`, "utf8");

export const readCorpus = (): readonly CekStepFuzzVector[] =>
  (
    JSON.parse(readFileSync(CORPUS_URL, "utf8")) as {
      readonly vectors: readonly CekStepFuzzVector[];
    }
  ).vectors;

/** Appends `vectors` to the checked-in corpus. */
export const recordCorpusVectors = (
  vectors: readonly CekStepFuzzVector[],
): void =>
  writeJson(CORPUS_URL, {
    ...(JSON.parse(readFileSync(CORPUS_URL, "utf8")) as object),
    vectors: [...readCorpus(), ...vectors],
  });

export const vectorOf = (
  program: CekStepFuzzProgram,
  sampleSeed: number,
): CekStepFuzzVector => ({
  label: program.label,
  programFlat: Buffer.from(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram([1, 1, 0], program.term),
    ),
  ).toString("hex"),
  contextCbor: Buffer.from(
    encodeMidgardCekPlutusData(program.context),
  ).toString("hex"),
  tightBudget: program.tightBudget,
  sampleSeed,
});

export const programOf = (vector: CekStepFuzzVector): CekStepFuzzProgram => ({
  label: vector.label,
  term: parseUPLC(Buffer.from(vector.programFlat, "hex"), "flat").body,
  context: dataFromCbor(vector.contextCbor),
  tightBudget: vector.tightBudget,
});

/**
 * Per builtin, the builtin-executing arms on which the two machines agreed;
 * and the machine arms on which they agreed at least once. Only genuine
 * steps count, never a negative twist.
 */
export type CekStepFuzzCoverage = {
  readonly builtins: Readonly<Record<string, readonly string[]>>;
  readonly arms: readonly string[];
};

export const coverageOf = (
  results: readonly CaseResult[],
): CekStepFuzzCoverage => {
  const builtins = new Map<number, Set<string>>();
  const arms = new Set<string>();
  for (const result of results) {
    for (const step of result.steps) {
      const agreed =
        step.twist === null &&
        step.verdict === "accepted" &&
        step.aikenPostHash === step.postHash;
      if (!agreed) continue;
      arms.add(step.kind);
      if (step.builtin === null) continue;
      const kinds = builtins.get(step.builtin) ?? new Set<string>();
      kinds.add(step.kind);
      builtins.set(step.builtin, kinds);
    }
  }
  return {
    builtins: Object.fromEntries(
      [...builtins.entries()]
        .sort(([left], [right]) => left - right)
        .map(([tag, kinds]) => [tag.toString(), [...kinds].sort()]),
    ),
    arms: [...arms].sort(),
  };
};

type CoverageReport = {
  readonly note: string;
  readonly seedSet: string;
  readonly coverage: CekStepFuzzCoverage;
};

export const readCoverageReport = (): CoverageReport =>
  JSON.parse(readFileSync(COVERAGE_URL, "utf8")) as CoverageReport;

export const writeCoverageReport = (report: CoverageReport): void =>
  writeJson(COVERAGE_URL, report);
