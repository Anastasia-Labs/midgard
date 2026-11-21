import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { aikenBinary } from "../../../scripts/lib/aiken.mjs";
import {
  exportAikenFunctions,
  STEP_EVALUATOR_EXPORTS,
} from "../aiken-export-cache.js";
import {
  app,
  applyExportedProgram,
  dataConstant,
  type FlatTerm,
  listOf,
  v,
} from "./uplc-flat.js";

/**
 * The Aiken function evaluator: the pinned compiler's own evaluator running
 * functions exported from the on-chain project as UPLC.
 *
 * The step evaluator runs the on-chain machine's step adjudicator,
 * `cek_machine_v1.verify_core_step_v1`, and its state hash,
 * `cek_machine_v1.hash_state_v1`. Many steps are adjudicated in one
 * evaluation: the exported function is applied to every step's arguments and
 * the results are collected into one list. Exports are cached by their
 * compiler inputs (`aiken-export-cache.js`), which records what one costs.
 */

/** The aiken `demo/flake.lock` pins, which every evaluation runs. */
export { aikenBinary };

const MAX_OUTPUT = 512 * 1024 * 1024;

const aiken = (args: readonly string[], cwd: string): string => {
  const run = spawnSync(aikenBinary(), [...args], {
    cwd,
    encoding: "utf8",
    maxBuffer: MAX_OUTPUT,
  });
  if (run.error !== undefined) throw run.error;
  if (run.status !== 0) {
    throw new AikenEvaluationFailed(
      `aiken ${args[0] ?? ""} ${args[1] ?? ""} exited ${String(run.status)}`,
      `${run.stdout ?? ""}${run.stderr ?? ""}`,
    );
  }
  return run.stdout;
};

export class AikenEvaluationFailed extends Error {
  constructor(
    message: string,
    readonly output: string,
  ) {
    super(message);
  }
}

type EvaluatedList =
  | { readonly kind: "list"; readonly items: readonly string[] }
  | { readonly kind: "failed"; readonly output: string };

/** The comma-separated items of a `(con (list t) [...])` result. */
const listItems = (result: string): readonly string[] | undefined => {
  const open = result.indexOf("[");
  const close = result.lastIndexOf("]");
  if (!result.startsWith("(con") || open < 0 || close < open) return undefined;
  const inner = result.slice(open + 1, close).trim();
  return inner === "" ? [] : inner.split(",").map((item) => item.trim());
};

export type AikenStepEvaluator = {
  /** `verify_core_step_v1(pre, post, witness)` for each step, in order. */
  readonly verify: (
    steps: readonly AikenStepArguments[],
  ) => readonly AikenVerdict[];
  /** `hash_state_v1(state)` for each state, in order, as hex. */
  readonly hash: (states: readonly Uint8Array[]) => readonly (string | null)[];
  readonly close: () => void;
};

/** One step's arguments, each the CBOR of its `CoreStepEvidenceV1` field. */
export type AikenStepArguments = {
  readonly pre: Uint8Array;
  readonly post: Uint8Array;
  readonly witness: Uint8Array;
};

/**
 * The adjudicator returned True, returned False, went over the evaluator's
 * budget (the Cardano per-transaction maximum), or failed otherwise.
 */
export type AikenVerdict = "accepted" | "refused" | "overBudget" | "failed";

/**
 * The evaluator's report of a run stopped by its budget. The CLI exits 1 for
 * every failure and tells a budget stop apart only in this line.
 */
const OVER_BUDGET_REPORT = "execution went over budget";

/**
 * Items per evaluation. The pinned evaluator's start-up (reading the 75 KB
 * adjudicator) dominates a small batch; a large one holds every argument in
 * one program.
 */
const BATCH_SIZE = 128;

const inBatches = <A, B>(
  items: readonly A[],
  run: (batch: readonly A[]) => readonly B[],
): readonly B[] => {
  const results: B[] = [];
  for (let start = 0; start < items.length; start += BATCH_SIZE) {
    results.push(...run(items.slice(start, start + BATCH_SIZE)));
  }
  return results;
};

/**
 * Builds the step evaluator: exports both functions, then evaluates batches.
 * On a cold cache, or while another process compiles the same exports, it
 * waits for two project compiles, so call it from a hook sized for them on a
 * loaded runner.
 */
export const makeAikenStepEvaluator = async (): Promise<AikenStepEvaluator> => {
  const [verifyFlat, hashFlat] = await exportAikenFunctions(
    STEP_EVALUATOR_EXPORTS,
  );
  const directory = mkdtempSync(join(tmpdir(), "midgard-aiken-evaluator-"));
  let counter = 0;

  const evaluate = (
    exported: Uint8Array,
    items: readonly FlatTerm[],
    element: "bool" | "bytestring",
  ): EvaluatedList => {
    counter += 1;
    const file = join(directory, `batch-${counter.toString()}.flat`);
    writeFileSync(file, applyExportedProgram(exported, listOf(items, element)));
    try {
      const output = aiken(["uplc", "eval", "--flat", file], directory);
      const parsed = JSON.parse(output) as { readonly result?: unknown };
      const items =
        typeof parsed.result === "string"
          ? listItems(parsed.result)
          : undefined;
      return items === undefined
        ? { kind: "failed", output }
        : { kind: "list", items };
    } catch (error) {
      if (error instanceof AikenEvaluationFailed) {
        return { kind: "failed", output: error.output };
      }
      throw error;
    } finally {
      rmSync(file, { force: true });
    }
  };

  const verifyItem = (step: AikenStepArguments): FlatTerm =>
    app(
      v(1),
      dataConstant(step.pre),
      dataConstant(step.post),
      dataConstant(step.witness),
    );

  const verdictOf = (item: string): AikenVerdict =>
    item === "True" ? "accepted" : item === "False" ? "refused" : "failed";

  const verifyBatch = (
    steps: readonly AikenStepArguments[],
  ): readonly AikenVerdict[] => {
    if (steps.length === 0) return [];
    const batch = evaluate(verifyFlat, steps.map(verifyItem), "bool");
    if (batch.kind === "list" && batch.items.length === steps.length) {
      return batch.items.map(verdictOf);
    }
    // One failing step fails the whole list; split it to find that step.
    if (steps.length === 1) {
      return [
        batch.kind === "failed" && batch.output.includes(OVER_BUDGET_REPORT)
          ? "overBudget"
          : "failed",
      ];
    }
    const half = Math.ceil(steps.length / 2);
    return [
      ...verifyBatch(steps.slice(0, half)),
      ...verifyBatch(steps.slice(half)),
    ];
  };

  const hashBatch = (
    states: readonly Uint8Array[],
  ): readonly (string | null)[] => {
    if (states.length === 0) return [];
    const batch = evaluate(
      hashFlat,
      states.map((state) => app(v(1), dataConstant(state))),
      "bytestring",
    );
    if (batch.kind === "list" && batch.items.length === states.length) {
      return batch.items.map((item) =>
        item.startsWith("#") ? item.slice(1) : null,
      );
    }
    if (states.length === 1) return [null];
    const half = Math.ceil(states.length / 2);
    return [
      ...hashBatch(states.slice(0, half)),
      ...hashBatch(states.slice(half)),
    ];
  };

  return {
    verify: (steps) => inBatches(steps, verifyBatch),
    hash: (states) => inBatches(states, hashBatch),
    close: () => rmSync(directory, { recursive: true, force: true }),
  };
};
