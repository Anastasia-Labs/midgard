import { type MidgardCekConstantWitness } from "../../src/cek-constant.js";
import { type MidgardCekExecutionStep } from "../../src/cek-executor.js";
import { plutusDataFromCborIterative } from "../../src/plutus-data-iterative.decode.js";
import { DataI, DataList } from "../../src/uplc-data.js";
import type { AikenVerdict } from "./cek-step-fuzz.oracle.js";

/**
 * The classes of genuine step that the on-chain machine does not adjudicate
 * today. A genuine step the Aiken machine does not accept is a divergence
 * unless it falls in one of these classes. Nothing here refuses a
 * transaction: admission stays Cardano-exact, and these classes only say which
 * steps a dispute cannot prove. Each class names its owner and the fix that
 * removes it; the class goes when that fix composes. The CI rows pin every
 * catalogued step to its vector (`cek-step-fuzz.pins.ts`).
 *
 * - `computationCost`: a step of a computation-cost kind of §P3, or a
 *   large-Data step, whose adjudication goes over the evaluator's budget.
 *   Owner: the §P3 computation-cost rows and E-N6 (ii). Pending the E-N6 (ii)
 *   cost update for the large-Data steps.
 * - `nestedConstantCost`: a step revealing a constant whose type nests one
 *   compound in another (a pair, or a list of lists), whose adjudication goes
 *   over the evaluator's budget. Owner: the on-chain constant step. Pending
 *   the on-chain constant-step cost update.
 * - `serialiseLoop`: a serialiseData step. The serialise loop
 *   (`cek_core_semantic_serialise_loop`) proves it hop by hop over its staged
 *   result, which the one-step adjudicator run here does not read. Its
 *   agreement with the TypeScript verifier is pinned by the loop's golden
 *   channel (`scripts/generate-cek-serialise-loop-v1-goldens.mjs`) instead.
 *   Owner: Cap 2. Stays while this differential runs one step at a time.
 */
export type CataloguedClass =
  | "computationCost"
  | "nestedConstantCost"
  | "serialiseLoop";

type StepWitness = MidgardCekExecutionStep["witness"];

/** The constant payloads a step reveals in full. */
const revealedConstants = (
  witness: StepWitness,
): MidgardCekConstantWitness[] => {
  const values = [
    ...("arguments" in witness ? witness.arguments : []),
    ...("result" in witness ? [witness.result] : []),
  ];
  return values.flatMap((value) =>
    value.kind === "constant" ? [value.witness] : [],
  );
};

/** Plutus constant type tags, as a constant witness's type lists them. */
const LIST_TYPE_TAG = 5n;
const PAIR_TYPE_TAG = 6n;

/** Whether a constant's type nests one compound in another. */
const typeIsNested = (typeCbor: Uint8Array): boolean => {
  const type = plutusDataFromCborIterative(typeCbor);
  if (!(type instanceof DataList)) return false;
  const tags = type.list.flatMap((tag) =>
    tag instanceof DataI ? [tag.int] : [],
  );
  return (
    tags.includes(PAIR_TYPE_TAG) ||
    tags.filter((tag) => tag === LIST_TYPE_TAG).length > 1
  );
};

const revealsNestedConstant = (witness: StepWitness): boolean =>
  revealedConstants(witness).some(({ typeCbor }) => typeIsNested(typeCbor));

/**
 * The builtins, as this tree numbers them, of the computation-cost rows of
 * §P3 and of the large-Data steps that wait on E-N6 (ii): multiplyInteger (2),
 * appendString (22), bData (41), unBData (46), serialiseData (51), final
 * verification (70), shiftByteString (82) and rotateByteString (83). The rows
 * for tags above 86 have no executor here yet.
 */
const COMPUTATION_COST_BUILTINS = new Set([2, 22, 41, 46, 51, 70, 82, 83]);
/**
 * The constant-revealing arms, whose cost grows with a large Data value. A
 * `computeConstant` step names only its value root, so its cost does not.
 */
const CONSTANT_REVEALING_KINDS = new Set<StepWitness["kind"]>([
  "computeContextConstant",
]);

const isComputationCostKind = (
  witness: StepWitness,
  builtin: number | null,
): boolean =>
  CONSTANT_REVEALING_KINDS.has(witness.kind) ||
  (builtin !== null && COMPUTATION_COST_BUILTINS.has(builtin));

/**
 * The catalogued class of a genuine step the Aiken machine did not accept, or
 * null when the step is in no class and its refusal is a divergence.
 */
export const cataloguedClassOf = (
  step: MidgardCekExecutionStep,
  builtin: number | null,
  verdict: AikenVerdict,
): CataloguedClass | null => {
  if (verdict === "accepted") return null;
  const witness = step.witness;
  if (witness.kind === "executeBuiltinSerialise") return "serialiseLoop";
  if (verdict === "overBudget" && isComputationCostKind(witness, builtin)) {
    return "computationCost";
  }
  if (verdict === "overBudget" && revealsNestedConstant(witness)) {
    return "nestedConstantCost";
  }
  return null;
};
