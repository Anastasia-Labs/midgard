import { DataB, DataI, DataList } from "@harmoniclabs/plutus-data";

import { type MidgardCekDirectValueWitness } from "../../src/cek-builtin.argument-kinds.js";
import {
  decodeMidgardCekConstantWitness,
  type MidgardCekConstantWitness,
} from "../../src/cek-constant.js";
import { type MidgardCekExecutionStep } from "../../src/cek-executor.js";
import { plutusDataFromCborIterative } from "../../src/plutus-data-iterative.decode.js";
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
 * - `blsInvalidPointPending`: a G1 or G2 uncompress failure outside the
 *   on-chain failure proof (for G1, not a wrong length, an invalid header or
 *   a point off the curve; for G2, not a wrong length or an invalid header).
 *   Owner: E-N8 (c). Pending the remaining BLS failure arms.
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
  | "blsInvalidPointPending"
  | "computationCost"
  | "nestedConstantCost"
  | "serialiseLoop";

type StepWitness = MidgardCekExecutionStep["witness"];

/** The constant payloads a step reveals in full. */
const revealedConstants = (
  witness: StepWitness,
): MidgardCekConstantWitness[] => {
  if (witness.kind === "computeConstant") return [witness.value];
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

// Cardano's flat tags for G1 and G2 uncompress.
const G1_UNCOMPRESS_TAG = 59;
const G2_UNCOMPRESS_TAG = 66;
const BLS12_381_FIELD_PRIME =
  0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaabn;

const bigEndian = (bytes: Uint8Array): bigint =>
  bytes.reduce((value, byte) => (value << 8n) | BigInt(byte), 0n);

const modularPower = (base: bigint, exponent: bigint, modulus: bigint) => {
  let result = 1n;
  let square = base % modulus;
  for (let rest = exponent; rest > 0n; rest >>= 1n) {
    if ((rest & 1n) === 1n) result = (result * square) % modulus;
    square = (square * square) % modulus;
  }
  return result;
};

/** Whether a compressed encoding's flags or coordinates are invalid. */
const blsHeaderIsInvalid = (encoded: Uint8Array): boolean => {
  const first = encoded[0] ?? 0;
  const firstCoordinate = bigEndian(
    Uint8Array.of(first % 32, ...encoded.subarray(1, 48)),
  );
  const secondCoordinate = bigEndian(encoded.subarray(48, 96));
  if (first < 128) return true;
  if (first % 128 >= 64) {
    return (
      first % 64 >= 32 || firstCoordinate !== 0n || secondCoordinate !== 0n
    );
  }
  return (
    firstCoordinate >= BLS12_381_FIELD_PRIME ||
    secondCoordinate >= BLS12_381_FIELD_PRIME
  );
};

/** Whether a G1 encoding's x has no point on the curve y^2 = x^3 + 4. */
const blsG1IsOffCurve = (encoded: Uint8Array): boolean => {
  const first = encoded[0] ?? 0;
  if (first % 128 >= 64) return false;
  const x = bigEndian(Uint8Array.of(first % 32, ...encoded.subarray(1, 48)));
  const ySquared = (x * x * x + 4n) % BLS12_381_FIELD_PRIME;
  return (
    ySquared !== 0n &&
    modularPower(
      ySquared,
      (BLS12_381_FIELD_PRIME - 1n) / 2n,
      BLS12_381_FIELD_PRIME,
    ) !== 1n
  );
};

/** Whether the on-chain failure proof covers an uncompress failure. */
const uncompressFailureIsProved = (
  tag: number,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const [argument] = arguments_;
  if (arguments_.length !== 1 || argument?.kind !== "constant") return false;
  const { payload } = decodeMidgardCekConstantWitness(argument.witness);
  if (!(payload instanceof DataB)) return false;
  const encoded = payload.bytes;
  if (tag === G1_UNCOMPRESS_TAG) {
    return (
      encoded.length !== 48 ||
      blsHeaderIsInvalid(encoded) ||
      blsG1IsOffCurve(encoded)
    );
  }
  return encoded.length !== 96 || blsHeaderIsInvalid(encoded);
};

const isPendingBlsFailure = (witness: StepWitness): boolean => {
  if (witness.kind !== "executeBuiltinFailure") return false;
  const tag = Number(witness.tag);
  return (
    (tag === G1_UNCOMPRESS_TAG || tag === G2_UNCOMPRESS_TAG) &&
    !uncompressFailureIsProved(tag, witness.arguments)
  );
};

/**
 * The builtins, as this tree numbers them, of the computation-cost rows of
 * §P3 and of the large-Data steps that wait on E-N6 (ii): multiplyInteger (2),
 * appendString (22), bData (41), unBData (46), serialiseData (51), final
 * verification (70), shiftByteString (82) and rotateByteString (83). The rows
 * for tags above 86 have no executor here yet.
 */
const COMPUTATION_COST_BUILTINS = new Set([2, 22, 41, 46, 51, 70, 82, 83]);
/** The constant-revealing arms, whose cost grows with a large Data value. */
const CONSTANT_REVEALING_KINDS = new Set<StepWitness["kind"]>([
  "computeConstant",
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
  if (isPendingBlsFailure(witness)) return "blsInvalidPointPending";
  if (verdict === "overBudget" && isComputationCostKind(witness, builtin)) {
    return "computationCost";
  }
  if (verdict === "overBudget" && revealsNestedConstant(witness)) {
    return "nestedConstantCost";
  }
  return null;
};
