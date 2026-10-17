import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";

import { type MidgardCekDirectValueWitness } from "../../src/cek-builtin.argument-kinds.js";
import {
  decodeMidgardCekConstantWitness,
  type MidgardCekConstantWitness,
} from "../../src/cek-constant.js";
import { type MidgardCekExecutionStep } from "../../src/cek-executor.js";
import { plutusDataFromCborIterative } from "../../src/plutus-data-iterative.decode.js";
import { isPlutusDataMap } from "../../src/plutus-data-narrowing.js";
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
 * - `interimDataDecoding`: a step that reveals, in direct form, an integer
 *   outside one CBOR head or a constructor index above 127, or whose semantic
 *   witness carries one in a preimage the on-chain machine decodes as Data
 *   (iData, unConstrData, unIData and serialiseData). Owner: E-N5 (the
 *   interim §P3 row). Pending the on-chain Data decoder update.
 * - `blsInvalidPointPending`: a G1 or G2 uncompress failure outside the
 *   on-chain failure proof (for G1, not a wrong length, an invalid header or
 *   a point off the curve; for G2, not a wrong length or an invalid header).
 *   Owner: E-N8 (c). Pending the remaining BLS failure arms.
 * - `computationCost`: a step of a computation-cost kind of §P3, or a
 *   large-Data step, whose adjudication goes over the evaluator's budget.
 *   Owner: the §P3 computation-cost rows and E-N6 (ii). Pending the E-N6 (ii)
 *   cost update for the large-Data steps.
 * - `trailingEmptyBytesDecoding`: a step the Aiken machine fails whose
 *   revealed payload, or a preimage it decodes as Data, ends in an empty byte
 *   string. Owner: the on-chain Data decoder. Pending the on-chain
 *   byte-string decoding update.
 * - `nestedConstantCost`: a step revealing a constant whose type nests one
 *   compound in another (a pair, or a list of lists), whose adjudication goes
 *   over the evaluator's budget. Owner: the on-chain constant step. Pending
 *   the on-chain constant-step cost update.
 */
export type CataloguedClass =
  | "interimDataDecoding"
  | "blsInvalidPointPending"
  | "computationCost"
  | "trailingEmptyBytesDecoding"
  | "nestedConstantCost";

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

/**
 * The semantic builtins whose scalar preimages the on-chain machine decodes as
 * Data: iData (40), unConstrData (42), unIData (45) and serialiseData (51).
 */
const DATA_DECODED_PREIMAGE_TAGS = new Set([40, 42, 45, 51]);

/** The preimages of a step that the on-chain machine decodes as Data. */
const dataDecodedPreimages = (witness: StepWitness): readonly Uint8Array[] =>
  witness.kind === "executeBuiltinSemantic" &&
  DATA_DECODED_PREIMAGE_TAGS.has(Number(witness.tag))
    ? witness.material.scalarPreimages
    : [];

/** The integers one CBOR head carries. */
const MIN_ONE_HEAD_INTEGER = -(2n ** 64n);
const MAX_ONE_HEAD_INTEGER = 2n ** 64n - 1n;
/** The largest constructor index with a compact CBOR tag. */
const MAX_COMPACT_CONSTR_INDEX = 127n;

/** Whether a payload holds an integer or constructor index of the row. */
const payloadIsInInterimRow = (payloadCbor: Uint8Array): boolean => {
  let pending: Data[];
  try {
    pending = [plutusDataFromCborIterative(payloadCbor)];
  } catch {
    return false;
  }
  for (let item = pending.pop(); item !== undefined; item = pending.pop()) {
    if (item instanceof DataI) {
      if (item.int < MIN_ONE_HEAD_INTEGER || item.int > MAX_ONE_HEAD_INTEGER)
        return true;
    } else if (item instanceof DataConstr) {
      if (item.constr > MAX_COMPACT_CONSTR_INDEX) return true;
      pending.push(...item.fields);
    } else if (item instanceof DataList) {
      pending.push(...item.list);
    } else if (isPlutusDataMap(item)) {
      for (const entry of item.map) pending.push(entry.fst, entry.snd);
    }
  }
  return false;
};

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

/** The empty byte string's CBOR. */
const EMPTY_BYTES_HEAD = 0x40;

/** Whether a payload's encoding ends in an empty byte string. */
const endsInEmptyBytes = (payloadCbor: Uint8Array): boolean => {
  if (payloadCbor[payloadCbor.length - 1] !== EMPTY_BYTES_HEAD) return false;
  let item: Data;
  try {
    item = plutusDataFromCborIterative(payloadCbor);
  } catch {
    return false;
  }
  // Follow the last field, item or value: the leaf encoded last.
  for (;;) {
    if (item instanceof DataB) return item.bytes.length === 0;
    const last: Data | undefined =
      item instanceof DataConstr
        ? item.fields.at(-1)
        : item instanceof DataList
          ? item.list.at(-1)
          : isPlutusDataMap(item)
            ? item.map.at(-1)?.snd
            : undefined;
    if (last === undefined) return false;
    item = last;
  }
};

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
  const payloads = [
    ...revealedConstants(witness).map(({ payloadCbor }) => payloadCbor),
    ...dataDecodedPreimages(witness),
  ];
  if (payloads.some(payloadIsInInterimRow)) return "interimDataDecoding";
  if (isPendingBlsFailure(witness)) return "blsInvalidPointPending";
  if (verdict === "overBudget" && isComputationCostKind(witness, builtin)) {
    return "computationCost";
  }
  if (verdict === "failed" && payloads.some(endsInEmptyBytes)) {
    return "trailingEmptyBytesDecoding";
  }
  if (verdict === "overBudget" && revealsNestedConstant(witness)) {
    return "nestedConstantCost";
  }
  return null;
};
