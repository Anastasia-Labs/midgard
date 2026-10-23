/**
 * The failures of a staged-route step (a byte-string, string or integer
 * builtin with an operand past the direct carriage bound). Protocol version
 * 11 fails these builtins on an argument past its bound, before costing
 * them, and on an index outside the byte string, a byte or a slice bound
 * outside its range, and bytes that are not UTF-8. Each failure is given the
 * witness of the arm that proves it:
 *
 * - a failure the committed arguments decide, indexByteString's and
 *   decodeUtf8's, and a division by zero of a dividend of 4,096 words, go to
 *   the staged failure hop (arm 36) with the sources it reads;
 * - consByteString's byte and sliceByteString's start and length, which are
 *   revealed, go to the direct failure arm (arm 38), the byte string
 *   compacted to its leaf;
 * - a signature builtin's message past its bound, and its malformed key or
 *   signature beside a message in bound (charged), go to the staged failure
 *   hop, the lengths read from the leaves.
 */
import { MIDGARD_CEK_STAGED_CHUNK_BYTES } from "@al-ft/midgard-core";
import { CEKConst } from "@harmoniclabs/plutus-machine";

import type { MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import {
  evaluateMidgardCekDirectBuiltin,
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "./cek-builtin.evaluate-reference-builtin.js";
import type { MidgardCekBuiltinBudget } from "./cek-cost.js";
import type { MidgardCekCoreStepWitness } from "./cek-machine.js";
import {
  midgardCekStagedBuiltinFailureBudget,
  schnorrKeyIsValid,
} from "./cek-staged-builtin.failure.js";
import {
  midgardCekPlaceholderStaging,
  type MidgardCekStagedOperandSource,
  midgardCekStagedReferenceInputs,
  stageMidgardCekStagedBuiltinMaterial,
} from "./cek-staged-builtin.material.js";
import { scalarOperands } from "./cek-staged-builtin.verify.js";

/** A staged step's argument, read from the semantic store. */
export type MidgardCekStagedOperand = {
  readonly type: "integer" | "bytes" | "string";
  /** A byte string's or string's bytes, or an integer's canonical CBOR. */
  readonly bytes: Buffer;
  readonly value: CEKConst;
  readonly leaf: MidgardCekStagedOperandSource;
};

export type MidgardCekStagedFailureWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinStagedFailure" | "executeBuiltinFailure" }
>;

const MAX_BOUNDED_BYTES = 65_536;
const MAX_BOUNDED_INTEGER_WORDS = 4_096n;
const INTEGER_LOWER_BOUND = -(2n ** 262_143n);
const INTEGER_UPPER_BOUND = 2n ** 262_143n - 1n;
const INT64_MIN = -(2n ** 63n);
const INT64_MAX = 2n ** 63n - 1n;

const utf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

const isUtf8 = (bytes: Uint8Array): boolean => {
  try {
    utf8.decode(bytes);
    return true;
  } catch {
    return false;
  }
};

/** The offset of the first character of `bytes` that is not UTF-8, or null
 * when all of them are. */
const firstInvalidCharacter = (bytes: Buffer): number | null => {
  let offset = 0;
  while (offset < bytes.length) {
    const lead = bytes[offset]!;
    const width = lead < 0xc0 ? 1 : lead < 0xe0 ? 2 : lead < 0xf0 ? 3 : 4;
    if (!isUtf8(bytes.subarray(offset, offset + width))) return offset;
    offset += width;
  }
  return null;
};

const words = (operand: MidgardCekStagedOperand): bigint =>
  operand.leaf.kind === "integerLeaf" ? operand.leaf.words : 1n;

const integer = (operand: MidgardCekStagedOperand): bigint =>
  operand.value.value as bigint;

const boundedBytesTag = (tag: number): boolean =>
  (tag >= 10 && tag <= 20 && tag !== 13) ||
  tag === 25 ||
  tag === 71 ||
  tag === 72 ||
  tag === 86;

type Classified = {
  readonly witness: MidgardCekStagedFailureWitness;
  /** Whether the failure is on the operands' content, which the reference
   * evaluator decides too, rather than on a bound. */
  readonly content: boolean;
};

/**
 * The failure witness of builtin `tag` over `arguments_` (presented) and
 * their `operands`, or null when no bound or content failure of the staged
 * route applies.
 */
const classify = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  operands: readonly MidgardCekStagedOperand[],
): Classified | null => {
  const stagedOperands: Buffer[] = [];
  const revealed = (bytes: Buffer): MidgardCekStagedOperandSource => {
    if (bytes.length <= MIDGARD_CEK_STAGED_CHUNK_BYTES) {
      return { kind: "inline", bytes };
    }
    stagedOperands.push(bytes);
    return { kind: "staged", operand: stagedOperands.length - 1 };
  };
  const staged = (
    sources: readonly MidgardCekStagedOperandSource[],
    content: boolean,
  ): Classified => ({
    witness: {
      kind: "executeBuiltinStagedFailure",
      tag,
      arguments: arguments_.map(semanticizeMidgardCekDirectConstant),
      material: { runs: { refs: [], operands: [] }, sources },
      stagedOperands,
    },
    content,
  });
  const number = Number(tag);
  if (number <= 9) {
    if (number === 7) return null;
    if (operands.some((operand) => words(operand) > MAX_BOUNDED_INTEGER_WORDS))
      return staged([], false);
    const past = operands.findIndex(
      (operand) =>
        words(operand) === MAX_BOUNDED_INTEGER_WORDS &&
        (integer(operand) < INTEGER_LOWER_BOUND ||
          integer(operand) > INTEGER_UPPER_BOUND),
    );
    if (past >= 0) {
      // Only the argument past the range is revealed; the rest by their leaf.
      const sources = operands.map((operand, index) =>
        index === past ? revealed(operand.bytes) : operand.leaf,
      );
      return staged(sources, false);
    }
    const [dividend, divisor] = operands;
    // A division by zero, charged: the hop reads the dividend's range from
    // its CBOR, which the direct failure arm does not have.
    if (
      number >= 3 &&
      number <= 6 &&
      words(dividend!) === MAX_BOUNDED_INTEGER_WORDS &&
      integer(divisor!) === 0n
    ) {
      return staged([revealed(dividend!.bytes), divisor!.leaf], true);
    }
    return null;
  }
  if (number === 21 || number === 53) {
    const [key, message, signature] = operands;
    // Only the message is bounded, and the bound is checked first.
    if (message!.bytes.length > MAX_BOUNDED_BYTES) return staged([], false);
    const keyBytes = key!.bytes;
    if (
      keyBytes.length === 32 &&
      signature!.bytes.length === 64 &&
      (number === 21 || schnorrKeyIsValid(keyBytes))
    ) {
      return null;
    }
    // A BIP-340 key of 32 bytes is revealed for its curve check.
    const keySource: MidgardCekStagedOperandSource =
      number === 53 && keyBytes.length === 32
        ? { kind: "inline", bytes: keyBytes }
        : key!.leaf;
    return staged([keySource, message!.leaf, signature!.leaf], true);
  }
  const [byteOperands, integerOperands] = scalarOperands(number, operands);
  if (
    boundedBytesTag(number) &&
    byteOperands.some((operand) => operand.bytes.length > MAX_BOUNDED_BYTES)
  ) {
    return staged([], false);
  }
  if (integerOperands.some((operand) => words(operand) > 1n)) {
    return staged([], false);
  }
  const integers = integerOperands.map(integer);
  const source = byteOperands[0]!;
  if (number === 11 || number === 12) {
    const fails =
      number === 11
        ? integers[0]! < 0n || integers[0]! > 255n
        : integers.some((value) => value < INT64_MIN || value > INT64_MAX);
    if (!fails) return null;
    return {
      witness: {
        kind: "executeBuiltinFailure",
        tag,
        arguments: [
          ...integers.map((value) =>
            referenceConstantToDirectWitness(CEKConst.int(value)),
          ),
          semanticizeMidgardCekDirectConstant(arguments_.at(-1)!),
        ],
      },
      content: true,
    };
  }
  if (number === 14) {
    const [index] = integers;
    if (index! >= 0n && index! < BigInt(source.bytes.length)) return null;
    return staged([source.leaf, { kind: "smallInteger", value: index! }], true);
  }
  if (number === 25) {
    const offset = firstInvalidCharacter(source.bytes);
    if (offset === null) return null;
    return staged(
      [revealed(source.bytes), { kind: "smallInteger", value: BigInt(offset) }],
      true,
    );
  }
  return null;
};

/** The budget the failure's arm charges, or null when it does not accept
 * the witness, staged on placeholder outputs. */
const provenBudget = (
  witness: MidgardCekStagedFailureWitness,
): MidgardCekBuiltinBudget | null => {
  if (witness.kind === "executeBuiltinFailure") {
    const evaluated = evaluateMidgardCekDirectBuiltin(
      witness.tag,
      witness.arguments,
    );
    return evaluated.kind === "failure" ? evaluated.budget : null;
  }
  const material = stageMidgardCekStagedBuiltinMaterial(
    witness.material,
    witness.stagedOperands,
    midgardCekPlaceholderStaging(witness.stagedOperands),
  );
  return midgardCekStagedBuiltinFailureBudget(
    witness.tag,
    witness.arguments,
    material,
    midgardCekStagedReferenceInputs(material, witness.stagedOperands),
  );
};

/** A staged step's failure: its witness and the budget its arm charges. */
export type MidgardCekStagedFailure = {
  readonly witness: MidgardCekStagedFailureWitness;
  readonly budget: MidgardCekBuiltinBudget;
};

/**
 * The failure of staged builtin `tag` over `arguments_` (presented) and their
 * `operands`, or null when it does not fail on a bound or on its operands'
 * content. `evaluatedFailure` is called for a content failure, which the
 * reference evaluator must agree with. Throws for a failure its arm would not
 * accept.
 */
export const midgardCekStagedFailure = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  operands: readonly MidgardCekStagedOperand[],
  evaluatedFailure: () => boolean,
): MidgardCekStagedFailure | null => {
  const classified = classify(tag, arguments_, operands);
  if (classified === null) return null;
  if (classified.content && !evaluatedFailure()) {
    throw new Error("CEK staged builtin failure disagrees with the evaluator");
  }
  const budget = provenBudget(classified.witness);
  if (budget === null) {
    throw new Error("CEK staged builtin failure is past its arm");
  }
  return { witness: classified.witness, budget };
};
