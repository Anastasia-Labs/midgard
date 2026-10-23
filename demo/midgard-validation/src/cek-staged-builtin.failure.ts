/**
 * The staged failure hop (arm 36 of the byte-string, string and integer
 * builtins): the TypeScript twin of `verify_staged_failure_v1`.
 *
 * Protocol version 11 fails these builtins on an argument past its bound
 * before costing them, and indexByteString and decodeUtf8 also fail on their
 * operand's content. The roots hop commits every argument compacted, with its
 * type, payload summary and memory, so most failures are decided from those
 * alone:
 *
 * - an integer argument of an integer builtin but equalsInteger of more than
 *   4,096 words, or of 4,096 words whose CBOR, revealed by its source, is
 *   outside [-2^262143, 2^262143 - 1];
 * - a byte-string argument of a bounded builtin of more than 8,192 words
 *   (more than 65,536 bytes);
 * - consByteString's byte, sliceByteString's start or length, or
 *   indexByteString's index of more than one word;
 * - indexByteString's index, revealed by its value, below zero or at or past
 *   the length its source binds;
 * - decodeUtf8 of bytes whose prefix up to the material's trailing value
 *   decodes and whose next character, as long as its first byte says, does
 *   not.
 *
 * Only compacted arguments are read: a revealed constant is past no bound.
 */
import {
  collectMidgardCekStagedRuns,
  type MidgardCekStagedReferenceInput,
} from "@al-ft/midgard-core";
import { CEKConst } from "@harmoniclabs/plutus-machine";

import type { MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import type {
  MidgardCekStagedBuiltinMaterial,
  MidgardCekStagedOperandSource,
} from "./cek-staged-builtin.material.js";
import {
  type ArmInputs,
  bindBytes,
  bindInteger,
  type Compacted,
  compactValue,
  refuse,
  sameCompacted,
  scalarOperands,
} from "./cek-staged-builtin.verify.js";

const MAX_BOUNDED_BYTES_WORDS = 8_192n;
const MAX_BOUNDED_INTEGER_WORDS = 4_096n;
const INTEGER_LOWER_BOUND = -(2n ** 262_143n);
const INTEGER_UPPER_BOUND = 2n ** 262_143n - 1n;

const utf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

const isUtf8 = (bytes: Uint8Array): boolean => {
  try {
    utf8.decode(bytes);
    return true;
  } catch {
    return false;
  }
};

/** True when `value` is compacted and its memory is past `words`. */
const memoryPast = (
  value: MidgardCekDirectValueWitness,
  words: bigint,
): boolean => value.kind === "semanticConstant" && value.witness.memory > words;

const compacted = (value: MidgardCekDirectValueWitness): Compacted =>
  value.kind === "semanticConstant"
    ? value.witness
    : refuse("argument not compacted");

/** The staging outputs `material` names, to bind its sources as the success
 * arms do. */
const failureInputs = (
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): ArmInputs => ({
  tag: 0,
  arguments: [],
  result: compactValue(CEKConst.bool(false)),
  runs: collectMidgardCekStagedRuns(referenceInputs, material.runs.refs),
  operands: material.runs.operands,
});

const smallIntegerSource = (
  source: MidgardCekStagedOperandSource | undefined,
): bigint =>
  source?.kind === "smallInteger" ? source.value : refuse("trailing source");

/** An integer argument of 4,096 words, bound to its CBOR by its source, is
 * outside the range. */
const integerPastRange = (
  values: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): boolean => {
  const inputs = failureInputs(material, referenceInputs);
  if (values.length !== material.sources.length) {
    return refuse("argument count");
  }
  return values.some((value, index) => {
    if (
      value.kind !== "semanticConstant" ||
      value.witness.memory !== MAX_BOUNDED_INTEGER_WORDS
    ) {
      return false;
    }
    const { integer } = bindInteger(
      inputs,
      [value.witness, material.sources[index]!],
      false,
    );
    return (
      integer !== null &&
      (integer < INTEGER_LOWER_BOUND || integer > INTEGER_UPPER_BOUND)
    );
  });
};

/** indexByteString's index, a one-word integer revealed by its value, is
 * below zero or at or past the length the byte string's source binds. */
const indexPastEnd = (
  values: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): boolean => {
  const [bytes, index] = values;
  const [source, trailing] = material.sources;
  if (values.length !== 2 || material.sources.length !== 2) {
    return refuse("argument count");
  }
  const value = smallIntegerSource(trailing);
  const committed = compacted(index!);
  if (
    committed.memory !== 1n ||
    !sameCompacted(compactValue(CEKConst.int(value)), committed)
  ) {
    return refuse("index leaf");
  }
  const operand = bindBytes(
    failureInputs(material, referenceInputs),
    [compacted(bytes!), source!],
    false,
    true,
  );
  return value < 0n || value >= operand.length;
};

/** decodeUtf8's operand is not UTF-8: its bytes up to the given offset
 * decode, so in any decoding of the whole a character starts there, and the
 * sequence starting there, as long as its first byte says, is not one. */
const notUtf8 = (
  values: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): boolean => {
  const [source, trailing] = material.sources;
  if (values.length !== 1 || material.sources.length !== 2) {
    return refuse("argument count");
  }
  const valid = smallIntegerSource(trailing);
  const operand = bindBytes(
    failureInputs(material, referenceInputs),
    [compacted(values[0]!), source!],
    false,
    true,
  );
  const whole = operand.bytes ?? refuse("operand bytes not revealed");
  if (valid < 0n || valid >= BigInt(whole.length)) {
    return refuse("offset outside the operand");
  }
  const offset = Number(valid);
  if (!isUtf8(whole.subarray(0, offset))) return refuse("prefix not UTF-8");
  const lead = whole[offset]!;
  const width = lead < 0xc0 ? 1 : lead < 0xe0 ? 2 : lead < 0xf0 ? 3 : 4;
  return !isUtf8(whole.subarray(offset, offset + width));
};

/** Whether builtin `tag` is bounded on its byte-string arguments. */
const boundedBytesTag = (tag: bigint): boolean =>
  (tag >= 10n && tag <= 20n && tag !== 13n) ||
  tag === 25n ||
  tag === 71n ||
  tag === 72n ||
  tag === 86n;

/**
 * Whether the staged failure hop accepts that builtin `tag` fails on
 * `arguments_` (compacted, as the witness carries them), with `material`
 * read against the transaction's `referenceInputs`.
 */
export const verifyMidgardCekStagedBuiltinFailure = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): boolean => {
  try {
    if (tag <= 9n) {
      return (
        tag !== 7n &&
        (arguments_.some((value) =>
          memoryPast(value, MAX_BOUNDED_INTEGER_WORDS),
        ) ||
          integerPastRange(arguments_, material, referenceInputs))
      );
    }
    if (!boundedBytesTag(tag)) return false;
    const [byteArguments, integerArguments] = scalarOperands(
      Number(tag),
      arguments_,
    );
    return (
      byteArguments.some((value) =>
        memoryPast(value, MAX_BOUNDED_BYTES_WORDS),
      ) ||
      integerArguments.some((value) => memoryPast(value, 1n)) ||
      (tag === 14n && indexPastEnd(arguments_, material, referenceInputs)) ||
      (tag === 25n && notUtf8(arguments_, material, referenceInputs))
    );
  } catch {
    return false;
  }
};
