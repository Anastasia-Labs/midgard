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
 *   not;
 * - verifyEd25519Signature's or verifySchnorrSecp256k1Signature's message,
 *   or byteStringToInteger's input, of more than 8,192 words;
 * - a byte-string argument of andByteString, orByteString, xorByteString,
 *   complementByteString, readBit, countSetBits or findFirstSetBit of more
 *   than 8,192 words;
 * - readBit's index, or shiftByteString's or rotateByteString's amount, of
 *   more than one word, or of one word revealed by its value and outside the
 *   machine integers;
 * - integerToByteString's width of more than one word, or of one word
 *   revealed by its value and outside 0..8,192.
 *
 * Only compacted arguments are read: a revealed constant is past no bound.
 *
 * These failures are raised before costing but a few: a division builtin
 * over a dividend of 4,096 words in the range and a zero divisor is charged
 * before it fails, and so is a signature builtin over a message in bound and
 * a key or signature of the wrong length, or a BIP-340 key off the curve,
 * verifyEcdsaSecp256k1Signature over any argument of the wrong length, and
 * integerToByteString with its width in range over an input of more than
 * 1,024 words. The direct failure arm leaves every dividend of 4,096 words or
 * more to this hop, which reads the dividend's range from its CBOR; it
 * proves a malformed key or signature only beside an inline message, and an
 * input of more than 1,024 words only within the direct carriage bound, with
 * the same charge.
 */
import {
  collectMidgardCekStagedRuns,
  type MidgardCekStagedReferenceInput,
} from "@al-ft/midgard-core";
import { CEKConst } from "@harmoniclabs/plutus-machine";
import { schnorr } from "@noble/curves/secp256k1.js";

import type { MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import { referenceConstantToDirectWitness } from "./cek-builtin.evaluate-reference-builtin.js";
import { midgardCekDirectBuiltinBudget } from "./cek-builtin.selected-control-result.js";
import type { MidgardCekBuiltinBudget } from "./cek-cost.js";
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
const INT64_MIN = -(2n ** 63n);
const INT64_MAX = 2n ** 63n - 1n;
/** integerToByteString's widest output, in bytes and in words. */
const MAX_INTEGER_TO_BYTES_WIDTH = 8_192n;
const MAX_INTEGER_TO_BYTES_WORDS = 1_024n;

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

/** The value of a one-word integer argument, revealed by its source and
 * re-encoded to its leaf; it may be outside the machine integers. */
const oneWordInteger = (
  value: MidgardCekDirectValueWitness,
  source: MidgardCekStagedOperandSource | undefined,
): bigint => {
  const integer = smallIntegerSource(source);
  const committed = compacted(value);
  if (
    committed.memory !== 1n ||
    !sameCompacted(compactValue(CEKConst.int(integer)), committed)
  ) {
    return refuse("integer leaf");
  }
  return integer;
};

/** indexByteString's index (`unit` 1) or readBit's (`unit` 8), a one-word
 * integer revealed by its value, is below zero or at or past the length in
 * bytes or bits the byte string's source binds. */
const indexPastEnd = (
  values: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
  unit: bigint,
): boolean => {
  const [bytes, index] = values;
  const [source, trailing] = material.sources;
  if (values.length !== 2 || material.sources.length !== 2) {
    return refuse("argument count");
  }
  const value = oneWordInteger(index!, trailing);
  const operand = bindBytes(
    failureInputs(material, referenceInputs),
    [compacted(bytes!), source!],
    false,
    true,
  );
  return value < 0n || value >= operand.length * unit;
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

/** A bitwise builtin fails: a byte-string argument (but shiftByteString's
 * and rotateByteString's input, which is unbounded) past its bound,
 * readBit's index outside the bits (which takes in every index outside the
 * machine integers), or the amount outside the machine integers. */
const bitwiseFails = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): boolean => {
  if (
    tag !== 82n &&
    tag !== 83n &&
    arguments_.some((value) => memoryPast(value, MAX_BOUNDED_BYTES_WORDS))
  ) {
    return true;
  }
  if (tag !== 79n && tag !== 82n && tag !== 83n) return false;
  if (arguments_.length !== 2) return refuse("argument count");
  const integer = arguments_[1]!;
  if (memoryPast(integer, 1n)) return true;
  if (tag === 79n) {
    return indexPastEnd(arguments_, material, referenceInputs, 8n);
  }
  if (material.sources.length !== 2) return refuse("source count");
  const value = oneWordInteger(integer, material.sources[1]);
  return value < INT64_MIN || value > INT64_MAX;
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
    if (tag === 21n || tag === 53n || tag === 74n) {
      // The signature builtins bound only their message, and
      // byteStringToInteger its input: both the second argument.
      const bounded = arguments_[1] ?? refuse("argument count");
      return memoryPast(bounded, MAX_BOUNDED_BYTES_WORDS);
    }
    if ((tag >= 75n && tag <= 79n) || (tag >= 82n && tag <= 85n)) {
      return bitwiseFails(tag, arguments_, material, referenceInputs);
    }
    if (tag === 73n) {
      const [, width] = arguments_;
      if (arguments_.length !== 3) return refuse("argument count");
      if (memoryPast(width!, 1n)) return true;
      if (material.sources.length !== 1) return refuse("source count");
      const value = oneWordInteger(width!, material.sources[0]);
      return value < 0n || value > MAX_INTEGER_TO_BYTES_WIDTH;
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
      (tag === 14n &&
        indexPastEnd(arguments_, material, referenceInputs, 1n)) ||
      (tag === 25n && notUtf8(arguments_, material, referenceInputs))
    );
  } catch {
    return false;
  }
};

const BYTES_TYPE = Buffer.from("9f01ff", "hex");
const INTEGER_TYPE = compactValue(CEKConst.int(0n)).typeCbor;

/** Whether `key` is an x-only BIP-340 key: x below p and on the curve. */
export const schnorrKeyIsValid = (key: Uint8Array): boolean => {
  try {
    schnorr.utils.lift_x(BigInt(`0x${Buffer.from(key).toString("hex")}`));
    return true;
  } catch {
    return false;
  }
};

/** verifyEd25519Signature or verifySchnorrSecp256k1Signature fails on its
 * key or signature: a key of other than 32 bytes, a signature of other than
 * 64, or a BIP-340 key off the curve, revealed; verifyEcdsaSecp256k1Signature
 * on a key of other than 33 bytes, a message of other than 32 or a signature
 * of other than 64. Every argument is a byte string; the lengths are read
 * from the leaves. */
const malformedSignature = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): boolean => {
  if (tag !== 21n && tag !== 52n && tag !== 53n) {
    return refuse("not a signature builtin");
  }
  const [keyValue, messageValue, signatureValue] = arguments_;
  const [keySource, messageSource, signatureSource] = material.sources;
  if (arguments_.length !== 3 || material.sources.length !== 3) {
    return refuse("argument count");
  }
  const inputs = failureInputs(material, referenceInputs);
  const key = bindBytes(
    inputs,
    [compacted(keyValue!), keySource!],
    false,
    false,
  );
  const signature = bindBytes(
    inputs,
    [compacted(signatureValue!), signatureSource!],
    false,
    false,
  );
  if (tag === 52n) {
    return (
      key.length !== 33n ||
      signature.length !== 64n ||
      bindBytes(
        inputs,
        [compacted(messageValue!), messageSource!],
        false,
        false,
      ).length !== 32n
    );
  }
  if (!Buffer.from(compacted(messageValue!).typeCbor).equals(BYTES_TYPE)) {
    return refuse("message type");
  }
  return (
    key.length !== 32n ||
    signature.length !== 64n ||
    (tag === 53n && !schnorrKeyIsValid(key.bytes ?? refuse("key not revealed")))
  );
};

const UNCHARGED: MidgardCekBuiltinBudget = Object.freeze({
  cpu: 0n,
  memory: 0n,
});

/** integerToByteString with its width in range fails after costing on an
 * input of more than 1,024 words: the endianness is a Boolean, the input an
 * integer, and the width, revealed by its source, is costed by its words. */
const integerToBytesCharge = (
  arguments_: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
): MidgardCekBuiltinBudget => {
  const [endianness, width, input] = arguments_;
  if (arguments_.length !== 3 || material.sources.length !== 1) {
    return refuse("argument count");
  }
  if (
    !sameCompacted(compacted(endianness!), compactValue(CEKConst.bool(true))) &&
    !sameCompacted(compacted(endianness!), compactValue(CEKConst.bool(false)))
  ) {
    return refuse("endianness type");
  }
  const value = oneWordInteger(width!, material.sources[0]);
  const committed = compacted(input!);
  if (!Buffer.from(committed.typeCbor).equals(INTEGER_TYPE)) {
    return refuse("input type");
  }
  if (committed.memory <= MAX_INTEGER_TO_BYTES_WORDS) {
    return refuse("input within the widest output");
  }
  return midgardCekDirectBuiltinBudget(73n, [
    endianness!,
    referenceConstantToDirectWitness(CEKConst.int(value)),
    input!,
  ]);
};

/** For a division builtin over a dividend of 4,096 words and a zero divisor,
 * whether the dividend, bound to its CBOR by its source, is in the range;
 * null for any other step. */
const dividendOverZero = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): boolean | null => {
  if (tag < 3n || tag > 6n) return null;
  if (arguments_.length !== 2) return refuse("argument count");
  const [dividend, divisor] = arguments_;
  if (
    dividend!.kind !== "semanticConstant" ||
    dividend!.witness.memory !== MAX_BOUNDED_INTEGER_WORDS ||
    divisor!.kind !== "semanticConstant" ||
    !sameCompacted(divisor!.witness, compactValue(CEKConst.int(0n)))
  ) {
    return null;
  }
  if (material.sources.length !== 2) return refuse("argument count");
  const { integer } = bindInteger(
    failureInputs(material, referenceInputs),
    [dividend!.witness, material.sources[0]!],
    false,
  );
  if (integer === null) return refuse("dividend by its leaf alone");
  return integer >= INTEGER_LOWER_BOUND && integer <= INTEGER_UPPER_BOUND;
};

/**
 * The budget the staged failure hop's step of builtin `tag` charges over
 * `arguments_` (compacted), with `material` read against `referenceInputs`,
 * or null when the hop does not accept that the builtin fails: the twin of
 * `staged_failure_budget_v1`.
 */
export const midgardCekStagedBuiltinFailureBudget = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): MidgardCekBuiltinBudget | null => {
  try {
    const inRange = dividendOverZero(
      tag,
      arguments_,
      material,
      referenceInputs,
    );
    if (inRange === true) return midgardCekDirectBuiltinBudget(tag, arguments_);
    if (inRange === false) return UNCHARGED;
    if (
      verifyMidgardCekStagedBuiltinFailure(
        tag,
        arguments_,
        material,
        referenceInputs,
      )
    ) {
      return UNCHARGED;
    }
    if (tag === 73n) return integerToBytesCharge(arguments_, material);
    // A signature builtin over a message in bound fails after costing.
    return malformedSignature(tag, arguments_, material, referenceInputs)
      ? midgardCekDirectBuiltinBudget(tag, arguments_)
      : null;
  } catch {
    return null;
  }
};
