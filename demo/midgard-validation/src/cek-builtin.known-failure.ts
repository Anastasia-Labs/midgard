/**
 * The builtin failures the direct failure arm proves: the TypeScript twin of
 * `known_builtin_failure_v1` (cek-builtin-v1.ak). A builtin failure outside
 * this set is proved by another arm (the type-failure, semantic-failure or
 * staged failure arm) or not at all, so the direct failure verifier requires
 * it as L1 does.
 *
 * Each reader below throws where its Aiken reader fails the script, and each
 * arm reads its arguments in the Aiken order, with `and`/`or` short-circuiting
 * as Aiken's do, so a throw stands exactly where L1 refuses.
 */
import { type MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import {
  decodeMidgardCekConstantTypeCbor,
  decodeMidgardCekConstantWitness,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "./uplc-data.js";

/**
 * G1/G2 uncompress (59, 66) and scalarMul (56, 63): builtins with failures
 * outside the known set that no L1 arm proves yet.
 */
export const MIDGARD_CEK_BLS_FAILURES_PENDING_ON_L1: ReadonlySet<bigint> =
  new Set([56n, 59n, 63n, 66n]);

const INT64_MIN = -(2n ** 63n);
const INT64_MAX = 2n ** 63n - 1n;

const SECP256K1_GROUP_ORDER =
  115792089237316195423570985008687907852837564279074904382605163141518161494337n;
const SECP256K1_FIELD_PRIME =
  115792089237316195423570985008687907853269984665640564039457584007908834671663n;
const BLS12_381_FIELD_PRIME =
  4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787n;

const refuse = (message: string): never => {
  throw new Error(`V1 known builtin failure: ${message}`);
};

/** `constant_type_v1`: a revealed or compacted constant's type. */
const constantType = (
  value: MidgardCekDirectValueWitness,
): MidgardCekConstantType =>
  value.kind === "constant"
    ? decodeMidgardCekConstantWitness(value.witness).type
    : value.kind === "semanticConstant"
      ? decodeMidgardCekConstantTypeCbor(value.witness.typeCbor)
      : refuse("expected constant value");

/** `constant_payload_v1`: a revealed constant's payload. */
const constantPayload = (value: MidgardCekDirectValueWitness): Data =>
  value.kind === "constant"
    ? decodeMidgardCekConstantWitness(value.witness).payload
    : refuse("expected revealed constant");

const isType = (
  value: MidgardCekDirectValueWitness,
  kind: MidgardCekConstantType["kind"],
): boolean => constantType(value).kind === kind;

/** `integer_v1`. */
const integer = (value: MidgardCekDirectValueWitness): bigint => {
  if (!isType(value, "integer")) refuse("expected integer");
  const payload = constantPayload(value);
  return payload instanceof DataI ? payload.int : refuse("integer payload");
};

/** `bytes_v1`. */
const bytes = (value: MidgardCekDirectValueWitness): Uint8Array => {
  if (!isType(value, "bytes")) refuse("expected byte string");
  const payload = constantPayload(value);
  return payload instanceof DataB ? payload.bytes : refuse("bytes payload");
};

/** `boolean_v1`. */
const boolean = (value: MidgardCekDirectValueWitness): boolean => {
  if (!isType(value, "boolean")) refuse("expected boolean");
  const payload = constantPayload(value);
  if (
    !(payload instanceof DataConstr) ||
    payload.fields.length !== 0 ||
    (payload.constr !== 0n && payload.constr !== 1n)
  ) {
    return refuse("boolean payload");
  }
  return payload.constr === 1n;
};

/** `list_payload_v1`: a list constant's element type and items. */
const listPayload = (
  value: MidgardCekDirectValueWitness,
): readonly [MidgardCekConstantType, readonly Data[]] => {
  const type = constantType(value);
  if (type.kind !== "list") return refuse("expected list");
  const payload = constantPayload(value);
  return payload instanceof DataList
    ? [type.element, payload.list]
    : refuse("list payload");
};

/** `data_payload_v1`. */
const dataPayload = (value: MidgardCekDirectValueWitness): Data => {
  if (!isType(value, "data")) refuse("expected data");
  return constantPayload(value);
};

/** `is_inline_bytes_v1`: an inline byte-string constant. */
const isInlineBytes = (value: MidgardCekDirectValueWitness): boolean =>
  value.kind === "constant" &&
  isType(value, "bytes") &&
  constantPayload(value) instanceof DataB;

/** `expect [..] = arguments`: exactly `count` arguments. */
const exactly = (
  arguments_: readonly MidgardCekDirectValueWitness[],
  count: number,
): readonly MidgardCekDirectValueWitness[] =>
  arguments_.length === count ? arguments_ : refuse("argument count");

const outsideInt64 = (value: bigint): boolean =>
  value < INT64_MIN || value > INT64_MAX;

/** `builtin.bytearray_to_integer(True, ..)`: big-endian, unsigned. */
const bigEndian = (source: Uint8Array): bigint => {
  let value = 0n;
  for (const byte of source) value = (value << 8n) | BigInt(byte);
  return value;
};

/** `modular_power_v1`. */
const modularPower = (
  base: bigint,
  exponent: bigint,
  modulus: bigint,
): bigint => {
  if (exponent < 0n || modulus <= 1n) refuse("modular power operands");
  let current = ((base % modulus) + modulus) % modulus;
  let remaining = exponent;
  let accumulator = 1n;
  while (remaining !== 0n) {
    if (remaining % 2n === 1n) accumulator = (accumulator * current) % modulus;
    current = (current * current) % modulus;
    remaining /= 2n;
  }
  return accumulator;
};

const continuationByte = (value: number): boolean =>
  value >= 0x80 && value <= 0xbf;

/** `is_valid_utf8_v1`: strict UTF-8, read as the Aiken reads it. */
const isValidUtf8 = (source: Uint8Array): boolean => {
  const length = source.length;
  let index = 0;
  const tail = (from: number, required: number): boolean => {
    for (let offset = 0; offset < required; offset += 1) {
      if (from + offset >= length || !continuationByte(source[from + offset]!))
        return false;
    }
    index = from + required;
    return true;
  };
  while (index < length) {
    const first = source[index]!;
    if (first <= 0x7f) {
      index += 1;
      continue;
    }
    if (first >= 0xc2 && first <= 0xdf) {
      if (!tail(index + 1, 1)) return false;
      continue;
    }
    if (index + 1 >= length) return false;
    const second = source[index + 1]!;
    let secondValid: boolean;
    let required: number;
    if (first === 0xe0) {
      secondValid = second >= 0xa0 && second <= 0xbf;
      required = 1;
    } else if (first === 0xed) {
      secondValid = second >= 0x80 && second <= 0x9f;
      required = 1;
    } else if (
      (first >= 0xe1 && first <= 0xec) ||
      (first >= 0xee && first <= 0xef)
    ) {
      secondValid = continuationByte(second);
      required = 1;
    } else if (first === 0xf0) {
      secondValid = second >= 0x90 && second <= 0xbf;
      required = 2;
    } else if (first >= 0xf1 && first <= 0xf3) {
      secondValid = continuationByte(second);
      required = 2;
    } else if (first === 0xf4) {
      secondValid = second >= 0x80 && second <= 0x8f;
      required = 2;
    } else {
      return false;
    }
    if (!secondValid || !tail(index + 2, required)) return false;
  }
  return true;
};

/** `secp256k1_x_is_on_curve_v1`. */
const secp256k1XIsOnCurve = (x: bigint): boolean => {
  if (x < 0n || x >= SECP256K1_FIELD_PRIME) return false;
  const ySquared = (x * x * x + 7n) % SECP256K1_FIELD_PRIME;
  return (
    ySquared === 0n ||
    modularPower(
      ySquared,
      (SECP256K1_FIELD_PRIME - 1n) / 2n,
      SECP256K1_FIELD_PRIME,
    ) === 1n
  );
};

/** `secp256k1_public_key_is_valid_v1`. */
const secp256k1PublicKeyIsValid = (tag: number, key: Uint8Array): boolean => {
  if (tag === 52) {
    return (
      key.length === 33 &&
      (key[0] === 2 || key[0] === 3) &&
      secp256k1XIsOnCurve(bigEndian(key.subarray(1, 33)))
    );
  }
  if (tag !== 53) refuse("secp256k1 tag");
  return key.length === 32 && secp256k1XIsOnCurve(bigEndian(key));
};

/** `ecdsa_signature_scalar_overflows_v1`. */
const ecdsaSignatureScalarOverflows = (signature: Uint8Array): boolean => {
  if (signature.length !== 64) refuse("ECDSA signature length");
  return (
    bigEndian(signature.subarray(0, 32)) >= SECP256K1_GROUP_ORDER ||
    bigEndian(signature.subarray(32, 64)) >= SECP256K1_GROUP_ORDER
  );
};

/** `bls_compressed_coordinate_v1`: 48 bytes from `offset`, flags cleared. */
const blsCompressedCoordinate = (
  encoded: Uint8Array,
  offset: number,
): bigint => {
  const coordinate = Uint8Array.from(encoded.subarray(offset, offset + 48));
  coordinate[0] = coordinate[0]! % 32;
  return bigEndian(coordinate);
};

/** `bls_uncompress_header_is_invalid_v1`. */
const blsUncompressHeaderIsInvalid = (
  tag: number,
  encoded: Uint8Array,
): boolean => {
  const expectedLength = tag === 59 ? 48 : tag === 66 ? 96 : refuse("BLS tag");
  if (encoded.length !== expectedLength) refuse("BLS encoding length");
  const first = encoded[0]!;
  const compressed = first >= 128;
  const infinity = first % 128 >= 64;
  const sort = first % 64 >= 32;
  const firstCoordinate = blsCompressedCoordinate(encoded, 0);
  const secondCoordinate =
    tag === 66 ? bigEndian(encoded.subarray(48, 96)) : 0n;
  return (
    !compressed ||
    (infinity
      ? sort || firstCoordinate !== 0n || secondCoordinate !== 0n
      : firstCoordinate >= BLS12_381_FIELD_PRIME ||
        secondCoordinate >= BLS12_381_FIELD_PRIME)
  );
};

/** `bls_g1_curve_encoding_is_invalid_v1`. */
const blsG1CurveEncodingIsInvalid = (encoded: Uint8Array): boolean => {
  if (encoded.length !== 48) refuse("BLS G1 encoding length");
  if (encoded[0]! % 128 >= 64) return false;
  const x = blsCompressedCoordinate(encoded, 0);
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

/** `unsigned_integer_byte_size_v1`. */
const unsignedIntegerByteSize = (value: bigint): bigint => {
  let size = 0n;
  for (let rest = value; rest !== 0n; rest /= 256n) size += 1n;
  return size;
};

/** `integer_to_bytes_fails_v1`. */
const integerToBytesFails = (
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const [endian, sizeValue, integerValue] = exactly(arguments_, 3);
  boolean(endian!);
  const size = integer(sizeValue!);
  const value = integer(integerValue!);
  const required = value < 0n ? 0n : unsignedIntegerByteSize(value);
  return (
    size < 0n ||
    size > 8192n ||
    value < 0n ||
    required > 8192n ||
    (size > 0n && required > size)
  );
};

/** `wrong_data_variant_v1`. */
const wrongDataVariant = (
  tag: number,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const [source] = exactly(arguments_, 1);
  const data = dataPayload(source!);
  if (tag === 42) return !(data instanceof DataConstr);
  if (tag === 43) return !(data instanceof DataMap);
  if (tag === 44) return !(data instanceof DataList);
  if (tag === 45) return !(data instanceof DataI);
  if (tag !== 46) refuse("data variant tag");
  return !(data instanceof DataB);
};

/** `malformed_signature_lengths_v1`. */
const malformedSignatureLengths = (
  tag: number,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const [key, message, signature] = exactly(arguments_, 3);
  const keyLength = bytes(key!).length;
  const signatureLength = bytes(signature!).length;
  if (!isInlineBytes(message!)) return false;
  if (tag === 21) return keyLength !== 32 || signatureLength !== 64;
  if (tag === 52) {
    if (
      keyLength !== 33 ||
      bytes(message!).length !== 32 ||
      signatureLength !== 64
    ) {
      return true;
    }
    return (
      !secp256k1PublicKeyIsValid(tag, bytes(key!)) ||
      ecdsaSignatureScalarOverflows(bytes(signature!))
    );
  }
  if (tag !== 53) refuse("signature tag");
  if (keyLength !== 32 || signatureLength !== 64) return true;
  return !secp256k1PublicKeyIsValid(tag, bytes(key!));
};

/** `all_bit_indices_valid_v1` over `semantic_integers_v1` of the items. */
const allBitIndicesValid = (
  items: readonly Data[],
  bitLength: bigint,
): boolean => {
  const indices = items.map((item) =>
    item instanceof DataI ? item.int : refuse("bit index payload"),
  );
  return indices.every((index) => index >= 0n && index < bitLength);
};

/**
 * Whether builtin `tag` over `arguments_` is a failure the direct failure arm
 * proves (`known_builtin_failure_v1`). Throws where that predicate fails the
 * script, so a caller treats a throw as a refusal.
 */
export const midgardCekKnownBuiltinFailure = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  if (tag < 0n || tag > 86n) return false;
  const number = Number(tag);
  if (number >= 3 && number <= 6) {
    const [dividend, divisor] = exactly(arguments_, 2);
    return (
      isType(dividend!, "integer") &&
      (dividend!.kind === "semanticConstant"
        ? dividend!.witness.memory < 4096n
        : true) &&
      integer(divisor!) === 0n
    );
  }
  if (number === 11) {
    const [byte, source] = exactly(arguments_, 2);
    const value = integer(byte!);
    return isType(source!, "bytes") && (value < 0n || value > 255n);
  }
  if (number === 12) {
    const [start, length, source] = exactly(arguments_, 3);
    const startValue = integer(start!);
    const lengthValue = integer(length!);
    return (
      isType(source!, "bytes") &&
      (outsideInt64(startValue) || outsideInt64(lengthValue))
    );
  }
  if (number === 14) {
    const [source, index] = exactly(arguments_, 2);
    const position = integer(index!);
    return (
      position >= BigInt(bytes(source!).length) ||
      position < 0n ||
      position > INT64_MAX
    );
  }
  if (number === 21 || number === 52 || number === 53) {
    return malformedSignatureLengths(number, arguments_);
  }
  if (number === 25) {
    const [source] = exactly(arguments_, 1);
    return !isValidUtf8(bytes(source!));
  }
  if (number === 33 || number === 34) {
    const [source] = exactly(arguments_, 1);
    return listPayload(source!)[1].length === 0;
  }
  if (number >= 42 && number <= 46) {
    return wrongDataVariant(number, arguments_);
  }
  if (number === 60 || number === 67) {
    const [message, domain] = exactly(arguments_, 2);
    return isType(message!, "bytes") && bytes(domain!).length > 255;
  }
  if (number === 59) {
    const [source] = exactly(arguments_, 1);
    const encoded = bytes(source!);
    if (encoded.length !== 48) return true;
    return (
      blsUncompressHeaderIsInvalid(number, encoded) ||
      blsG1CurveEncodingIsInvalid(encoded)
    );
  }
  if (number === 66) {
    const [source] = exactly(arguments_, 1);
    const encoded = bytes(source!);
    return (
      encoded.length !== 96 || blsUncompressHeaderIsInvalid(number, encoded)
    );
  }
  if (number === 73) return integerToBytesFails(arguments_);
  if (number === 79) {
    const [source, index] = exactly(arguments_, 2);
    const position = integer(index!);
    return position >= BigInt(bytes(source!).length) * 8n || position < 0n;
  }
  if (number === 80) {
    const [source, indices, bit] = exactly(arguments_, 3);
    const [elementType, indexData] = listPayload(indices!);
    const length = BigInt(bytes(source!).length);
    return (
      elementType.kind === "integer" &&
      isType(bit!, "boolean") &&
      (length > 4096n || !allBitIndicesValid(indexData, length * 8n))
    );
  }
  if (number === 81) {
    const [lengthValue, byteValue] = exactly(arguments_, 2);
    const length = integer(lengthValue!);
    const byte = integer(byteValue!);
    return length < 0n || length > 8192n || byte < 0n || byte > 255n;
  }
  if (number === 82 || number === 83) {
    const [source, amount] = exactly(arguments_, 2);
    return isType(source!, "bytes") && outsideInt64(integer(amount!));
  }
  return false;
};
