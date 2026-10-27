/**
 * The argument readers of the direct failure predicates' TypeScript twins
 * (`cek-builtin.known-failure.ts` and its protocol version 11 part): each
 * throws where its Aiken reader in cek-builtin-v1.ak fails the script.
 */
import { type MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import {
  decodeMidgardCekConstantTypeCbor,
  decodeMidgardCekConstantWitness,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import { type Data, DataB, DataConstr, DataI, DataList } from "./uplc-data.js";

export const INT64_MIN = -(2n ** 63n);
export const INT64_MAX = 2n ** 63n - 1n;

export const refuse = (message: string): never => {
  throw new Error(`V1 known builtin failure: ${message}`);
};

/** `constant_type_v1`: a revealed or compacted constant's type. */
export const constantType = (
  value: MidgardCekDirectValueWitness,
): MidgardCekConstantType =>
  value.kind === "constant"
    ? decodeMidgardCekConstantWitness(value.witness).type
    : value.kind === "semanticConstant"
      ? decodeMidgardCekConstantTypeCbor(value.witness.typeCbor)
      : refuse("expected constant value");

/** `constant_payload_v1`: a revealed constant's payload. */
export const constantPayload = (value: MidgardCekDirectValueWitness): Data =>
  value.kind === "constant"
    ? decodeMidgardCekConstantWitness(value.witness).payload
    : refuse("expected revealed constant");

export const isType = (
  value: MidgardCekDirectValueWitness,
  kind: MidgardCekConstantType["kind"],
): boolean => constantType(value).kind === kind;

/** `integer_v1`. */
export const integer = (value: MidgardCekDirectValueWitness): bigint => {
  if (!isType(value, "integer")) refuse("expected integer");
  const payload = constantPayload(value);
  return payload instanceof DataI ? payload.int : refuse("integer payload");
};

/** `bytes_v1`. */
export const bytes = (value: MidgardCekDirectValueWitness): Uint8Array => {
  if (!isType(value, "bytes")) refuse("expected byte string");
  const payload = constantPayload(value);
  return payload instanceof DataB ? payload.bytes : refuse("bytes payload");
};

/** `boolean_v1`. */
export const boolean = (value: MidgardCekDirectValueWitness): boolean => {
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
export const listPayload = (
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
export const dataPayload = (value: MidgardCekDirectValueWitness): Data => {
  if (!isType(value, "data")) refuse("expected data");
  return constantPayload(value);
};

/** `is_inline_bytes_v1`: an inline byte-string constant. */
export const isInlineBytes = (value: MidgardCekDirectValueWitness): boolean =>
  value.kind === "constant" &&
  isType(value, "bytes") &&
  constantPayload(value) instanceof DataB;

/** `expect [..] = arguments`: exactly `count` arguments. */
export const exactly = (
  arguments_: readonly MidgardCekDirectValueWitness[],
  count: number,
): readonly MidgardCekDirectValueWitness[] =>
  arguments_.length === count ? arguments_ : refuse("argument count");

export const outsideInt64 = (value: bigint): boolean =>
  value < INT64_MIN || value > INT64_MAX;
