import { asBytes, decodeSingleCbor, encodeCbor } from "./cbor.js";
import { computeHash32, ensureHash32, type Hash32 } from "./hash.js";
import type {
  MidgardNativeTxBodyCanonical,
  MidgardNativeTxBodyCompact,
} from "./native.js";
import { asFixedArray, asSigned, asUnsigned } from "./native-validation.js";

type NativeTxBodyCompactValue = readonly [
  Hash32,
  Hash32,
  Hash32,
  bigint,
  bigint,
  bigint,
  Hash32,
  Hash32,
  Hash32,
  Hash32,
  Hash32,
  bigint,
];

type NativeTxBodyCanonicalValue = readonly [
  Buffer,
  Buffer,
  Buffer,
  bigint,
  bigint,
  bigint,
  Buffer,
  Buffer,
  Buffer,
  Hash32,
  Hash32,
  bigint,
];

const itemField = (fieldName: string, index: number): string =>
  `${fieldName}[${index}]`;

const hashItem = (
  value: readonly unknown[],
  index: number,
  fieldName: string,
): Hash32 => {
  const field = itemField(fieldName, index);
  return ensureHash32(asBytes(value[index], field), field);
};

const bytesItem = (
  value: readonly unknown[],
  index: number,
  fieldName: string,
): Buffer => asBytes(value[index], itemField(fieldName, index));

export const encodeNativeTxBodyCompactValue = (
  body: MidgardNativeTxBodyCompact,
): NativeTxBodyCompactValue => [
  ensureHash32(
    body.spendInputsHash,
    "transaction_body_compact.spend_inputs_hash",
  ),
  ensureHash32(
    body.referenceInputsHash,
    "transaction_body_compact.reference_inputs_hash",
  ),
  ensureHash32(body.outputsHash, "transaction_body_compact.outputs_hash"),
  asUnsigned(body.fee, "transaction_body_compact.fee"),
  asSigned(
    body.validityIntervalStart,
    "transaction_body_compact.validity_interval_start",
  ),
  asSigned(
    body.validityIntervalEnd,
    "transaction_body_compact.validity_interval_end",
  ),
  ensureHash32(
    body.requiredObserversHash,
    "transaction_body_compact.required_observers_hash",
  ),
  ensureHash32(
    body.requiredSignersHash,
    "transaction_body_compact.required_signers_hash",
  ),
  ensureHash32(body.mintHash, "transaction_body_compact.mint_hash"),
  ensureHash32(
    body.scriptIntegrityHash,
    "transaction_body_compact.script_integrity_hash",
  ),
  ensureHash32(
    body.auxiliaryDataHash,
    "transaction_body_compact.auxiliary_data_hash",
  ),
  asUnsigned(body.networkId, "transaction_body_compact.network_id"),
];

export const decodeNativeTxBodyCompactValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxBodyCompact => {
  const v = asFixedArray(value, 12, fieldName);
  return {
    spendInputsHash: hashItem(v, 0, fieldName),
    referenceInputsHash: hashItem(v, 1, fieldName),
    outputsHash: hashItem(v, 2, fieldName),
    fee: asUnsigned(v[3], `${fieldName}[3]`),
    validityIntervalStart: asSigned(v[4], `${fieldName}[4]`),
    validityIntervalEnd: asSigned(v[5], `${fieldName}[5]`),
    requiredObserversHash: hashItem(v, 6, fieldName),
    requiredSignersHash: hashItem(v, 7, fieldName),
    mintHash: hashItem(v, 8, fieldName),
    scriptIntegrityHash: hashItem(v, 9, fieldName),
    auxiliaryDataHash: hashItem(v, 10, fieldName),
    networkId: asUnsigned(v[11], `${fieldName}[11]`),
  };
};

export const encodeNativeTxBodyCanonicalValue = (
  body: MidgardNativeTxBodyCanonical,
): NativeTxBodyCanonicalValue => [
  Buffer.from(body.spendInputsPreimageCbor),
  Buffer.from(body.referenceInputsPreimageCbor),
  Buffer.from(body.outputsPreimageCbor),
  asUnsigned(body.fee, "transaction_body.fee"),
  asSigned(
    body.validityIntervalStart,
    "transaction_body.validity_interval_start",
  ),
  asSigned(body.validityIntervalEnd, "transaction_body.validity_interval_end"),
  Buffer.from(body.requiredObserversPreimageCbor),
  Buffer.from(body.requiredSignersPreimageCbor),
  Buffer.from(body.mintPreimageCbor),
  ensureHash32(
    body.scriptIntegrityHash,
    "transaction_body.script_integrity_hash",
  ),
  ensureHash32(body.auxiliaryDataHash, "transaction_body.auxiliary_data_hash"),
  asUnsigned(body.networkId, "transaction_body.network_id"),
];

export const decodeNativeTxBodyCanonicalValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxBodyCanonical => {
  const v = asFixedArray(value, 12, fieldName);
  return {
    spendInputsPreimageCbor: bytesItem(v, 0, fieldName),
    referenceInputsPreimageCbor: bytesItem(v, 1, fieldName),
    outputsPreimageCbor: bytesItem(v, 2, fieldName),
    fee: asUnsigned(v[3], `${fieldName}[3]`),
    validityIntervalStart: asSigned(v[4], `${fieldName}[4]`),
    validityIntervalEnd: asSigned(v[5], `${fieldName}[5]`),
    requiredObserversPreimageCbor: bytesItem(v, 6, fieldName),
    requiredSignersPreimageCbor: bytesItem(v, 7, fieldName),
    mintPreimageCbor: bytesItem(v, 8, fieldName),
    scriptIntegrityHash: hashItem(v, 9, fieldName),
    auxiliaryDataHash: hashItem(v, 10, fieldName),
    networkId: asUnsigned(v[11], `${fieldName}[11]`),
  };
};

export const deriveNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCanonical,
): MidgardNativeTxBodyCompact => ({
  spendInputsHash: computeHash32(body.spendInputsPreimageCbor),
  referenceInputsHash: computeHash32(body.referenceInputsPreimageCbor),
  outputsHash: computeHash32(body.outputsPreimageCbor),
  fee: body.fee,
  validityIntervalStart: body.validityIntervalStart,
  validityIntervalEnd: body.validityIntervalEnd,
  requiredObserversHash: computeHash32(body.requiredObserversPreimageCbor),
  requiredSignersHash: computeHash32(body.requiredSignersPreimageCbor),
  mintHash: computeHash32(body.mintPreimageCbor),
  scriptIntegrityHash: body.scriptIntegrityHash,
  auxiliaryDataHash: body.auxiliaryDataHash,
  networkId: body.networkId,
});

export const encodeNativeTxBodyCompactCbor = (
  body: MidgardNativeTxBodyCompact,
): Buffer => encodeCbor(encodeNativeTxBodyCompactValue(body));

export const decodeNativeTxBodyCompactCbor = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompact =>
  decodeNativeTxBodyCompactValue(decodeSingleCbor(bytes), "transaction_body");

export const encodeNativeTxBodyCanonicalCbor = (
  body: MidgardNativeTxBodyCanonical,
): Buffer => encodeCbor(encodeNativeTxBodyCanonicalValue(body));

export const decodeNativeTxBodyCanonicalCbor = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCanonical =>
  decodeNativeTxBodyCanonicalValue(decodeSingleCbor(bytes), "transaction_body");
