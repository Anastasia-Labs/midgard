import { asArray, asBigInt, asBytes } from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { ensureHash32, type Hash32 } from "./hash.js";
import { MIDGARD_NATIVE_TX_V1_VERSION } from "./native-constants.js";

export const MidgardTxValidityCodes = {
  TxIsValid: 0n,
  NonExistentInputUtxo: 1n,
  InvalidSignature: 2n,
  FailedScript: 3n,
  FeeTooLow: 4n,
  UnbalancedTx: 5n,
} as const;

export type MidgardTxValidity = keyof typeof MidgardTxValidityCodes;

const TX_VALIDITY_TO_CODE = new Map<MidgardTxValidity, bigint>(
  Object.entries(MidgardTxValidityCodes).map(([k, v]) => [
    k as MidgardTxValidity,
    v,
  ]),
);

const TX_CODE_TO_VALIDITY = new Map<bigint, MidgardTxValidity>(
  Object.entries(MidgardTxValidityCodes).map(([k, v]) => [
    v,
    k as MidgardTxValidity,
  ]),
);

export const asFixedArray = (
  value: unknown,
  expectedLength: number,
  fieldName: string,
): unknown[] => {
  const arr = asArray(value, fieldName);
  if (arr.length !== expectedLength) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must have exactly ${expectedLength} elements`,
      `length=${arr.length}`,
    );
  }
  return arr;
};

const itemField = (fieldName: string, index: number): string =>
  `${fieldName}[${index}]`;

export const hashItem = (
  value: readonly unknown[],
  index: number,
  fieldName: string,
): Hash32 => {
  const field = itemField(fieldName, index);
  return ensureHash32(asBytes(value[index], field), field);
};

export const bytesItem = (
  value: readonly unknown[],
  index: number,
  fieldName: string,
): Buffer => asBytes(value[index], itemField(fieldName, index));

export const asUnsigned = (value: unknown, fieldName: string): bigint => {
  const intValue = asBigInt(value, fieldName);
  if (intValue < 0n) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be unsigned`,
      intValue.toString(),
    );
  }
  return intValue;
};

export const asSigned = (value: unknown, fieldName: string): bigint => {
  if (typeof value === "bigint") {
    return value;
  }
  if (typeof value === "number" && Number.isInteger(value)) {
    return BigInt(value);
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    `${fieldName} must be an integer`,
  );
};

export const decodeVersion = (value: unknown, fieldName: string): bigint => {
  const version = asUnsigned(value, fieldName);
  if (version !== MIDGARD_NATIVE_TX_V1_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must equal ${MIDGARD_NATIVE_TX_V1_VERSION.toString()}`,
      `actual=${version.toString()}`,
    );
  }
  return version;
};

export const encodeValidityCode = (validity: MidgardTxValidity): bigint => {
  const code = TX_VALIDITY_TO_CODE.get(validity);
  if (code === undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Unsupported Midgard tx validity variant",
      validity,
    );
  }
  return code;
};

export const decodeValidityCode = (
  value: unknown,
  fieldName: string,
): MidgardTxValidity => {
  const code = asUnsigned(value, fieldName);
  const validity = TX_CODE_TO_VALIDITY.get(code);
  if (validity === undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Unsupported Midgard tx validity code",
      code.toString(),
    );
  }
  return validity;
};
