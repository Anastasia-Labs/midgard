import { CML } from "@lucid-evolution/lucid";
import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32, ensureHashMatch, ensureHash32 } from "./hash.js";
import type { Hash32 } from "./types.js";

export const MIDGARD_NATIVE_TX_VERSION = 1n;
export const MIDGARD_POSIX_TIME_NONE = -1n;
export const MIDGARD_NATIVE_NETWORK_ID_NONE = 255n;

export const MidgardTxValidityCodes = {
  TxIsValid: 0n,
  NonExistentInputUtxo: 1n,
  InvalidSignature: 2n,
  FailedScript: 3n,
  FeeTooLow: 4n,
  UnbalancedTx: 5n,
} as const;

export type MidgardTxValidity = keyof typeof MidgardTxValidityCodes;

export type MidgardNativeTxCompact = {
  readonly version: bigint;
  readonly transactionBodyHash: Hash32;
  readonly transactionWitnessSetHash: Hash32;
  readonly validity: MidgardTxValidity;
};

export type MidgardNativeTxBodyCompact = {
  readonly spendInputsRoot: Hash32;
  readonly referenceInputsRoot: Hash32;
  readonly outputsRoot: Hash32;
  readonly fee: bigint;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly requiredObserversRoot: Hash32;
  readonly requiredSignersRoot: Hash32;
  readonly mintRoot: Hash32;
  readonly scriptIntegrityHash: Hash32;
  readonly auxiliaryDataHash: Hash32;
  readonly networkId: bigint;
};

export type MidgardNativeTxWitnessSetCompact = {
  readonly addrTxWitsRoot: Hash32;
  readonly scriptTxWitsRoot: Hash32;
  readonly redeemerTxWitsRoot: Hash32;
};

export type MidgardNativeTxBodyFull = {
  readonly spendInputsRoot: Hash32;
  readonly spendInputsPreimageCbor: Buffer;
  readonly referenceInputsRoot: Hash32;
  readonly referenceInputsPreimageCbor: Buffer;
  readonly outputsRoot: Hash32;
  readonly outputsPreimageCbor: Buffer;
  readonly fee: bigint;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly requiredObserversRoot: Hash32;
  readonly requiredObserversPreimageCbor: Buffer;
  readonly requiredSignersRoot: Hash32;
  readonly requiredSignersPreimageCbor: Buffer;
  readonly mintRoot: Hash32;
  readonly mintPreimageCbor: Buffer;
  readonly scriptIntegrityHash: Hash32;
  readonly auxiliaryDataHash: Hash32;
  readonly networkId: bigint;
};

export type MidgardNativeTxWitnessSetFull = {
  readonly addrTxWitsRoot: Hash32;
  readonly addrTxWitsPreimageCbor: Buffer;
  readonly scriptTxWitsRoot: Hash32;
  readonly scriptTxWitsPreimageCbor: Buffer;
  readonly redeemerTxWitsRoot: Hash32;
  readonly redeemerTxWitsPreimageCbor: Buffer;
};

export type MidgardNativeTxFull = {
  readonly version: bigint;
  readonly compact: MidgardNativeTxCompact;
  readonly body: MidgardNativeTxBodyFull;
  readonly witnessSet: MidgardNativeTxWitnessSetFull;
};

export type MidgardNativeCodecOptions = {
  readonly enforceConsistency?: boolean;
};

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

const asFixedArray = (
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

const asUnsigned = (value: unknown, fieldName: string): bigint => {
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

const asSigned = (value: unknown, fieldName: string): bigint => {
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

const decodeVersion = (value: unknown, fieldName: string): bigint => {
  const version = asUnsigned(value, fieldName);
  if (version !== MIDGARD_NATIVE_TX_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `Unsupported Midgard native tx version`,
      `expected=${MIDGARD_NATIVE_TX_VERSION} actual=${version}`,
    );
  }
  return version;
};

const encodeValidityCode = (validity: MidgardTxValidity): bigint => {
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

const decodeValidityCode = (
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

const encodeNativeTxCompactValue = (
  tx: MidgardNativeTxCompact,
): readonly [bigint, Hash32, Hash32, bigint] => [
  decodeVersion(tx.version, "transaction_compact.version"),
  ensureHash32(tx.transactionBodyHash, "transaction_compact.transaction_body"),
  ensureHash32(
    tx.transactionWitnessSetHash,
    "transaction_compact.transaction_witness_set",
  ),
  encodeValidityCode(tx.validity),
];

const decodeNativeTxCompactValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxCompact => {
  const v = asFixedArray(value, 4, fieldName);
  return {
    version: decodeVersion(v[0], `${fieldName}[0]`),
    transactionBodyHash: ensureHash32(
      asBytes(v[1], `${fieldName}[1]`),
      `${fieldName}[1]`,
    ),
    transactionWitnessSetHash: ensureHash32(
      asBytes(v[2], `${fieldName}[2]`),
      `${fieldName}[2]`,
    ),
    validity: decodeValidityCode(v[3], `${fieldName}[3]`),
  };
};

const encodeNativeTxBodyCompactValue = (
  body: MidgardNativeTxBodyCompact,
): readonly [
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
] => [
  ensureHash32(body.spendInputsRoot, "transaction_body_compact.spend_inputs"),
  ensureHash32(
    body.referenceInputsRoot,
    "transaction_body_compact.reference_inputs",
  ),
  ensureHash32(body.outputsRoot, "transaction_body_compact.outputs"),
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
    body.requiredObserversRoot,
    "transaction_body_compact.required_observers",
  ),
  ensureHash32(
    body.requiredSignersRoot,
    "transaction_body_compact.required_signers",
  ),
  ensureHash32(body.mintRoot, "transaction_body_compact.mint"),
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

const decodeNativeTxBodyCompactValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxBodyCompact => {
  const v = asFixedArray(value, 12, fieldName);
  return {
    spendInputsRoot: ensureHash32(
      asBytes(v[0], `${fieldName}[0]`),
      `${fieldName}[0]`,
    ),
    referenceInputsRoot: ensureHash32(
      asBytes(v[1], `${fieldName}[1]`),
      `${fieldName}[1]`,
    ),
    outputsRoot: ensureHash32(asBytes(v[2], `${fieldName}[2]`), `${fieldName}[2]`),
    fee: asUnsigned(v[3], `${fieldName}[3]`),
    validityIntervalStart: asSigned(v[4], `${fieldName}[4]`),
    validityIntervalEnd: asSigned(v[5], `${fieldName}[5]`),
    requiredObserversRoot: ensureHash32(
      asBytes(v[6], `${fieldName}[6]`),
      `${fieldName}[6]`,
    ),
    requiredSignersRoot: ensureHash32(
      asBytes(v[7], `${fieldName}[7]`),
      `${fieldName}[7]`,
    ),
    mintRoot: ensureHash32(asBytes(v[8], `${fieldName}[8]`), `${fieldName}[8]`),
    scriptIntegrityHash: ensureHash32(
      asBytes(v[9], `${fieldName}[9]`),
      `${fieldName}[9]`,
    ),
    auxiliaryDataHash: ensureHash32(
      asBytes(v[10], `${fieldName}[10]`),
      `${fieldName}[10]`,
    ),
    networkId: asUnsigned(v[11], `${fieldName}[11]`),
  };
};

const encodeNativeTxWitnessSetCompactValue = (
  witnessSet: MidgardNativeTxWitnessSetCompact,
): readonly [Hash32, Hash32, Hash32] => [
  ensureHash32(
    witnessSet.addrTxWitsRoot,
    "transaction_witness_set_compact.addr_tx_wits",
  ),
  ensureHash32(
    witnessSet.scriptTxWitsRoot,
    "transaction_witness_set_compact.script_tx_wits",
  ),
  ensureHash32(
    witnessSet.redeemerTxWitsRoot,
    "transaction_witness_set_compact.redeemer_tx_wits",
  ),
];

const decodeNativeTxWitnessSetCompactValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxWitnessSetCompact => {
  const v = asFixedArray(value, 3, fieldName);
  return {
    addrTxWitsRoot: ensureHash32(
      asBytes(v[0], `${fieldName}[0]`),
      `${fieldName}[0]`,
    ),
    scriptTxWitsRoot: ensureHash32(
      asBytes(v[1], `${fieldName}[1]`),
      `${fieldName}[1]`,
    ),
    redeemerTxWitsRoot: ensureHash32(
      asBytes(v[2], `${fieldName}[2]`),
      `${fieldName}[2]`,
    ),
  };
};

const encodeNativeTxBodyFullValue = (
  body: MidgardNativeTxBodyFull,
): readonly [
  Hash32,
  Buffer,
  Hash32,
  Buffer,
  Hash32,
  Buffer,
  bigint,
  bigint,
  bigint,
  Hash32,
  Buffer,
  Hash32,
  Buffer,
  Hash32,
  Buffer,
  Hash32,
  Hash32,
  bigint,
] => [
  ensureHash32(body.spendInputsRoot, "transaction_body_full.spend_inputs_root"),
  Buffer.from(body.spendInputsPreimageCbor),
  ensureHash32(
    body.referenceInputsRoot,
    "transaction_body_full.reference_inputs_root",
  ),
  Buffer.from(body.referenceInputsPreimageCbor),
  ensureHash32(body.outputsRoot, "transaction_body_full.outputs_root"),
  Buffer.from(body.outputsPreimageCbor),
  asUnsigned(body.fee, "transaction_body_full.fee"),
  asSigned(
    body.validityIntervalStart,
    "transaction_body_full.validity_interval_start",
  ),
  asSigned(
    body.validityIntervalEnd,
    "transaction_body_full.validity_interval_end",
  ),
  ensureHash32(
    body.requiredObserversRoot,
    "transaction_body_full.required_observers_root",
  ),
  Buffer.from(body.requiredObserversPreimageCbor),
  ensureHash32(
    body.requiredSignersRoot,
    "transaction_body_full.required_signers_root",
  ),
  Buffer.from(body.requiredSignersPreimageCbor),
  ensureHash32(body.mintRoot, "transaction_body_full.mint_root"),
  Buffer.from(body.mintPreimageCbor),
  ensureHash32(
    body.scriptIntegrityHash,
    "transaction_body_full.script_integrity_hash",
  ),
  ensureHash32(
    body.auxiliaryDataHash,
    "transaction_body_full.auxiliary_data_hash",
  ),
  asUnsigned(body.networkId, "transaction_body_full.network_id"),
];

const decodeNativeTxBodyFullValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxBodyFull => {
  const v = asFixedArray(value, 18, fieldName);
  return {
    spendInputsRoot: ensureHash32(
      asBytes(v[0], `${fieldName}[0]`),
      `${fieldName}[0]`,
    ),
    spendInputsPreimageCbor: Buffer.from(asBytes(v[1], `${fieldName}[1]`)),
    referenceInputsRoot: ensureHash32(
      asBytes(v[2], `${fieldName}[2]`),
      `${fieldName}[2]`,
    ),
    referenceInputsPreimageCbor: Buffer.from(asBytes(v[3], `${fieldName}[3]`)),
    outputsRoot: ensureHash32(asBytes(v[4], `${fieldName}[4]`), `${fieldName}[4]`),
    outputsPreimageCbor: Buffer.from(asBytes(v[5], `${fieldName}[5]`)),
    fee: asUnsigned(v[6], `${fieldName}[6]`),
    validityIntervalStart: asSigned(v[7], `${fieldName}[7]`),
    validityIntervalEnd: asSigned(v[8], `${fieldName}[8]`),
    requiredObserversRoot: ensureHash32(
      asBytes(v[9], `${fieldName}[9]`),
      `${fieldName}[9]`,
    ),
    requiredObserversPreimageCbor: Buffer.from(asBytes(v[10], `${fieldName}[10]`)),
    requiredSignersRoot: ensureHash32(
      asBytes(v[11], `${fieldName}[11]`),
      `${fieldName}[11]`,
    ),
    requiredSignersPreimageCbor: Buffer.from(asBytes(v[12], `${fieldName}[12]`)),
    mintRoot: ensureHash32(asBytes(v[13], `${fieldName}[13]`), `${fieldName}[13]`),
    mintPreimageCbor: Buffer.from(asBytes(v[14], `${fieldName}[14]`)),
    scriptIntegrityHash: ensureHash32(
      asBytes(v[15], `${fieldName}[15]`),
      `${fieldName}[15]`,
    ),
    auxiliaryDataHash: ensureHash32(
      asBytes(v[16], `${fieldName}[16]`),
      `${fieldName}[16]`,
    ),
    networkId: asUnsigned(v[17], `${fieldName}[17]`),
  };
};

const encodeNativeTxWitnessSetFullValue = (
  witnessSet: MidgardNativeTxWitnessSetFull,
): readonly [Hash32, Buffer, Hash32, Buffer, Hash32, Buffer] => [
  ensureHash32(
    witnessSet.addrTxWitsRoot,
    "transaction_witness_set_full.addr_tx_wits_root",
  ),
  Buffer.from(witnessSet.addrTxWitsPreimageCbor),
  ensureHash32(
    witnessSet.scriptTxWitsRoot,
    "transaction_witness_set_full.script_tx_wits_root",
  ),
  Buffer.from(witnessSet.scriptTxWitsPreimageCbor),
  ensureHash32(
    witnessSet.redeemerTxWitsRoot,
    "transaction_witness_set_full.redeemer_tx_wits_root",
  ),
  Buffer.from(witnessSet.redeemerTxWitsPreimageCbor),
];

const decodeNativeTxWitnessSetFullValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxWitnessSetFull => {
  const v = asFixedArray(value, 6, fieldName);
  return {
    addrTxWitsRoot: ensureHash32(
      asBytes(v[0], `${fieldName}[0]`),
      `${fieldName}[0]`,
    ),
    addrTxWitsPreimageCbor: Buffer.from(asBytes(v[1], `${fieldName}[1]`)),
    scriptTxWitsRoot: ensureHash32(
      asBytes(v[2], `${fieldName}[2]`),
      `${fieldName}[2]`,
    ),
    scriptTxWitsPreimageCbor: Buffer.from(asBytes(v[3], `${fieldName}[3]`)),
    redeemerTxWitsRoot: ensureHash32(
      asBytes(v[4], `${fieldName}[4]`),
      `${fieldName}[4]`,
    ),
    redeemerTxWitsPreimageCbor: Buffer.from(asBytes(v[5], `${fieldName}[5]`)),
  };
};

export const deriveMidgardNativeTxBodyCompactFromFull = (
  body: MidgardNativeTxBodyFull,
): MidgardNativeTxBodyCompact => ({
  spendInputsRoot: body.spendInputsRoot,
  referenceInputsRoot: body.referenceInputsRoot,
  outputsRoot: body.outputsRoot,
  fee: body.fee,
  validityIntervalStart: body.validityIntervalStart,
  validityIntervalEnd: body.validityIntervalEnd,
  requiredObserversRoot: body.requiredObserversRoot,
  requiredSignersRoot: body.requiredSignersRoot,
  mintRoot: body.mintRoot,
  scriptIntegrityHash: body.scriptIntegrityHash,
  auxiliaryDataHash: body.auxiliaryDataHash,
  networkId: body.networkId,
});

export const deriveMidgardNativeTxWitnessSetCompactFromFull = (
  witnessSet: MidgardNativeTxWitnessSetFull,
): MidgardNativeTxWitnessSetCompact => ({
  addrTxWitsRoot: witnessSet.addrTxWitsRoot,
  scriptTxWitsRoot: witnessSet.scriptTxWitsRoot,
  redeemerTxWitsRoot: witnessSet.redeemerTxWitsRoot,
});

export const deriveMidgardNativeTxCompact = (
  body: MidgardNativeTxBodyFull,
  witnessSet: MidgardNativeTxWitnessSetFull,
  validity: MidgardTxValidity,
): MidgardNativeTxCompact => {
  const bodyCompact = deriveMidgardNativeTxBodyCompactFromFull(body);
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompactFromFull(
    witnessSet,
  );
  return {
    version: MIDGARD_NATIVE_TX_VERSION,
    transactionBodyHash: computeHash32(
      encodeMidgardNativeTxBodyCompact(bodyCompact),
    ),
    transactionWitnessSetHash: computeHash32(
      encodeMidgardNativeTxWitnessSetCompact(witnessCompact),
    ),
    validity,
  };
};

export const verifyMidgardNativeTxFullConsistency = (
  tx: MidgardNativeTxFull,
): void => {
  if (tx.version !== MIDGARD_NATIVE_TX_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Unsupported Midgard native tx version",
      `${tx.version}`,
    );
  }

  const bodyCompact = deriveMidgardNativeTxBodyCompactFromFull(tx.body);
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompactFromFull(
    tx.witnessSet,
  );

  const ensureRootMatchesPreimage = (
    root: Uint8Array,
    preimageCbor: Uint8Array,
    fieldName: string,
  ) => ensureHashMatch(root, preimageCbor, fieldName);

  ensureRootMatchesPreimage(
    tx.body.spendInputsRoot,
    tx.body.spendInputsPreimageCbor,
    "transaction_body_full.spend_inputs_root",
  );
  ensureRootMatchesPreimage(
    tx.body.referenceInputsRoot,
    tx.body.referenceInputsPreimageCbor,
    "transaction_body_full.reference_inputs_root",
  );
  ensureRootMatchesPreimage(
    tx.body.outputsRoot,
    tx.body.outputsPreimageCbor,
    "transaction_body_full.outputs_root",
  );
  ensureRootMatchesPreimage(
    tx.body.requiredObserversRoot,
    tx.body.requiredObserversPreimageCbor,
    "transaction_body_full.required_observers_root",
  );
  ensureRootMatchesPreimage(
    tx.body.requiredSignersRoot,
    tx.body.requiredSignersPreimageCbor,
    "transaction_body_full.required_signers_root",
  );
  ensureRootMatchesPreimage(
    tx.body.mintRoot,
    tx.body.mintPreimageCbor,
    "transaction_body_full.mint_root",
  );

  ensureRootMatchesPreimage(
    tx.witnessSet.addrTxWitsRoot,
    tx.witnessSet.addrTxWitsPreimageCbor,
    "transaction_witness_set_full.addr_tx_wits_root",
  );
  ensureRootMatchesPreimage(
    tx.witnessSet.scriptTxWitsRoot,
    tx.witnessSet.scriptTxWitsPreimageCbor,
    "transaction_witness_set_full.script_tx_wits_root",
  );
  ensureRootMatchesPreimage(
    tx.witnessSet.redeemerTxWitsRoot,
    tx.witnessSet.redeemerTxWitsPreimageCbor,
    "transaction_witness_set_full.redeemer_tx_wits_root",
  );

  const encodedBodyCompact = encodeMidgardNativeTxBodyCompact(bodyCompact);
  const encodedWitnessCompact =
    encodeMidgardNativeTxWitnessSetCompact(witnessCompact);

  ensureHashMatch(
    tx.compact.transactionBodyHash,
    encodedBodyCompact,
    "transaction_compact.transaction_body_hash",
  );
  ensureHashMatch(
    tx.compact.transactionWitnessSetHash,
    encodedWitnessCompact,
    "transaction_compact.transaction_witness_set_hash",
  );
};

export const encodeMidgardNativeTxCompact = (
  tx: MidgardNativeTxCompact,
): Buffer => encodeCbor(encodeNativeTxCompactValue(tx));

export const decodeMidgardNativeTxCompact = (
  bytes: Uint8Array,
): MidgardNativeTxCompact =>
  decodeNativeTxCompactValue(decodeSingleCbor(bytes), "transaction_compact");

export const encodeMidgardNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCompact,
): Buffer => encodeCbor(encodeNativeTxBodyCompactValue(body));

export const decodeMidgardNativeTxBodyCompact = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompact =>
  decodeNativeTxBodyCompactValue(decodeSingleCbor(bytes), "transaction_body");

export const encodeMidgardNativeTxWitnessSetCompact = (
  witnessSet: MidgardNativeTxWitnessSetCompact,
): Buffer => encodeCbor(encodeNativeTxWitnessSetCompactValue(witnessSet));

export const decodeMidgardNativeTxWitnessSetCompact = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact =>
  decodeNativeTxWitnessSetCompactValue(
    decodeSingleCbor(bytes),
    "transaction_witness_set",
  );

export const encodeMidgardNativeTxBodyFull = (
  body: MidgardNativeTxBodyFull,
): Buffer => encodeCbor(encodeNativeTxBodyFullValue(body));

export const decodeMidgardNativeTxBodyFull = (
  bytes: Uint8Array,
): MidgardNativeTxBodyFull =>
  decodeNativeTxBodyFullValue(decodeSingleCbor(bytes), "transaction_body_full");

export const encodeMidgardNativeTxWitnessSetFull = (
  witnessSet: MidgardNativeTxWitnessSetFull,
): Buffer => encodeCbor(encodeNativeTxWitnessSetFullValue(witnessSet));

export const decodeMidgardNativeTxWitnessSetFull = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetFull =>
  decodeNativeTxWitnessSetFullValue(
    decodeSingleCbor(bytes),
    "transaction_witness_set_full",
  );

export const encodeMidgardNativeTxFull = (
  tx: MidgardNativeTxFull,
  options: MidgardNativeCodecOptions = {},
): Buffer => {
  if (options.enforceConsistency !== false) {
    verifyMidgardNativeTxFullConsistency(tx);
  }
  return encodeCbor([
    decodeVersion(tx.version, "transaction_full.version"),
    encodeNativeTxCompactValue(tx.compact),
    encodeNativeTxBodyFullValue(tx.body),
    encodeNativeTxWitnessSetFullValue(tx.witnessSet),
  ]);
};

export const decodeMidgardNativeTxFull = (
  bytes: Uint8Array,
  options: MidgardNativeCodecOptions = {},
): MidgardNativeTxFull => {
  const decoded = decodeSingleCbor(bytes);
  const v = asFixedArray(decoded, 4, "transaction_full");
  const tx: MidgardNativeTxFull = {
    version: decodeVersion(v[0], "transaction_full[0]"),
    compact: decodeNativeTxCompactValue(v[1], "transaction_full[1]"),
    body: decodeNativeTxBodyFullValue(v[2], "transaction_full[2]"),
    witnessSet: decodeNativeTxWitnessSetFullValue(v[3], "transaction_full[3]"),
  };
  if (options.enforceConsistency !== false) {
    verifyMidgardNativeTxFullConsistency(tx);
  }
  return tx;
};

export const computeMidgardNativeTxIdFromCompact = (
  compact: MidgardNativeTxCompact,
): Buffer => computeHash32(encodeMidgardNativeTxCompact(compact));

export const computeMidgardNativeTxIdFromFull = (
  tx: MidgardNativeTxFull,
): Buffer => computeMidgardNativeTxIdFromCompact(tx.compact);

export const decodeMidgardNativeByteListPreimage = (
  preimageCbor: Uint8Array,
  fieldName = "preimage_cbor",
): Buffer[] => {
  const decoded = decodeSingleCbor(preimageCbor);
  const arr = asArray(decoded, fieldName);
  return arr.map((item, index) =>
    Buffer.from(asBytes(item, `${fieldName}[${index}]`)),
  );
};

const parseCardanoTx = (
  txBytes: Uint8Array,
): InstanceType<typeof CML.Transaction> => {
  try {
    return CML.Transaction.from_cbor_bytes(txBytes);
  } catch (e) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Invalid Cardano transaction bytes",
      String(e),
    );
  }
};

const emptyHash32 = (): Hash32 => computeHash32(encodeCbor(null));

type CmlCollectionLike = {
  len(): number;
  get(index: number): unknown;
};

type CmlMintLike = {
  policy_count(): number;
};

const asCmlCallable = (
  value: unknown,
  methodName: "to_cbor_bytes" | "to_raw_bytes",
): (() => Uint8Array) | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const method = (value as Record<string, unknown>)[methodName];
  if (typeof method !== "function") {
    return undefined;
  }
  return (method as () => Uint8Array).bind(value);
};

const cmlObjectToBytes = (value: unknown, fieldName: string): Buffer => {
  const toCbor = asCmlCallable(value, "to_cbor_bytes");
  if (toCbor !== undefined) {
    return Buffer.from(toCbor());
  }
  const toRaw = asCmlCallable(value, "to_raw_bytes");
  if (toRaw !== undefined) {
    return Buffer.from(toRaw());
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    `Cannot serialize CML value in ${fieldName}`,
  );
};

const asCollectionLike = (value: unknown): CmlCollectionLike | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const maybeLen = (value as Record<string, unknown>).len;
  const maybeGet = (value as Record<string, unknown>).get;
  if (typeof maybeLen === "function" && typeof maybeGet === "function") {
    return value as CmlCollectionLike;
  }
  return undefined;
};

const asMintLike = (value: unknown): CmlMintLike | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const maybePolicyCount = (value as Record<string, unknown>).policy_count;
  if (typeof maybePolicyCount !== "function") {
    return undefined;
  }
  return value as CmlMintLike;
};

const cmlCollectionToPreimageCbor = (
  collection: CmlCollectionLike | undefined,
  fieldName: string,
): Buffer => {
  if (collection === undefined) {
    return encodeCbor([]);
  }
  const entries: Buffer[] = [];
  for (let i = 0; i < collection.len(); i++) {
    entries.push(cmlObjectToBytes(collection.get(i), `${fieldName}[${i}]`));
  }
  return encodeCbor(entries);
};

const cmlAnyToPreimageCbor = (value: unknown, fieldName: string): Buffer => {
  if (value === undefined) {
    return encodeCbor([]);
  }
  const mint = asMintLike(value);
  if (mint !== undefined) {
    if (mint.policy_count() === 0) {
      return encodeCbor([]);
    }
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Cardano tx cannot be converted to Midgard native format without dropping fields",
      fieldName,
    );
  }
  const toCbor = asCmlCallable(value, "to_cbor_bytes");
  if (toCbor !== undefined) {
    return Buffer.from(toCbor());
  }
  const collection = asCollectionLike(value);
  if (collection !== undefined) {
    return cmlCollectionToPreimageCbor(collection, fieldName);
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    `Cannot serialize CML container in ${fieldName}`,
  );
};

const failLossyConversion = (fieldName: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
    "Cardano tx cannot be converted to Midgard native format without dropping fields",
    fieldName,
  );
};

const hasAnyCmlEntries = (value: unknown): boolean => {
  const collection = asCollectionLike(value);
  return collection !== undefined && collection.len() > 0;
};

const assertCardanoTxConvertibleToNative = (
  tx: InstanceType<typeof CML.Transaction>,
): void => {
  const txBody = tx.body();
  const txWitnessSet = tx.witness_set();

  if (tx.auxiliary_data() !== undefined) {
    failLossyConversion("auxiliary_data");
  }

  if (hasAnyCmlEntries(txBody.certs())) {
    failLossyConversion("certificates");
  }

  const withdrawals = txBody.withdrawals();
  if (withdrawals !== undefined && withdrawals.len() > 0) {
    failLossyConversion("withdrawals");
  }
  const mint = asMintLike(txBody.mint());
  if (mint !== undefined && mint.policy_count() > 0) {
    failLossyConversion("mint");
  }

  if (hasAnyCmlEntries(txBody.collateral_inputs())) {
    failLossyConversion("collateral_inputs");
  }
  if (txBody.collateral_return() !== undefined) {
    failLossyConversion("collateral_return");
  }
  if (txBody.total_collateral() !== undefined) {
    failLossyConversion("total_collateral");
  }
  if (txBody.voting_procedures() !== undefined) {
    failLossyConversion("voting_procedures");
  }
  if (txBody.proposal_procedures() !== undefined) {
    failLossyConversion("proposal_procedures");
  }
  if (txBody.current_treasury_value() !== undefined) {
    failLossyConversion("current_treasury_value");
  }
  if (txBody.donation() !== undefined) {
    failLossyConversion("donation");
  }

  if (hasAnyCmlEntries(txWitnessSet.bootstrap_witnesses())) {
    failLossyConversion("bootstrap_witnesses");
  }
  if (hasAnyCmlEntries(txWitnessSet.plutus_v1_scripts())) {
    failLossyConversion("plutus_v1_scripts");
  }
  if (hasAnyCmlEntries(txWitnessSet.plutus_v2_scripts())) {
    failLossyConversion("plutus_v2_scripts");
  }
  if (hasAnyCmlEntries(txWitnessSet.plutus_v3_scripts())) {
    failLossyConversion("plutus_v3_scripts");
  }
  if (hasAnyCmlEntries(txWitnessSet.plutus_datums())) {
    failLossyConversion("plutus_datums");
  }
  if (txWitnessSet.redeemers() !== undefined) {
    failLossyConversion("redeemers");
  }
};

export const cardanoTxBytesToMidgardNativeTxFull = (
  cardanoTxBytes: Uint8Array,
): MidgardNativeTxFull => {
  const tx = parseCardanoTx(cardanoTxBytes);
  assertCardanoTxConvertibleToNative(tx);
  const txBody = tx.body();
  const txWitnessSet = tx.witness_set();
  const txOutputs = txBody.outputs();

  const spendInputsPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txBody.inputs()),
    "transaction_body.inputs",
  );
  const referenceInputsPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txBody.reference_inputs()),
    "transaction_body.reference_inputs",
  );
  const outputsPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txOutputs),
    "transaction_body.outputs",
  );
  const requiredObserversPreimageCbor = encodeCbor([]);
  const requiredSignersPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txBody.required_signers()),
    "transaction_body.required_signers",
  );
  const mintPreimageCbor = cmlAnyToPreimageCbor(
    txBody.mint(),
    "transaction_body.mint",
  );

  const addrTxWitsPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txWitnessSet.vkeywitnesses()),
    "transaction_witness_set.vkeywitnesses",
  );
  const scriptTxWitsPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txWitnessSet.native_scripts()),
    "transaction_witness_set.native_scripts",
  );
  const redeemerTxWitsPreimageCbor = cmlAnyToPreimageCbor(
    txWitnessSet.redeemers(),
    "transaction_witness_set.redeemers",
  );

  const scriptDataHash = txBody.script_data_hash();
  const auxDataHash = txBody.auxiliary_data_hash();
  const network = txBody.network_id();
  const encodedNetworkId =
    network === undefined
      ? MIDGARD_NATIVE_NETWORK_ID_NONE
      : BigInt(network.network());

  const body: MidgardNativeTxBodyFull = {
    spendInputsRoot: computeHash32(spendInputsPreimageCbor),
    spendInputsPreimageCbor,
    referenceInputsRoot: computeHash32(referenceInputsPreimageCbor),
    referenceInputsPreimageCbor,
    outputsRoot: computeHash32(outputsPreimageCbor),
    outputsPreimageCbor,
    fee: txBody.fee(),
    validityIntervalStart:
      txBody.validity_interval_start() ?? MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: txBody.ttl() ?? MIDGARD_POSIX_TIME_NONE,
    requiredObserversRoot: computeHash32(requiredObserversPreimageCbor),
    requiredObserversPreimageCbor,
    requiredSignersRoot: computeHash32(requiredSignersPreimageCbor),
    requiredSignersPreimageCbor,
    mintRoot: computeHash32(mintPreimageCbor),
    mintPreimageCbor,
    scriptIntegrityHash:
      scriptDataHash === undefined
        ? emptyHash32()
        : ensureHash32(
            Buffer.from(scriptDataHash.to_raw_bytes()),
            "script_integrity_hash",
          ),
    auxiliaryDataHash:
      auxDataHash === undefined
        ? emptyHash32()
        : ensureHash32(
            Buffer.from(auxDataHash.to_raw_bytes()),
            "auxiliary_data_hash",
          ),
    networkId: encodedNetworkId,
  };

  const witnessSet: MidgardNativeTxWitnessSetFull = {
    addrTxWitsRoot: computeHash32(addrTxWitsPreimageCbor),
    addrTxWitsPreimageCbor,
    scriptTxWitsRoot: computeHash32(scriptTxWitsPreimageCbor),
    scriptTxWitsPreimageCbor,
    redeemerTxWitsRoot: computeHash32(redeemerTxWitsPreimageCbor),
    redeemerTxWitsPreimageCbor,
  };

  const compact = deriveMidgardNativeTxCompact(
    body,
    witnessSet,
    tx.is_valid() ? "TxIsValid" : "FailedScript",
  );

  return {
    version: MIDGARD_NATIVE_TX_VERSION,
    compact,
    body,
    witnessSet,
  };
};

export const cardanoTxBytesToMidgardNativeTxFullBytes = (
  cardanoTxBytes: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxFull(
    cardanoTxBytesToMidgardNativeTxFull(cardanoTxBytes),
  );
