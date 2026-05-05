import { CML } from "@lucid-evolution/lucid";
import {
  asArray,
  asBigInt,
  asBytes,
  asMap,
  decodeSingleCbor,
  encodeCbor,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import {
  computeHash32,
  ensureHashMatch,
  ensureHash32,
  type Hash32,
} from "./hash.js";
import {
  encodeMidgardTxOutput,
  decodeMidgardTxOutput,
  type MidgardTxOutput,
} from "./output.js";
import { decodeMidgardAddressBytes } from "./address.js";
import {
  encodeMidgardVersionedScriptListPreimage,
  decodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "./versioned-script.js";
import { decodeMidgardNativeScript } from "./native-script.js";
import { type MidgardValue } from "./value.js";

export const MIDGARD_NATIVE_TX_VERSION = 1n;
export const MIDGARD_POSIX_TIME_NONE = -1n;
export const MIDGARD_NATIVE_NETWORK_ID_NONE = 255n;
export const EMPTY_CBOR_LIST = encodeCbor([]);
export const EMPTY_CBOR_NULL = encodeCbor(null);
export const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);

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

export type MidgardNativeTxBodyCanonical = {
  readonly spendInputsPreimageCbor: Buffer;
  readonly referenceInputsPreimageCbor: Buffer;
  readonly outputsPreimageCbor: Buffer;
  readonly fee: bigint;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly requiredObserversPreimageCbor: Buffer;
  readonly requiredSignersPreimageCbor: Buffer;
  readonly mintPreimageCbor: Buffer;
  readonly scriptIntegrityHash: Hash32;
  readonly auxiliaryDataHash: Hash32;
  readonly networkId: bigint;
};

export type MidgardNativeTxWitnessSetCanonical = {
  readonly addrTxWitsPreimageCbor: Buffer;
  readonly scriptTxWitsPreimageCbor: Buffer;
  readonly redeemerTxWitsPreimageCbor: Buffer;
};

export type MidgardNativeTxCanonical = {
  readonly version: bigint;
  readonly validity: MidgardTxValidity;
  readonly body: MidgardNativeTxBodyCanonical;
  readonly witnessSet: MidgardNativeTxWitnessSetCanonical;
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
      `supported=${MIDGARD_NATIVE_TX_VERSION} actual=${version}`,
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
    outputsRoot: ensureHash32(
      asBytes(v[2], `${fieldName}[2]`),
      `${fieldName}[2]`,
    ),
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
  _version: bigint,
  witnessSet: MidgardNativeTxWitnessSetCompact,
): readonly Hash32[] => {
  return [
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
};

const decodeNativeTxWitnessSetCompactValue = (
  value: unknown,
  fieldName: string,
  _version: bigint,
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
    outputsRoot: ensureHash32(
      asBytes(v[4], `${fieldName}[4]`),
      `${fieldName}[4]`,
    ),
    outputsPreimageCbor: Buffer.from(asBytes(v[5], `${fieldName}[5]`)),
    fee: asUnsigned(v[6], `${fieldName}[6]`),
    validityIntervalStart: asSigned(v[7], `${fieldName}[7]`),
    validityIntervalEnd: asSigned(v[8], `${fieldName}[8]`),
    requiredObserversRoot: ensureHash32(
      asBytes(v[9], `${fieldName}[9]`),
      `${fieldName}[9]`,
    ),
    requiredObserversPreimageCbor: Buffer.from(
      asBytes(v[10], `${fieldName}[10]`),
    ),
    requiredSignersRoot: ensureHash32(
      asBytes(v[11], `${fieldName}[11]`),
      `${fieldName}[11]`,
    ),
    requiredSignersPreimageCbor: Buffer.from(
      asBytes(v[12], `${fieldName}[12]`),
    ),
    mintRoot: ensureHash32(
      asBytes(v[13], `${fieldName}[13]`),
      `${fieldName}[13]`,
    ),
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
  _version: bigint,
  witnessSet: MidgardNativeTxWitnessSetFull,
): readonly (Hash32 | Buffer)[] => {
  return [
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
};

const decodeNativeTxWitnessSetFullValue = (
  value: unknown,
  fieldName: string,
  _version: bigint,
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

const deriveMidgardNativeTxBodyCompactFromCanonical = (
  body: MidgardNativeTxBodyCanonical,
): MidgardNativeTxBodyCompact => ({
  spendInputsRoot: computeHash32(body.spendInputsPreimageCbor),
  referenceInputsRoot: computeHash32(body.referenceInputsPreimageCbor),
  outputsRoot: computeHash32(body.outputsPreimageCbor),
  fee: body.fee,
  validityIntervalStart: body.validityIntervalStart,
  validityIntervalEnd: body.validityIntervalEnd,
  requiredObserversRoot: computeHash32(body.requiredObserversPreimageCbor),
  requiredSignersRoot: computeHash32(body.requiredSignersPreimageCbor),
  mintRoot: computeHash32(body.mintPreimageCbor),
  scriptIntegrityHash: body.scriptIntegrityHash,
  auxiliaryDataHash: body.auxiliaryDataHash,
  networkId: body.networkId,
});

const deriveMidgardNativeTxWitnessSetCompactFromCanonical = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
  version = MIDGARD_NATIVE_TX_VERSION,
): MidgardNativeTxWitnessSetCompact => {
  return {
    addrTxWitsRoot: computeHash32(witnessSet.addrTxWitsPreimageCbor),
    scriptTxWitsRoot: computeHash32(witnessSet.scriptTxWitsPreimageCbor),
    redeemerTxWitsRoot: computeHash32(witnessSet.redeemerTxWitsPreimageCbor),
  };
};

const toBodyCanonicalFromFull = (
  body: MidgardNativeTxBodyFull,
): MidgardNativeTxBodyCanonical => ({
  spendInputsPreimageCbor: Buffer.from(body.spendInputsPreimageCbor),
  referenceInputsPreimageCbor: Buffer.from(body.referenceInputsPreimageCbor),
  outputsPreimageCbor: Buffer.from(body.outputsPreimageCbor),
  fee: body.fee,
  validityIntervalStart: body.validityIntervalStart,
  validityIntervalEnd: body.validityIntervalEnd,
  requiredObserversPreimageCbor: Buffer.from(
    body.requiredObserversPreimageCbor,
  ),
  requiredSignersPreimageCbor: Buffer.from(body.requiredSignersPreimageCbor),
  mintPreimageCbor: Buffer.from(body.mintPreimageCbor),
  scriptIntegrityHash: ensureHash32(
    body.scriptIntegrityHash,
    "transaction_body_full.script_integrity_hash",
  ),
  auxiliaryDataHash: ensureHash32(
    body.auxiliaryDataHash,
    "transaction_body_full.auxiliary_data_hash",
  ),
  networkId: body.networkId,
});

const toWitnessSetCanonicalFromFull = (
  witnessSet: MidgardNativeTxWitnessSetFull,
): MidgardNativeTxWitnessSetCanonical => ({
  addrTxWitsPreimageCbor: Buffer.from(witnessSet.addrTxWitsPreimageCbor),
  scriptTxWitsPreimageCbor: Buffer.from(witnessSet.scriptTxWitsPreimageCbor),
  redeemerTxWitsPreimageCbor: Buffer.from(
    witnessSet.redeemerTxWitsPreimageCbor,
  ),
});

export const deriveMidgardNativeTxBodyCompactFromFull = (
  body: MidgardNativeTxBodyFull,
): MidgardNativeTxBodyCompact =>
  deriveMidgardNativeTxBodyCompactFromCanonical(toBodyCanonicalFromFull(body));

export const deriveMidgardNativeTxWitnessSetCompactFromFull = (
  witnessSet: MidgardNativeTxWitnessSetFull,
  version = MIDGARD_NATIVE_TX_VERSION,
): MidgardNativeTxWitnessSetCompact =>
  deriveMidgardNativeTxWitnessSetCompactFromCanonical(
    toWitnessSetCanonicalFromFull(witnessSet),
    version,
  );

export const materializeMidgardNativeTxBodyFull = (
  body: MidgardNativeTxBodyCanonical,
): MidgardNativeTxBodyFull => {
  const compact = deriveMidgardNativeTxBodyCompactFromCanonical(body);
  return {
    spendInputsRoot: compact.spendInputsRoot,
    spendInputsPreimageCbor: Buffer.from(body.spendInputsPreimageCbor),
    referenceInputsRoot: compact.referenceInputsRoot,
    referenceInputsPreimageCbor: Buffer.from(body.referenceInputsPreimageCbor),
    outputsRoot: compact.outputsRoot,
    outputsPreimageCbor: Buffer.from(body.outputsPreimageCbor),
    fee: body.fee,
    validityIntervalStart: body.validityIntervalStart,
    validityIntervalEnd: body.validityIntervalEnd,
    requiredObserversRoot: compact.requiredObserversRoot,
    requiredObserversPreimageCbor: Buffer.from(
      body.requiredObserversPreimageCbor,
    ),
    requiredSignersRoot: compact.requiredSignersRoot,
    requiredSignersPreimageCbor: Buffer.from(body.requiredSignersPreimageCbor),
    mintRoot: compact.mintRoot,
    mintPreimageCbor: Buffer.from(body.mintPreimageCbor),
    scriptIntegrityHash: ensureHash32(
      body.scriptIntegrityHash,
      "transaction_body_canonical.script_integrity_hash",
    ),
    auxiliaryDataHash: ensureHash32(
      body.auxiliaryDataHash,
      "transaction_body_canonical.auxiliary_data_hash",
    ),
    networkId: body.networkId,
  };
};

export const materializeMidgardNativeTxWitnessSetFull = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
  version = MIDGARD_NATIVE_TX_VERSION,
): MidgardNativeTxWitnessSetFull => {
  const compact = deriveMidgardNativeTxWitnessSetCompactFromCanonical(
    witnessSet,
    version,
  );
  return {
    addrTxWitsRoot: compact.addrTxWitsRoot,
    addrTxWitsPreimageCbor: Buffer.from(witnessSet.addrTxWitsPreimageCbor),
    scriptTxWitsRoot: compact.scriptTxWitsRoot,
    scriptTxWitsPreimageCbor: Buffer.from(witnessSet.scriptTxWitsPreimageCbor),
    redeemerTxWitsRoot: compact.redeemerTxWitsRoot,
    redeemerTxWitsPreimageCbor: Buffer.from(
      witnessSet.redeemerTxWitsPreimageCbor,
    ),
  };
};

export const toMidgardNativeTxCanonical = (
  tx: MidgardNativeTxFull,
): MidgardNativeTxCanonical => ({
  version: tx.version,
  validity: tx.compact.validity,
  body: toBodyCanonicalFromFull(tx.body),
  witnessSet: toWitnessSetCanonicalFromFull(tx.witnessSet),
});

export const deriveMidgardNativeTxCompact = (
  body: MidgardNativeTxBodyFull,
  witnessSet: MidgardNativeTxWitnessSetFull,
  validity: MidgardTxValidity,
  version = MIDGARD_NATIVE_TX_VERSION,
): MidgardNativeTxCompact => {
  const bodyCompact = deriveMidgardNativeTxBodyCompactFromFull(body);
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompactFromFull(
    witnessSet,
    version,
  );
  return {
    version,
    transactionBodyHash: computeHash32(
      encodeMidgardNativeTxBodyCompact(bodyCompact),
    ),
    transactionWitnessSetHash: computeHash32(
      encodeMidgardNativeTxWitnessSetCompact(witnessCompact, version),
    ),
    validity,
  };
};

export const materializeMidgardNativeTxFromCanonical = (
  canonical: MidgardNativeTxCanonical,
): MidgardNativeTxFull => {
  const version = decodeVersion(
    canonical.version,
    "transaction_canonical.version",
  );
  const body = materializeMidgardNativeTxBodyFull(canonical.body);
  const witnessSet = materializeMidgardNativeTxWitnessSetFull(
    canonical.witnessSet,
    version,
  );
  const compact = deriveMidgardNativeTxCompact(
    body,
    witnessSet,
    canonical.validity,
    version,
  );
  return {
    version,
    compact,
    body,
    witnessSet,
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
  if (tx.compact.version !== tx.version) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "transaction_full.version must match transaction_compact.version",
      `${tx.version} != ${tx.compact.version}`,
    );
  }
  const bodyCompact = deriveMidgardNativeTxBodyCompactFromFull(tx.body);
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompactFromFull(
    tx.witnessSet,
    tx.version,
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
  const encodedWitnessCompact = encodeMidgardNativeTxWitnessSetCompact(
    witnessCompact,
    tx.version,
  );

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
  version = MIDGARD_NATIVE_TX_VERSION,
): Buffer =>
  encodeCbor(encodeNativeTxWitnessSetCompactValue(version, witnessSet));

export const decodeMidgardNativeTxWitnessSetCompact = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact =>
  (() => {
    const decoded = decodeSingleCbor(bytes);
    const arr = asArray(decoded, "transaction_witness_set");
    if (arr.length === 3) {
      return decodeNativeTxWitnessSetCompactValue(
        decoded,
        "transaction_witness_set",
        MIDGARD_NATIVE_TX_VERSION,
      );
    }
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "transaction_witness_set must have exactly 3 elements",
      `length=${arr.length}`,
    );
  })();

export const encodeMidgardNativeTxBodyFull = (
  body: MidgardNativeTxBodyFull,
): Buffer => encodeCbor(encodeNativeTxBodyFullValue(body));

export const decodeMidgardNativeTxBodyFull = (
  bytes: Uint8Array,
): MidgardNativeTxBodyFull =>
  decodeNativeTxBodyFullValue(decodeSingleCbor(bytes), "transaction_body_full");

export const encodeMidgardNativeTxWitnessSetFull = (
  witnessSet: MidgardNativeTxWitnessSetFull,
  version = MIDGARD_NATIVE_TX_VERSION,
): Buffer => encodeCbor(encodeNativeTxWitnessSetFullValue(version, witnessSet));

export const decodeMidgardNativeTxWitnessSetFull = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetFull =>
  (() => {
    const decoded = decodeSingleCbor(bytes);
    const arr = asArray(decoded, "transaction_witness_set_full");
    if (arr.length === 6) {
      return decodeNativeTxWitnessSetFullValue(
        decoded,
        "transaction_witness_set_full",
        MIDGARD_NATIVE_TX_VERSION,
      );
    }
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "transaction_witness_set_full must have exactly 6 elements",
      `length=${arr.length}`,
    );
  })();

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
    encodeNativeTxWitnessSetFullValue(tx.version, tx.witnessSet),
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
    witnessSet: decodeNativeTxWitnessSetFullValue(
      v[3],
      "transaction_full[3]",
      decodeVersion(v[0], "transaction_full[0]"),
    ),
  };
  if (options.enforceConsistency !== false) {
    verifyMidgardNativeTxFullConsistency(tx);
  }
  return tx;
};

export const computeMidgardNativeTxIdFromCompact = (
  compact: MidgardNativeTxCompact,
): Buffer => Buffer.from(compact.transactionBodyHash);

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
  keys(): CmlCollectionLike;
  get_assets(
    scriptHash: InstanceType<typeof CML.ScriptHash>,
  ): InstanceType<typeof CML.MapAssetNameToNonZeroInt64> | undefined;
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

const cmlValueToMidgardValue = (
  value: InstanceType<typeof CML.Value>,
): MidgardValue => {
  const policies = new Map<string, Map<string, bigint>>();
  const multiasset = value.multi_asset();
  if (multiasset !== undefined) {
    const policyIds = multiasset.keys();
    for (let i = 0; i < policyIds.len(); i += 1) {
      const policy = policyIds.get(i);
      const assets = multiasset.get_assets(policy);
      if (assets === undefined) {
        continue;
      }
      const inner = new Map<string, bigint>();
      const assetNames = assets.keys();
      for (let j = 0; j < assetNames.len(); j += 1) {
        const assetName = assetNames.get(j);
        const quantity = assets.get(assetName);
        if (quantity !== undefined && quantity !== 0n) {
          inner.set(
            Buffer.from(assetName.to_raw_bytes()).toString("hex"),
            BigInt(quantity.toString(10)),
          );
        }
      }
      if (inner.size > 0) {
        policies.set(policy.to_hex(), inner);
      }
    }
  }
  return {
    lovelace: value.coin(),
    assets: policies,
  };
};

const cmlScriptToMidgardVersionedScript = (
  script: InstanceType<typeof CML.Script>,
  fieldName: string,
): MidgardVersionedScript => {
  const native = script.as_native();
  if (native !== undefined) {
    const decodedNative = decodeMidgardNativeScript(native.to_cbor_bytes());
    return {
      language: "NativeCardano",
      scriptBytes: decodedNative.cbor,
      nativeScript: decodedNative.script,
    };
  }
  const plutusV1 = script.as_plutus_v1();
  if (plutusV1 !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Midgard outputs do not support PlutusV1 reference scripts",
      fieldName,
    );
  }
  const plutusV2 = script.as_plutus_v2();
  if (plutusV2 !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Midgard outputs do not support PlutusV2 reference scripts",
      fieldName,
    );
  }
  const plutusV3 = script.as_plutus_v3();
  if (plutusV3 !== undefined) {
    return {
      language: "PlutusV3",
      scriptBytes: Buffer.from(plutusV3.to_raw_bytes()),
    };
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
    "Unsupported Cardano script reference for Midgard output",
    fieldName,
  );
};

const cmlOutputToMidgardOutputBytes = (
  output: InstanceType<typeof CML.TransactionOutput>,
  fieldName: string,
): Buffer => {
  const datum = output.datum();
  if (datum?.as_hash() !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Midgard outputs must not use datum hashes; use inline datums",
      fieldName,
    );
  }
  const inlineDatum = datum?.as_datum();
  const scriptRef = output.script_ref();
  const midgardOutput: MidgardTxOutput = {
    address: Buffer.from(output.address().to_raw_bytes()),
    value: cmlValueToMidgardValue(output.amount()),
    ...(inlineDatum === undefined
      ? {}
      : {
          datum: {
            kind: "inline" as const,
            cbor: Buffer.from(inlineDatum.to_cbor_bytes()),
          },
        }),
    ...(scriptRef === undefined
      ? {}
      : {
          script_ref: cmlScriptToMidgardVersionedScript(
            scriptRef,
            `${fieldName}.script_ref`,
          ),
        }),
  };
  return encodeMidgardTxOutput(midgardOutput);
};

const cmlOutputsToNativePreimageCbor = (
  collection: CmlCollectionLike | undefined,
): Buffer => {
  if (collection === undefined) {
    return encodeCbor([]);
  }
  const entries: Buffer[] = [];
  for (let i = 0; i < collection.len(); i += 1) {
    const output = collection.get(i);
    if (!(output instanceof CML.TransactionOutput)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Unexpected output in transaction_body.outputs[${i}]`,
      );
    }
    entries.push(
      cmlOutputToMidgardOutputBytes(output, `transaction_body.outputs[${i}]`),
    );
  }
  return encodeCbor(entries);
};

const cmlMintToPreimageCbor = (
  mint: CmlMintLike,
  fieldName: string,
): Buffer => {
  if (mint.policy_count() === 0) {
    return encodeCbor([]);
  }

  const policies = new Map<Buffer, Map<Buffer, bigint>>();
  const policyIds = mint.keys();
  for (let i = 0; i < policyIds.len(); i++) {
    const policyId = policyIds.get(i);
    if (!(policyId instanceof CML.ScriptHash)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Unexpected policy id in ${fieldName}[${i}]`,
      );
    }
    const assets = mint.get_assets(policyId);
    if (assets === undefined) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Missing assets for policy in ${fieldName}[${i}]`,
      );
    }

    const encodedAssets = new Map<Buffer, bigint>();
    const assetNames = assets.keys();
    for (let j = 0; j < assetNames.len(); j++) {
      const assetName = assetNames.get(j);
      if (!(assetName instanceof CML.AssetName)) {
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.SchemaMismatch,
          `Unexpected asset name in ${fieldName}[${i}][${j}]`,
        );
      }
      const quantity = assets.get(assetName);
      if (quantity === undefined) {
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.SchemaMismatch,
          `Missing quantity for asset in ${fieldName}[${i}][${j}]`,
        );
      }
      encodedAssets.set(
        Buffer.from(assetName.to_raw_bytes()),
        BigInt(quantity.toString(10)),
      );
    }

    policies.set(Buffer.from(policyId.to_raw_bytes()), encodedAssets);
  }

  return encodeCbor(policies);
};

const cmlAnyToPreimageCbor = (value: unknown, fieldName: string): Buffer => {
  if (value === undefined) {
    return encodeCbor([]);
  }
  const mint = asMintLike(value);
  if (mint !== undefined) {
    return cmlMintToPreimageCbor(mint, fieldName);
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

const withdrawalsToRequiredObserversPreimageCbor = (
  withdrawals: InstanceType<typeof CML.MapRewardAccountToCoin> | undefined,
): Buffer => {
  if (withdrawals === undefined) {
    return encodeCbor([]);
  }
  const keys = withdrawals.keys();
  const observers: Buffer[] = [];
  for (let i = 0; i < keys.len(); i++) {
    const rewardAddr = keys.get(i);
    const amount = withdrawals.get(rewardAddr);
    if (amount === undefined) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        "Withdrawal map missing amount",
        `transaction_body.withdrawals[${i}]`,
      );
    }
    if (amount !== 0n) {
      failLossyConversion("withdrawals");
    }
    const scriptHash = rewardAddr.payment().as_script();
    if (scriptHash === undefined) {
      failLossyConversion("withdrawals");
      continue;
    }
    observers.push(Buffer.from(scriptHash.to_raw_bytes()));
  }
  return encodeCbor(observers);
};

const scriptWitnessesToPreimageCbor = (
  txWitnessSet: InstanceType<typeof CML.TransactionWitnessSet>,
): Buffer => {
  const scripts: MidgardVersionedScript[] = [];

  const nativeScripts = txWitnessSet.native_scripts();
  if (nativeScripts !== undefined) {
    for (let i = 0; i < nativeScripts.len(); i++) {
      const decoded = decodeMidgardNativeScript(
        nativeScripts.get(i).to_cbor_bytes(),
      );
      scripts.push({
        language: "NativeCardano",
        scriptBytes: decoded.cbor,
        nativeScript: decoded.script,
      });
    }
  }

  const plutusV1Scripts = txWitnessSet.plutus_v1_scripts();
  if (plutusV1Scripts !== undefined && plutusV1Scripts.len() > 0) {
    failLossyConversion("transaction_witness_set.plutus_v1_scripts");
  }

  const plutusV2Scripts = txWitnessSet.plutus_v2_scripts();
  if (plutusV2Scripts !== undefined && plutusV2Scripts.len() > 0) {
    failLossyConversion("transaction_witness_set.plutus_v2_scripts");
  }

  const plutusV3Scripts = txWitnessSet.plutus_v3_scripts();
  if (plutusV3Scripts !== undefined) {
    for (let i = 0; i < plutusV3Scripts.len(); i++) {
      scripts.push({
        language: "PlutusV3",
        scriptBytes: Buffer.from(plutusV3Scripts.get(i).to_raw_bytes()),
      });
    }
  }

  return encodeMidgardVersionedScriptListPreimage(scripts);
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
  if (withdrawals !== undefined) {
    withdrawalsToRequiredObserversPreimageCbor(withdrawals);
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
  if (hasAnyCmlEntries(txWitnessSet.plutus_datums())) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Midgard native transactions do not support Plutus datum witnesses; use inline datums",
      "transaction_witness_set.plutus_datums",
    );
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
  const outputsPreimageCbor = cmlOutputsToNativePreimageCbor(
    asCollectionLike(txOutputs),
  );
  const requiredObserversPreimageCbor =
    withdrawalsToRequiredObserversPreimageCbor(txBody.withdrawals());
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
  const scriptTxWitsPreimageCbor = scriptWitnessesToPreimageCbor(txWitnessSet);
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

  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: tx.is_valid() ? "TxIsValid" : "FailedScript",
    body: {
      spendInputsPreimageCbor,
      referenceInputsPreimageCbor,
      outputsPreimageCbor,
      fee: txBody.fee(),
      validityIntervalStart:
        txBody.validity_interval_start() ?? MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: txBody.ttl() ?? MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor,
      requiredSignersPreimageCbor,
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
    },
    witnessSet: {
      addrTxWitsPreimageCbor,
      scriptTxWitsPreimageCbor,
      redeemerTxWitsPreimageCbor,
    },
  };

  return materializeMidgardNativeTxFromCanonical(canonical);
};

export const cardanoTxBytesToMidgardNativeTxFullBytes = (
  cardanoTxBytes: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxFull(
    cardanoTxBytesToMidgardNativeTxFull(cardanoTxBytes),
  );

const decodeNativeCredentialObserver = (
  observerBytes: Uint8Array,
  fieldName: string,
): InstanceType<typeof CML.Credential> => {
  if (observerBytes.length === 28) {
    return CML.Credential.new_script(
      CML.ScriptHash.from_raw_bytes(observerBytes),
    );
  }
  try {
    const credential = CML.Credential.from_cbor_bytes(observerBytes);
    if (credential.kind() !== CML.CredentialKind.Script) {
      throw new Error("observer credential must be a script credential");
    }
    return credential;
  } catch (e) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Midgard observer must be a script hash or a CBOR-encoded script credential",
      `${fieldName}: ${String(e)}`,
    );
  }
};

const toCardanoNetworkId = (
  networkId: bigint,
  fieldName: string,
): InstanceType<typeof CML.NetworkId> | undefined => {
  if (networkId === MIDGARD_NATIVE_NETWORK_ID_NONE) {
    return undefined;
  }
  if (networkId === 0n) {
    return CML.NetworkId.testnet();
  }
  if (networkId === 1n) {
    return CML.NetworkId.mainnet();
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    "Unsupported Cardano network id for reverse conversion",
    `${fieldName}: ${networkId.toString(10)}`,
  );
};

const decodeNativeRequiredSignersToCardano = (
  preimageCbor: Uint8Array,
): InstanceType<typeof CML.Ed25519KeyHashList> => {
  const signerBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.required_signers",
  );
  const signers = CML.Ed25519KeyHashList.new();
  for (let i = 0; i < signerBytes.length; i++) {
    const signer = signerBytes[i];
    if (signer.length !== 28) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        "Required signer must be 28 bytes",
        `native.required_signers[${i}]`,
      );
    }
    signers.add(CML.Ed25519KeyHash.from_raw_bytes(signer));
  }
  return signers;
};

const decodeNativeObserversToWithdrawals = (
  preimageCbor: Uint8Array,
  networkId: InstanceType<typeof CML.NetworkId> | undefined,
): InstanceType<typeof CML.MapRewardAccountToCoin> | undefined => {
  const observerBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.required_observers",
  );
  if (observerBytes.length === 0) {
    return undefined;
  }
  if (networkId === undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Observer-to-withdrawal conversion requires an explicit Cardano network id",
      "native.network_id",
    );
  }
  const withdrawals = CML.MapRewardAccountToCoin.new();
  for (let i = 0; i < observerBytes.length; i++) {
    const credential = decodeNativeCredentialObserver(
      observerBytes[i],
      `native.required_observers[${i}]`,
    );
    withdrawals.insert(
      CML.RewardAddress.new(Number(networkId.network()), credential),
      0n,
    );
  }
  return withdrawals;
};

const decodeNativeInputsToCardano = (
  preimageCbor: Uint8Array,
  fieldName: string,
): InstanceType<typeof CML.TransactionInputList> => {
  const inputBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    fieldName,
  );
  const inputs = CML.TransactionInputList.new();
  for (let i = 0; i < inputBytes.length; i++) {
    inputs.add(CML.TransactionInput.from_cbor_bytes(inputBytes[i]));
  }
  return inputs;
};

const midgardValueToCmlValue = (
  value: MidgardValue,
): InstanceType<typeof CML.Value> => {
  const multiasset = CML.MultiAsset.new();
  for (const [policyHex, assets] of value.assets.entries()) {
    const cmlAssets = CML.MapAssetNameToCoin.new();
    let assetCount = 0;
    for (const [assetNameHex, quantity] of assets.entries()) {
      if (quantity <= 0n) {
        continue;
      }
      cmlAssets.insert(
        CML.AssetName.from_raw_bytes(Buffer.from(assetNameHex, "hex")),
        quantity,
      );
      assetCount += 1;
    }
    if (assetCount > 0) {
      multiasset.insert_assets(CML.ScriptHash.from_hex(policyHex), cmlAssets);
    }
  }
  return multiasset.policy_count() === 0
    ? CML.Value.from_coin(value.lovelace)
    : CML.Value.new(value.lovelace, multiasset);
};

const midgardVersionedScriptToCardano = (
  script: MidgardVersionedScript,
  fieldName: string,
): InstanceType<typeof CML.Script> => {
  switch (script.language) {
    case "NativeCardano":
      return CML.Script.new_native(
        CML.NativeScript.from_cbor_bytes(script.scriptBytes),
      );
    case "PlutusV3":
      return CML.Script.new_plutus_v3(
        CML.PlutusV3Script.from_raw_bytes(script.scriptBytes),
      );
    case "MidgardV1":
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
        "MidgardV1 scripts cannot be represented as Cardano script references",
        fieldName,
      );
  }
};

const midgardOutputBytesToCardano = (
  outputBytes: Uint8Array,
  fieldName: string,
): InstanceType<typeof CML.TransactionOutput> => {
  const decoded = decodeMidgardTxOutput(outputBytes);
  const address = decodeMidgardAddressBytes(decoded.address);
  if (address.protected) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Protected Midgard addresses cannot be represented as Cardano TxOut addresses",
      fieldName,
    );
  }
  const output = CML.ConwayFormatTxOut.new(
    CML.Address.from_raw_bytes(decoded.address),
    midgardValueToCmlValue(decoded.value),
  );
  if (decoded.datum !== undefined) {
    output.set_datum_option(
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_bytes(decoded.datum.cbor),
      ),
    );
  }
  if (decoded.script_ref !== undefined) {
    output.set_script_reference(
      midgardVersionedScriptToCardano(
        decoded.script_ref,
        `${fieldName}.script_ref`,
      ),
    );
  }
  return CML.TransactionOutput.new_conway_format_tx_out(output);
};

const decodeNativeOutputsToCardano = (
  preimageCbor: Uint8Array,
): InstanceType<typeof CML.TransactionOutputList> => {
  const outputBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.outputs",
  );
  const outputs = CML.TransactionOutputList.new();
  for (let i = 0; i < outputBytes.length; i++) {
    outputs.add(
      midgardOutputBytesToCardano(outputBytes[i], `native.outputs[${i}]`),
    );
  }
  return outputs;
};

const decodeNativeAddrWitnessesToCardano = (
  preimageCbor: Uint8Array,
): InstanceType<typeof CML.VkeywitnessList> | undefined => {
  const witnessBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.addr_tx_wits",
  );
  if (witnessBytes.length === 0) {
    return undefined;
  }
  const witnesses = CML.VkeywitnessList.new();
  for (let i = 0; i < witnessBytes.length; i++) {
    witnesses.add(CML.Vkeywitness.from_cbor_bytes(witnessBytes[i]));
  }
  return witnesses;
};

type DecodedCardanoScripts = {
  readonly nativeScripts?: InstanceType<typeof CML.NativeScriptList>;
  readonly plutusV1Scripts?: InstanceType<typeof CML.PlutusV1ScriptList>;
  readonly plutusV2Scripts?: InstanceType<typeof CML.PlutusV2ScriptList>;
  readonly plutusV3Scripts?: InstanceType<typeof CML.PlutusV3ScriptList>;
};

export type DecodedMidgardNativeMint = {
  readonly mint: InstanceType<typeof CML.Mint>;
  readonly policyIds: readonly string[];
  readonly mintedValue: InstanceType<typeof CML.Value>;
  readonly burnedValue: InstanceType<typeof CML.Value>;
};

const decodeNativeScriptsToCardano = (
  preimageCbor: Uint8Array,
): DecodedCardanoScripts => {
  const scripts = decodeMidgardVersionedScriptListPreimage(
    preimageCbor,
    "native.script_tx_wits",
  );
  if (scripts.length === 0) {
    return {};
  }
  const nativeScripts = CML.NativeScriptList.new();
  const plutusV3Scripts = CML.PlutusV3ScriptList.new();
  for (let i = 0; i < scripts.length; i++) {
    const script = scripts[i];
    switch (script.language) {
      case "NativeCardano":
        nativeScripts.add(CML.NativeScript.from_cbor_bytes(script.scriptBytes));
        break;
      case "PlutusV3":
        plutusV3Scripts.add(
          CML.PlutusV3Script.from_raw_bytes(script.scriptBytes),
        );
        break;
      case "MidgardV1":
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
          "MidgardV1 inline scripts cannot be represented in Cardano witness sets",
          `native.script_tx_wits[${i}]`,
        );
    }
  }
  return {
    nativeScripts: nativeScripts.len() > 0 ? nativeScripts : undefined,
    plutusV3Scripts: plutusV3Scripts.len() > 0 ? plutusV3Scripts : undefined,
  };
};

const valueFromMultiasset = (
  multiasset: InstanceType<typeof CML.MultiAsset>,
): InstanceType<typeof CML.Value> =>
  multiasset.policy_count() === 0
    ? CML.Value.zero()
    : CML.Value.new(0n, multiasset);

export const decodeMidgardNativeMint = (
  preimageCbor: Uint8Array,
): DecodedMidgardNativeMint | undefined => {
  const decoded = decodeSingleCbor(preimageCbor);
  if (Array.isArray(decoded)) {
    if (decoded.length === 0) {
      return undefined;
    }
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Midgard mint preimage must be an empty array or a CBOR map",
      "native.mint",
    );
  }

  const policies = asMap(decoded, "native.mint");
  if (policies.size === 0) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Midgard mint map cannot be empty",
      "native.mint",
    );
  }

  const mint = CML.Mint.new();
  for (const [policyBytesValue, assetsValue] of policies.entries()) {
    const policyBytes = asBytes(policyBytesValue, "native.mint.policy");
    if (policyBytes.length !== 28) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        "Mint policy id must be 28 bytes",
        "native.mint.policy",
      );
    }

    const assetsMap = asMap(assetsValue, "native.mint.assets");
    if (assetsMap.size === 0) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        "Mint policy asset map cannot be empty",
        "native.mint.assets",
      );
    }
    const assets = CML.MapAssetNameToNonZeroInt64.new();
    for (const [assetNameValue, quantityValue] of assetsMap.entries()) {
      const assetName = asBytes(assetNameValue, "native.mint.asset_name");
      const quantity = asSigned(quantityValue, "native.mint.quantity");
      if (quantity === 0n) {
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.InvalidFieldType,
          "Mint quantity cannot be zero",
          "native.mint.quantity",
        );
      }
      assets.insert(CML.AssetName.from_raw_bytes(assetName), quantity);
    }

    mint.insert_assets(CML.ScriptHash.from_raw_bytes(policyBytes), assets);
  }

  const policyIds = Array.from({ length: mint.keys().len() }, (_, index) =>
    mint.keys().get(index).to_hex(),
  ).sort((a, b) => a.localeCompare(b));

  return {
    mint,
    policyIds,
    mintedValue: valueFromMultiasset(mint.as_positive_multiasset()),
    burnedValue: valueFromMultiasset(mint.as_negative_multiasset()),
  };
};

const decodeNativeRedeemersToCardano = (
  preimageCbor: Uint8Array,
): InstanceType<typeof CML.Redeemers> | undefined => {
  const decoded = decodeSingleCbor(preimageCbor);
  if (Array.isArray(decoded) && decoded.length === 0) {
    return undefined;
  }
  return CML.Redeemers.from_cbor_bytes(preimageCbor);
};

export type MidgardToCardanoTxEncodingOptions = {
  readonly omitVkeyWitnesses?: boolean;
};

export const midgardNativeTxFullToCardanoTxEncoding = (
  tx: MidgardNativeTxFull,
  options?: MidgardToCardanoTxEncodingOptions,
): Buffer => {
  verifyMidgardNativeTxFullConsistency(tx);

  const inputs = decodeNativeInputsToCardano(
    tx.body.spendInputsPreimageCbor,
    "native.spend_inputs",
  );
  const outputs = decodeNativeOutputsToCardano(tx.body.outputsPreimageCbor);
  const body = CML.TransactionBody.new(inputs, outputs, tx.body.fee);
  const networkId = toCardanoNetworkId(tx.body.networkId, "native.network_id");
  if (networkId !== undefined) {
    body.set_network_id(networkId);
  }

  const referenceInputs = decodeNativeInputsToCardano(
    tx.body.referenceInputsPreimageCbor,
    "native.reference_inputs",
  );
  if (referenceInputs.len() > 0) {
    body.set_reference_inputs(referenceInputs);
  }

  if (tx.body.validityIntervalStart !== MIDGARD_POSIX_TIME_NONE) {
    body.set_validity_interval_start(tx.body.validityIntervalStart);
  }
  if (tx.body.validityIntervalEnd !== MIDGARD_POSIX_TIME_NONE) {
    body.set_ttl(tx.body.validityIntervalEnd);
  }

  const withdrawals = decodeNativeObserversToWithdrawals(
    tx.body.requiredObserversPreimageCbor,
    networkId,
  );
  if (withdrawals !== undefined) {
    body.set_withdrawals(withdrawals);
  }

  const requiredSigners = decodeNativeRequiredSignersToCardano(
    tx.body.requiredSignersPreimageCbor,
  );
  if (requiredSigners.len() > 0) {
    body.set_required_signers(requiredSigners);
  }

  const decodedMint = decodeMidgardNativeMint(tx.body.mintPreimageCbor);
  if (decodedMint !== undefined) {
    body.set_mint(decodedMint.mint);
  }

  const emptyNullRoot = computeHash32(encodeCbor(null));
  if (!tx.body.scriptIntegrityHash.equals(emptyNullRoot)) {
    body.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(tx.body.scriptIntegrityHash),
    );
  }
  if (!tx.body.auxiliaryDataHash.equals(emptyNullRoot)) {
    body.set_auxiliary_data_hash(
      CML.AuxiliaryDataHash.from_raw_bytes(tx.body.auxiliaryDataHash),
    );
  }

  const witnessSet = CML.TransactionWitnessSet.new();
  if (options?.omitVkeyWitnesses !== true) {
    const vkeyWitnesses = decodeNativeAddrWitnessesToCardano(
      tx.witnessSet.addrTxWitsPreimageCbor,
    );
    if (vkeyWitnesses !== undefined) {
      witnessSet.set_vkeywitnesses(vkeyWitnesses);
    }
  }

  const scripts = decodeNativeScriptsToCardano(
    tx.witnessSet.scriptTxWitsPreimageCbor,
  );
  if (scripts.nativeScripts !== undefined) {
    witnessSet.set_native_scripts(scripts.nativeScripts);
  }
  if (scripts.plutusV1Scripts !== undefined) {
    witnessSet.set_plutus_v1_scripts(scripts.plutusV1Scripts);
  }
  if (scripts.plutusV2Scripts !== undefined) {
    witnessSet.set_plutus_v2_scripts(scripts.plutusV2Scripts);
  }
  if (scripts.plutusV3Scripts !== undefined) {
    witnessSet.set_plutus_v3_scripts(scripts.plutusV3Scripts);
  }

  const redeemers = decodeNativeRedeemersToCardano(
    tx.witnessSet.redeemerTxWitsPreimageCbor,
  );
  if (redeemers !== undefined) {
    witnessSet.set_redeemers(redeemers);
  }

  return Buffer.from(
    CML.Transaction.new(
      body,
      witnessSet,
      tx.compact.validity === "TxIsValid",
      undefined,
    ).to_cbor_bytes(),
  );
};
