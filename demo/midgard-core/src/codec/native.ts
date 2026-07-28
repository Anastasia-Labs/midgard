import { CML } from "@lucid-evolution/lucid";

import { decodeMidgardAddressBytes } from "./address.js";
import {
  asArray,
  asBytes,
  asMap,
  decodeSingleCbor,
  encodeCbor,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32, ensureHash32, type Hash32 } from "./hash.js";
import {
  decodeNativeTxBodyCanonicalCbor,
  decodeNativeTxBodyCanonicalValue,
  decodeNativeTxBodyCompactCbor,
  decodeNativeTxBodyCompactValue,
  deriveNativeTxBodyCompact,
  encodeNativeTxBodyCanonicalCbor,
  encodeNativeTxBodyCanonicalValue,
  encodeNativeTxBodyCompactCbor,
  encodeNativeTxBodyCompactValue,
} from "./native-body.js";
import { cardanoTxBytesToMidgardNativeTxCanonicalV1 } from "./native-cardano-conversion.js";
import { verifyNativeTxFullConsistency } from "./native-consistency.js";
import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "./native-constants.js";
import { midgardRedeemersToCardano } from "./native-redeemer.js";
import {
  asFixedArray,
  asSigned,
  asUnsigned,
  decodeValidityCode,
  decodeVersion,
  encodeValidityCode,
  type MidgardTxValidity,
} from "./native-validation.js";
import {
  decodeNativeTxWitnessPreimagesCbor,
  decodeNativeTxWitnessSetCanonicalValue,
  decodeNativeTxWitnessSetCompactCbor,
  deriveNativeTxWitnessSetCompact,
  encodeNativeTxWitnessPreimagesCbor,
  encodeNativeTxWitnessSetCanonicalValue,
  encodeNativeTxWitnessSetCompactCbor,
} from "./native-witness.js";
import { decodeMidgardTxOutput } from "./output.js";
import { midgardValueToCmlValue } from "./value.js";
import {
  decodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "./versioned-script.js";
export {
  EMPTY_CBOR_LIST,
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "./native-constants.js";
export {
  decodeValidityCode,
  encodeValidityCode,
  type MidgardTxValidity,
  MidgardTxValidityCodes,
} from "./native-validation.js";

export type MidgardNativeTxCompactV1 = {
  readonly version: bigint;
  readonly transactionBody: MidgardNativeTxBodyCompactV1;
  readonly transactionWitnessSetHash: Hash32;
  readonly validity: MidgardTxValidity;
};

export type MidgardNativeTxBodyCompactV1 = {
  readonly spendInputsHash: Hash32;
  readonly referenceInputsHash: Hash32;
  readonly outputsHash: Hash32;
  readonly fee: bigint;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly requiredObserversHash: Hash32;
  readonly requiredSignersHash: Hash32;
  readonly mintHash: Hash32;
  readonly scriptIntegrityHash: Hash32;
  readonly auxiliaryDataHash: Hash32;
  readonly networkId: bigint;
};

export type MidgardNativeTxWitnessSetCompactV1 = {
  readonly addrTxWitsHash: Hash32;
  readonly scriptTxWitsHash: Hash32;
  readonly redeemerTxWitsHash: Hash32;
};

export type MidgardNativeTxBodyCanonicalV1 = {
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

export type MidgardNativeTxWitnessSetCanonicalV1 = {
  readonly addrTxWitsPreimageCbor: Buffer;
  readonly scriptTxWitsPreimageCbor: Buffer;
  readonly redeemerTxWitsPreimageCbor: Buffer;
};

export type MidgardNativeTxCanonicalV1 = {
  readonly version: bigint;
  readonly validity: MidgardTxValidity;
  readonly body: MidgardNativeTxBodyCanonicalV1;
  readonly witnessSet: MidgardNativeTxWitnessSetCanonicalV1;
};

export type MidgardNativeTxFullV1 = MidgardNativeTxCanonicalV1 & {
  readonly compact: MidgardNativeTxCompactV1;
};

/**
 * Compact L1 source for the canonical V1 transaction proof.
 *
 * The compact transaction commits all six body-field preimages and the hash
 * of the compact witness set. The compact witness set commits the remaining
 * three witness-field preimages. Neither source member contains the aggregate
 * canonical transaction, so membership proofs remain bounded when the full
 * transaction is larger than one L1 proof envelope.
 */
export type MidgardNativeTxProofSourceV1 = {
  readonly compactCbor: Buffer;
  readonly witnessSetCompactCbor: Buffer;
  readonly fieldPreimageLengthsCbor: Buffer;
};

export type MidgardNativeCodecOptions = {
  readonly enforceConsistency?: boolean;
};

const requireNativeTxV1Version = (
  value: unknown,
  fieldName: string,
): typeof MIDGARD_NATIVE_TX_V1_VERSION => {
  const version = decodeVersion(value, fieldName);
  if (version !== MIDGARD_NATIVE_TX_V1_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must equal ${MIDGARD_NATIVE_TX_V1_VERSION.toString()}`,
      `actual=${version.toString()}`,
    );
  }
  return MIDGARD_NATIVE_TX_V1_VERSION;
};

const encodeNativeTxCompactValue = (
  tx: MidgardNativeTxCompactV1,
): readonly [
  bigint,
  ReturnType<typeof encodeNativeTxBodyCompactValue>,
  Hash32,
  bigint,
] => [
  requireNativeTxV1Version(tx.version, "transaction_compact.version"),
  encodeNativeTxBodyCompactValue(tx.transactionBody),
  ensureHash32(
    tx.transactionWitnessSetHash,
    "transaction_compact.transaction_witness_set",
  ),
  encodeValidityCode(tx.validity),
];

const decodeNativeTxCompactValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxCompactV1 => {
  const v = asFixedArray(value, 4, fieldName);
  return {
    version: requireNativeTxV1Version(v[0], `${fieldName}[0]`),
    transactionBody: decodeNativeTxBodyCompactValue(v[1], `${fieldName}[1]`),
    transactionWitnessSetHash: ensureHash32(
      asBytes(v[2], `${fieldName}[2]`),
      `${fieldName}[2]`,
    ),
    validity: decodeValidityCode(v[3], `${fieldName}[3]`),
  };
};

export const deriveMidgardNativeTxBodyCompactV1 = (
  body: MidgardNativeTxBodyCanonicalV1,
): MidgardNativeTxBodyCompactV1 => deriveNativeTxBodyCompact(body);

export const deriveMidgardNativeTxWitnessSetCompactV1 = (
  witnessSet: MidgardNativeTxWitnessSetCanonicalV1,
): MidgardNativeTxWitnessSetCompactV1 =>
  deriveNativeTxWitnessSetCompact(witnessSet);

export const toMidgardNativeTxCanonicalV1 = (
  tx: MidgardNativeTxFullV1,
): MidgardNativeTxCanonicalV1 => ({
  version: tx.version,
  validity: tx.validity,
  body: {
    ...tx.body,
    spendInputsPreimageCbor: Buffer.from(tx.body.spendInputsPreimageCbor),
    referenceInputsPreimageCbor: Buffer.from(
      tx.body.referenceInputsPreimageCbor,
    ),
    outputsPreimageCbor: Buffer.from(tx.body.outputsPreimageCbor),
    requiredObserversPreimageCbor: Buffer.from(
      tx.body.requiredObserversPreimageCbor,
    ),
    requiredSignersPreimageCbor: Buffer.from(
      tx.body.requiredSignersPreimageCbor,
    ),
    mintPreimageCbor: Buffer.from(tx.body.mintPreimageCbor),
  },
  witnessSet: {
    addrTxWitsPreimageCbor: Buffer.from(tx.witnessSet.addrTxWitsPreimageCbor),
    scriptTxWitsPreimageCbor: Buffer.from(
      tx.witnessSet.scriptTxWitsPreimageCbor,
    ),
    redeemerTxWitsPreimageCbor: Buffer.from(
      tx.witnessSet.redeemerTxWitsPreimageCbor,
    ),
  },
});

export const deriveMidgardNativeTxCompactV1 = (
  body: MidgardNativeTxBodyCanonicalV1,
  witnessSet: MidgardNativeTxWitnessSetCanonicalV1,
  validity: MidgardTxValidity,
  version = MIDGARD_NATIVE_TX_V1_VERSION,
): MidgardNativeTxCompactV1 => {
  requireNativeTxV1Version(version, "transaction_compact.version");
  const bodyCompact = deriveMidgardNativeTxBodyCompactV1(body);
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompactV1(witnessSet);
  return {
    version,
    transactionBody: bodyCompact,
    transactionWitnessSetHash: computeHash32(
      encodeMidgardNativeTxWitnessSetCompactV1(witnessCompact),
    ),
    validity,
  };
};

export const materializeMidgardNativeTxFromCanonicalV1 = (
  canonical: MidgardNativeTxCanonicalV1,
): MidgardNativeTxFullV1 => {
  const version = requireNativeTxV1Version(
    canonical.version,
    "transaction_canonical.version",
  );
  const compact = deriveMidgardNativeTxCompactV1(
    canonical.body,
    canonical.witnessSet,
    canonical.validity,
    version,
  );
  return {
    version,
    validity: canonical.validity,
    compact,
    body: canonical.body,
    witnessSet: canonical.witnessSet,
  };
};

export const verifyMidgardNativeTxFullConsistencyV1 = (
  tx: MidgardNativeTxFullV1,
): void => verifyNativeTxFullConsistency(tx);

export const encodeMidgardNativeTxCompactV1 = (
  tx: MidgardNativeTxCompactV1,
): Buffer => encodeCbor(encodeNativeTxCompactValue(tx));

export const decodeMidgardNativeTxCompactV1 = (
  bytes: Uint8Array,
): MidgardNativeTxCompactV1 =>
  decodeNativeTxCompactValue(decodeSingleCbor(bytes), "transaction_compact");

export const encodeMidgardNativeTxBodyCompactV1 = (
  body: MidgardNativeTxBodyCompactV1,
): Buffer => encodeNativeTxBodyCompactCbor(body);

export const decodeMidgardNativeTxBodyCompactV1 = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompactV1 => decodeNativeTxBodyCompactCbor(bytes);

export const encodeMidgardNativeTxWitnessSetCompactV1 = (
  witnessSet: MidgardNativeTxWitnessSetCompactV1,
): Buffer => encodeNativeTxWitnessSetCompactCbor(witnessSet);

export const decodeMidgardNativeTxWitnessSetCompactV1 = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompactV1 =>
  decodeNativeTxWitnessSetCompactCbor(bytes);

export const encodeMidgardNativeTxBodyCanonicalV1 = (
  body: MidgardNativeTxBodyCanonicalV1,
): Buffer => encodeNativeTxBodyCanonicalCbor(body);

export const decodeMidgardNativeTxBodyCanonicalV1 = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCanonicalV1 => decodeNativeTxBodyCanonicalCbor(bytes);

export const encodeMidgardNativeTxWitnessPreimagesV1 = (
  witnessSet: MidgardNativeTxWitnessSetCanonicalV1,
): Buffer => encodeNativeTxWitnessPreimagesCbor(witnessSet);

export const decodeMidgardNativeTxWitnessPreimagesV1 = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCanonicalV1 =>
  decodeNativeTxWitnessPreimagesCbor(bytes);

const hasDerivedCompact = (
  tx: MidgardNativeTxCanonicalV1 | MidgardNativeTxFullV1,
): tx is MidgardNativeTxFullV1 => "compact" in tx;

export const encodeMidgardNativeTxCanonicalV1 = (
  tx: MidgardNativeTxCanonicalV1 | MidgardNativeTxFullV1,
  options: MidgardNativeCodecOptions = {},
): Buffer => {
  if (options.enforceConsistency !== false && hasDerivedCompact(tx)) {
    verifyMidgardNativeTxFullConsistencyV1(tx);
  }
  const version = requireNativeTxV1Version(tx.version, "transaction.version");
  return encodeCbor([
    version,
    encodeNativeTxBodyCanonicalValue(tx.body),
    encodeNativeTxWitnessSetCanonicalValue(version, tx.witnessSet),
    encodeValidityCode(tx.validity),
  ]);
};

export const decodeMidgardNativeTxCanonicalV1 = (
  bytes: Uint8Array,
): MidgardNativeTxCanonicalV1 => {
  const decoded = decodeSingleCbor(bytes);
  const v = asFixedArray(decoded, 4, "transaction");
  const version = requireNativeTxV1Version(v[0], "transaction[0]");
  return {
    version,
    body: decodeNativeTxBodyCanonicalValue(v[1], "transaction[1]"),
    witnessSet: decodeNativeTxWitnessSetCanonicalValue(
      v[2],
      "transaction[2]",
      version,
    ),
    validity: decodeValidityCode(v[3], "transaction[3]"),
  };
};

export const decodeMidgardNativeTxFullV1FromCanonicalCbor = (
  bytes: Uint8Array,
  options: MidgardNativeCodecOptions = {},
): MidgardNativeTxFullV1 => {
  const tx = materializeMidgardNativeTxFromCanonicalV1(
    decodeMidgardNativeTxCanonicalV1(bytes),
  );
  if (options.enforceConsistency !== false) {
    verifyMidgardNativeTxFullConsistencyV1(tx);
  }
  return tx;
};

export const computeMidgardNativeTxIdV1 = (
  tx: MidgardNativeTxFullV1 | MidgardNativeTxCompactV1,
): Buffer => {
  const compact = "compact" in tx ? tx.compact : tx;
  const version = requireNativeTxV1Version(
    compact.version,
    "transaction_compact.version",
  );
  const bodyCbor = encodeMidgardNativeTxBodyCompactV1(compact.transactionBody);
  return computeHash32(
    Buffer.concat([
      Buffer.from("MidgardNativeTxBodyV1", "ascii"),
      encodeCbor(version),
      bodyCbor,
    ]),
  );
};

/**
 * Commits the exact canonical V1 full transaction, including all witness
 * preimages. This is distinct from the transaction id, which intentionally
 * identifies the compact body.
 */
export const computeMidgardNativeTxFullHashV1 = (
  tx: MidgardNativeTxFullV1,
): Buffer => {
  if (tx.version !== MIDGARD_NATIVE_TX_V1_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Full transaction commitment requires native transaction V1",
      `actual=${tx.version.toString()}`,
    );
  }
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(tx);
  return computeHash32(
    Buffer.concat([
      Buffer.from("MidgardNativeTxFullV1", "ascii"),
      encodeCbor(tx.version),
      canonicalCbor,
    ]),
  );
};

const MIDGARD_NATIVE_TX_PROOF_SOURCE_V1_DOMAIN = Buffer.from(
  "MidgardNativeTxProofSourceV1",
  "ascii",
);

export const encodeMidgardNativeTxProofSourceV1 = (
  source: MidgardNativeTxProofSourceV1,
): Buffer =>
  encodeCbor([
    Buffer.from(source.compactCbor),
    Buffer.from(source.witnessSetCompactCbor),
    Buffer.from(source.fieldPreimageLengthsCbor),
  ]);

export const decodeMidgardNativeTxProofFieldLengthsV1 = (
  fieldPreimageLengthsCbor: Uint8Array,
): readonly number[] => {
  const values = asFixedArray(
    decodeSingleCbor(fieldPreimageLengthsCbor),
    9,
    "proof_source.field_preimage_lengths",
  );
  return values.map((value, index) => {
    const length = asUnsigned(
      value,
      `proof_source.field_preimage_lengths[${index.toString()}]`,
    );
    if (length > BigInt(Number.MAX_SAFE_INTEGER)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        "V1 field-preimage length exceeds the exact integer range",
        `index=${index.toString()},length=${length.toString()}`,
      );
    }
    return Number(length);
  });
};

const proofFieldPreimageLengths = (
  tx: MidgardNativeTxFullV1,
): readonly number[] => [
  tx.body.spendInputsPreimageCbor.length,
  tx.body.referenceInputsPreimageCbor.length,
  tx.body.outputsPreimageCbor.length,
  tx.body.requiredObserversPreimageCbor.length,
  tx.body.requiredSignersPreimageCbor.length,
  tx.body.mintPreimageCbor.length,
  tx.witnessSet.addrTxWitsPreimageCbor.length,
  tx.witnessSet.scriptTxWitsPreimageCbor.length,
  tx.witnessSet.redeemerTxWitsPreimageCbor.length,
];

export const encodeMidgardNativeTxProofFieldLengthsV1 = (
  lengths: readonly number[],
): Buffer => {
  if (
    lengths.length !== 9 ||
    lengths.some(
      (length) => !Number.isSafeInteger(length) || length < 0,
    )
  ) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "V1 field-preimage lengths must contain exactly nine non-negative safe integers",
    );
  }
  return encodeCbor(lengths.map((length) => BigInt(length)));
};

export const computeMidgardNativeTxCanonicalSizeFromProofSourceV1 = (
  source: MidgardNativeTxProofSourceV1,
): number => {
  const compact = decodeMidgardNativeTxCompactV1(source.compactCbor);
  const lengths = decodeMidgardNativeTxProofFieldLengthsV1(
    source.fieldPreimageLengthsCbor,
  );
  return encodeCbor([
    compact.version,
    [
      Buffer.alloc(lengths[0]!),
      Buffer.alloc(lengths[1]!),
      Buffer.alloc(lengths[2]!),
      compact.transactionBody.fee,
      compact.transactionBody.validityIntervalStart,
      compact.transactionBody.validityIntervalEnd,
      Buffer.alloc(lengths[3]!),
      Buffer.alloc(lengths[4]!),
      Buffer.alloc(lengths[5]!),
      compact.transactionBody.scriptIntegrityHash,
      compact.transactionBody.auxiliaryDataHash,
      compact.transactionBody.networkId,
    ],
    [
      Buffer.alloc(lengths[6]!),
      Buffer.alloc(lengths[7]!),
      Buffer.alloc(lengths[8]!),
    ],
    encodeValidityCode(compact.validity),
  ]).length;
};

export const computeMidgardNativeTxProofCommitmentV1 = (
  source: MidgardNativeTxProofSourceV1,
): Buffer =>
  computeHash32(
    Buffer.concat([
      MIDGARD_NATIVE_TX_PROOF_SOURCE_V1_DOMAIN,
      encodeCbor(1n),
      encodeMidgardNativeTxProofSourceV1(source),
    ]),
  );

export const deriveMidgardNativeTxProofSourceV1 = (
  tx: MidgardNativeTxFullV1,
): MidgardNativeTxProofSourceV1 => {
  if (tx.version !== MIDGARD_NATIVE_TX_V1_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 transaction sources require native transaction version 1",
      `actual=${tx.version.toString()}`,
    );
  }
  verifyMidgardNativeTxFullConsistencyV1(tx);
  return {
    compactCbor: encodeMidgardNativeTxCompactV1(tx.compact),
    witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompactV1(
      deriveMidgardNativeTxWitnessSetCompactV1(tx.witnessSet),
    ),
    fieldPreimageLengthsCbor:
      encodeMidgardNativeTxProofFieldLengthsV1(
        proofFieldPreimageLengths(tx),
      ),
  };
};

export const deriveMidgardNativeTxProofSourceV1FromCanonicalCbor = (
  canonicalTransactionCbor: Uint8Array,
): MidgardNativeTxProofSourceV1 =>
  deriveMidgardNativeTxProofSourceV1(
    decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalTransactionCbor),
  );

export const verifyMidgardNativeTxProofSourceV1 = ({
  transactionId,
  source,
}: {
  readonly transactionId: Uint8Array;
  readonly source: MidgardNativeTxProofSourceV1;
}): MidgardNativeTxCompactV1 => {
  const compact = decodeMidgardNativeTxCompactV1(source.compactCbor);
  const canonicalCompact = encodeMidgardNativeTxCompactV1(compact);
  if (!canonicalCompact.equals(source.compactCbor)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 compact transaction source is not canonical",
    );
  }
  if (compact.version !== MIDGARD_NATIVE_TX_V1_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 compact transaction source has an unsupported native version",
      `actual=${compact.version.toString()}`,
    );
  }
  const witnessSetCompact = decodeMidgardNativeTxWitnessSetCompactV1(
    source.witnessSetCompactCbor,
  );
  const canonicalWitnessSetCompact =
    encodeMidgardNativeTxWitnessSetCompactV1(witnessSetCompact);
  if (!canonicalWitnessSetCompact.equals(source.witnessSetCompactCbor)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 compact witness-set source is not canonical",
    );
  }
  const expectedWitnessSetHash = computeHash32(canonicalWitnessSetCompact);
  if (!expectedWitnessSetHash.equals(compact.transactionWitnessSetHash)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.HashMismatch,
      "V1 compact witness set does not match the transaction source",
    );
  }
  const fieldLengths = decodeMidgardNativeTxProofFieldLengthsV1(
    source.fieldPreimageLengthsCbor,
  );
  const canonicalFieldLengths =
    encodeMidgardNativeTxProofFieldLengthsV1(fieldLengths);
  if (!canonicalFieldLengths.equals(source.fieldPreimageLengthsCbor)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 field-preimage lengths are not canonical",
    );
  }
  const expectedTransactionId = computeMidgardNativeTxIdV1(compact);
  if (!expectedTransactionId.equals(transactionId)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.HashMismatch,
      "V1 compact transaction does not match the transaction id",
    );
  }
  return compact;
};

export const decodeMidgardNativeByteListPreimage = (
  preimageCbor: Uint8Array,
  fieldName = "preimage_cbor",
): Buffer[] => {
  const decoded = decodeSingleCbor(preimageCbor);
  const arr = asArray(decoded, fieldName);
  return arr.map((item, index) => asBytes(item, `${fieldName}[${index}]`));
};

export const cardanoTxBytesToMidgardNativeTxFullV1 = (
  cardanoTxBytes: Uint8Array,
): MidgardNativeTxFullV1 => {
  const canonical = cardanoTxBytesToMidgardNativeTxCanonicalV1(cardanoTxBytes, {
    nativeTxVersion: MIDGARD_NATIVE_TX_V1_VERSION,
    posixTimeNone: MIDGARD_POSIX_TIME_NONE,
    networkIdNone: MIDGARD_NATIVE_NETWORK_ID_NONE,
  });
  return materializeMidgardNativeTxFromCanonicalV1(canonical);
};

export const cardanoTxBytesToMidgardNativeTxCanonicalCborV1 = (
  cardanoTxBytes: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxCanonicalV1(
    cardanoTxBytesToMidgardNativeTxCanonicalV1(cardanoTxBytes, {
      nativeTxVersion: MIDGARD_NATIVE_TX_V1_VERSION,
      posixTimeNone: MIDGARD_POSIX_TIME_NONE,
      networkIdNone: MIDGARD_NATIVE_NETWORK_ID_NONE,
    }),
  );

const decodeNativeCredentialObserver = (
  observerBytes: Uint8Array,
  fieldName: string,
): CML.Credential => {
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
): CML.NetworkId | undefined => {
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
): CML.Ed25519KeyHashList => {
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
  networkId: CML.NetworkId | undefined,
): CML.MapRewardAccountToCoin | undefined => {
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
): CML.TransactionInputList => {
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

const midgardVersionedScriptToCardano = (
  script: MidgardVersionedScript,
  fieldName: string,
): CML.Script => {
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
): CML.TransactionOutput => {
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
): CML.TransactionOutputList => {
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
): CML.VkeywitnessList | undefined => {
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
  readonly nativeScripts?: CML.NativeScriptList;
  readonly plutusV3Scripts?: CML.PlutusV3ScriptList;
};

export type DecodedMidgardNativeMint = {
  readonly mint: CML.Mint;
  readonly policyIds: readonly string[];
  readonly mintedValue: CML.Value;
  readonly burnedValue: CML.Value;
};

const decodeNativeScriptsToCardano = (
  preimageCbor: Uint8Array,
): DecodedCardanoScripts => {
  const scripts = decodeMidgardVersionedScriptListPreimage(
    preimageCbor,
    "native.script_tx_wits",
  );
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

const valueFromMultiasset = (multiasset: CML.MultiAsset): CML.Value =>
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
): CML.Redeemers | undefined => {
  return midgardRedeemersToCardano(preimageCbor);
};

export type MidgardToCardanoTxEncodingOptions = {
  readonly omitVkeyWitnesses?: boolean;
};

export const midgardNativeTxFullToCardanoTxEncoding = (
  tx: MidgardNativeTxFullV1,
  options?: MidgardToCardanoTxEncodingOptions,
): Buffer => {
  verifyMidgardNativeTxFullConsistencyV1(tx);

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

  if (!tx.body.scriptIntegrityHash.equals(EMPTY_NULL_ROOT)) {
    body.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(tx.body.scriptIntegrityHash),
    );
  }
  if (!tx.body.auxiliaryDataHash.equals(EMPTY_NULL_ROOT)) {
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
      tx.validity === "TxIsValid",
      undefined,
    ).to_cbor_bytes(),
  );
};
