import { asBytes, decodeSingleCbor, encodeCbor } from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32, ensureHash32, type Hash32 } from "./hash.js";
import {
  decodeNativeTxBodyCanonicalCbor,
  decodeNativeTxBodyCompactCbor,
  decodeNativeTxBodyCompactValue,
  deriveNativeTxBodyCompact,
  encodeNativeTxBodyCanonicalCbor,
  encodeNativeTxBodyCompactCbor,
  encodeNativeTxBodyCompactValue,
} from "./native-body.js";
import { verifyNativeTxFullConsistency } from "./native-consistency.js";
import { MIDGARD_NATIVE_TX_VERSION } from "./native-constants.js";
import { decodeMidgardFieldPreimage } from "./native-tx-field-access.js";
import {
  asFixedArray,
  decodeValidityCode,
  decodeVersion,
  encodeValidityCode,
  type MidgardTxValidity,
} from "./native-validation.js";
import {
  decodeNativeTxWitnessPreimagesCbor,
  decodeNativeTxWitnessSetCompactCbor,
  deriveNativeTxWitnessSetCompact,
  encodeNativeTxWitnessPreimagesCbor,
  encodeNativeTxWitnessSetCompactCbor,
} from "./native-witness.js";

export type MidgardNativeTxCompact = {
  readonly version: bigint;
  readonly transactionBody: MidgardNativeTxBodyCompact;
  readonly transactionWitnessSetHash: Hash32;
  readonly validity: MidgardTxValidity;
};

export type MidgardNativeTxBodyCompact = {
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

export type MidgardNativeTxWitnessSetCompact = {
  readonly addrTxWitsHash: Hash32;
  readonly scriptTxWitsHash: Hash32;
  readonly redeemerTxWitsHash: Hash32;
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

export type MidgardNativeTxFull = MidgardNativeTxCanonical & {
  readonly compact: MidgardNativeTxCompact;
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
export type MidgardNativeTxProofSource = {
  readonly compactCbor: Buffer;
  readonly witnessSetCompactCbor: Buffer;
  readonly fieldPreimageLengthsCbor: Buffer;
};

export const requireNativeTxVersion = (
  value: unknown,
  fieldName: string,
): typeof MIDGARD_NATIVE_TX_VERSION => {
  const version = decodeVersion(value, fieldName);
  if (version !== MIDGARD_NATIVE_TX_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must equal ${MIDGARD_NATIVE_TX_VERSION.toString()}`,
      `actual=${version.toString()}`,
    );
  }
  return MIDGARD_NATIVE_TX_VERSION;
};

const encodeNativeTxCompactValue = (
  tx: MidgardNativeTxCompact,
): readonly [
  bigint,
  ReturnType<typeof encodeNativeTxBodyCompactValue>,
  Hash32,
  bigint,
] => [
  requireNativeTxVersion(tx.version, "transaction_compact.version"),
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
): MidgardNativeTxCompact => {
  const v = asFixedArray(value, 4, fieldName);
  return {
    version: requireNativeTxVersion(v[0], `${fieldName}[0]`),
    transactionBody: decodeNativeTxBodyCompactValue(v[1], `${fieldName}[1]`),
    transactionWitnessSetHash: ensureHash32(
      asBytes(v[2], `${fieldName}[2]`),
      `${fieldName}[2]`,
    ),
    validity: decodeValidityCode(v[3], `${fieldName}[3]`),
  };
};

export const deriveMidgardNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCanonical,
): MidgardNativeTxBodyCompact => deriveNativeTxBodyCompact(body);

export const deriveMidgardNativeTxWitnessSetCompact = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): MidgardNativeTxWitnessSetCompact =>
  deriveNativeTxWitnessSetCompact(witnessSet);

export const toMidgardNativeTxCanonical = (
  tx: MidgardNativeTxFull,
): MidgardNativeTxCanonical => ({
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

export const deriveMidgardNativeTxCompact = (
  body: MidgardNativeTxBodyCanonical,
  witnessSet: MidgardNativeTxWitnessSetCanonical,
  validity: MidgardTxValidity,
  version = MIDGARD_NATIVE_TX_VERSION,
): MidgardNativeTxCompact => {
  requireNativeTxVersion(version, "transaction_compact.version");
  const bodyCompact = deriveMidgardNativeTxBodyCompact(body);
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompact(witnessSet);
  return {
    version,
    transactionBody: bodyCompact,
    transactionWitnessSetHash: computeHash32(
      encodeMidgardNativeTxWitnessSetCompact(witnessCompact),
    ),
    validity,
  };
};

export const materializeMidgardNativeTxFromCanonical = (
  canonical: MidgardNativeTxCanonical,
): MidgardNativeTxFull => {
  const version = requireNativeTxVersion(
    canonical.version,
    "transaction_canonical.version",
  );
  validateMidgardNativeTxCanonical(canonical);
  const compact = deriveMidgardNativeTxCompact(
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

export const verifyMidgardNativeTxFullConsistency = (
  tx: MidgardNativeTxFull,
): void => verifyNativeTxFullConsistency(tx);

export const encodeMidgardNativeTxCompact = (
  tx: MidgardNativeTxCompact,
): Buffer => encodeCbor(encodeNativeTxCompactValue(tx));

export const decodeMidgardNativeTxCompact = (
  bytes: Uint8Array,
): MidgardNativeTxCompact =>
  decodeNativeTxCompactValue(decodeSingleCbor(bytes), "transaction_compact");

export const encodeMidgardNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCompact,
): Buffer => encodeNativeTxBodyCompactCbor(body);

export const decodeMidgardNativeTxBodyCompact = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompact => decodeNativeTxBodyCompactCbor(bytes);

export const encodeMidgardNativeTxWitnessSetCompact = (
  witnessSet: MidgardNativeTxWitnessSetCompact,
): Buffer => encodeNativeTxWitnessSetCompactCbor(witnessSet);

export const decodeMidgardNativeTxWitnessSetCompact = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact =>
  decodeNativeTxWitnessSetCompactCbor(bytes);

export const encodeMidgardNativeTxBodyCanonical = (
  body: MidgardNativeTxBodyCanonical,
): Buffer => encodeNativeTxBodyCanonicalCbor(body);

export const decodeMidgardNativeTxBodyCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCanonical => decodeNativeTxBodyCanonicalCbor(bytes);

export const encodeMidgardNativeTxWitnessPreimages = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): Buffer => encodeNativeTxWitnessPreimagesCbor(witnessSet);

export const decodeMidgardNativeTxWitnessPreimages = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCanonical =>
  decodeNativeTxWitnessPreimagesCbor(bytes);

/**
 * §5.1's fail-closed check over all nine field preimages: wrapper/length
 * mismatch, a non-minimal header, an item count disagreeing with the walked
 * content, and trailing bytes after item `N-1` all reject.
 *
 * This is the canonical form's only structural validator, and it is deliberately
 * **not** inside {@link deriveNativeTxBodyCompact}. §4 defines a field commitment
 * over bytes, so the hash must not depend on a parse, and the Aiken twins keep
 * the two apart for the same reason (`field_commitment` first, then an in-place
 * walk). It runs where given bytes become a committed transaction — the encoder,
 * the canonical decoder, and materialisation — so a caller cannot obtain a
 * commitment for a preimage no decoder would accept, while a caller that already
 * holds producer-built bytes pays only for the hash.
 *
 * The retired counted derivation provided this incidentally: building a bounded
 * collection had to split every preimage into items first, so this function used
 * to be a bare `deriveMidgardNativeTxCompact` call kept for its exceptions.
 * Under §4 nothing forces the split, so the check is stated rather than borrowed.
 */
export const validateMidgardNativeTxCanonical = (
  tx: Pick<MidgardNativeTxCanonical, "body" | "witnessSet">,
): void => {
  const fields: readonly (readonly [string, Uint8Array])[] = [
    ["spend_inputs", tx.body.spendInputsPreimageCbor],
    ["reference_inputs", tx.body.referenceInputsPreimageCbor],
    ["outputs", tx.body.outputsPreimageCbor],
    ["required_observers", tx.body.requiredObserversPreimageCbor],
    ["required_signers", tx.body.requiredSignersPreimageCbor],
    ["mint", tx.body.mintPreimageCbor],
    ["script_tx_wits", tx.witnessSet.scriptTxWitsPreimageCbor],
    ["addr_tx_wits", tx.witnessSet.addrTxWitsPreimageCbor],
    ["redeemer_tx_wits", tx.witnessSet.redeemerTxWitsPreimageCbor],
  ];
  for (const [fieldName, preimage] of fields) {
    try {
      decodeMidgardFieldPreimage(preimage);
    } catch (error) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.CborDecode,
        `transaction_canonical.${fieldName} is not a canonical §5.1 field preimage`,
        String(error),
      );
    }
  }
};
