import { asBytes, decodeSingleCbor, encodeCbor } from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32, ensureHash32 } from "./hash.js";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxProofFieldLengths,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofSource,
  encodeMidgardNativeTxWitnessSetCompact,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxCompact,
  type MidgardNativeTxFull,
  midgardNativeTxProofFieldPreimageLengths,
  validateMidgardNativeTxCanonical,
  verifyMidgardNativeTxFullConsistency,
} from "./native.js";
import {
  decodeNativeTxBodyCanonicalValue,
  decodeNativeTxBodyCompactValue,
  encodeNativeTxBodyCanonicalValue,
  encodeNativeTxBodyCompactValue,
} from "./native-body.js";
import { asFixedArray, decodeVersion } from "./native-validation.js";
import {
  decodeNativeTxWitnessSetCanonicalValue,
  encodeNativeTxWitnessSetCanonicalValue,
} from "./native-witness.js";

/** User-authored material. Neither this type nor its compact encoding has a verdict. */
export type MidgardForcedTxCanonical = Omit<
  MidgardNativeTxCanonical,
  "validity"
>;
export type MidgardForcedTxCompact = Omit<MidgardNativeTxCompact, "validity">;
export type MidgardForcedTxFull = MidgardForcedTxCanonical & {
  readonly compact: MidgardForcedTxCompact;
};
export type MidgardForcedTxProofSource = {
  readonly compactCbor: Buffer;
  readonly witnessSetCompactCbor: Buffer;
  readonly fieldPreimageLengthsCbor: Buffer;
};

const requireCanonical = (actual: Uint8Array, encoded: Buffer): void => {
  if (!encoded.equals(actual)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Forced submission is not canonical CBOR",
    );
  }
};

export const encodeMidgardForcedTxCompact = (
  tx: MidgardForcedTxCompact,
): Buffer =>
  encodeCbor([
    decodeVersion(tx.version, "forced.version"),
    encodeNativeTxBodyCompactValue(tx.transactionBody),
    ensureHash32(tx.transactionWitnessSetHash, "forced.witness_set_hash"),
  ]);

export const decodeMidgardForcedTxCompact = (
  bytes: Uint8Array,
): MidgardForcedTxCompact => {
  const value = asFixedArray(decodeSingleCbor(bytes), 3, "forced.compact");
  const tx: MidgardForcedTxCompact = {
    version: decodeVersion(value[0], "forced.version"),
    transactionBody: decodeNativeTxBodyCompactValue(value[1], "forced.body"),
    transactionWitnessSetHash: ensureHash32(
      asBytes(value[2], "forced.witness_set_hash"),
      "forced.witness_set_hash",
    ),
  };
  requireCanonical(bytes, encodeMidgardForcedTxCompact(tx));
  return tx;
};

const encodeEnvelope = (tx: MidgardForcedTxCanonical): Buffer =>
  encodeCbor([
    decodeVersion(tx.version, "forced.version"),
    encodeNativeTxBodyCanonicalValue(tx.body),
    encodeNativeTxWitnessSetCanonicalValue(tx.version, tx.witnessSet),
  ]);

/** Inner field byte strings remain opaque so malformed fields can be challenged. */
export const decodeMidgardForcedTxCanonicalEnvelopeForFaultEvidence = (
  bytes: Uint8Array,
): MidgardForcedTxCanonical => {
  const value = asFixedArray(decodeSingleCbor(bytes), 3, "forced.transaction");
  const version = decodeVersion(value[0], "forced.version");
  const tx: MidgardForcedTxCanonical = {
    version,
    body: decodeNativeTxBodyCanonicalValue(value[1], "forced.body"),
    witnessSet: decodeNativeTxWitnessSetCanonicalValue(
      value[2],
      "forced.witness_set",
      version,
    ),
  };
  requireCanonical(bytes, encodeEnvelope(tx));
  return tx;
};

export const decodeMidgardForcedTxCanonical = (
  bytes: Uint8Array,
): MidgardForcedTxCanonical => {
  const tx = decodeMidgardForcedTxCanonicalEnvelopeForFaultEvidence(bytes);
  validateMidgardNativeTxCanonical(tx);
  return tx;
};

export const deriveMidgardForcedTxCompact = (
  tx: MidgardForcedTxCanonical,
): MidgardForcedTxCompact => ({
  version: decodeVersion(tx.version, "forced.version"),
  transactionBody: deriveMidgardNativeTxBodyCompact(tx.body),
  transactionWitnessSetHash: computeHash32(
    encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet),
    ),
  ),
});

export const verifyMidgardForcedTxFullConsistency = (
  tx: MidgardForcedTxFull,
): void => {
  requireCanonical(
    encodeMidgardForcedTxCompact(tx.compact),
    encodeMidgardForcedTxCompact(deriveMidgardForcedTxCompact(tx)),
  );
  validateMidgardNativeTxCanonical(tx);
};

export const encodeMidgardForcedTxCanonical = (
  tx: MidgardForcedTxCanonical | MidgardForcedTxFull,
): Buffer => {
  if ("compact" in tx) verifyMidgardForcedTxFullConsistency(tx);
  else validateMidgardNativeTxCanonical(tx);
  return encodeEnvelope(tx);
};

export const materializeMidgardForcedTxFromCanonical = (
  tx: MidgardForcedTxCanonical,
): MidgardForcedTxFull => {
  validateMidgardNativeTxCanonical(tx);
  return {
    version: tx.version,
    body: tx.body,
    witnessSet: tx.witnessSet,
    compact: deriveMidgardForcedTxCompact(tx),
  };
};

export const decodeMidgardForcedTxFullFromCanonicalCbor = (
  bytes: Uint8Array,
): MidgardForcedTxFull =>
  materializeMidgardForcedTxFromCanonical(
    decodeMidgardForcedTxCanonical(bytes),
  );

/** Explicit client projection before L1 submission; never a forced-source decoder. */
export const submittedForcedTransactionFromNative = (
  tx: MidgardNativeTxFull,
): MidgardForcedTxFull => {
  verifyMidgardNativeTxFullConsistency(tx);
  if (tx.validity !== "TxIsValid") {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Forced submission construction requires an admission-valid native transaction",
    );
  }
  return materializeMidgardForcedTxFromCanonical({
    version: tx.version,
    body: tx.body,
    witnessSet: tx.witnessSet,
  });
};

export const encodeMidgardForcedTxProofSource = (
  source: MidgardForcedTxProofSource,
): Buffer => encodeMidgardNativeTxProofSource(source);

export const computeMidgardForcedTxProofCommitment = (
  source: MidgardForcedTxProofSource,
): Buffer =>
  computeHash32(
    Buffer.concat([
      Buffer.from("MidgardForcedTxProofSourceV1", "ascii"),
      encodeCbor(1n),
      encodeMidgardForcedTxProofSource(source),
    ]),
  );

const proofSourceFromMaterial = (
  tx: MidgardForcedTxCanonical,
  compact: MidgardForcedTxCompact,
): MidgardForcedTxProofSource => ({
  compactCbor: encodeMidgardForcedTxCompact(compact),
  witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet),
  ),
  fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(
    midgardNativeTxProofFieldPreimageLengths(tx),
  ),
});

export const deriveMidgardForcedTxProofSource = (
  tx: MidgardForcedTxFull,
): MidgardForcedTxProofSource => {
  verifyMidgardForcedTxFullConsistency(tx);
  return proofSourceFromMaterial(tx, tx.compact);
};

export const deriveMidgardForcedTxProofSourceFromCanonicalCbor = (
  bytes: Uint8Array,
): MidgardForcedTxProofSource =>
  deriveMidgardForcedTxProofSource(
    decodeMidgardForcedTxFullFromCanonicalCbor(bytes),
  );

export const deriveMidgardForcedTxFaultEvidenceMaterial = (
  bytes: Uint8Array,
) => {
  const canonical =
    decodeMidgardForcedTxCanonicalEnvelopeForFaultEvidence(bytes);
  const compact = deriveMidgardForcedTxCompact(canonical);
  return {
    canonical,
    compact,
    transactionId: computeMidgardNativeTxId(compact),
    proofSource: proofSourceFromMaterial(canonical, compact),
    fieldPreimages: [
      canonical.body.spendInputsPreimageCbor,
      canonical.body.referenceInputsPreimageCbor,
      canonical.body.outputsPreimageCbor,
      canonical.body.requiredObserversPreimageCbor,
      canonical.body.requiredSignersPreimageCbor,
      canonical.body.mintPreimageCbor,
      canonical.witnessSet.scriptTxWitsPreimageCbor,
      canonical.witnessSet.addrTxWitsPreimageCbor,
      canonical.witnessSet.redeemerTxWitsPreimageCbor,
    ],
  };
};

export const verifyMidgardForcedTxProofSource = ({
  transactionId,
  source,
}: {
  readonly transactionId: Uint8Array;
  readonly source: MidgardForcedTxProofSource;
}): MidgardForcedTxCompact => {
  const compact = decodeMidgardForcedTxCompact(source.compactCbor);
  const witness = decodeMidgardNativeTxWitnessSetCompact(
    source.witnessSetCompactCbor,
  );
  requireCanonical(
    source.witnessSetCompactCbor,
    encodeMidgardNativeTxWitnessSetCompact(witness),
  );
  requireCanonical(
    source.fieldPreimageLengthsCbor,
    encodeMidgardNativeTxProofFieldLengths(
      decodeMidgardNativeTxProofFieldLengths(source.fieldPreimageLengthsCbor),
    ),
  );
  if (
    !computeHash32(source.witnessSetCompactCbor).equals(
      compact.transactionWitnessSetHash,
    ) ||
    !computeMidgardNativeTxId(compact).equals(transactionId)
  ) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.HashMismatch,
      "Forced source does not match its transaction ID or witness commitment",
    );
  }
  return compact;
};

/** Logical ledger size retains the one-byte charge of the original transaction envelope. */
export const computeMidgardForcedTxCanonicalSizeFromProofSource = (
  source: MidgardForcedTxProofSource,
): number => {
  const compact = decodeMidgardForcedTxCompact(source.compactCbor);
  const lengths = decodeMidgardNativeTxProofFieldLengths(
    source.fieldPreimageLengthsCbor,
  );
  const bytesSize = (length: number): number =>
    length +
    (length < 24
      ? 1
      : length <= 0xff
        ? 2
        : length <= 0xffff
          ? 3
          : length <= 0xffffffff
            ? 5
            : 9);
  const body = compact.transactionBody;
  const size =
    1 +
    encodeCbor(compact.version).length +
    1 +
    1 +
    1 +
    lengths.reduce((sum, length) => sum + bytesSize(length), 0) +
    [
      body.fee,
      body.validityIntervalStart,
      body.validityIntervalEnd,
      body.networkId,
    ].reduce((sum, value) => sum + encodeCbor(value).length, 0) +
    68;
  if (!Number.isSafeInteger(size))
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Forced ledger size exceeds the exact integer range",
    );
  return size;
};
