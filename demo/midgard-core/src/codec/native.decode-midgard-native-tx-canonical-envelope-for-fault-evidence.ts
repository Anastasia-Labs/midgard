import { decodeSingleCbor, encodeCbor } from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32 } from "./hash.js";
import {
  decodeMidgardNativeTxCompact,
  encodeMidgardNativeTxBodyCompact,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxCompact,
  type MidgardNativeTxFull,
  type MidgardNativeTxProofSource,
  type MidgardNativeTxWitnessSetCanonical,
  requireNativeTxVersion,
  validateMidgardNativeTxCanonical,
  verifyMidgardNativeTxFullConsistency,
} from "./native.validate-midgard-native-tx-canonical.js";
import {
  decodeNativeTxBodyCanonicalValue,
  encodeNativeTxBodyCanonicalValue,
} from "./native-body.js";
import { MIDGARD_NATIVE_TX_VERSION } from "./native-constants.js";
import {
  asFixedArray,
  asUnsigned,
  decodeValidityCode,
  encodeValidityCode,
} from "./native-validation.js";
import {
  decodeNativeTxWitnessSetCanonicalValue,
  encodeNativeTxWitnessSetCanonicalValue,
} from "./native-witness.js";

const hasDerivedCompact = (
  tx: MidgardNativeTxCanonical | MidgardNativeTxFull,
): tx is MidgardNativeTxFull => "compact" in tx;

export const encodeMidgardNativeTxCanonical = (
  tx: MidgardNativeTxCanonical | MidgardNativeTxFull,
): Buffer => {
  const version = requireNativeTxVersion(tx.version, "transaction.version");
  if (hasDerivedCompact(tx)) {
    verifyMidgardNativeTxFullConsistency(tx);
  } else {
    validateMidgardNativeTxCanonical(tx);
  }
  return encodeCbor([
    version,
    encodeNativeTxBodyCanonicalValue(tx.body),
    encodeNativeTxWitnessSetCanonicalValue(version, tx.witnessSet),
    encodeValidityCode(tx.validity),
  ]);
};

const encodeMidgardNativeTxCanonicalEnvelope = (
  tx: MidgardNativeTxCanonical,
): Buffer =>
  encodeCbor([
    requireNativeTxVersion(tx.version, "transaction.version"),
    encodeNativeTxBodyCanonicalValue(tx.body),
    encodeNativeTxWitnessSetCanonicalValue(tx.version, tx.witnessSet),
    encodeValidityCode(tx.validity),
  ]);

/**
 * Decodes the exact outer native-V1 transaction envelope for fraud evidence.
 *
 * Unlike {@link decodeMidgardNativeTxCanonical}, this boundary deliberately
 * keeps the nine committed field-preimage byte strings opaque. It exists so a
 * watcher can authenticate and prove a block whose operator committed a
 * malformed §5.1 field envelope. The outer transaction/body/witness records,
 * version, scalar fields, hashes and CBOR encoding remain strict and canonical;
 * normal transaction admission must continue to use the strict decoder below.
 */
export const decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidence = (
  bytes: Uint8Array,
): MidgardNativeTxCanonical => {
  const source = Buffer.from(bytes);
  const decoded = decodeSingleCbor(source);
  const value = asFixedArray(decoded, 4, "transaction");
  const version = requireNativeTxVersion(value[0], "transaction[0]");
  const tx: MidgardNativeTxCanonical = {
    version,
    body: decodeNativeTxBodyCanonicalValue(value[1], "transaction[1]"),
    witnessSet: decodeNativeTxWitnessSetCanonicalValue(
      value[2],
      "transaction[2]",
      version,
    ),
    validity: decodeValidityCode(value[3], "transaction[3]"),
  };
  if (!encodeMidgardNativeTxCanonicalEnvelope(tx).equals(source)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "fault-evidence transaction envelope is not canonical CBOR",
    );
  }
  return tx;
};

export const decodeMidgardNativeTxCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxCanonical => {
  const tx = decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidence(bytes);
  validateMidgardNativeTxCanonical(tx);
  return tx;
};

export const decodeMidgardNativeTxFullFromCanonicalCbor = (
  bytes: Uint8Array,
): MidgardNativeTxFull => {
  const tx = materializeMidgardNativeTxFromCanonical(
    decodeMidgardNativeTxCanonical(bytes),
  );
  verifyMidgardNativeTxFullConsistency(tx);
  return tx;
};

export const computeMidgardNativeTxId = (
  tx:
    | MidgardNativeTxFull
    | MidgardNativeTxCompact
    | Pick<MidgardNativeTxCompact, "version" | "transactionBody">
    | {
        readonly compact: Pick<
          MidgardNativeTxCompact,
          "version" | "transactionBody"
        >;
      },
): Buffer => {
  const compact = "compact" in tx ? tx.compact : tx;
  const version = requireNativeTxVersion(
    compact.version,
    "transaction_compact.version",
  );
  const bodyCbor = encodeMidgardNativeTxBodyCompact(compact.transactionBody);
  return computeHash32(
    Buffer.concat([
      Buffer.from("MidgardNativeTxBodyV1", "ascii"),
      encodeCbor(version),
      bodyCbor,
    ]),
  );
};

const MIDGARD_NATIVE_TX_FULL_HASH_DOMAIN = Buffer.from(
  "MidgardNativeTxFullV1",
  "ascii",
);

/**
 * Commits already-validated canonical V1 transaction bytes without decoding
 * or normalizing them. Admission persistence uses this form so an integrity
 * check commits the exact bytes that crossed the strict ingress boundary.
 */
export const computeMidgardNativeTxFullHashFromCanonicalCbor = (
  canonicalTransactionCbor: Uint8Array,
): Buffer =>
  computeHash32(
    Buffer.concat([
      MIDGARD_NATIVE_TX_FULL_HASH_DOMAIN,
      encodeCbor(MIDGARD_NATIVE_TX_VERSION),
      Buffer.from(canonicalTransactionCbor),
    ]),
  );

/**
 * Commits the exact canonical V1 full transaction, including all witness
 * preimages. This is distinct from the transaction id, which intentionally
 * identifies the compact body.
 */
export const computeMidgardNativeTxFullHash = (
  tx: MidgardNativeTxFull,
): Buffer => {
  if (tx.version !== MIDGARD_NATIVE_TX_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Full transaction commitment requires native transaction V1",
      `actual=${tx.version.toString()}`,
    );
  }
  const canonicalCbor = encodeMidgardNativeTxCanonical(tx);
  return computeMidgardNativeTxFullHashFromCanonicalCbor(canonicalCbor);
};

const MIDGARD_NATIVE_TX_PROOF_SOURCE_DOMAIN = Buffer.from(
  "MidgardNativeTxProofSourceV1",
  "ascii",
);

export const encodeMidgardNativeTxProofSource = (
  source: MidgardNativeTxProofSource,
): Buffer =>
  encodeCbor([
    Buffer.from(source.compactCbor),
    Buffer.from(source.witnessSetCompactCbor),
    Buffer.from(source.fieldPreimageLengthsCbor),
  ]);

export const decodeMidgardNativeTxProofFieldLengths = (
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

/**
 * The nine field-preimage byte lengths **in `docs/spec/midgard-tx.md` §2.4 wire
 * order** — which is not the record's declaration order.
 *
 * §2.4 places `script_witnesses` at wire position 6 and `address_witnesses` at
 * 7, transposed relative to `NativeTxFieldPreimageLengthsV1`, which declares
 * address before script. Both twins already agree on this and MUST NOT change
 * it, so this is where the transposition lives on the TypeScript side and the
 * only place it can be observed: `encodeMidgardNativeTxProofFieldLengths`
 * below takes an already-ordered array and cannot express it.
 *
 * Exported so the §2.4 cross-language golden vector can drive this function
 * rather than a positional array — a vector that only re-serialises nine
 * numbers proves array order, not wire order, and would still pass with the two
 * witness slots swapped.
 */
export const midgardNativeTxProofFieldPreimageLengths = ({
  body,
  witnessSet,
}: {
  readonly body: MidgardNativeTxBodyCanonical;
  readonly witnessSet: MidgardNativeTxWitnessSetCanonical;
}): readonly number[] => [
  body.spendInputsPreimageCbor.length,
  body.referenceInputsPreimageCbor.length,
  body.outputsPreimageCbor.length,
  body.requiredObserversPreimageCbor.length,
  body.requiredSignersPreimageCbor.length,
  body.mintPreimageCbor.length,
  witnessSet.scriptTxWitsPreimageCbor.length,
  witnessSet.addrTxWitsPreimageCbor.length,
  witnessSet.redeemerTxWitsPreimageCbor.length,
];

export const proofFieldPreimageLengths = (
  tx: MidgardNativeTxFull,
): readonly number[] =>
  midgardNativeTxProofFieldPreimageLengths({
    body: tx.body,
    witnessSet: tx.witnessSet,
  });

export const encodeMidgardNativeTxProofFieldLengths = (
  lengths: readonly number[],
): Buffer => {
  if (
    lengths.length !== 9 ||
    lengths.some((length) => !Number.isSafeInteger(length) || length < 0)
  ) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "V1 field-preimage lengths must contain exactly nine non-negative safe integers",
    );
  }
  return encodeCbor(lengths.map((length) => BigInt(length)));
};

export const computeMidgardNativeTxCanonicalSizeFromProofSource = (
  source: MidgardNativeTxProofSource,
): number => {
  const compact = decodeMidgardNativeTxCompact(source.compactCbor);
  const lengths = decodeMidgardNativeTxProofFieldLengths(
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
      Buffer.alloc(lengths[7]!),
      Buffer.alloc(lengths[6]!),
      Buffer.alloc(lengths[8]!),
    ],
    encodeValidityCode(compact.validity),
  ]).length;
};

export const computeMidgardNativeTxProofCommitment = (
  source: MidgardNativeTxProofSource,
): Buffer =>
  computeHash32(
    Buffer.concat([
      MIDGARD_NATIVE_TX_PROOF_SOURCE_DOMAIN,
      encodeCbor(1n),
      encodeMidgardNativeTxProofSource(source),
    ]),
  );
