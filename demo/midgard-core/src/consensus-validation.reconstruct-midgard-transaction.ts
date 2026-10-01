import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardForcedTxCompact,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
  verifyMidgardForcedTxProofSource,
} from "./codec/forced.js";
import {
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengths,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
  type MidgardNativeTxProofSource,
  verifyMidgardNativeTxProofSource,
} from "./codec/native.js";
import { midgardFieldCommitment } from "./codec/native-tx-field-access.js";

export type MidgardConsensusViolationCode =
  | "E_TX_VERSION"
  | "E_TX_SIZE"
  | "E_IS_VALID_FALSE_FORBIDDEN"
  | "E_AUX_DATA_FORBIDDEN"
  | "E_INPUT_COUNT"
  | "E_REFERENCE_INPUT_COUNT"
  | "E_OUTPUT_COUNT"
  | "E_ADDRESS_WITNESS_COUNT"
  | "E_REQUIRED_SIGNER_COUNT"
  | "E_SCRIPT_EXECUTION_COUNT"
  | "E_OBSERVER_COUNT"
  | "E_FIELD_PREIMAGE_SIZE"
  | "E_LEDGER_OUTPUT_SIZE"
  | "E_VALUE_SIZE"
  | "E_SCRIPT_PROGRAM_SIZE"
  | "E_SCRIPT_PROGRAM_ENCODING";

export type MidgardConsensusViolation = {
  readonly code: MidgardConsensusViolationCode;
  readonly featureId: string;
  readonly detail: string;
};

export const MIDGARD_TX_FIELD_NAMES = [
  "spend_inputs",
  "reference_inputs",
  "outputs",
  "required_observers",
  "required_signers",
  "mint",
  "script_witnesses",
  "address_witnesses",
  "redeemers",
] as const;

export type MidgardTxFieldName = (typeof MIDGARD_TX_FIELD_NAMES)[number];

export type MidgardTxFieldPreimage = {
  readonly fieldIndex: number;
  readonly fieldName: MidgardTxFieldName;
  readonly preimageCbor: Buffer;
  readonly expectedHash: Buffer;
};

/**
 * §4's nine committed field hashes, extracted **positionally** from a proof
 * source's own compact structures.
 *
 * The twin of `native_tx_field_access_v1.field_commitment_at`, and the reason
 * both halves of a §8 door call can name the same expected commitment: §4 removed
 * field-index domain separation, so a flat hash says nothing about which slot it
 * came from and the slot has to come from the structure. Fields 0–5 are read off
 * the compact body, 6–8 off the compact witness set — §2.5's split, which is why
 * a consumer that only checked one of the two structures would pass a transaction
 * whose material lives in the other.
 *
 * It does **not** authenticate the source. `verifyMidgardNativeTxProofSource`
 * and the `transaction_commitment` comparison are the caller's, exactly as the
 * on-chain door leaves `witness_set_hash`'s own provenance to its caller.
 */
export const midgardTxFieldCommitmentsFromSource = (
  source: MidgardNativeTxProofSource,
  sourceKind: "normal" | "forced" = "normal",
): readonly Buffer[] => {
  const compact = (
    sourceKind === "forced"
      ? decodeMidgardForcedTxCompact
      : decodeMidgardNativeTxCompact
  )(source.compactCbor);
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    source.witnessSetCompactCbor,
  );
  return [
    compact.transactionBody.spendInputsHash,
    compact.transactionBody.referenceInputsHash,
    compact.transactionBody.outputsHash,
    compact.transactionBody.requiredObserversHash,
    compact.transactionBody.requiredSignersHash,
    compact.transactionBody.mintHash,
    witnessSet.scriptTxWitsHash,
    witnessSet.addrTxWitsHash,
    witnessSet.redeemerTxWitsHash,
  ].map((hash) => Buffer.from(hash));
};

export const deriveMidgardTxFieldPreimages = (
  canonicalTransactionCbor: Uint8Array,
  sourceKind: "normal" | "forced" = "normal",
): readonly MidgardTxFieldPreimage[] => {
  const tx = (
    sourceKind === "forced"
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor
  )(canonicalTransactionCbor);
  const source = (
    sourceKind === "forced"
      ? deriveMidgardForcedTxProofSourceFromCanonicalCbor
      : deriveMidgardNativeTxProofSourceFromCanonicalCbor
  )(canonicalTransactionCbor);
  const preimages = [
    tx.body.spendInputsPreimageCbor,
    tx.body.referenceInputsPreimageCbor,
    tx.body.outputsPreimageCbor,
    tx.body.requiredObserversPreimageCbor,
    tx.body.requiredSignersPreimageCbor,
    tx.body.mintPreimageCbor,
    tx.witnessSet.scriptTxWitsPreimageCbor,
    tx.witnessSet.addrTxWitsPreimageCbor,
    tx.witnessSet.redeemerTxWitsPreimageCbor,
  ] as const;
  const hashes = midgardTxFieldCommitmentsFromSource(source, sourceKind);
  return preimages.map((preimageCbor, fieldIndex) => ({
    fieldIndex,
    fieldName: MIDGARD_TX_FIELD_NAMES[fieldIndex]!,
    preimageCbor: Buffer.from(preimageCbor),
    expectedHash: hashes[fieldIndex]!,
  }));
};

export const verifyMidgardTxFieldPreimage = ({
  transactionId,
  transactionCommitment,
  source,
  sourceKind = "normal",
  fieldIndex,
  preimageCbor,
}: {
  readonly transactionId: Uint8Array;
  readonly transactionCommitment: Uint8Array;
  readonly source: MidgardNativeTxProofSource;
  readonly sourceKind?: "normal" | "forced";
  readonly fieldIndex: number;
  readonly preimageCbor: Uint8Array;
}): MidgardTxFieldPreimage => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_TX_FIELD_NAMES.length
  ) {
    throw new Error(`unknown V1 transaction field index ${fieldIndex}`);
  }
  (sourceKind === "forced"
    ? verifyMidgardForcedTxProofSource
    : verifyMidgardNativeTxProofSource)({ transactionId, source });
  const computedCommitment = (
    sourceKind === "forced"
      ? computeMidgardForcedTxProofCommitment
      : computeMidgardNativeTxProofCommitment
  )(source);
  if (!computedCommitment.equals(Buffer.from(transactionCommitment))) {
    throw new Error(
      "V1 transaction field source does not match transaction commitment",
    );
  }
  const hashes = midgardTxFieldCommitmentsFromSource(source, sourceKind);
  const committedLength = decodeMidgardNativeTxProofFieldLengths(
    source.fieldPreimageLengthsCbor,
  )[fieldIndex]!;
  if (preimageCbor.length !== committedLength) {
    throw new Error(
      `V1 ${MIDGARD_TX_FIELD_NAMES[fieldIndex]} preimage length does not match its compact source: ${preimageCbor.length.toString()} != ${committedLength.toString()}`,
    );
  }
  const expectedHash = hashes[fieldIndex]!;
  if (!midgardFieldCommitment(preimageCbor).equals(expectedHash)) {
    throw new Error(
      `V1 ${MIDGARD_TX_FIELD_NAMES[fieldIndex]} preimage hash mismatch`,
    );
  }
  return {
    fieldIndex,
    fieldName: MIDGARD_TX_FIELD_NAMES[fieldIndex]!,
    preimageCbor: Buffer.from(preimageCbor),
    expectedHash,
  };
};

export const reconstructMidgardTransaction = ({
  transactionId,
  transactionCommitment,
  source,
  sourceKind = "normal",
  fieldPreimages,
}: {
  readonly transactionId: Uint8Array;
  readonly transactionCommitment: Uint8Array;
  readonly source: MidgardNativeTxProofSource;
  readonly sourceKind?: "normal" | "forced";
  readonly fieldPreimages: readonly Uint8Array[];
}): Buffer => {
  if (fieldPreimages.length !== MIDGARD_TX_FIELD_NAMES.length) {
    throw new Error(
      `V1 transaction reconstruction requires exactly ${MIDGARD_TX_FIELD_NAMES.length.toString()} field preimages`,
    );
  }
  const verified = fieldPreimages.map((preimageCbor, fieldIndex) =>
    verifyMidgardTxFieldPreimage({
      transactionId,
      transactionCommitment,
      source,
      sourceKind,
      fieldIndex,
      preimageCbor,
    }),
  );
  const compact = (
    sourceKind === "forced"
      ? verifyMidgardForcedTxProofSource
      : verifyMidgardNativeTxProofSource
  )({ transactionId, source });
  const material = {
    version: compact.version,
    body: {
      spendInputsPreimageCbor: verified[0]!.preimageCbor,
      referenceInputsPreimageCbor: verified[1]!.preimageCbor,
      outputsPreimageCbor: verified[2]!.preimageCbor,
      fee: compact.transactionBody.fee,
      validityIntervalStart: compact.transactionBody.validityIntervalStart,
      validityIntervalEnd: compact.transactionBody.validityIntervalEnd,
      requiredObserversPreimageCbor: verified[3]!.preimageCbor,
      requiredSignersPreimageCbor: verified[4]!.preimageCbor,
      mintPreimageCbor: verified[5]!.preimageCbor,
      scriptIntegrityHash: Buffer.from(
        compact.transactionBody.scriptIntegrityHash,
      ),
      auxiliaryDataHash: Buffer.from(compact.transactionBody.auxiliaryDataHash),
      networkId: compact.transactionBody.networkId,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: verified[7]!.preimageCbor,
      scriptTxWitsPreimageCbor: verified[6]!.preimageCbor,
      redeemerTxWitsPreimageCbor: verified[8]!.preimageCbor,
    },
  };
  return sourceKind === "forced"
    ? encodeMidgardForcedTxCanonical(material)
    : encodeMidgardNativeTxCanonical({
        ...material,
        validity: decodeMidgardNativeTxCompact(source.compactCbor).validity,
      });
};

export const violation = (
  code: MidgardConsensusViolationCode,
  featureId: string,
  detail: string,
): MidgardConsensusViolation => ({ code, featureId, detail });

export const enforceCount = (
  count: number,
  maximum: number,
  code: MidgardConsensusViolationCode,
  featureId: string,
): MidgardConsensusViolation | null =>
  count <= maximum
    ? null
    : violation(code, featureId, `${count.toString()} > ${maximum.toString()}`);

export const enforcePreimageSize = (
  bytes: Uint8Array,
  maximum: number,
  featureId: string,
): MidgardConsensusViolation | null =>
  bytes.length <= maximum
    ? null
    : violation(
        "E_FIELD_PREIMAGE_SIZE",
        featureId,
        `${bytes.length.toString()} > ${maximum.toString()}`,
      );
