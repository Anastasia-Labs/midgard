import { CML } from "@lucid-evolution/lucid";
import {
  decodeMidgardTransactionCompact,
  encodeMidgardTransactionCompact,
} from "./compact.js";
import {
  decodeMidgardTransactionFull,
  encodeMidgardTransactionFull,
} from "./full.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32, ensureHashMatch } from "./hash.js";
import {
  CardanoToMidgardOptions,
  DecodeMode,
  MidgardTransactionCompact,
} from "./types.js";

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

export const cardanoTxBytesToMidgardFullBytes = (
  cardanoTxBytes: Uint8Array,
  options: CardanoToMidgardOptions = {},
): Buffer => {
  const tx = parseCardanoTx(cardanoTxBytes);

  if (
    options.strictAuxiliaryDataNull === true &&
    tx.auxiliary_data() !== undefined
  ) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Cardano tx contains auxiliary_data, Midgard full requires null auxiliary_data",
    );
  }

  return encodeMidgardTransactionFull({
    transactionBodyCbor: Buffer.from(tx.body().to_cbor_bytes()),
    transactionWitnessSetCbor: Buffer.from(tx.witness_set().to_cbor_bytes()),
    isValid: tx.is_valid(),
    auxiliaryData: null,
  });
};

export const cardanoTxBytesToMidgardCompact = (
  cardanoTxBytes: Uint8Array,
): MidgardTransactionCompact => {
  const tx = parseCardanoTx(cardanoTxBytes);
  const bodyBytes = Buffer.from(tx.body().to_cbor_bytes());
  const witnessBytes = Buffer.from(tx.witness_set().to_cbor_bytes());

  return {
    transactionBodyHash: computeHash32(bodyBytes),
    transactionWitnessSetHash: computeHash32(witnessBytes),
    isValid: tx.is_valid(),
  };
};

export const cardanoTxBytesToMidgardCompactBytes = (
  cardanoTxBytes: Uint8Array,
): Buffer =>
  encodeMidgardTransactionCompact(
    cardanoTxBytesToMidgardCompact(cardanoTxBytes),
  );

export const midgardFullBytesToCardanoTxBytes = (
  midgardFullBytes: Uint8Array,
): Buffer => {
  const decoded = decodeMidgardTransactionFull(midgardFullBytes);
  const txBytes = encodeMidgardTransactionFull(decoded);
  parseCardanoTx(txBytes);
  return txBytes;
};

export const midgardCompactBytesToCardanoTxBytes = (
  _midgardCompactBytes: Uint8Array,
): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.UnsupportedCompactToCardano,
    "Compact Midgard tx cannot be converted to Cardano tx without preimage resolver",
  );
};

export const verifyCompactHashesAgainstFull = (
  midgardCompactBytes: Uint8Array,
  midgardFullBytes: Uint8Array,
  mode: DecodeMode = "strict",
): void => {
  const compact = decodeMidgardTransactionCompact(midgardCompactBytes, mode);
  const full = decodeMidgardTransactionFull(midgardFullBytes);

  ensureHashMatch(
    compact.transactionBodyHash,
    full.transactionBodyCbor,
    "transaction_body_hash",
  );
  ensureHashMatch(
    compact.transactionWitnessSetHash,
    full.transactionWitnessSetCbor,
    "transaction_witness_set_hash",
  );
};
