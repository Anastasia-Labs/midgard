import { CML } from "@lucid-evolution/lucid";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { asBytes } from "./cbor.js";
import { MidgardTransactionFull, MidgardCodecRoundTrip } from "./types.js";

const boolToken = (value: boolean) => Buffer.from([value ? 0xf5 : 0xf4]);
const nullToken = Buffer.from([0xf6]);
const array4Token = Buffer.from([0x84]);

const ensureCmlTransaction = (
  txBytes: Uint8Array,
): InstanceType<typeof CML.Transaction> => {
  try {
    return CML.Transaction.from_cbor_bytes(txBytes);
  } catch (e) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Invalid Midgard full transaction bytes",
      String(e),
    );
  }
};

export const decodeMidgardTransactionFull = (
  txBytes: Uint8Array,
): MidgardTransactionFull => {
  const tx = ensureCmlTransaction(txBytes);
  if (tx.auxiliary_data() !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.DisallowedField,
      "transaction auxiliary data must be absent",
    );
  }

  return {
    transactionBodyCbor: Buffer.from(tx.body().to_cbor_bytes()),
    transactionWitnessSetCbor: Buffer.from(tx.witness_set().to_cbor_bytes()),
    isValid: tx.is_valid(),
    auxiliaryData: null,
  };
};

export const encodeMidgardTransactionFull = (
  tx: MidgardTransactionFull,
): Buffer => {
  if (tx.auxiliaryData !== null) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.DisallowedField,
      "transaction.auxiliary_data must be null",
    );
  }

  const bodyBytes = asBytes(
    tx.transactionBodyCbor,
    "transaction.transaction_body",
  );
  const witnessBytes = asBytes(
    tx.transactionWitnessSetCbor,
    "transaction.transaction_witness_set",
  );

  const encoded = Buffer.concat([
    array4Token,
    bodyBytes,
    witnessBytes,
    boolToken(tx.isValid),
    nullToken,
  ]);

  ensureCmlTransaction(encoded);
  return encoded;
};

export const normalizeMidgardTransactionFullBytes = (
  txBytes: Uint8Array,
): MidgardCodecRoundTrip => {
  const decoded = decodeMidgardTransactionFull(txBytes);
  const normalized = encodeMidgardTransactionFull(decoded);
  return {
    original: Buffer.from(txBytes),
    normalized,
  };
};
