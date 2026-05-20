import { encodeCbor } from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { ensureHashMatch } from "./hash.js";
import {
  deriveNativeTxBodyCompact,
  encodeNativeTxBodyCompactValue,
} from "./native-body.js";
import { MIDGARD_NATIVE_TX_VERSION } from "./native-constants.js";
import {
  deriveNativeTxWitnessSetCompact,
  encodeNativeTxWitnessSetCompactValue,
} from "./native-witness.js";
import type { MidgardNativeTxFull } from "./native.js";

const schemaMismatch = (message: string, detail: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    message,
    detail,
  );
};

export const verifyNativeTxFullConsistency = (
  tx: MidgardNativeTxFull,
): void => {
  if (tx.version !== MIDGARD_NATIVE_TX_VERSION) {
    schemaMismatch("Unsupported Midgard native tx version", `${tx.version}`);
  }
  if (tx.compact.version !== tx.version) {
    schemaMismatch(
      "transaction_full.version must match transaction_compact.version",
      `${tx.version} != ${tx.compact.version}`,
    );
  }
  if (tx.compact.validity !== tx.validity) {
    schemaMismatch(
      "transaction.validity must match transaction_compact.validity",
      `${tx.validity} != ${tx.compact.validity}`,
    );
  }

  const encodedBodyCompact = encodeCbor(
    encodeNativeTxBodyCompactValue(deriveNativeTxBodyCompact(tx.body)),
  );
  const encodedCompactBody = encodeCbor(
    encodeNativeTxBodyCompactValue(tx.compact.transactionBody),
  );

  if (!encodedBodyCompact.equals(encodedCompactBody)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.HashMismatch,
      "transaction_compact.transaction_body must match the derived compact body",
    );
  }

  ensureHashMatch(
    tx.compact.transactionWitnessSetHash,
    encodeCbor(
      encodeNativeTxWitnessSetCompactValue(
        deriveNativeTxWitnessSetCompact(tx.witnessSet),
      ),
    ),
    "transaction_compact.transaction_witness_set_hash",
  );
};
