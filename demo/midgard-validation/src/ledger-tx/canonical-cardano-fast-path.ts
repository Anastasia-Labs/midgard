import {
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";

import type { MidgardOutRef } from "./types.js";

const TX_ID_LENGTH = 32;
const MAX_UINT64 = 0xffff_ffff_ffff_ffffn;

/**
 * Proves and decodes the exact canonical CML TransactionInput wire shape.
 *
 * Returning undefined is intentional: the caller delegates every shape not
 * proven by this fast path to CML's authoritative Cardano decoder.
 */
export const decodeCanonicalTransactionInput = (
  bytes: Uint8Array,
): MidgardOutRef | undefined => {
  try {
    const array = readCborArrayHeader(bytes, 0, "TransactionInput");
    if (array.length !== 2) return undefined;
    const txId = readCborBytes(
      bytes,
      array.nextOffset,
      "TransactionInput.transaction_id",
    );
    if (txId.value.length !== TX_ID_LENGTH) return undefined;
    const index = readCborUnsigned(
      bytes,
      txId.nextOffset,
      "TransactionInput.index",
    );
    if (index.value > MAX_UINT64 || index.nextOffset !== bytes.length) {
      return undefined;
    }
    return { txId: txId.value, index: index.value };
  } catch {
    return undefined;
  }
};
