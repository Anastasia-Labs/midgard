import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk/core";

/**
 * Whether a committed normal `transactions_root` leaf carries a non-zero
 * validity code. A normal leaf is the operator's acceptance verdict, so such a
 * leaf is itself an `l2TxMistag` fault (`l2-tx-mistag/step-01.ak`), and every
 * family whose step opens a normal leaf only at code 0 must pass it over. The
 * code is read from the leaf's committed compact alone, before any input,
 * output or field of the transaction is decoded, so a leaf that is broken in
 * any other way is still recognised. A compact that does not decode is not
 * reported invalid here: it is a source-leaf defect for `daHashPreimage`.
 */
export const isCommittedInvalidNormalLeaf = (
  source: SDK.L2TransactionSource,
): boolean => {
  try {
    return (
      decodeMidgardNativeTxCompact(
        Buffer.from(source.source.compact_cbor, "hex"),
      ).validity !== "TxIsValid"
    );
  } catch {
    return false;
  }
};
