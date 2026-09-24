import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import type { HistoryChainTransaction } from "./l1-event-history-transaction.js";
import type { LedgerSnapshotOutput } from "./l1-ledger-snapshot.js";

/** Verify untrusted creating BODY bytes against an actual reference of a valid,
 * source-admitted observing transaction. The source supplies existence/liveness;
 * the hash supplies contents. Neither an archive's validity flag nor a matching
 * standalone datum hash establishes this authority. Preserve original CBOR.
 */
export const verifyEventHistoryReferenceBody = ({
  transaction,
  ref,
  creatingBodyCbor,
  maximumBodyBytes,
}: {
  readonly transaction: HistoryChainTransaction;
  readonly ref: OutRefLike;
  readonly creatingBodyCbor: string;
  readonly maximumBodyBytes: number;
}): LedgerSnapshotOutput => {
  if (
    transaction.spends !== "inputs" ||
    ref.txHash === transaction.txHash ||
    !Number.isSafeInteger(ref.outputIndex) ||
    ref.outputIndex < 0 ||
    !transaction.references.some(
      (entry) =>
        entry.txHash === ref.txHash && entry.outputIndex === ref.outputIndex,
    )
  )
    throw new Error(
      "History reference is not an admitted ordinary transaction reference",
    );
  if (
    !Number.isSafeInteger(maximumBodyBytes) ||
    maximumBodyBytes <= 0 ||
    typeof creatingBodyCbor !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(creatingBodyCbor) ||
    creatingBodyCbor.length / 2 > maximumBodyBytes
  )
    throw new Error("History creating body exceeds its exact byte bounds");
  if (
    computeHash32(Buffer.from(creatingBodyCbor, "hex")).toString("hex") !==
    ref.txHash
  )
    throw new Error(
      "History creating body does not match its referenced transaction hash",
    );
  const body = CML.TransactionBody.from_cbor_hex(creatingBodyCbor);
  try {
    if (body.to_cbor_hex() !== creatingBodyCbor)
      throw new Error("History creating body encoding was not preserved");
    const outputs = body.outputs();
    try {
      const output =
        ref.outputIndex < outputs.len()
          ? outputs.get(ref.outputIndex)
          : ref.outputIndex === outputs.len()
            ? body.collateral_return()
            : undefined;
      if (output === undefined)
        throw new Error("History referenced creating output does not exist");
      try {
        const decoded = coreToTxOutput(output);
        return Object.freeze({
          txHash: ref.txHash,
          outputIndex: ref.outputIndex,
          address: decoded.address,
          assets: Object.freeze({ ...decoded.assets }),
          ...(decoded.datum == null ? {} : { datum: decoded.datum }),
          ...(decoded.datumHash == null
            ? {}
            : { datumHash: decoded.datumHash }),
          hasReferenceScript: decoded.scriptRef != null,
        });
      } finally {
        output.free();
      }
    } finally {
      outputs.free();
    }
  } finally {
    body.free();
  }
};
