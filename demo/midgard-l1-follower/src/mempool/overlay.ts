/**
 * The node's mempool as a pending layer over the ledger at its tip.
 *
 * The node checks the next submitted transaction against the ledger with
 * every mempool transaction applied in mempool order, so a read that decides
 * what to build next uses the same state: an input a pending transaction
 * spends (regular inputs only; collateral is consumed only when scripts fail)
 * is gone, and an output it creates exists. Spent wins over created, so an
 * output one pending transaction creates and a later one spends is listed
 * nowhere.
 *
 * A pending transaction has no inclusion point and no depth: it is never
 * ledger evidence and its type carries nothing that could pass for it.
 *
 * Each listed transaction is decoded once, here: the follower's own decoder
 * gives each output exactly the Lucid shape the follower gives the same
 * output once it lands, and CML gives the body, the witness set and each
 * output in the canonical encoding an admitted snapshot holds. Every wasm
 * object is freed before the call returns.
 */
import { CML, type UTxO } from "@lucid-evolution/lucid";

import { decodeTransaction } from "../decode/tx.js";
import { toLucidUtxo } from "../provider/utxo.js";
import type { OutRef } from "../types.js";

/** One output in canonical CBOR, as an admitted snapshot holds it. */
export type RawOutput = Readonly<{
  /** `${txHash}#${index}`. */
  outRef: string;
  outputCbor: string;
  datumCbor: string | null;
  referenceScriptCbor: string | null;
}>;

/** One output a pending transaction creates. */
export type PendingOutput = Readonly<{
  /** `${txHash}#${index}`. */
  outRef: string;
  /** The full output: value, datum and script reference. */
  utxo: UTxO;
  raw: RawOutput;
}>;

/** One transaction of the node's mempool, decoded. */
export type PendingTransaction = Readonly<{
  txHash: string;
  bodyCbor: string;
  witnessSetCbor: string;
  /** Regular inputs, as out-ref labels. */
  spends: readonly string[];
  /** Reference inputs, as out-ref labels. */
  reads: readonly string[];
  creates: readonly PendingOutput[];
  /** The TTL slot, when the transaction sets one. */
  validTo: bigint | undefined;
}>;

/** The pending transactions applied to the ledger. */
export type PendingOverlay = Readonly<{
  /** Every out-ref a pending transaction spends. */
  spent: ReadonlySet<string>;
  /** The outputs pending transactions create that none of them spends, in
   * mempool order. */
  created: readonly PendingOutput[];
}>;

const label = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

/** The outputs of the transaction `txHash`, as raw outputs; frees each. */
const rawOutputsOf = (
  txHash: string,
  outputs: CML.TransactionOutputList,
): readonly RawOutput[] =>
  Object.freeze(
    Array.from({ length: outputs.len() }, (_, index) => {
      const output = outputs.get(index);
      const datum = output.datum();
      const inline = datum?.as_datum();
      const scriptRef = output.script_ref();
      try {
        return Object.freeze({
          outRef: `${txHash}#${index.toString()}`,
          outputCbor: output.to_canonical_cbor_hex(),
          datumCbor: inline?.to_canonical_cbor_hex() ?? null,
          referenceScriptCbor: scriptRef?.to_canonical_cbor_hex() ?? null,
        });
      } finally {
        scriptRef?.free();
        inline?.free();
        datum?.free();
        output.free();
      }
    }),
  );

/** The outputs the transaction body `bodyCbor` of `txHash` pays, as raw
 * outputs. */
export const bodyRawOutputs = (
  txHash: string,
  bodyCbor: string,
): readonly RawOutput[] => {
  const body = CML.TransactionBody.from_cbor_hex(bodyCbor);
  const outputs = body.outputs();
  try {
    return rawOutputsOf(txHash, outputs);
  } finally {
    outputs.free();
    body.free();
  }
};

const decodeOne = (bytes: Uint8Array): PendingTransaction => {
  const decoded = decodeTransaction(bytes);
  const txHash = decoded.hash.toString("hex");
  const tx = CML.Transaction.from_cbor_bytes(bytes);
  const body = tx.body();
  const witnesses = tx.witness_set();
  const outputs = body.outputs();
  try {
    const raws = rawOutputsOf(txHash, outputs);
    return Object.freeze({
      txHash,
      bodyCbor: body.to_cbor_hex(),
      witnessSetCbor: witnesses.to_cbor_hex(),
      spends: Object.freeze(decoded.inputs.map(label)),
      reads: Object.freeze(decoded.referenceInputs.map(label)),
      creates: Object.freeze(
        decoded.outputs.map((output, index) =>
          Object.freeze({
            outRef: `${txHash}#${index.toString()}`,
            utxo: toLucidUtxo({ txHash: decoded.hash, index }, output),
            raw: raws[index]!,
          }),
        ),
      ),
      validTo: decoded.invalidAfter ?? undefined,
    });
  } finally {
    outputs.free();
    witnesses.free();
    body.free();
    tx.free();
  }
};

/**
 * The mempool transactions `txs` (each one's full CBOR), decoded in order.
 * One that does not decode is skipped: it is not ours, since ours decode.
 */
export const decodePendingTransactions = (
  txs: readonly Uint8Array[],
): readonly PendingTransaction[] =>
  Object.freeze(
    txs.flatMap((bytes) => {
      try {
        return [decodeOne(bytes)];
      } catch {
        return [];
      }
    }),
  );

/** The overlay of `pending`, in mempool order. */
export const overlayPending = (
  pending: readonly PendingTransaction[],
): PendingOverlay => {
  const spent: ReadonlySet<string> = new Set(
    pending.flatMap((tx) => tx.spends),
  );
  return Object.freeze({
    spent,
    created: Object.freeze(
      pending
        .flatMap((tx) => tx.creates)
        .filter((output) => !spent.has(output.outRef)),
    ),
  });
};
