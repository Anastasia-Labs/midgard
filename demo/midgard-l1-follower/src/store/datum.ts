import { witnessDatum } from "../decode/witness.js";
import { asBuffer, asNullableBuffer, type SqlTx } from "../sql/backend.js";

/** How many outputs naming the hash are searched for its preimage. */
const CANDIDATE_LIMIT = 64;

/**
 * The preimage of a datum hash from the stored facts: the witness sets of
 * the txs that created or spent a stored output carrying that datum hash.
 * Null when no stored tx carries it (a seed row's creator, a pruned tx, or
 * an output the follower never tracked). The lookup has no index on
 * `datum_hash`: it scans `l1_outputs`.
 */
export const datumByHashIn = async (
  tx: SqlTx,
  hash: Buffer,
): Promise<Buffer | null> => {
  const outputs = await tx.query(
    "SELECT tx_hash, spent_tx FROM l1_outputs WHERE datum_hash = ? LIMIT ?",
    [hash, CANDIDATE_LIMIT],
  );
  const searched = new Set<string>();
  for (const output of outputs)
    for (const txHash of [
      asBuffer(output.tx_hash),
      asNullableBuffer(output.spent_tx),
    ]) {
      if (txHash === null || searched.has(txHash.toString("hex"))) continue;
      searched.add(txHash.toString("hex"));
      const row = (
        await tx.query("SELECT witness_cbor FROM l1_txs WHERE tx_hash = ?", [
          txHash,
        ])
      )[0];
      if (row === undefined) continue;
      const datum = witnessDatum(asBuffer(row.witness_cbor), hash);
      if (datum !== null) return datum;
    }
  return null;
};
