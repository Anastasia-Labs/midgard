import type { FraudProofRawL1Utxo } from "@al-ft/midgard-fault-proofs";
import type { OutRef, SqlTx } from "@al-ft/midgard-l1-follower";
import { CML } from "@lucid-evolution/lucid";

/**
 * Exact output reads over the follower's facts, in the shape the old
 * Kupmios raw source returns them (`rawUtxoFromOutput`): the output's
 * canonical CBOR as re-encoded from its creating transaction body.
 */

export const outRefLabel = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

export const parseOutRefLabel = (label: string): OutRef => {
  const [txHash, index] = label.split("#");
  if (
    txHash === undefined ||
    index === undefined ||
    !/^[0-9a-f]{64}$/u.test(txHash) ||
    !/^(?:0|[1-9][0-9]*)$/u.test(index)
  )
    throw new Error(`not an output reference: ${label}`);
  return { txHash: Buffer.from(txHash, "hex"), index: Number(index) };
};

/** One output as the Kupmios raw source reports it. */
export const rawUtxo = (
  outRef: string,
  output: CML.TransactionOutput,
): FraudProofRawL1Utxo => ({
  outRef,
  outputCbor: output.to_canonical_cbor_hex(),
  datumCbor: output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null,
  referenceScriptCbor: output.script_ref()?.to_canonical_cbor_hex() ?? null,
});

const truthy = (value: unknown): boolean =>
  value === true || value === 1 || value === 1n || value === "1";

/** The output a stored tx created at `index` (a failed tx creates only its collateral return). */
export const createdOutputOf = (
  body: CML.TransactionBody,
  isValid: boolean,
  index: number,
): CML.TransactionOutput | undefined => {
  const outputs = body.outputs();
  if (isValid) return index < outputs.len() ? outputs.get(index) : undefined;
  return index === outputs.len() ? body.collateral_return() : undefined;
};

/**
 * The exact output at `outRef`, re-read from its creating transaction's body
 * when the follower stores that transaction (every transaction that touched
 * the tracked set, while it is retained). Any output of a stored body
 * resolves, tracked or not: the body's bytes are the L1 fact. Null when the
 * creating transaction is not stored (untracked, pruned, or created before
 * the origin: a seed row carries no creating body), which plan §12.3 resolves
 * in phase B (ruling 2).
 */
export const resolveRawUtxoIn = async (
  tx: SqlTx,
  outRef: OutRef,
): Promise<FraudProofRawL1Utxo | null> => {
  const row = (
    await tx.query("SELECT body_cbor, is_valid FROM l1_txs WHERE tx_hash = ?", [
      outRef.txHash,
    ])
  )[0];
  if (row === undefined) return null;
  const body = CML.TransactionBody.from_cbor_bytes(
    Buffer.from(row.body_cbor as Uint8Array),
  );
  try {
    const output = createdOutputOf(body, truthy(row.is_valid), outRef.index);
    return output === undefined ? null : rawUtxo(outRefLabel(outRef), output);
  } finally {
    body.free();
  }
};
