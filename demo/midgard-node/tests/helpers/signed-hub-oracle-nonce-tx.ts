import {
  CML,
  credentialToAddress,
  type UTxO,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";

/** The wallet output a signed nonce transaction spends. */
export const NONCE_TX_INPUT = { txHash: "11".repeat(32), outputIndex: 3 };

/**
 * A signed nonce transaction whose bytes a resume can resubmit: it spends
 * {@link NONCE_TX_INPUT} and pays the nonce, carrying `inlineDatum`, as its
 * first output. `nonce` is that output as the ledger holds it once landed.
 */
export const signedNonceTx = (inlineDatum = "d87980") => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(NONCE_TX_INPUT.txHash),
      BigInt(NONCE_TX_INPUT.outputIndex),
    ),
  );
  const output = {
    address: credentialToAddress("Preprod", {
      type: "Key",
      hash: "aa".repeat(28),
    }),
    assets: { lovelace: 5_000_000n },
    datum: inlineDatum,
  };
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    utxoToTransactionOutput({ ...output, txHash: "", outputIndex: 0 }),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  const txHash = CML.hash_transaction(body).to_hex();
  const nonce: UTxO = { ...output, txHash, outputIndex: 0 };
  return {
    txHash,
    signedTxCbor: CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex(),
    nonce,
  };
};
