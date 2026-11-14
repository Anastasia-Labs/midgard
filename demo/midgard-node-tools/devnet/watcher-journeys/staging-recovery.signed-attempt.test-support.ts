import { CML } from "@lucid-evolution/lucid";

export const signedAttempt = () => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("77".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_ttl(30n);
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    signedCbor: transaction.to_cbor_hex(),
  };
};
