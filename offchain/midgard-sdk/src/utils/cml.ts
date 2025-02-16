import { CML, OutRef } from "@lucid-evolution/lucid";

export const cmlInputToOutRef = (cmlInput: CML.TransactionInput): OutRef => {
  const outRef = {
    txHash: cmlInput.transaction_id().to_hex(),
    outputIndex: Number(cmlInput.index()),
  };
  cmlInput.free();
  return outRef;
};
