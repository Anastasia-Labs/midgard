import { CML } from "@lucid-evolution/lucid";

import { key } from "./fault-proof-funding-fixture.sources-for.js";

export const signFundingRecoveryFixtureBody = (body: CML.TransactionBody) => {
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      key.to_public(),
      key.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const signedTransactionCborHex = CML.Transaction.new(
    body,
    witnesses,
    true,
    undefined,
  ).to_cbor_hex();
  const transactionHash = CML.hash_transaction(body).to_hex();
  return { signedTransactionCborHex, transactionHash };
};
