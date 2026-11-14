import {
  CML,
  generateSeedPhrase,
  walletFromSeed,
} from "@lucid-evolution/lucid";

import type * as Journal from "../../src/database/settlement.js";

/** A signed-looking withdrawal initialize body, valid until slot `ttl`. */
export const pendingSettlementAttempt = ({
  deploymentId,
  eventId,
  ttl = 120n,
}: {
  readonly deploymentId: string;
  readonly eventId: string;
  readonly ttl?: bigint;
}): Journal.SettlementAttempt => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("ab".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(
        walletFromSeed(generateSeedPhrase(), { network: "Preprod" }).address,
      ),
      CML.Value.from_coin(2_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_ttl(ttl);
  const tx = CML.Transaction.new(body, CML.TransactionWitnessSet.new(), true);
  return {
    deployment_id: deploymentId,
    kind: "withdrawal",
    event_id: eventId,
    phase: "initialize",
    signed_cbor: tx.to_cbor_hex(),
    tx_hash: CML.hash_transaction(body).to_hex(),
    required_outputs: [0],
    fee_inputs: [`${"ab".repeat(32)}#0`],
    status: "pending",
  };
};
