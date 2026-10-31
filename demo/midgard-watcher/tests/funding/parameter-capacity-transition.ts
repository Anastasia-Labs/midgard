import { createHash } from "node:crypto";

import type { WorkflowFundingPreparedTransition } from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import type { WatcherProverFundingReservationRecord } from "../../src/funding/prover-funding-reservation.js";
import { key, walletAddress } from "../support/fault-proof-funding-fixture.js";

export const capacityTransition = (
  record: WatcherProverFundingReservationRecord,
): WorkflowFundingPreparedTransition => {
  const ordinary = record.activeInputs.filter(({ role }) => role === "funding");
  const inputs = CML.TransactionInputList.new();
  const collateral = CML.TransactionInputList.new();
  for (const input of record.activeInputs) {
    const [hash, index] = input.outRef.split("#");
    (input.role === "funding" ? inputs : collateral).add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(hash!),
        BigInt(index!),
      ),
    );
  }
  const lovelace =
    ordinary.reduce((sum, input) => sum + BigInt(input.lovelace), 0n) -
    1_000_000n;
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(lovelace),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
  body.set_collateral_inputs(collateral);
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      key.to_public(),
      key.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const transactionHash = CML.hash_transaction(body).to_hex();
  return {
    actionKind: "step-one",
    signedTransactionCborHex: CML.Transaction.new(
      body,
      witnesses,
      true,
      undefined,
    ).to_cbor_hex(),
    transactionHash,
    transactionBodySha256: createHash("sha256")
      .update(Buffer.from(body.to_cbor_hex(), "hex"))
      .digest("hex"),
    consumedOutRefs: ordinary.map(({ outRef }) => outRef),
    producedInputs: [
      {
        outRef: `${transactionHash}#0`,
        role: "funding",
        lovelace: lovelace.toString(),
        assets: [],
      },
    ],
  };
};
