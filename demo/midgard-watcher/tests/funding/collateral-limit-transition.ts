import { createHash } from "node:crypto";

import type { WorkflowFundingPreparedTransition } from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import type { WatcherProverFundingReservationRecord } from "../../src/funding/prover-funding-reservation.js";
import { key, walletAddress } from "../support/fault-proof-funding-fixture.js";

/**
 * A signed step that spends the reservation's first coin and posts the next
 * `collateralCount` reserved coins as collateral without spending them.
 */
export const collateralLimitTransition = (
  record: WatcherProverFundingReservationRecord,
  collateralCount: number,
): WorkflowFundingPreparedTransition => {
  const [spent, ...rest] = record.activeInputs;
  const posted = rest.slice(0, collateralCount);
  if (spent === undefined || posted.length !== collateralCount)
    throw new Error("reservation holds too few coins for this transition");
  const inputList = (outRefs: readonly string[]) => {
    const list = CML.TransactionInputList.new();
    for (const outRef of outRefs) {
      const [hash, index] = outRef.split("#");
      list.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(hash!),
          BigInt(index!),
        ),
      );
    }
    return list;
  };
  const inputs = inputList([spent.outRef]);
  const collateral = inputList(posted.map(({ outRef }) => outRef));
  const lovelace = BigInt(spent.lovelace) - 1_000_000n;
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
    consumedOutRefs: [spent.outRef],
    producedInputs: [
      {
        outRef: `${transactionHash}#0`,
        lovelace: lovelace.toString(),
        assets: [],
      },
    ],
  };
};
