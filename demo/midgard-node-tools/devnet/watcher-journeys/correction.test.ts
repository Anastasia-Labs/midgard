import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  toUnit,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { verifyJourneyCorrectedTail } from "./correction.js";

const headerHash = "a1".repeat(28);
const successorHash = "b2".repeat(28);
const policyId = "c3".repeat(28);
const address = credentialToAddress("Preprod", {
  type: "Script",
  hash: "d4".repeat(28),
});
const unit = toUnit(
  policyId,
  SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
);

const transactionWithTail = (next: SDK.LinkedListNodeView["next"]) => {
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    utxoToCore({
      txHash: "00".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 5_000_000n, [unit]: 1n },
      datum: SDK.encodeLinkedListNodeView({
        key: { Key: { key: headerHash } },
        next,
        data: 0n,
      }),
    }).output(),
  );
  return CML.Transaction.new(
    CML.TransactionBody.new(CML.TransactionInputList.new(), outputs, 200_000n),
    CML.TransactionWitnessSet.new(),
    true,
  );
};

it("verifies the recorded correction after a later successor changes the live tail", async () => {
  const correction = transactionWithTail("Empty");
  const later = transactionWithTail({ Key: { key: successorHash } });
  const expected = { address, unit, headerHash };
  await expect(verifyJourneyCorrectedTail(later, expected)).rejects.toThrow();
  const verified = await verifyJourneyCorrectedTail(correction, expected);
  expect(verified.txHash).toBe(
    CML.hash_transaction(correction.body()).to_hex(),
  );
  expect(verified.txHash).not.toBe(CML.hash_transaction(later.body()).to_hex());
});

it("refuses an unrelated empty tail as evidence for the expected predecessor", async () => {
  const correction = transactionWithTail("Empty");
  await expect(
    verifyJourneyCorrectedTail(correction, {
      address,
      unit: toUnit(
        policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + successorHash,
      ),
      headerHash: successorHash,
    }),
  ).rejects.toThrow();
});
