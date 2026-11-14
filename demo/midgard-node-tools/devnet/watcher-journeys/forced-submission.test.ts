import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

it("retains one forced submission across both operator claims while replay independently rejects it", async () => {
  const transaction = buildFixtureTransaction({
    spendInputs: [],
    fee: 0n,
    networkId: 0n,
  });
  const predecessor = await buildCanonicalBlockFixture({ transactions: [] });
  const input = {
    canonicalTransactionCbor: transaction.canonicalCbor,
    predecessor,
    ledgerEntries: [],
    operatorVkey: predecessor.header.operatorVkey,
    endTime: 60_020n,
    blockSlot: 60n,
  };
  const orderKey = { transactionId: "52".repeat(32), outputIndex: 0n };
  const verdicts: SDK.OperatorVerdict[] = [
    "ForcedTxValid",
    { ForcedTxInvalid: { reason: "EmptyInputs" } },
  ];
  const blocks = [];
  for (const verdict of verdicts) {
    const block = await retainedTransactionFixture({
      ...input,
      source: { kind: "forced", orderKey, verdict },
    });
    expect(block.replay.trace.verdict).toBe("rejected");
    const body = block.payload.block_body;
    expect(body.forced_transaction_preimages).toHaveLength(1);
    const submitted = Buffer.from(
      body.forced_transaction_preimages[0]![1],
      "hex",
    );
    expect(decodeMidgardForcedTxFullFromCanonicalCbor(submitted).body).toEqual(
      decodeMidgardNativeTxFullFromCanonicalCbor(transaction.canonicalCbor)
        .body,
    );
    const leaf = Data.from(
      body.forced_transactions[0]![1],
      SDK.ForcedInclusionTxV1,
    );
    expect(leaf).toMatchObject({ tx_id: transaction.txId, verdict });
    blocks.push(block);
  }
  expect(blocks[0]!.payload.block_body.forced_transaction_preimages).toEqual(
    blocks[1]!.payload.block_body.forced_transaction_preimages,
  );
  expect(blocks[0]!.header.forcedTransactionsRoot).not.toBe(
    blocks[1]!.header.forcedTransactionsRoot,
  );

  const normal = await retainedTransactionFixture(input);
  expect(normal.replay.trace.verdict).toBe("rejected");
  expect(normal.payload.block_body.forced_transaction_preimages).toEqual([]);
  expect(normal.payload.block_body.transaction_preimages).toEqual([
    [transaction.txId, transaction.canonicalCbor.toString("hex")],
  ]);
});
