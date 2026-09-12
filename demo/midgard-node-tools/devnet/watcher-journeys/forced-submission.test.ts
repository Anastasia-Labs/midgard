import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "@al-ft/midgard-fault-proofs";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  generateSeedPhrase,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { verifyJourneyFixture } from "./fixture-verification.js";
import { buildJourneyScriptForcedFault } from "./script-cases.js";
import { prepareJourneyNativeExecutionEvidence } from "./script-execution-evidence.js";

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

it("reconstructs forced native execution evidence from the journey's retained validation trace", async () => {
  const ledgerOwnerSeedPhrase = generateSeedPhrase();
  const wallet = walletFromSeed(ledgerOwnerSeedPhrase, { network: "Custom" });
  const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const owner = Buffer.from(key.to_public().hash().to_raw_bytes());
  const predecessor = await buildCanonicalBlockFixture({
    transactions: [],
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    utxos: [
      {
        key: encodeMidgardSpendInputItem({
          txId: Buffer.alloc(32, 0x31),
          outputIndex: 0,
        }),
        value: encodeMidgardTxOutput({
          address: Buffer.concat([Buffer.from([0x60]), owner]),
          value: { lovelace: 10_000_000n, assets: new Map() },
        }),
      },
    ],
  });
  const input = {
    predecessor,
    ledgerOwnerSeedPhrase,
    operatorVkey: predecessor.header.operatorVkey,
    endTime: 60_020n,
    blockSlot: 60n,
    orderKey: { transactionId: "52".repeat(32), outputIndex: 0n },
  };
  const block = await buildJourneyScriptForcedFault({
    ...input,
    category: "executionNativeScriptInvalid",
  });
  expect(block.replay.trace.verdict).toBe("accepted");
  const classified = await verifyJourneyFixture({
    category: "executionNativeScriptInvalid",
    replayer: EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
    block,
    predecessor,
  });
  if (classified.evidence === undefined)
    throw new Error("Missing retained canonical evidence");
  const prepared = await prepareJourneyNativeExecutionEvidence(
    classified.evidence,
    predecessor,
  );
  expect(prepared.evidence.contradiction).toBe(true);

  const plutus = await buildJourneyScriptForcedFault({
    ...input,
    category: "unusedRedeemer",
  });
  expect(plutus.replay.trace.verdict).toBe("accepted");
});
