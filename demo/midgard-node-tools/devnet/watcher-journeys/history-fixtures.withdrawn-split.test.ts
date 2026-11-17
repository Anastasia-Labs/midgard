import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  ownedHistoryLedgerEntries,
  retainHistoryTransactions,
  signHistoryTransaction,
} from "./history-cases.js";
import {
  fundedJourneyWithdrawalBody,
  largestOwnedLedgerEntry,
  withdrawnInputSplitBody,
} from "./history-fixtures.js";
import { checkedFixtureBlock } from "./journey-fixture-ledger.test-support.js";

// A fresh devnet's first journey: the ledger owner holds the 10 ADA honest
// deposit and a 2 ADA output whose reference sorts first. A 2 ADA output
// cannot fund a withdrawal Order (the drill's run 14 refused exactly that).
const key = CML.PrivateKey.from_bech32(
  walletFromSeed(generateEmulatorAccount({ lovelace: 1n }).seedPhrase, {
    network: "Custom",
  }).paymentKey,
);
const owner = key.to_public().hash().to_hex();
const ownedOutput = (lovelace: bigint) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.from(owner, "hex")]),
    value: { lovelace, assets: new Map() },
  });
const small = { key: outRefCbor(0x00, 0n), value: ownedOutput(2_000_000n) };
const deposit = { key: outRefCbor(0xa0, 1n), value: ownedOutput(10_000_000n) };
const predecessor = () =>
  buildCanonicalBlockFixture({
    transactions: [],
    utxos: [small, deposit],
  }).then(checkedFixtureBlock);

it("splits the owner's largest output, not whichever sorts first", async () => {
  const block = await predecessor();
  expect(ownedHistoryLedgerEntries(block, owner)[0]?.outRef).toEqual(small.key);
  expect(largestOwnedLedgerEntry(block, owner)?.outRef).toEqual(deposit.key);
});

it("withdraws the owner's largest output, which can fund an Order", async () => {
  const block = await predecessor();
  const body = fundedJourneyWithdrawalBody(block, owner);
  expect(body.l2_value.get("")?.get("")).toBe(10_000_000n);
});

it("withdraws the split's own output even when a smaller owned output sorts first", async () => {
  const before = await predecessor();
  const split = signHistoryTransaction(
    {
      spendInputs: [deposit.key],
      outputs: [ownedOutput(5_000_000n), ownedOutput(5_000_000n)],
      fee: 0n,
      networkId: before.header.expectedNetworkId,
    },
    key,
  );
  const block = await retainHistoryTransactions({
    predecessor: before,
    operatorVkey: "82".repeat(28),
    endTime: before.header.endTime + 40_000n,
    blockSlot: before.header.blockSlot + 40n,
    transactions: [split],
  });
  expect(block.replays.map((replay) => replay.trace.verdict)).toEqual([
    "accepted",
  ]);
  expect(ownedHistoryLedgerEntries(block, owner)[0]?.outRef).toEqual(small.key);
  const body = withdrawnInputSplitBody(block, owner);
  expect(
    encodeMidgardSpendInputItem({
      txId: Buffer.from(body.l2_outref.transactionId, "hex"),
      outputIndex: Number(body.l2_outref.outputIndex),
    }),
  ).toEqual(
    encodeMidgardSpendInputItem({
      txId: Buffer.from(split.txId, "hex"),
      outputIndex: 0,
    }),
  );
  expect(body.l2_value.get("")?.get("")).toBe(5_000_000n);
});
