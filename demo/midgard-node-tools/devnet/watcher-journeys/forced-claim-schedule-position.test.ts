/**
 * `InputAssetAccumulationLimit.input_index` names a position in the input
 * resolution schedule, which lists the spend inputs and the reference inputs
 * together in outRef order. The forced transaction here spends one input and
 * reads one reference input whose outRef sorts first, so its schedule has two
 * positions and the spend input sits at position 1.
 */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { prepareJourneyForcedTransaction } from "./forced-transaction.js";
import { classifyFullCatalogueTransactionFixture } from "./journey-classification.test-support.js";
import { checkedFixtureBlock } from "./journey-fixture-ledger.test-support.js";

const account = generateEmulatorAccount({ lovelace: 100_000_000n });
const owner = Buffer.from(
  CML.PrivateKey.from_bech32(
    walletFromSeed(account.seedPhrase, { network: "Custom" }).paymentKey,
  )
    .to_public()
    .hash()
    .to_raw_bytes(),
);
const orderKey = { transactionId: "a1".repeat(32), outputIndex: 0n };
const referenced = outRefCbor(0x01, 0n);

type Predecessor = Parameters<
  typeof prepareJourneyForcedTransaction
>[0]["predecessor"];
let predecessor: Predecessor;
let transaction: ReturnType<
  typeof prepareJourneyForcedTransaction
>["transaction"];
beforeAll(async () => {
  predecessor = await checkedFixtureBlock(
    await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [
        {
          key: outRefCbor(0x81, 0n),
          value: encodeMidgardTxOutput({
            address: Buffer.concat([Buffer.from([0x60]), owner]),
            value: { lovelace: 100_000_000n, assets: new Map() },
          }),
        },
        {
          key: referenced,
          value: encodeMidgardTxOutput({
            address: Buffer.concat([
              Buffer.from([0x60]),
              Buffer.alloc(28, 0x52),
            ]),
            value: { lovelace: 2_000_000n, assets: new Map() },
          }),
        },
      ],
    }),
  );
  transaction = prepareJourneyForcedTransaction({
    predecessor,
    ledgerOwnerSeedPhrase: account.seedPhrase,
    referenceInputs: [referenced],
  }).transaction;
});

/** The full classification of the block whose forced leaf rejects the
 * transaction for an input-asset crossing at schedule position `inputIndex`. */
const classifyClaim = async (inputIndex: bigint) => {
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: transaction.canonicalCbor,
    predecessor,
    ledgerEntries: predecessor.ledgerEntries,
    operatorVkey: "82".repeat(28),
    endTime: predecessor.header.endTime + 40_000n,
    blockSlot: predecessor.header.blockSlot + 40n,
    source: {
      kind: "forced",
      orderKey,
      verdict: {
        ForcedTxInvalid: {
          reason: {
            InputAssetAccumulationLimit: {
              input_index: inputIndex,
              asset_index: 0n,
            },
          },
        },
      },
    },
  });
  return classifyFullCatalogueTransactionFixture({
    block,
    predecessor,
    history: [],
    forced: { transaction, orderKey },
  });
};

/** The category the block's decision convicts, or the decision itself. */
const convicted = async (inputIndex: bigint) => {
  const { decision } = await classifyClaim(inputIndex);
  return decision.decision === "fault_detected" ? decision.category : decision;
};

describe("judging an input-asset rejection by its schedule position", () => {
  it.each([
    ["the spend input at the last schedule position", 1n],
    ["the position past the schedule", 2n],
    ["a negative position", -1n],
    ["a position past 2^53", (1n << 53n) + 1n],
  ])(
    "contradicts a rejection naming %s",
    async (_label, inputIndex) => {
      expect(await convicted(inputIndex)).toBe(
        "distinctAssetAccumulationLimit",
      );
    },
    120_000,
  );
});
