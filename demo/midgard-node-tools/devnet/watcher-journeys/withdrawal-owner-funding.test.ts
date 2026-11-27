import {
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  MIDGARD_MAX_OUTPUT_INDEX,
} from "@al-ft/midgard-core";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk/core";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type HistoryPredecessor,
  ownedHistoryLedgerEntries,
  retainHistoryTransactions,
} from "./history-cases.js";
import { ownedOutputWithdrawalBody } from "./history-events.js";
import { checkedFixtureBlock } from "./journey-fixture-ledger.test-support.js";
import {
  ensureWithdrawalOwnerFunding,
  ownerOutputRequirement,
  signOwnerSplit,
  smallestQualifyingOutput,
  type WithdrawalFundingShape,
  withdrawalOwnerRequirement,
  type WithdrawalQuote,
  withdrawalRequirement,
} from "./withdrawal-owner-funding.js";

const key = CML.PrivateKey.from_bech32(
  walletFromSeed(generateEmulatorAccount({ lovelace: 1n }).seedPhrase, {
    network: "Custom",
  }).paymentKey,
);
const owner = key.to_public().hash().to_hex();
const ownerAddress = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.from(owner, "hex"),
]);
const ownedOutput = (lovelace: bigint) =>
  encodeMidgardTxOutput({
    address: ownerAddress,
    value: { lovelace, assets: new Map() },
  });

// The SDK's requirement grows with the withdrawal's encoded size; this
// stand-in prices each body byte, so an amount's encoding width moves it the
// way the SDK's own quote does. A body of this owner's needs a few ADA.
const quote: WithdrawalQuote = (body) => {
  const bytes = BigInt(SDK.withdrawalBodyBytes(body).length / 2);
  return Promise.resolve({
    orderLovelace: 25_000n * bytes,
    targetMinimum: 20_000n * bytes,
  });
};

type Utxo = { key: Buffer; value: Buffer };
const tailOf = (utxos: readonly Utxo[]) =>
  buildCanonicalBlockFixture({ transactions: [], utxos: [...utxos] }).then(
    checkedFixtureBlock,
  );
const lovelaceOf = (output: Buffer) =>
  decodeMidgardTxOutput(output).value.lovelace;

// A fresh devnet's first journey leaves the owner a 2 ADA output, below any
// Order's minimum, beside the 10 ADA honest deposit. An earlier family drained
// the deposit, so only the 2 ADA output is left.
const small = { key: outRefCbor(0x00, 0n), value: ownedOutput(2_000_000n) };
const drained = () => [small];

/**
 * A run of withdrawal families on one ledger: each family's funding before
 * its timer, then the family itself, which spends what it withdraws.
 */
const runFamilies = async (
  shapes: readonly WithdrawalFundingShape[],
  options: { topUp: boolean },
) => {
  let utxos: Utxo[] = drained();
  let deposits = 0;
  const outcomes: string[] = [];
  for (const shape of shapes) {
    const outcome = await ensureWithdrawalOwnerFunding({
      owner,
      readTail: () => tailOf(utxos),
      requirement: (tail) =>
        withdrawalOwnerRequirement({
          shape,
          quote,
          key,
          header: tail.header,
          topUpAddress: ownerAddress,
        }),
      topUp: (requirement) => {
        if (options.topUp)
          utxos = [
            ...utxos,
            {
              key: outRefCbor(0xd0 + deposits++, 0n),
              value: ownedOutput(requirement.topUpLovelace),
            },
          ];
        return Promise.resolve();
      },
      settle: () => Promise.resolve(),
      onStage: () => undefined,
    });
    outcomes.push(outcome);
    const tail = await tailOf(utxos);
    const required = ownerOutputRequirement({
      shape,
      quote,
      key,
      header: tail.header,
    });
    // The family's funding precondition: an owned output that funds it.
    const entry = await smallestQualifyingOutput(
      ownedHistoryLedgerEntries(tail, owner),
      {
        funds: async (candidate) =>
          lovelaceOf(candidate.output) >= (await required(candidate)).total,
      },
    );
    expect(entry, `a ${shape} after ${outcomes.join(", ")}`).toBeDefined();
    const withdrawn = entry!;
    if (shape === "withdrawal") {
      utxos = utxos.filter(({ key }) => !key.equals(withdrawn.outRef));
      continue;
    }
    const { orderLovelace } = await required(withdrawn);
    const split = signOwnerSplit({
      header: tail.header,
      key,
      entry: withdrawn,
      orderLovelace,
    });
    const block = await retainHistoryTransactions({
      predecessor: tail,
      operatorVkey: "82".repeat(28),
      endTime: tail.header.endTime + 40_000n,
      blockSlot: tail.header.blockSlot + 40n,
      transactions: [split],
    });
    expect(block.replays.map((replay) => replay.trace.verdict)).toEqual([
      "accepted",
    ]);
    // The split's first output holds exactly what its withdrawal needs, and
    // the family withdraws it.
    const firstOutRef = encodeMidgardSpendInputItem({
      txId: Buffer.from(split.txId, "hex"),
      outputIndex: 0,
    });
    const first = block.ledgerEntries.find(({ outRef }) =>
      outRef.equals(firstOutRef),
    );
    expect(lovelaceOf(first!.output)).toBe(orderLovelace);
    expect(
      await withdrawalRequirement(quote, {
        owner,
        outRef: firstOutRef,
        output: decodeMidgardTxOutput(first!.output),
      }),
    ).toBe(orderLovelace);
    utxos = block.ledgerEntries
      .filter(({ outRef }) => !outRef.equals(firstOutRef))
      .map(({ outRef, output }) => ({ key: outRef, value: output }));
  }
  return outcomes;
};

describe("a withdrawal family's funding before its timer", () => {
  it.each([
    [["withdrawal", "split", "withdrawal", "split"]],
    [["split", "withdrawal", "split", "withdrawal"]],
  ] as const)(
    "meets each family's precondition after an earlier family drained the owner, in the order %j",
    async (shapes) => {
      const outcomes = await runFamilies(shapes, { topUp: true });
      expect(outcomes[0]).toBe("topped-up");
    },
  );

  it("refuses a top-up that leaves no output funding the family", async () => {
    await expect(runFamilies(["withdrawal"], { topUp: false })).rejects.toThrow(
      /left no output that funds a withdrawal/,
    );
  });
});

const predecessorWith = (...lovelace: bigint[]): Promise<HistoryPredecessor> =>
  tailOf(
    lovelace.map((amount, index) => ({
      key: outRefCbor(0xa0 + index, 1n),
      value: ownedOutput(amount),
    })),
  );

it("tops up nothing while an owned output already funds the family", async () => {
  const tail = await predecessorWith(2_000_000n, 10_000_000n);
  let toppedUp = false;
  const outcome = await ensureWithdrawalOwnerFunding({
    owner,
    readTail: () => Promise.resolve(tail),
    requirement: () =>
      withdrawalOwnerRequirement({
        shape: "split",
        quote,
        key,
        header: tail.header,
        topUpAddress: ownerAddress,
      }),
    topUp: () => {
      toppedUp = true;
      return Promise.resolve();
    },
    settle: () => Promise.resolve(),
    onStage: () => undefined,
  });
  expect(outcome).toBe("funded");
  expect(toppedUp).toBe(false);
});

it("withdraws the smallest owned output that funds the withdrawal", async () => {
  const tail = await predecessorWith(2_000_000n, 30_000_000n, 9_000_000n);
  const required = ownerOutputRequirement({
    shape: "withdrawal",
    quote,
    key,
    header: tail.header,
  });
  const entry = await smallestQualifyingOutput(
    ownedHistoryLedgerEntries(tail, owner),
    {
      funds: async (candidate) =>
        lovelaceOf(candidate.output) >= (await required(candidate)).total,
    },
  );
  expect(lovelaceOf(entry!.output)).toBe(9_000_000n);
});

it("sizes a withdrawal at exactly what the quote requires of it", async () => {
  const output = decodeMidgardTxOutput(ownedOutput(0n));
  const outRef = outRefCbor(0xa0, 1n);
  const least = await withdrawalRequirement(quote, { owner, outRef, output });
  const at = async (lovelace: bigint) => {
    const quoted = await quote(
      ownedOutputWithdrawalBody({
        owner,
        outRef,
        output: { ...output, value: { ...output.value, lovelace } },
      }),
    );
    return quoted.orderLovelace > quoted.targetMinimum
      ? quoted.orderLovelace
      : quoted.targetMinimum;
  };
  expect(await at(least)).toBe(least);
  expect(await at(least - 1n)).toBeGreaterThan(least - 1n);
});

it.each<WithdrawalFundingShape>(["withdrawal", "split"])(
  "sizes a %s family's top-up to fund its deposit at any output reference the encoding allows",
  async (shape) => {
    const tail = await predecessorWith(2_000_000n);
    const requirement = await withdrawalOwnerRequirement({
      shape,
      quote,
      key,
      header: tail.header,
      topUpAddress: ownerAddress,
    });
    // The output index's Data encoding widens at 24 and 256; the
    // transaction id is always 32 bytes.
    for (const outputIndex of [0, 23, 24, 255, 256, MIDGARD_MAX_OUTPUT_INDEX])
      expect(
        await requirement.requiredOf({
          outRef: encodeMidgardSpendInputItem({
            txId: Buffer.alloc(32, 0xff),
            outputIndex,
          }),
          output: ownedOutput(requirement.topUpLovelace),
        }),
      ).toBeLessThanOrEqual(requirement.topUpLovelace);
  },
);
