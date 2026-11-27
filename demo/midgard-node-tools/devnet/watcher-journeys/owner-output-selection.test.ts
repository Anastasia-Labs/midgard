import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { familyOwnerRequirement } from "./family-owner-requirement.js";
import { buildJourneyHistoryTransaction } from "./history-cases.js";
import { checkedFixtureBlock } from "./journey-fixture-ledger.test-support.js";
import { buildJourneyScriptForcedFault } from "./script-cases.js";
import { buildJourneyValidationTraceControl } from "./script-validation-trace-case.js";
import { stackNodeFeeSchedule } from "./stack-fee-schedule.test-support.js";
import {
  buildJourneyTransactionControl,
  buildJourneyTransactionFault,
} from "./transaction-cases.js";
import { buildJourneyForcedTransaction } from "./transaction-forced-cases.js";
import {
  ensureWithdrawalOwnerFunding,
  type OwnedOutput,
} from "./withdrawal-owner-funding.js";

const account = generateEmulatorAccount({ lovelace: 1n });
const key = CML.PrivateKey.from_bech32(
  walletFromSeed(account.seedPhrase, { network: "Custom" }).paymentKey,
);
const owner = key.to_public().hash().to_hex();
const ownerAddress = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.from(owner, "hex"),
]);
const ownedOutput = (outRef: Buffer, lovelace: bigint): OwnedOutput => ({
  outRef,
  output: encodeMidgardTxOutput({
    address: ownerAddress,
    value: { lovelace, assets: new Map() },
  }),
});

// The drill's owner after its withdrawn-input families: the uneven split's
// change, at about its minimum ADA, sorts first.
const dust = ownedOutput(outRefCbor(0x01, 0n), 866_310n);
const funding = ownedOutput(outRefCbor(0x02, 0n), 2_000_000n);
const large = ownedOutput(outRefCbor(0x03, 0n), 10_000_000n);

const predecessorHolding = async (outputs: readonly OwnedOutput[]) =>
  checkedFixtureBlock(
    await buildCanonicalBlockFixture({
      transactions: [],
      ...(await stackNodeFeeSchedule()),
      utxos: outputs.map(({ outRef, output }) => ({
        key: outRef,
        value: output,
      })),
    }),
  );

const timed = (
  predecessor: Awaited<ReturnType<typeof predecessorHolding>>,
) => ({
  predecessor,
  ledgerOwnerSeedPhrase: account.seedPhrase,
  operatorVkey: "82".repeat(28),
  endTime: predecessor.header.endTime + 40_000n,
  blockSlot: predecessor.header.blockSlot + 40n,
});
const orderKey = { transactionId: "a1".repeat(32), outputIndex: 0n };

/** The outputs of the owner's a block's accepted transactions spent. */
const spentOf = (
  predecessor: Awaited<ReturnType<typeof predecessorHolding>>,
  block: { postLedgerEntries: readonly { outRef: Buffer }[] },
) =>
  [dust, funding, large]
    .filter(({ outRef }) =>
      predecessor.ledgerEntries.some((entry) => entry.outRef.equals(outRef)),
    )
    .filter(
      ({ outRef }) =>
        !block.postLedgerEntries.some((entry) => entry.outRef.equals(outRef)),
    );

describe("a fixture that spends an output of the ledger owner's, whose first output is dust", () => {
  it("builds a forced-order script family's valid control from the smallest output that funds it", async () => {
    const predecessor = await predecessorHolding([dust, funding, large]);
    const fault = await buildJourneyScriptForcedFault({
      ...timed(predecessor),
      category: "missingRedeemer",
      orderKey,
    });
    expect(fault.control.replay.trace.verdict).toBe("accepted");
    expect(spentOf(predecessor, fault.control)).toEqual([funding]);
  });

  it("builds a forced-order transaction family's valid control from an output that funds it", async () => {
    const predecessor = await predecessorHolding([dust, funding, large]);
    const block = await buildJourneyForcedTransaction({
      ...timed(predecessor),
      category: "minAda",
      orderKey,
      honest: true,
    });
    expect(spentOf(predecessor, block)).toEqual([funding]);
  });

  it("builds a transaction family's fault and control from the same output, one that funds both", async () => {
    const predecessor = await predecessorHolding([dust, funding, large]);
    const control = await buildJourneyTransactionControl({
      ...timed(predecessor),
      category: "mintAuthorization",
    });
    expect(spentOf(predecessor, control)).toEqual([funding]);
    const fault = await buildJourneyTransactionFault({
      ...timed(predecessor),
      category: "mintAuthorization",
    });
    expect(fault.replay.trace.rejectionCode).not.toBe("E_MIN_ADA");
  });

  it("builds the validation-trace dispute's passing mint from an output that funds its reference script", async () => {
    const predecessor = await predecessorHolding([dust, funding, large]);
    const control = await buildJourneyValidationTraceControl(
      timed(predecessor),
    );
    expect(spentOf(predecessor, control)).toHaveLength(1);
    expect(spentOf(predecessor, control)).not.toEqual([dust]);
  });

  it("builds a history family's chained control from an output that funds every transaction in it", async () => {
    const predecessor = await predecessorHolding([dust, funding, large]);
    const block = await buildJourneyHistoryTransaction({
      ...timed(predecessor),
      category: "referenceInputNoIdx",
      honest: true,
    });
    expect(block.replays.map((replay) => replay.trace.verdict)).toEqual([
      "accepted",
      "accepted",
    ]);
  });

  it("tops the owner up before a forced-order family when only dust is left, and the family then builds", async () => {
    const drained = await predecessorHolding([dust]);
    let tail = drained;
    const requirement = familyOwnerRequirement("missingRedeemer", {
      ledgerOwnerSeedPhrase: account.seedPhrase,
      quote: () => {
        throw new Error("A forced-order family withdraws nothing");
      },
    })!;
    const outcome = await ensureWithdrawalOwnerFunding({
      owner,
      readTail: async () => tail,
      requirement: (current) => requirement(current, ownerAddress),
      topUp: async ({ topUpLovelace }) => {
        tail = await predecessorHolding([
          dust,
          ownedOutput(outRefCbor(0x04, 0n), topUpLovelace),
        ]);
      },
      settle: async () => undefined,
      onStage: () => undefined,
    });
    expect(outcome).toBe("topped-up");
    const fault = await buildJourneyScriptForcedFault({
      ...timed(tail),
      category: "missingRedeemer",
      orderKey,
    });
    expect(fault.control.replay.trace.verdict).toBe("accepted");
  });

  it("refuses a forced-order fixture whose owner holds only dust, naming why each output falls short", async () => {
    const predecessor = await predecessorHolding([dust]);
    await expect(
      buildJourneyScriptForcedFault({
        ...timed(predecessor),
        category: "missingRedeemer",
        orderKey,
      }),
    ).rejects.toThrow(/No output of the ledger owner's funds/u);
  });
});
