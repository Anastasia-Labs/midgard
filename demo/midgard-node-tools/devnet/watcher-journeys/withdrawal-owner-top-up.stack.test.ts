import {
  decodeMidgardTxOutput,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { checkedFixtureBlock } from "./journey-fixture-ledger.test-support.js";
import type { StackLedgerOwnerDeposit } from "./stack-ledger-owner.js";
import {
  ensureWithdrawalOwnerFunding,
  type OwnedOutput,
  withdrawalOwnerRequirement,
  type WithdrawalQuote,
} from "./withdrawal-owner-funding.js";
import { stackWithdrawalOwnerFundingPorts } from "./withdrawal-owner-top-up.stack.js";

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
// A stand-in for the SDK's quote that grows with the body's encoded size.
const quote: WithdrawalQuote = (body) => {
  const bytes = BigInt(SDK.withdrawalBodyBytes(body).length / 2);
  return Promise.resolve({
    orderLovelace: 25_000n * bytes,
    targetMinimum: 20_000n * bytes,
  });
};

// The stack's clock, in its own units: a deposit falls due a while after it
// is built, lands on L1 before then, and our node absorbs it once due; the
// funding allows it its absorption bound after that.
const DUE_AFTER_MS = 300_000;
const LANDS_AFTER_MS = 60_000;
const ABSORB_WITHIN_MS = 1_800_000;
const POLL_MS = 30_000;

/**
 * An attached stack whose ledger owner holds `outputs`, on one clock that
 * moves only while the funding waits. A deposit is absorbed once it is due,
 * and our node's ledger and the attested tail then both hold its output.
 * One shared record holds a pending deposit, whichever family made it. Set
 * `stopsAfterLanding` and the drill stops in its next wait once a deposit
 * has landed, before its record is cleared.
 */
const stack = async (initial: readonly OwnedOutput[]) => {
  let clock = 0;
  let outputs = [...initial];
  let record: StackLedgerOwnerDeposit | undefined;
  const archived: string[] = [];
  const deposits: (StackLedgerOwnerDeposit & {
    lovelace: bigint;
    landsAt: number;
    absorbed: boolean;
  })[] = [];
  const state = { stopsAfterLanding: false };
  const advance = (ms: number) => {
    clock += ms;
    for (const deposit of deposits)
      if (!deposit.absorbed && clock >= deposit.inclusionTime) {
        deposit.absorbed = true;
        outputs.push({
          outRef: outRefCbor(0xd0 + deposits.indexOf(deposit), 0n),
          output: ownedOutput(deposit.lovelace),
        });
      }
  };
  const readTail = () =>
    buildCanonicalBlockFixture({
      transactions: [],
      utxos: outputs.map(({ outRef, output }) => ({
        key: outRef,
        value: output,
      })),
    }).then(checkedFixtureBlock);
  const ports = stackWithdrawalOwnerFundingPorts({
    owner,
    readTail,
    readOurOutputs: async () => outputs,
    requirement: (tail) =>
      withdrawalOwnerRequirement({
        shape: "withdrawal",
        quote,
        key,
        header: tail.header,
        topUpAddress: ownerAddress,
      }),
    fundingPorts: async (lovelace) => ({
      ourLedgerHoldsOutput: async () => outputs.length > 0,
      ownerHoldsOutput: async () => outputs.length > 0,
      readPendingDeposit: async () => record,
      depositStatus: async (deposit) => {
        const landsAt = deposits.find(
          ({ txHash }) => txHash === deposit.txHash,
        )!.landsAt;
        return clock >= landsAt
          ? "landed"
          : clock > deposit.validTo
            ? "expired"
            : "pending";
      },
      archivePendingDeposit: async () => {
        archived.push(record!.txHash);
        record = undefined;
      },
      beforeDeposit: async () => undefined,
      buildDeposit: async () => {
        const deposit = {
          txHash: (deposits.length + 1).toString(16).padStart(64, "0"),
          inclusionTime: clock + DUE_AFTER_MS,
          validTo: clock + DUE_AFTER_MS - 1,
        };
        return {
          ...deposit,
          submit: async () => {
            record = deposit;
            deposits.push({
              ...deposit,
              lovelace,
              landsAt: clock + LANDS_AFTER_MS,
              absorbed: false,
            });
          },
        };
      },
      admitsDeposit: async () => true,
      clearPendingDeposit: async () => {
        record = undefined;
      },
      now: () => clock,
      pause: async () => {
        if (
          state.stopsAfterLanding &&
          deposits.some(({ landsAt }) => clock >= landsAt)
        ) {
          state.stopsAfterLanding = false;
          throw new Error("the drill stopped");
        }
        advance(POLL_MS);
      },
      absorbWithinMs: ABSORB_WITHIN_MS,
      onStage: () => undefined,
    }),
    onStage: () => undefined,
  });
  return {
    state,
    deposits,
    archived,
    record: () => record,
    advance,
    /** Spends every output a deposit created. */
    spendDeposits: () => {
      outputs = outputs.filter((output) => initial.includes(output));
    },
    /** One withdrawal family: its funding, then its withdrawal. */
    family: async () => {
      const outcome = await ensureWithdrawalOwnerFunding(ports);
      const tail = await readTail();
      const requirement = await ports.requirement(tail);
      const ascending = [...outputs].sort((left, right) =>
        Number(
          decodeMidgardTxOutput(left.output).value.lovelace -
            decodeMidgardTxOutput(right.output).value.lovelace,
        ),
      );
      for (const entry of ascending)
        if (
          decodeMidgardTxOutput(entry.output).value.lovelace >=
          (await requirement.requiredOf(entry))
        ) {
          outputs = outputs.filter((output) => output !== entry);
          return outcome;
        }
      throw new Error("no output of the owner's funds the family");
    },
  };
};

// The owner is left only an output below any Order's minimum.
const drained = [
  { outRef: outRefCbor(0x01, 0n), output: ownedOutput(2_000_000n) },
];

it("deposits again for a later family after a stop between a top-up's landing and its clearing", async () => {
  const run = await stack(drained);
  run.state.stopsAfterLanding = true;
  await expect(run.family()).rejects.toThrow("the drill stopped");
  expect(run.record()).toEqual(
    expect.objectContaining({ txHash: run.deposits[0]!.txHash }),
  );
  // The stack absorbs the deposit while the drill is stopped. The resumed
  // family finds the owner funded and withdraws the output; the next family,
  // well within the first deposit's absorption deadline, needs a top-up of
  // its own.
  run.advance(DUE_AFTER_MS);
  expect(await run.family()).toBe("funded");
  expect(run.record()).toBeUndefined();
  expect(await run.family()).toBe("topped-up");
  expect(run.deposits).toHaveLength(2);
  expect(run.record()).toBeUndefined();
});

it("deposits again, setting the record aside, when a landed top-up is still recorded past its absorption deadline", async () => {
  const run = await stack(drained);
  run.state.stopsAfterLanding = true;
  await expect(run.family()).rejects.toThrow("the drill stopped");
  // The stack absorbed it and its output was spent before the drill resumed,
  // past the record's absorption deadline, with no family finding it first.
  run.advance(ABSORB_WITHIN_MS + DUE_AFTER_MS);
  run.spendDeposits();
  expect(await run.family()).toBe("topped-up");
  expect(run.deposits).toHaveLength(2);
  expect(run.archived).toEqual([run.deposits[0]!.txHash]);
  expect(run.record()).toBeUndefined();
});
