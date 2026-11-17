import { randomUUID } from "node:crypto";
import { tmpdir } from "node:os";
import { join } from "node:path";

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
import { verifiedPredecessorBeforeFamily } from "./stack-verified-predecessor.before-family.js";
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
const lovelaceOf = (entry: OwnedOutput) =>
  decodeMidgardTxOutput(entry.output).value.lovelace;
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
// The heal's transfer and its fee.
const HEAL_TRANSFER = 1_000_000n;
const HEAL_FEE = 200_000n;

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
  const fund = () => ensureWithdrawalOwnerFunding(ports);
  const withdraw = async () => {
    const requirement = await ports.requirement(await readTail());
    const ascending = [...outputs].sort((left, right) =>
      Number(lovelaceOf(left) - lovelaceOf(right)),
    );
    for (const entry of ascending)
      if (lovelaceOf(entry) >= (await requirement.requiredOf(entry))) {
        outputs = outputs.filter((output) => output !== entry);
        return;
      }
    throw new Error("no output of the owner's funds the family");
  };
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
    /** A withdrawal family's funding check. */
    fund,
    /** A withdrawal family's withdrawal, of its smallest funding output. */
    withdraw,
    /** One withdrawal family: its funding, then its withdrawal. */
    family: async () => {
      const outcome = await fund();
      await withdraw();
      return outcome;
    },
    /**
     * The verified-predecessor heal: the owner's transfer to itself, which
     * spends its largest output and splits it into the transfer and change.
     */
    heal: async () => {
      const [largest] = [...outputs].sort((left, right) =>
        Number(lovelaceOf(right) - lovelaceOf(left)),
      );
      const healed = deposits.length + outputs.length;
      outputs = [
        ...outputs.filter((output) => output !== largest),
        {
          outRef: outRefCbor(0xe0 + healed, 0n),
          output: ownedOutput(HEAL_TRANSFER),
        },
        {
          outRef: outRefCbor(0xe0 + healed, 1n),
          output: ownedOutput(lovelaceOf(largest!) - HEAL_TRANSFER - HEAL_FEE),
        },
      ];
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

it("finds the output a withdrawal family needs after the heal's transfer has moved the owner's outputs", async () => {
  const run = await stack(drained);
  await expect(
    verifiedPredecessorBeforeFamily({
      familyDirectory: join(tmpdir(), `no-family-${randomUUID()}`),
      isClosed: () => false,
      close: () => Promise.resolve(),
      checkPredecessor: run.heal,
      checkFunding: run.fund,
      onStage: () => undefined,
    }),
  ).resolves.toBe("checked");
  await run.withdraw();
  expect(run.deposits).toHaveLength(1);
});
