import {
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
} from "@lucid-evolution/lucid";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  EMPTY_HEADER_TRANSITION_COMMITMENTS,
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
  hashBlockHeader,
  type Header as HeaderType,
  StateQueueNode,
  type StateQueueUTxO,
  utxoToStateQueueUTxO,
} from "../src/index.js";
import {
  buildTestContracts,
  buildTransactionsRoot,
  commitBlockSlot,
  emulatorL1SlotConfig,
  TWO_TRANSACTION_HEADER_COMMITMENTS,
} from "./state-queue.build-test-contracts.js";
import {
  alwaysSucceedsBlueprintPath,
  EMULATOR_PROTOCOL_PARAMETERS,
  readBlueprint,
  realBlueprintPath,
} from "./state-queue.state-queue-operator-funding-inputs.js";
import { submitCommitHeaderTx } from "./state-queue.submit-commit-header-tx.js";
import { submitSetupTx } from "./state-queue.submit-setup-tx.js";

const COMMIT_REFUSED = /failed script execution Withdraw\[0\]/;

/**
 * A fresh queue (root only) behind the real state-queue validator and commit
 * yield, built with the emulator's slot mapping.
 */
const setupQueue = async () => {
  const funder = generateEmulatorAccount({ lovelace: 60_000_000_000n });
  const emulator = new Emulator([funder], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(funder.seedPhrase);
  const contracts = await buildTestContracts(
    readBlueprint(realBlueprintPath),
    readBlueprint(alwaysSucceedsBlueprintPath),
    emulatorL1SlotConfig(lucid),
  );
  const paymentCredential = getAddressDetails(
    await lucid.wallet().address(),
  ).paymentCredential;
  if (paymentCredential?.type !== "Key")
    throw new Error("Expected emulator wallet to expose a payment key hash");
  const operator = paymentCredential.hash;
  // Lucid omits validity_start when it maps to slot zero.
  emulator.awaitSlot(1);
  const initValidFrom = BigInt(emulator.now());
  const initValidTo = initValidFrom + 120_000n;
  const genesisTime = initValidTo - 1n;
  const nonceUtxo = (await lucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined)
    throw new Error("Expected wallet to expose a setup nonce UTxO");
  const setup = await submitSetupTx({
    lucid,
    contracts,
    nonceUtxo,
    operator,
    schedulerStartTime: genesisTime,
    stateQueueGenesisTime: genesisTime,
    initValidFrom,
    initValidTo,
    // No block here is removed for fraud; the setup only needs a token name.
    fraudulentHeaderHash: "fe".repeat(28),
  });
  const root = await Effect.runPromise(
    utxoToStateQueueUTxO(setup.stateQueueRoot, contracts.stateQueue.policyId),
  );
  const commitArgs = {
    emulator,
    lucid,
    contracts,
    operator,
    scheduler: setup.scheduler,
    hubOracle: setup.hubOracle,
    correctionLock: setup.correctionLock,
    commitYield: setup.commitYield,
  };
  return {
    lucid,
    operator,
    genesisTime,
    root,
    activeOperatorInput: setup.activeOperatorInput,
    commitArgs,
  };
};

const nodeBlockTime = (block: StateQueueUTxO): bigint =>
  Data.castFrom(block.datum.data, StateQueueNode).block_time;

describe("state-queue commit binds the commit time (R1-R3)", () => {
  it("refuses every mis-timed commit and lands an event-only catch-up block, then an L2 block", async () => {
    const { lucid, operator, genesisTime, root, activeOperatorInput, ...q } =
      await setupQueue();
    const transactionsRoot = await buildTransactionsRoot();
    // The commit's validity upper bound, slot-aligned; its inclusive bound is
    // the honest block_time.
    const validTo = genesisTime + 1n + 60_000n;
    const blockTime = validTo - 1n;
    // An event-only (catch-up) header whose covered interval ends a whole
    // minute before the commit.
    const catchUp: HeaderType = {
      prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: h32(0x88),
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS,
      transitionTraceRoot: h32(0x55),
      eventToStepRoot: h32(0x66),
      depositCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
      startTime: genesisTime,
      endTime: genesisTime + 1_000n,
      blockSlot: commitBlockSlot(lucid, blockTime),
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: GENESIS_HEADER_HASH,
      operatorVkey: operator,
      protocolVersion: 1n,
    };
    const attempt = (
      header: HeaderType,
      overrides: {
        readonly validTo?: bigint;
        readonly blockTime?: bigint;
      } = {},
    ) =>
      submitCommitHeaderTx({
        ...q.commitArgs,
        anchor: root,
        activeOperatorInput,
        header,
        validTo,
        ...overrides,
      });

    // R1: block_time is the commit's inclusive upper bound, not one slot
    // earlier (backdated) or later. Each mutant keeps R2 and R3 true.
    for (const shiftedBlockTime of [blockTime - 1_000n, blockTime + 1_000n])
      await expect(
        attempt(
          { ...catchUp, blockSlot: commitBlockSlot(lucid, shiftedBlockTime) },
          { blockTime: shiftedBlockTime },
        ),
      ).rejects.toThrow(COMMIT_REFUSED);
    // R1: the covered interval ends no later than block_time.
    await expect(
      attempt({ ...catchUp, endTime: blockTime + 1n }),
    ).rejects.toThrow(COMMIT_REFUSED);
    // R2: a block carrying L2 transactions must end at its block_time.
    const l2EndingEarly: HeaderType = {
      ...catchUp,
      ...TWO_TRANSACTION_HEADER_COMMITMENTS,
      transactionsRoot,
      depositsRoot: EMPTY_MERKLE_TREE_ROOT,
      depositCount: 0n,
    };
    await expect(attempt(l2EndingEarly)).rejects.toThrow(COMMIT_REFUSED);
    // R3: block_slot is exactly the slot whose last millisecond is block_time.
    for (const blockSlot of [catchUp.blockSlot - 1n, catchUp.blockSlot + 1n])
      await expect(attempt({ ...catchUp, blockSlot })).rejects.toThrow(
        COMMIT_REFUSED,
      );

    // The honest catch-up block lands: it covers (genesis, genesis + 1 s] and
    // records the commit time separately.
    const catchUpCommit = await attempt(catchUp);
    expect(nodeBlockTime(catchUpCommit.block)).toBe(blockTime);
    expect(catchUp.endTime).toBeLessThan(blockTime);

    // The next block carries L2 transactions and ends at its own block_time.
    const nextValidTo = validTo + 60_000n;
    const l2Block: HeaderType = {
      ...l2EndingEarly,
      startTime: catchUp.endTime,
      endTime: nextValidTo - 1n,
      blockSlot: commitBlockSlot(lucid, nextValidTo - 1n),
      prevHeaderHash: await Effect.runPromise(hashBlockHeader(catchUp)),
    };
    const l2Commit = await submitCommitHeaderTx({
      ...q.commitArgs,
      anchor: catchUpCommit.block,
      activeOperatorInput: catchUpCommit.activeOperatorInput,
      header: l2Block,
      validTo: nextValidTo,
    });
    expect(nodeBlockTime(l2Commit.block)).toBe(nextValidTo - 1n);
  });
});
