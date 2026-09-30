import "./state-queue.state-queue-abi.js";

import {
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  CORRECTION_LOCK_ASSET_NAME,
  EMPTY_HEADER_TRANSITION_COMMITMENTS,
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  type Header as HeaderType,
  headerHashFromStateQueueUTxO,
  incompleteRemoveFraudulentBlocksLinkTxProgram,
  incompleteRemoveLastFraudulentBlockHeaderTxProgram,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_NODE_MIN_LOVELACE,
  STATE_QUEUE_ROOT_ASSET_NAME,
  utxoToStateQueueUTxO,
} from "../src/index.js";
import {
  buildTestContracts,
  buildTransactionsRoot,
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

describe("state-queue emulator builders", () => {
  it("commits a block carrying a native transactions_root and removes it through the real tail-removal path", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 60_000_000_000n });
    const emulator = new Emulator([funder], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(funder.seedPhrase);

    const contracts = await buildTestContracts(realBlueprint, alwaysBlueprint);
    // This is a state-queue-focused smoke test. Non-state-queue contracts are
    // scaffolded with always-succeeds scripts, while the state-queue validator
    // still reads their datums/assets/redeemers through its real checks.
    const funderAddress = await lucid.wallet().address();
    const paymentCredential =
      getAddressDetails(funderAddress).paymentCredential;
    if (paymentCredential === undefined || paymentCredential.type !== "Key") {
      throw new Error("Expected emulator wallet to expose a payment key hash");
    }
    const operator = paymentCredential.hash;
    // Lucid omits validity_start when it maps to slot zero. Advance one
    // emulator slot so the real initializer receives a closed range.
    emulator.awaitSlot(1);
    const initValidFrom = BigInt(emulator.now());
    const initValidTo = initValidFrom + 120_000n;
    const genesisTime = initValidTo - 1n;
    const transactionsRoot = await buildTransactionsRoot();
    const header: HeaderType = {
      prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS,
      ...TWO_TRANSACTION_HEADER_COMMITMENTS,
      transactionsRoot,
      depositsRoot: EMPTY_MERKLE_TREE_ROOT,
      startTime: genesisTime,
      endTime: genesisTime + 1_000n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: GENESIS_HEADER_HASH,
      operatorVkey: operator,
      protocolVersion: 1n,
    };
    const headerHash = await Effect.runPromise(hashBlockHeader(header));
    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected wallet to expose a setup nonce UTxO");
    }

    const setup = await submitSetupTx({
      lucid,
      contracts,
      nonceUtxo,
      operator,
      schedulerStartTime: genesisTime,
      stateQueueGenesisTime: genesisTime,
      initValidFrom,
      initValidTo,
      fraudulentHeaderHash: headerHash,
    });
    const stateQueueRoot = await Effect.runPromise(
      utxoToStateQueueUTxO(setup.stateQueueRoot, contracts.stateQueue.policyId),
    );
    const commitArgs = {
      emulator,
      lucid,
      contracts,
      anchor: stateQueueRoot,
      header,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      correctionLock: setup.correctionLock,
      commitYield: setup.commitYield,
      activeOperatorInput: setup.activeOperatorInput,
    };
    // The real state-queue validator refuses a new node one lovelace below
    // the on-chain floor; the honest commit below pays exactly the floor.
    await expect(
      submitCommitHeaderTx({
        ...commitArgs,
        headerNodeLovelace: STATE_QUEUE_NODE_MIN_LOVELACE - 1n,
      }),
    ).rejects.toThrow(/failed script execution Withdraw\[0\]/);
    const commit = await submitCommitHeaderTx({
      emulator,
      lucid,
      contracts,
      anchor: stateQueueRoot,
      header,
      headerNodeLovelace: STATE_QUEUE_NODE_MIN_LOVELACE,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      correctionLock: setup.correctionLock,
      commitYield: setup.commitYield,
      activeOperatorInput: setup.activeOperatorInput,
    });

    const blockUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
    );
    const rootUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_ROOT_ASSET_NAME,
    );
    const [continuedRootUtxo] = await lucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      rootUnit,
    );
    if (continuedRootUtxo === undefined) {
      throw new Error(
        "Commit transaction did not preserve the state-queue root",
      );
    }
    const committedBlock = commit.block;
    expect(committedBlock.utxo.assets.lovelace).toBe(
      STATE_QUEUE_NODE_MIN_LOVELACE,
    );
    const continuedRoot = await Effect.runPromise(
      utxoToStateQueueUTxO(continuedRootUtxo, contracts.stateQueue.policyId),
    );
    const committedHeader = await Effect.runPromise(
      getHeaderFromStateQueueDatum(committedBlock.datum),
    );
    expect(committedHeader.transactionsRoot).toBe(transactionsRoot);
    await expect(
      Effect.runPromise(headerHashFromStateQueueUTxO(committedBlock)),
    ).resolves.toBe(headerHash);
    expect(continuedRoot.datum.next).toEqual({ Key: { key: headerHash } });

    const removeTx = incompleteRemoveLastFraudulentBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: continuedRoot,
        fraudulentBlockUTxO: committedBlock,
        fraudulentOperator: operator,
        fraudulentBlocksHeaderHash: headerHash,
        fraudProofRefInput: setup.fraudProof,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        hubOracleRefInput: setup.hubOracle,
        correctionLockInput: {
          utxo: setup.correctionLock,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        correctionLockSpendingScript: contracts.correctionLock.spendingScript,
        slashing: {
          kind: "operatorAlreadySlashed",
          activeOperatorsElementRefInput: setup.activeOperatorsRoot,
          retiredOperatorsElementRefInput: setup.retiredOperatorsRoot,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
        yieldWitness: {
          referenceInput: setup.fraudRemovalYield,
          script: contracts.fraudRemovalYield.spendingScript,
        },
      },
    );
    const removeUnsigned = await removeTx.complete({ localUPLCEval: true });
    const removeSigned = await removeUnsigned.sign.withWallet().complete();
    await lucid.awaitTx(await removeSigned.submit());

    await expect(
      lucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        blockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalRootUtxo] = await lucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      rootUnit,
    );
    if (finalRootUtxo === undefined) {
      throw new Error(
        "Remove transaction did not preserve the state-queue root",
      );
    }
    const finalRoot = await Effect.runPromise(
      utxoToStateQueueUTxO(finalRootUtxo, contracts.stateQueue.policyId),
    );
    expect(finalRoot.datum.next).toBe("Empty");
  });

  it("removes the immediate successor of a fraud-proved non-tail block", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 60_000_000_000n });
    const emulator = new Emulator([funder], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(funder.seedPhrase);

    const contracts = await buildTestContracts(realBlueprint, alwaysBlueprint);
    const funderAddress = await lucid.wallet().address();
    const paymentCredential =
      getAddressDetails(funderAddress).paymentCredential;
    if (paymentCredential === undefined || paymentCredential.type !== "Key") {
      throw new Error("Expected emulator wallet to expose a payment key hash");
    }
    const operator = paymentCredential.hash;
    // Lucid omits validity_start when it maps to slot zero. Advance one
    // emulator slot so the real initializer receives a closed range.
    emulator.awaitSlot(1);
    const initValidFrom = BigInt(emulator.now());
    const initValidTo = initValidFrom + 120_000n;
    const genesisTime = initValidTo - 1n;
    const firstHeader: HeaderType = {
      prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS,
      ...TWO_TRANSACTION_HEADER_COMMITMENTS,
      transactionsRoot: await buildTransactionsRoot(),
      depositsRoot: EMPTY_MERKLE_TREE_ROOT,
      startTime: genesisTime,
      endTime: genesisTime + 1_000n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: GENESIS_HEADER_HASH,
      operatorVkey: operator,
      protocolVersion: 1n,
    };
    const firstHeaderHash = await Effect.runPromise(
      hashBlockHeader(firstHeader),
    );
    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected wallet to expose a setup nonce UTxO");
    }
    const setup = await submitSetupTx({
      lucid,
      contracts,
      nonceUtxo,
      operator,
      schedulerStartTime: genesisTime,
      stateQueueGenesisTime: genesisTime,
      initValidFrom,
      initValidTo,
      fraudulentHeaderHash: firstHeaderHash,
    });
    const stateQueueRoot = await Effect.runPromise(
      utxoToStateQueueUTxO(setup.stateQueueRoot, contracts.stateQueue.policyId),
    );
    const firstCommit = await submitCommitHeaderTx({
      emulator,
      lucid,
      contracts,
      anchor: stateQueueRoot,
      header: firstHeader,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      correctionLock: setup.correctionLock,
      commitYield: setup.commitYield,
      activeOperatorInput: setup.activeOperatorInput,
    });
    const secondHeader: HeaderType = {
      ...firstHeader,
      prevUtxosRoot: firstHeader.utxosRoot,
      startTime: firstHeader.endTime,
      endTime: firstHeader.endTime + 1_000n,
      prevHeaderHash: firstHeaderHash,
    };
    const secondHeaderHash = await Effect.runPromise(
      hashBlockHeader(secondHeader),
    );
    const secondCommit = await submitCommitHeaderTx({
      emulator,
      lucid,
      contracts,
      anchor: firstCommit.block,
      header: secondHeader,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      correctionLock: setup.correctionLock,
      commitYield: setup.commitYield,
      activeOperatorInput: firstCommit.activeOperatorInput,
    });

    const firstBlockUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + firstHeaderHash,
    );
    const secondBlockUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + secondHeaderHash,
    );
    const [continuedFirstBlockUtxo] = await lucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      firstBlockUnit,
    );
    if (continuedFirstBlockUtxo === undefined) {
      throw new Error("Second commit did not preserve the first block");
    }
    const continuedFirstBlock = await Effect.runPromise(
      utxoToStateQueueUTxO(
        continuedFirstBlockUtxo,
        contracts.stateQueue.policyId,
      ),
    );
    expect(continuedFirstBlock.datum.next).toEqual({
      Key: { key: secondHeaderHash },
    });

    const removeSuccessorTx = incompleteRemoveFraudulentBlocksLinkTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        fraudulentBlockUTxO: continuedFirstBlock,
        removedBlockUTxO: secondCommit.block,
        fraudulentOperator: operator,
        fraudulentBlocksHeaderHash: firstHeaderHash,
        fraudProofRefInput: setup.fraudProof,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        hubOracleRefInput: setup.hubOracle,
        correctionLockInput: {
          utxo: setup.correctionLock,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        correctionLockSpendingScript: contracts.correctionLock.spendingScript,
        slashing: {
          kind: "operatorAlreadySlashed",
          activeOperatorsElementRefInput: setup.activeOperatorsRoot,
          retiredOperatorsElementRefInput: setup.retiredOperatorsRoot,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
        yieldWitness: {
          referenceInput: setup.fraudRemovalYield,
          script: contracts.fraudRemovalYield.spendingScript,
        },
      },
    );
    const removeUnsigned = await removeSuccessorTx.complete({
      localUPLCEval: true,
    });
    const removeSigned = await removeUnsigned.sign.withWallet().complete();
    await lucid.awaitTx(await removeSigned.submit());

    await expect(
      lucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        secondBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalFirstBlockUtxo] = await lucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      firstBlockUnit,
    );
    if (finalFirstBlockUtxo === undefined) {
      throw new Error("Successor removal did not preserve the first block");
    }
    const finalFirstBlock = await Effect.runPromise(
      utxoToStateQueueUTxO(finalFirstBlockUtxo, contracts.stateQueue.policyId),
    );
    expect(finalFirstBlock.datum.next).toBe("Empty");
  });
});
