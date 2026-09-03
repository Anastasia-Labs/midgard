/**
 * Double-spend fraud-proof journeys and the state-queue removals they drive.
 *
 * Sibling files `submit-init-emulator-removal-lease.test.ts`,
 * `submit-init-emulator-ledger-rules.test.ts`,
 * `submit-init-emulator-transition-trace.test.ts`,
 * `submit-init-emulator-validation-dispute.test.ts` and
 * `submit-init-emulator-soundness.test.ts` carry the remaining journeys of the
 * same `fault-proof emulator integration` suite. The division is not
 * cosmetic: `@lucid-evolution/uplc` leaks wasm linear memory on every script
 * evaluation and vitest isolates per FILE, so each file is a budget against
 * the ~4 GiB wasm32 ceiling. See tests/support/uplc-heap-guard.ts before
 * moving a journey between these files.
 */

import { outRefLabel } from "@al-ft/midgard-core";
import {
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  EMPTY_MERKLE_TREE_ROOT,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  referenceScriptAuthPolicyDeploymentInfo,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  Data,
  getAddressDetails,
  type Script,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitStep01,
  submitStep02,
  submitStep03,
  submitStep04,
} from "./support/legacy-submit-emulator.js";
import {
  buildProvedDoubleSpendFixture,
  buildTransactionInclusionFixture,
  countedTransactionsRoot,
  createRecordingLeaseCoordinator,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  eventIndexes,
  expectRemovedFraudProofState,
  expectStateQueueHeaderOrder,
  instrumentLucidForRemoval,
  midgardTxInput,
  positiveNonAdaAssets,
  type RemovalEvent,
  submitRemovalForFixture,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  deploymentManifest,
  expectProofFit,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  publishRemovalReferenceScripts,
  type RemovalReferenceScriptName,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

describe("fault-proof emulator integration", () => {
  it("proves and removes a non-tail double-spend block by pruning successors first", async () => {
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      nonceUtxo,
      contracts,
      catalogue,
      witnessReferenceScripts,
      faultProofReferenceScripts,
    } = await makeFaultProofEmulatorHarness();
    const transactionInclusion = await buildTransactionInclusionFixture();
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const funderKeyHash = await funderPaymentKeyHash(funderLucid);
    const baseFraudulentHeader = makeHeader(
      funderKeyHash,
      headerStartTime,
      await countedTransactionsRoot(
        transactionInclusion.transactionsRoot,
        transactionInclusion.l2TransactionCount,
      ),
      transactionInclusion.l2TransactionCount,
    );
    const fraudulentHeader = {
      ...baseFraudulentHeader,
      endTime:
        baseFraudulentHeader.startTime +
        BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS),
    };
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    const successorStart = emulatorSuccessorHeaderStart({
      predecessorEndTime: fraudulentHeader.endTime,
      emulator,
    });
    const successor = await submitSuccessorBlockTx({
      lucid: funderLucid,
      emulator,
      contracts,
      anchorBlockUnit: setup.stateQueueBlockUnit,
      header: {
        ...makeHeader(funderKeyHash, successorStart, EMPTY_MERKLE_TREE_ROOT),
        prevHeaderHash: headerHash,
      },
      hubOracle: setup.hubOracle,
      scheduler: setup.scheduler,
      activeOperatorNode: setup.activeOperatorNode,
      activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
    });
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash, successor.successorHeaderHash],
    });
    const fraudulentBlockOutRef = successor.continuedAnchorOutRef;
    const deploymentEntry = (
      scriptHash: string,
      script: Script,
      referenceName?: RemovalReferenceScriptName,
    ) => {
      const published =
        referenceName === undefined
          ? undefined
          : removalReferenceScriptPublications.published[referenceName];
      return {
        scriptHash,
        refScriptUTxO:
          published === undefined
            ? null
            : {
                txHash: published.txHash,
                outputIndex: published.outputIndex,
              },
        contract: {
          type: script.type,
          cborHex: script.script,
        },
      };
    };
    const deploymentInfo = deploymentManifest(
      {
        hubOracleMint: { scriptHash: contracts.hubOracle.policyId },
        fraudProofCatalogueMint: {
          scriptHash: contracts.fraudProofCatalogue.policyId,
          fraudProofCatalogue: catalogue,
        },
        fraudProofCatalogueSpend: {
          scriptHash: contracts.fraudProofCatalogue.spendingScriptHash,
        },
        fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
        fraudProofSpend: {
          scriptHash: contracts.fraudProof.spendingScriptHash,
        },
        fraudProofDoubleSpend: {
          scriptHash: contracts.fraudProofs.doubleSpend.spendingScriptHash,
        },
        stateQueueMint: deploymentEntry(
          contracts.stateQueue.policyId,
          contracts.stateQueue.mintingScript,
          "stateQueueMint",
        ),
        correctionLockSpend: deploymentEntry(
          contracts.correctionLock.spendingScriptHash,
          contracts.correctionLock.spendingScript,
          "correctionLockSpend",
        ),
        stateQueueSpend: deploymentEntry(
          contracts.stateQueue.spendingScriptHash,
          contracts.stateQueue.spendingScript,
          "stateQueueSpend",
        ),
        stateQueueFraudRemovalWithdraw: deploymentEntry(
          contracts.stateQueue.yields.fraudRemoval.withdrawalScriptHash,
          contracts.stateQueue.yields.fraudRemoval.withdrawalScript,
          "stateQueueFraudRemovalWithdraw",
        ),
        retiredOperatorsMint: deploymentEntry(
          contracts.retiredOperators.policyId,
          contracts.retiredOperators.mintingScript,
          "retiredOperatorsMint",
        ),
        retiredOperatorsSpend: deploymentEntry(
          contracts.retiredOperators.spendingScriptHash,
          contracts.retiredOperators.spendingScript,
          "retiredOperatorsSpend",
        ),
        registeredOperatorsMint: {
          scriptHash: contracts.registeredOperators.policyId,
        },
        registeredOperatorsSpend: deploymentEntry(
          contracts.registeredOperators.spendingScriptHash,
          contracts.registeredOperators.spendingScript,
        ),
        activeOperatorsMint: deploymentEntry(
          contracts.activeOperators.policyId,
          contracts.activeOperators.mintingScript,
          "activeOperatorsMint",
        ),
        activeOperatorsSpend: deploymentEntry(
          contracts.activeOperators.spendingScriptHash,
          contracts.activeOperators.spendingScript,
          "activeOperatorsSpend",
        ),
        schedulerMint: { scriptHash: contracts.scheduler.policyId },
        schedulerSpend: deploymentEntry(
          contracts.scheduler.spendingScriptHash,
          contracts.scheduler.spendingScript,
          "schedulerSpend",
        ),
        settlementMint: { scriptHash: contracts.settlement.policyId },
      },
      referenceScriptAuthPolicyDeploymentInfo(contracts.referenceScriptAuth),
    );

    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    const resultCapture = await captureEmulatorSubmission(emulator, async () =>
      submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudulentBlockOutRef,
        witnessReferenceScripts,
        awaitConfirmation: true,
      }),
    );
    const result = resultCapture.result;
    proofFit["init"] = resultCapture.measurement;

    expect(result.txHash).toHaveLength(64);
    expect(result.fraudulentHeaderHash).toBe(headerHash);
    expect(result.computationThreadAssetName).toBe(
      `${catalogue.categories.doubleSpend.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      result.firstStepAddress,
      result.computationThreadUnit,
    );
    const stepDatum = Data.from(
      firstStepUtxo.datum!,
      FraudProofComputationThreadStepDatum,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    expect(stepDatum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: null,
    });
    expect(firstStepUtxo.assets[result.computationThreadUnit]).toBe(1n);
    expect(positiveNonAdaAssets(firstStepUtxo)).toEqual([
      [result.computationThreadUnit, 1n],
    ]);

    const step01ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        submitStep01({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(firstStepUtxo),
          stateQueueBlockOutRef: fraudulentBlockOutRef,
          txInclusion: parseSubmitStep01TxInclusion(
            transactionInclusion.tx1.inclusion,
          ),
          referenceScriptUtxo:
            faultProofReferenceScripts.fraudProofDoubleSpend!.utxo,
          witnessReferenceScripts,
          awaitConfirmation: true,
        }),
    );
    const step01Result = step01ResultCapture.result;
    proofFit["step-01"] = step01ResultCapture.measurement;

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.nativeTxId).toBe(transactionInclusion.tx1.nativeTxId);
    const remainingFirstStepUtxos = await proverLucid.utxosAtWithUnit(
      result.firstStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingFirstStepUtxos).toHaveLength(0);
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      result.computationThreadUnit,
    );
    const step02Datum = Data.from(
      secondStepUtxo.datum!,
      DoubleSpendStep02Datum,
    );
    expect(step02Datum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: {
        verified_tx1_id: transactionInclusion.tx1.nativeTxId,
      },
    });
    expect(secondStepUtxo.assets[result.computationThreadUnit]).toBe(1n);

    const step02ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        submitStep02({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          stateQueueBlockOutRef: fraudulentBlockOutRef,
          txInclusion: parseSubmitStep01TxInclusion(
            transactionInclusion.tx2.inclusion,
          ),
          referenceScriptUtxo:
            faultProofReferenceScripts.fraudProofDoubleSpendStep02!.utxo,
          witnessReferenceScripts,
          awaitConfirmation: true,
        }),
    );
    const step02Result = step02ResultCapture.result;
    proofFit["step-02"] = step02ResultCapture.measurement;

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.verifiedTx1Id).toBe(
      transactionInclusion.tx1.nativeTxId,
    );
    expect(step02Result.nativeTx2Id).toBe(transactionInclusion.tx2.nativeTxId);
    // #604: the thread carries both §2.5 anchors; the field-0 commitments it
    // used to forward are re-derived at the door instead.
    expect(step02Result.verifiedTx2Id).toBe(
      transactionInclusion.tx2.nativeTxId,
    );
    const remainingSecondStepUtxos = await proverLucid.utxosAtWithUnit(
      step01Result.secondStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingSecondStepUtxos).toHaveLength(0);
    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      result.computationThreadUnit,
    );
    const step03Datum = Data.from(thirdStepUtxo.datum!, DoubleSpendStep03Datum);
    expect(step03Datum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: {
        verified_tx1_id: transactionInclusion.tx1.nativeTxId,
        verified_tx2_id: transactionInclusion.tx2.nativeTxId,
      },
    });
    expect(thirdStepUtxo.assets[result.computationThreadUnit]).toBe(1n);

    const step03ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        submitStep03({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(thirdStepUtxo),
          tx1SpendInputCbors: parseSpendInputCbors(
            transactionInclusion.tx1SpendInputCbors,
            "--tx1-inputs",
          ),
          nativeTxCompactCbor: parseSubmitStep01TxInclusion(
            transactionInclusion.tx1.inclusion,
          ).nativeTxCompactCbor,
          doubleSpentInputIndex: 1n,
          referenceScriptUtxo:
            faultProofReferenceScripts.fraudProofDoubleSpendStep03!.utxo,
          awaitConfirmation: true,
        }),
    );
    const step03Result = step03ResultCapture.result;
    proofFit["step-03"] = step03ResultCapture.measurement;

    expect(step03Result.txHash).toHaveLength(64);
    expect(step03Result.verifiedTx1SpendInputsHash).toBe(
      transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
    );
    expect(step03Result.doubleSpentInputIndex).toBe(1);
    expect(step03Result.doubleSpentInput).toEqual(
      midgardTxInput(transactionInclusion.tx1InputsPreimage[1]!),
    );
    expect(step03Result.doubleSpentInputCbor).toEqual(
      transactionInclusion.tx1SpendInputCbors[1],
    );
    const remainingThirdStepUtxos = await proverLucid.utxosAtWithUnit(
      step02Result.thirdStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingThirdStepUtxos).toHaveLength(0);
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      result.computationThreadUnit,
    );
    const step04Datum = Data.from(
      fourthStepUtxo.datum!,
      DoubleSpendStep04Datum,
    );
    expect(step04Datum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: {
        verified_tx2_id: transactionInclusion.tx2.nativeTxId,
        double_spent_input: midgardTxInput(
          transactionInclusion.tx1InputsPreimage[1]!,
        ),
      },
    });
    expect(fourthStepUtxo.assets[result.computationThreadUnit]).toBe(1n);

    const step04ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        submitStep04({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(fourthStepUtxo),
          tx2SpendInputCbors: parseSpendInputCbors(
            transactionInclusion.tx2SpendInputCbors,
            "--tx2-inputs",
          ),
          nativeTxCompactCbor: parseSubmitStep01TxInclusion(
            transactionInclusion.tx2.inclusion,
          ).nativeTxCompactCbor,
          doubleSpentInputIndex: 1n,
          referenceScriptUtxo:
            faultProofReferenceScripts.fraudProofDoubleSpendStep04!.utxo,
          witnessReferenceScripts,
          awaitConfirmation: true,
        }),
    );
    const step04Result = step04ResultCapture.result;
    proofFit["step-04"] = step04ResultCapture.measurement;

    expect(step04Result.txHash).toHaveLength(64);
    expect(step04Result.doubleSpentInputIndex).toBe(1);
    expect(step04Result.doubleSpentInput).toEqual(
      midgardTxInput(transactionInclusion.tx2InputsPreimage[1]!),
    );
    expect(step04Result.doubleSpentInputCbor).toEqual(
      transactionInclusion.tx2SpendInputCbors[1],
    );
    expect(step04Result.fraudProofAssetName).toBe(
      result.computationThreadAssetName,
    );
    expect(step04Result.fraudProofUnit).toBe(
      toUnit(contracts.fraudProof.policyId, result.computationThreadAssetName),
    );
    expect(step04Result.fraudProofMintRedeemerIndex).not.toBe(
      step04Result.computationThreadMintRedeemerIndex,
    );

    const remainingFourthStepUtxos = await proverLucid.utxosAtWithUnit(
      step03Result.fourthStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingFourthStepUtxos).toHaveLength(0);
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    const fraudProofDatum = Data.from(
      fraudProofUtxo.datum!,
      FraudProofTokenDatum,
    );
    expect(fraudProofDatum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
    });
    expect(fraudProofUtxo.assets[step04Result.fraudProofUnit]).toBe(1n);
    expect(positiveNonAdaAssets(fraudProofUtxo)).toEqual([
      [step04Result.fraudProofUnit, 1n],
    ]);

    const removeNow = BigInt(emulator.now());
    const removeResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        submitRemoveFraudulentBlock({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          fraudulentHeaderHash: headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
          validTo: removeNow + 300_000n,
          stateQueueMutationLeaseCoordinator: {
            acquire: async () => ({
              token: "emulator-fault-proof-removal",
              source: "emulator",
              renew: async () => {},
              release: async () => {},
              fail: async () => {},
            }),
          },
        }),
    );
    const removeResult = removeResultCapture.result;
    proofFit["remove"] = removeResultCapture.measurement;
    expect(removeResult.fraudulentHeaderHash).toBe(headerHash);
    expect(removeResult.fraudProver).toBe(proverPaymentCredential!.hash);
    expect(removeResult.stateQueueMutationLease).toEqual({
      token: "emulator-fault-proof-removal",
      source: "emulator",
      released: true,
    });
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [successor.successorHeaderHash, headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
      "OperatorAlreadySlashed",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });

    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        successor.successorBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalSchedulerUtxo] = await funderLucid.utxosAtWithUnit(
      contracts.scheduler.spendingScriptAddress,
      toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
    );
    if (finalSchedulerUtxo === undefined) {
      throw new Error("Remove transaction did not preserve the scheduler");
    }
    expect(Data.from(finalSchedulerUtxo.datum!, SchedulerDatum)).toBe(
      "NoActiveOperators",
    );
    const [finalRootUtxo] = await funderLucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueRootUnit,
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
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step04Result.fraudProofUnit]).toBe(1n);

    // Q10 double-spend proof fit (GOAL_SPEC.md 3.3). Every transaction of the
    // complete correction path is measured, not just the largest one. Note the
    // removal here prunes a successor first, so "remove" is the last submitted
    // transaction of that multi-transaction removal.
    expect(Object.keys(proofFit)).toEqual([
      "init",
      "step-01",
      "step-02",
      "step-03",
      "step-04",
      "remove",
    ]);
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFit({
        stage: `double-spend ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    for (const measurement of removeResultCapture.measurements) {
      expectProofFit({
        stage: "double-spend removal transaction",
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    if (process.env["MIDGARD_PRINT_PROOF_FIT"] === "1") {
      console.log(
        `double-spend proof fit: ${JSON.stringify(
          {
            ...Object.fromEntries(
              Object.entries(proofFit).map(([stage, measurement]) => [
                stage,
                {
                  bytes: measurement.completeSignedBytes,
                  l1ByteMargin: measurement.l1ByteMargin,
                  memory: measurement.executionMemory.toString(),
                  steps: measurement.executionSteps.toString(),
                },
              ]),
            ),
            removalTransactions: removeResultCapture.measurements.map(
              (measurement) => ({
                bytes: measurement.completeSignedBytes,
                l1ByteMargin: measurement.l1ByteMargin,
                memory: measurement.executionMemory.toString(),
                steps: measurement.executionSteps.toString(),
              }),
            ),
          },
          null,
          2,
        )}`,
      );
    }
  }, 180_000);

  it("removes a tail double-spend block without acquiring a lease", async () => {
    const fixture = await buildProvedDoubleSpendFixture();
    // Owner ruling 2026-08-26: witness scripts resolve from published
    // reference scripts — the init and step-04 transactions must carry no
    // inline script witnesses at all (every script is a reference input).
    for (const measurement of [
      fixture.submitInitMeasurement,
      fixture.step04Measurement,
    ]) {
      expect(measurement.plutusV1ScriptCount).toBe(0);
      expect(measurement.plutusV2ScriptCount).toBe(0);
      expect(measurement.plutusV3ScriptCount).toBe(0);
      expect(measurement.nativeScriptCount).toBe(0);
    }
    expect(
      fixture.submitInitMeasurement.referenceInputCount,
    ).toBeGreaterThanOrEqual(5);
    // Step-04 resolves its step spend, the computation-thread mint, and the
    // fraud-proof mint from three published reference scripts.
    expect(
      fixture.step04Measurement.referenceInputCount,
    ).toBeGreaterThanOrEqual(3);
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
    });

    expect(removeResult.stateQueueMutationLease).toBeNull();
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [fixture.headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    expect(eventIndexes(events, "lease.acquire")).toHaveLength(0);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(0);
    expect(eventIndexes(events, "lease.release")).toHaveLength(0);
    expect(eventIndexes(events, "lease.fail")).toHaveLength(0);
    expect(eventIndexes(events, "stateQueue.utxosAt")).toHaveLength(1);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(1);

    await expectRemovedFraudProofState(fixture);
  }, 180_000);

  it("removes a non-tail double-spend block with multiple successors in queue order", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 2 });
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
      stateQueueMutationLeaseCoordinator:
        createRecordingLeaseCoordinator(events),
    });

    expect(removeResult.stateQueueMutationLease).toEqual({
      token: "emulator-fault-proof-removal",
      source: "emulator",
      released: true,
    });
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [
        fixture.successors[0]!.successorHeaderHash,
        fixture.successors[1]!.successorHeaderHash,
        fixture.headerHash,
      ],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
      "OperatorAlreadySlashed",
      "OperatorAlreadySlashed",
    ]);

    expect(eventIndexes(events, "stateQueue.utxosAt")).toHaveLength(4);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(6);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(3);
    expect(eventIndexes(events, "lease.release")).toHaveLength(1);
    expect(eventIndexes(events, "lease.fail")).toHaveLength(0);

    await expectRemovedFraudProofState(fixture);
  }, 300_000);
});
