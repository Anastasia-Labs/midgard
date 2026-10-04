import { outRefLabel } from "@al-ft/midgard-core";
import {
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  EMPTY_MERKLE_TREE_ROOT,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitStep01,
  submitStep02,
  submitStep03,
  submitStep04,
} from "./legacy-submit-emulator.js";
import { countedTransactionsRoot } from "./submit-init-emulator-fixtures.build-non-existent-input-fixture.js";
import { buildTransactionInclusionFixture } from "./submit-init-emulator-fixtures.build-transaction-inclusion-fixture.js";
import {
  bindCanonicalFixtureHeader,
  buildProvedFixtureDeploymentContext,
  commitInitializedProvedFixtureHeader,
} from "./submit-init-emulator-fixtures.deployment-context.js";
import {
  expectStateQueueHeaderOrder,
  midgardTxInput,
  positiveNonAdaAssets,
} from "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
import {
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  type ProvedDoubleSpendFixture,
} from "./submit-init-emulator-fixtures.instrument-lucid-for-removal.js";
import {
  DOUBLE_SPEND_STEP_REFERENCE_NAMES,
  submitSuccessorBlockTx,
  type SuccessorBlockFixture,
} from "./submit-init-emulator-fixtures.submit-successor-block-tx.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  makeHeader,
  network,
  publishFaultProofWitnessReferenceScripts,
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export const buildProvedDoubleSpendFixture = async ({
  successorCount = 0,
  successorsAfterProofCount = 0,
  headerMinimumFee = 0n,
  coherentCanonicalEvidence = false,
}: {
  /** Successors committed before `submitInit` mints the computation thread. */
  readonly successorCount?: number;
  /**
   * Successors committed after the fraud-proof token is minted and before any
   * removal, appended after the `successorCount` ones. `successors` lists both
   * sets in queue order; `fraudulentBlockOutRef` stays the one the proof used.
   */
  readonly successorsAfterProofCount?: number;
  /** Optional second violation used by cross-family Q53 idempotency tests. */
  readonly headerMinimumFee?: bigint;
  readonly coherentCanonicalEvidence?: boolean;
} = {}): Promise<
  ProvedDoubleSpendFixture & {
    readonly canonicalEvidence?: Awaited<
      ReturnType<typeof bindCanonicalFixtureHeader>
    >;
  }
> => {
  const {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
    canonical,
  } = await buildProvedFixtureDeploymentContext(
    headerMinimumFee,
    coherentCanonicalEvidence,
  );
  const transactionInclusion = await buildTransactionInclusionFixture({
    emptyAddressWitnesses: headerMinimumFee > 0n,
    canonicalTransactions: canonical?.nativeTransactions,
  });
  // Removal needs the state-queue, operator-directory and scheduler validators.
  // Publishing them as reference-script UTxOs is what the deployed node does and
  // is what keeps the removal transaction inside the literal 16,384-byte L1
  // envelope; `publishPlainReferenceScriptUtxo` refuses any publication that
  // does not itself fit that envelope. Published from the prover wallet before
  // the header clock is sampled so the funder's nonce UTxO survives and the
  // whole fixture timeline shifts uniformly.
  const removalReferenceScriptPublications =
    canonical?.removalReferenceScriptPublications ??
    (await publishRemovalReferenceScripts({
      lucid: proverLucid,
      contracts,
    }));
  // Owner ruling 2026-08-26: every script a fault-proof transaction executes
  // is consumed from a published reference script, never inline-attached. The
  // four double-spend step validators and the shared witness scripts are
  // published from the prover wallet alongside the removal roster above.
  const doubleSpendStepReferenceScripts =
    canonical?.doubleSpendStepReferenceScripts ??
    (await publishFraudProofChainReferenceScripts({
      lucid: proverLucid,
      steps: contracts.fraudProofContracts.doubleSpend.steps,
      entryNames: DOUBLE_SPEND_STEP_REFERENCE_NAMES,
      familyLabel: "double-spend",
    }));
  const witnessReferenceScripts =
    canonical?.witnessReferenceScripts ??
    (await publishFaultProofWitnessReferenceScripts({
      lucid: proverLucid,
      realBlueprint,
      computationThreadMintingScript: contracts.computationThread.mintingScript,
      fraudProofMintingScript: contracts.fraudProof.mintingScript,
    }));
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const funderPaymentCredential = getAddressDetails(
    await funderLucid.wallet().address(),
  ).paymentCredential;
  if (
    funderPaymentCredential === undefined ||
    funderPaymentCredential.type !== "Key"
  ) {
    throw new Error("Expected funder wallet to expose a payment key hash");
  }
  const fraudulentHeader = {
    ...makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        transactionInclusion.transactionsRoot,
        transactionInclusion.l2TransactionCount,
      ),
      transactionInclusion.l2TransactionCount,
    ),
    ...(canonical === undefined
      ? {}
      : {
          ...canonical.header,
          operatorVkey: funderPaymentCredential.hash,
          startTime: BigInt(headerStartTime),
        }),
    minFeeA: 0n,
    minFeeB: headerMinimumFee,
    endTime:
      BigInt(headerStartTime) + BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS),
  };
  const setup =
    canonical !== undefined
      ? await commitInitializedProvedFixtureHeader({
          lucid: funderLucid,
          contracts,
          header: fraudulentHeader,
        })
      : nonceUtxo !== undefined
        ? await submitSetupTx({
            lucid: funderLucid,
            contracts,
            nonceUtxo,
            catalogue,
            header: fraudulentHeader,
          })
        : (() => {
            throw new Error("Legacy setup omitted its actual nonce");
          })();
  if (canonical !== undefined)
    expect(fraudulentHeader.transactionsRoot).toBe(
      await countedTransactionsRoot(
        transactionInclusion.transactionsRoot,
        transactionInclusion.l2TransactionCount,
      ),
    );
  const { headerHash } = setup;
  const successors: SuccessorBlockFixture[] = [];
  let anchorBlockUnit = setup.stateQueueBlockUnit;
  let activeOperatorNode = setup.activeOperatorNode;
  let previousHeader = fraudulentHeader;
  let previousHeaderHash = headerHash;
  const appendSuccessors = async (count: number, afterProof: boolean) => {
    for (let index = 0; index < count; index += 1) {
      // The proof steps run the clock past the target's end, so the first
      // post-proof successor still starts exactly there (contiguity) but ends
      // one headroom past the live clock rather than past its own start. The
      // lag is rounded up to whole seconds so the end stays on the same
      // slot-boundary offset the commit's inclusive upper bound normalizes to.
      const successorStart = afterProof
        ? Number(previousHeader.endTime)
        : emulatorSuccessorHeaderStart({
            predecessorEndTime: previousHeader.endTime,
            emulator,
          });
      const baseSuccessorHeader = makeHeader(
        funderPaymentCredential.hash,
        successorStart,
        EMPTY_MERKLE_TREE_ROOT,
      );
      const successorHeader = {
        ...baseSuccessorHeader,
        endTime:
          baseSuccessorHeader.startTime +
          BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS) +
          (afterProof
            ? BigInt(
                Math.ceil(Math.max(0, emulator.now() - successorStart) / 1000) *
                  1000,
              )
            : 0n),
        prevHeaderHash: previousHeaderHash,
      };
      const successor = await submitSuccessorBlockTx({
        lucid: funderLucid,
        emulator,
        contracts,
        anchorBlockUnit,
        header: successorHeader,
        hubOracle: setup.hubOracle,
        scheduler: setup.scheduler,
        activeOperatorNode,
        activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
      });
      successors.push({ ...successor, header: successorHeader });
      anchorBlockUnit = successor.successorBlockUnit;
      activeOperatorNode = successor.activeOperatorNode;
      previousHeader = successorHeader;
      previousHeaderHash = successor.successorHeaderHash;
    }
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [
        headerHash,
        ...successors.map((successor) => successor.successorHeaderHash),
      ],
    });
  };
  await appendSuccessors(successorCount, false);
  const deploymentInfo =
    canonical?.manifest ??
    buildRemovalDeploymentInfo(contracts, catalogue, {
      removalReferenceScripts: removalReferenceScriptPublications.published,
    });
  const fraudulentBlockOutRef =
    successors[0]?.continuedAnchorOutRef ?? setup.fraudulentBlockOutRef;
  const submitInitCapture = await captureEmulatorSubmission(emulator, () =>
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
  const submitInitResult = submitInitCapture.result;
  const submitInitMeasurement = submitInitCapture.measurement;

  expect(submitInitResult.txHash).toHaveLength(64);
  expect(submitInitResult.fraudulentHeaderHash).toBe(headerHash);
  expect(submitInitResult.computationThreadAssetName).toBe(
    `${catalogue.categories.doubleSpend.categoryId}${headerHash}`,
  );

  const firstStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    submitInitResult.firstStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const stepDatum = Data.from(
    firstStepUtxo.datum!,
    FraudProofComputationThreadStepDatum,
  );
  const proverPaymentCredential = getAddressDetails(
    await proverLucid.wallet().address(),
  ).paymentCredential;
  expect(proverPaymentCredential?.type).toBe("Key");
  const proverPaymentKeyHash = proverPaymentCredential!.hash;
  expect(stepDatum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: null,
  });
  expect(firstStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(1n);
  expect(positiveNonAdaAssets(firstStepUtxo)).toEqual([
    [submitInitResult.computationThreadUnit, 1n],
  ]);

  const step01Result = await submitStep01({
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
      doubleSpendStepReferenceScripts["fraudProofDoubleSpend"]!.utxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });

  expect(step01Result.txHash).toHaveLength(64);
  expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
  expect(step01Result.nativeTxId).toBe(transactionInclusion.tx1.nativeTxId);
  const remainingFirstStepUtxos = await proverLucid.utxosAtWithUnit(
    submitInitResult.firstStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingFirstStepUtxos).toHaveLength(0);
  const secondStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step01Result.secondStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const step02Datum = Data.from(secondStepUtxo.datum!, DoubleSpendStep02Datum);
  expect(step02Datum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: {
      verified_tx1_id: transactionInclusion.tx1.nativeTxId,
    },
  });
  expect(secondStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(
    1n,
  );

  const step02Result = await submitStep02({
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
      doubleSpendStepReferenceScripts["fraudProofDoubleSpendStep02"]!.utxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });

  expect(step02Result.txHash).toHaveLength(64);
  expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
  expect(step02Result.verifiedTx1Id).toBe(transactionInclusion.tx1.nativeTxId);
  expect(step02Result.nativeTx2Id).toBe(transactionInclusion.tx2.nativeTxId);
  // #604: the thread carries both §2.5 anchors; the field-0 commitments it used
  // to forward are re-derived at the door from the compact structures instead.
  expect(step02Result.verifiedTx2Id).toBe(transactionInclusion.tx2.nativeTxId);
  const remainingSecondStepUtxos = await proverLucid.utxosAtWithUnit(
    step01Result.secondStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingSecondStepUtxos).toHaveLength(0);
  const thirdStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step02Result.thirdStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const step03Datum = Data.from(thirdStepUtxo.datum!, DoubleSpendStep03Datum);
  expect(step03Datum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: {
      verified_tx1_id: transactionInclusion.tx1.nativeTxId,
      verified_tx2_id: transactionInclusion.tx2.nativeTxId,
    },
  });
  expect(thirdStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(1n);

  const step03Result = await submitStep03({
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
      doubleSpendStepReferenceScripts["fraudProofDoubleSpendStep03"]!.utxo,
    awaitConfirmation: true,
  });

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
    submitInitResult.computationThreadUnit,
  );
  expect(remainingThirdStepUtxos).toHaveLength(0);
  const fourthStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step03Result.fourthStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const step04Datum = Data.from(fourthStepUtxo.datum!, DoubleSpendStep04Datum);
  expect(step04Datum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: {
      verified_tx2_id: transactionInclusion.tx2.nativeTxId,
      double_spent_input: midgardTxInput(
        transactionInclusion.tx1InputsPreimage[1]!,
      ),
    },
  });
  expect(fourthStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(
    1n,
  );

  const step04Capture = await captureEmulatorSubmission(emulator, () =>
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
        doubleSpendStepReferenceScripts["fraudProofDoubleSpendStep04"]!.utxo,
      witnessReferenceScripts,
      awaitConfirmation: true,
    }),
  );
  const step04Result = step04Capture.result;
  const step04Measurement = step04Capture.measurement;

  expect(step04Result.txHash).toHaveLength(64);
  expect(step04Result.doubleSpentInputIndex).toBe(1);
  expect(step04Result.doubleSpentInput).toEqual(
    midgardTxInput(transactionInclusion.tx2InputsPreimage[1]!),
  );
  expect(step04Result.doubleSpentInputCbor).toEqual(
    transactionInclusion.tx2SpendInputCbors[1],
  );
  expect(step04Result.fraudProofAssetName).toBe(
    submitInitResult.computationThreadAssetName,
  );
  expect(step04Result.fraudProofUnit).toBe(
    toUnit(
      contracts.fraudProof.policyId,
      submitInitResult.computationThreadAssetName,
    ),
  );
  expect(step04Result.fraudProofMintRedeemerIndex).not.toBe(
    step04Result.computationThreadMintRedeemerIndex,
  );

  const remainingFourthStepUtxos = await proverLucid.utxosAtWithUnit(
    step03Result.fourthStepAddress,
    submitInitResult.computationThreadUnit,
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
    fraud_prover: proverPaymentKeyHash,
  });
  expect(fraudProofUtxo.assets[step04Result.fraudProofUnit]).toBe(1n);
  expect(positiveNonAdaAssets(fraudProofUtxo)).toEqual([
    [step04Result.fraudProofUnit, 1n],
  ]);

  await appendSuccessors(successorsAfterProofCount, true);

  return {
    emulator,
    realBlueprint,
    funderLucid,
    proverLucid,
    proverSigner,
    contracts,
    catalogue,
    transactionInclusion,
    fraudulentHeader,
    headerHash,
    setup,
    successors,
    deploymentInfo,
    removalReferenceScriptPublications,
    fraudulentBlockOutRef,
    submitInitResult,
    submitInitMeasurement,
    step04Result,
    step04Measurement,
    doubleSpendStepReferenceScripts,
    witnessReferenceScripts,
    fraudProofUtxo,
    proverPaymentKeyHash,
    ...(canonical === undefined
      ? {}
      : {
          canonicalEvidence: await bindCanonicalFixtureHeader(
            canonical,
            fraudulentHeader,
            headerHash,
          ),
        }),
  };
};
