/**
 * Ledger-rule fraud-proof journeys: invalid validity range, zero inputs, and
 * non-existent input.
 *
 * Split out of `submit-init-emulator.test.ts` to keep each file's leaked wasm
 * heap far below the ~4 GiB wasm32 ceiling; see
 * tests/support/uplc-heap-guard.ts.
 */

import { outRefLabel } from "@al-ft/midgard-core";
import {
  EMPTY_SPEND_INPUTS_HASH,
  FraudProofTokenDatum,
  InvalidRangeStep02Datum,
  ZeroInputStep02Datum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  toUnit,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  resolveProverSigner,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import {
  neSubmitStep01,
  neSubmitStep02,
  neSubmitStep03,
  neSubmitStep04,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitInvalidRangeStep01,
  submitInvalidRangeStep02,
  submitZeroInputStep01,
  submitZeroInputStep02,
} from "./support/legacy-submit-emulator.js";
import {
  buildInvalidRangeTransactionInclusionFixture,
  buildNonExistentInputFixture,
  buildTransactionInclusionFixture,
  buildZeroInputTransactionInclusionFixture,
  countedTransactionsRoot,
  expectStateQueueHeaderOrder,
  registerPexcludesExclusionRewardAccount,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  makeHeader,
  network,
  publishRemovalReferenceScripts,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/**
 * GOAL_SPEC.md 3.3 proof-fit thresholds, applied to every proof-step and
 * removal transaction of a family lifecycle:
 *
 * 1. Byte fit: the complete signed transaction is at or below the real L1
 *    `maxTxSize`. `CompleteSignedTransactionMeasurement.l1ByteMargin` is
 *    already computed against `PROTOCOL_PARAMETERS_DEFAULT.maxTxSize`
 *    (16,384) rather than the emulator's relaxed 65,536 ceiling, so a
 *    positive margin is exactly the 3.3 item-1 check.
 * 2. Execution fit: memory and CPU are at or below the deployment's measured
 *    limits with at least a 20% reserve. A path that fits the raw limit but
 *    not the reserve is a FAILING result, not a smaller margin.
 */
const EXECUTION_RESERVE_FRACTION = 20n;

const expectProofFitV1 = ({
  stage,
  measurement,
  maxTxExMem,
  maxTxExSteps,
}: {
  readonly stage: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly maxTxExMem: bigint;
  readonly maxTxExSteps: bigint;
}): void => {
  // 3.3 item 1 - byte fit against the real L1 envelope.
  expect(
    measurement.l1ByteMargin,
    `${stage} exceeds the 16,384-byte L1 envelope`,
  ).toBeGreaterThanOrEqual(0);
  // 3.3 item 2 - execution fit with a 20% reserve.
  const memoryCeiling =
    (maxTxExMem * (100n - EXECUTION_RESERVE_FRACTION)) / 100n;
  const stepCeiling =
    (maxTxExSteps * (100n - EXECUTION_RESERVE_FRACTION)) / 100n;
  expect(
    measurement.executionMemory <= memoryCeiling,
    `${stage} execution memory ${measurement.executionMemory.toString()} exceeds the 20%-reserve ceiling ${memoryCeiling.toString()}`,
  ).toBe(true);
  expect(
    measurement.executionSteps <= stepCeiling,
    `${stage} execution steps ${measurement.executionSteps.toString()} exceeds the 20%-reserve ceiling ${stepCeiling.toString()}`,
  ).toBe(true);
};

describe("fault-proof emulator integration", () => {
  it("proves and removes a tail invalid-range block end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realInvalidRange: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
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
    const invalidRangeInclusion =
      await buildInvalidRangeTransactionInclusionFixture({
        blockValidFrom: BigInt(headerStartTime),
        blockValidTo: BigInt(headerStartTime + 1_000),
      });
    expect(invalidRangeInclusion.violationReason).toBe("lower-before-block");

    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        invalidRangeInclusion.transactionsRoot,
        invalidRangeInclusion.l2TransactionCount,
      ),
      invalidRangeInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    const initResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        submitInit({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          fraudCategory: "invalidRange",
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          awaitConfirmation: true,
        }),
    );
    const initResult = initResultCapture.result;
    proofFit["init"] = initResultCapture.measurement;

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("invalidRange");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.invalidRange.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.invalidRange.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const step01ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        submitInvalidRangeStep01({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(firstStepUtxo),
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          txInclusion: parseSubmitStep01TxInclusion(
            invalidRangeInclusion.badTx.inclusion,
          ),
          awaitConfirmation: true,
        }),
    );
    const step01Result = step01ResultCapture.result;
    proofFit["step-01"] = step01ResultCapture.measurement;

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.nativeTxId).toBe(
      invalidRangeInclusion.badTx.nativeTxId,
    );
    expect(step01Result.blockValidFrom).toBe(fraudulentHeader.startTime);
    expect(step01Result.blockValidTo).toBe(fraudulentHeader.endTime);
    expect(step01Result.normalizedValidityRange).toEqual(
      invalidRangeInclusion.normalizedValidityRange,
    );
    expect(step01Result.violationReason).toBe("lower-before-block");
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Datum = Data.from(
      secondStepUtxo.datum!,
      InvalidRangeStep02Datum,
    );
    expect(step02Datum).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        block_valid_from: fraudulentHeader.startTime,
        block_valid_to: fraudulentHeader.endTime,
        bad_tx_normalized_validity_range:
          invalidRangeInclusion.normalizedValidityRange,
      },
    });

    const step02ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        submitInvalidRangeStep02({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          awaitConfirmation: true,
        }),
    );
    const step02Result = step02ResultCapture.result;
    proofFit["step-02"] = step02ResultCapture.measurement;

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(step02Result.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    expect(step02Result.violationReason).toBe("lower-before-block");
    expect(step02Result.normalizedValidityRange).toEqual(
      invalidRangeInclusion.normalizedValidityRange,
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

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
          fraudCategory: "invalidRange",
          fraudulentHeaderHash: headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
          validTo: removeNow + 300_000n,
        }),
    );
    const removeResult = removeResultCapture.result;
    proofFit["remove"] = removeResultCapture.measurement;

    expect(removeResult.fraudCategory).toBe("invalidRange");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.invalidRange.categoryId,
    );
    expect(removeResult.stateQueueMutationLease).toBeNull();
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
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
        contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step02Result.fraudProofUnit]).toBe(1n);

    // invalid-range proof fit (GOAL_SPEC.md 3.3). Every transaction of the complete
    // correction path is measured, not just the largest one.
    expect(Object.keys(proofFit)).toEqual([
      "init",
      "step-01",
      "step-02",
      "remove",
    ]);
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({
        stage: `invalid-range ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    if (process.env["MIDGARD_PRINT_PROOF_FIT"] === "1") {
      console.log(
        `invalid-range proof fit: ${JSON.stringify(
          Object.fromEntries(
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
          null,
          2,
        )}`,
      );
    }
  }, 180_000);

  it("proves and removes a tail zero-input block end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realZeroInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const zeroInputInclusion =
      await buildZeroInputTransactionInclusionFixture();

    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        zeroInputInclusion.transactionsRoot,
        zeroInputInclusion.l2TransactionCount,
      ),
      zeroInputInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    const initCapture = await captureEmulatorSubmission(emulator, async () =>
      submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "zeroInput",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        awaitConfirmation: true,
      }),
    );
    const initResult = initCapture.result;
    proofFit["init"] = initCapture.measurement;

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("zeroInput");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.zeroInput.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.zeroInput.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const step01Capture = await captureEmulatorSubmission(emulator, async () =>
      submitZeroInputStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(
          zeroInputInclusion.badTx.inclusion,
        ),
        awaitConfirmation: true,
      }),
    );
    const step01Result = step01Capture.result;
    proofFit["step-01"] = step01Capture.measurement;

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.nativeTxId).toBe(zeroInputInclusion.badTx.nativeTxId);
    expect(step01Result.badTxSpendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    expect(Data.from(secondStepUtxo.datum!, ZeroInputStep02Datum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: { bad_tx_spend_inputs_hash: EMPTY_SPEND_INPUTS_HASH },
    });

    const step02Capture = await captureEmulatorSubmission(emulator, async () =>
      submitZeroInputStep02({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStepUtxo),
        awaitConfirmation: true,
      }),
    );
    const step02Result = step02Capture.result;
    proofFit["step-02"] = step02Capture.measurement;

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.badTxSpendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    expect(step02Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(step02Result.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeCapture = await captureEmulatorSubmission(emulator, async () =>
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "zeroInput",
        fraudulentHeaderHash: headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    const removeResult = removeCapture.result;
    proofFit["remove"] = removeCapture.measurement;

    expect(removeResult.fraudCategory).toBe("zeroInput");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.zeroInput.categoryId,
    );
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
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
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step02Result.fraudProofUnit]).toBe(1n);

    // Q14 zero-input proof fit (GOAL_SPEC.md 3.3). Every transaction of the
    // complete correction path is measured, not just the largest one.
    expect(Object.keys(proofFit)).toEqual([
      "init",
      "step-01",
      "step-02",
      "remove",
    ]);
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({
        stage: `zero-input ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    if (process.env["MIDGARD_PRINT_PROOF_FIT"] === "1") {
      console.log(
        `zero-input proof fit: ${JSON.stringify(
          Object.fromEntries(
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
          null,
          2,
        )}`,
      );
    }
  }, 180_000);

  it("rejects a spending transaction before a zero-input thread can advance", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realZeroInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const transactionInclusion = await buildTransactionInclusionFixture();

    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        transactionInclusion.transactionsRoot,
        transactionInclusion.l2TransactionCount,
      ),
      transactionInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "zeroInput",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    await expect(
      submitZeroInputStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(
          transactionInclusion.tx1.inclusion,
        ),
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(
      "--tx-inclusion.nativeTx spends at least one input, so it does not violate the zero-input ledger rule.",
    );

    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      proverLucid.utxosAtWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        toUnit(
          contracts.fraudProof.policyId,
          initResult.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
  }, 180_000);

  it("proves and removes a tail non-existent-input block end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    await registerPexcludesExclusionRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realNonExistentInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const fixture = await buildNonExistentInputFixture();
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
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
    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        fixture.transactionsRoot,
        fixture.l2TransactionCount,
      ),
      fixture.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    const initResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        submitInit({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          fraudCategory: "nonExistentInput",
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          awaitConfirmation: true,
        }),
    );
    const initResult = initResultCapture.result;
    proofFit["init"] = initResultCapture.measurement;
    expect(initResult.fraudCategoryName).toBe("nonExistentInput");
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.nonExistentInput.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        neSubmitStep01({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(firstStepUtxo),
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          txInclusion: fixture.inclusion,
          awaitConfirmation: true,
        }),
    );
    const step01Result = step01ResultCapture.result;
    proofFit["step-01"] = step01ResultCapture.measurement;
    expect(step01Result.nativeTxId).toBe(fixture.nativeTxId);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        neSubmitStep02({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          inputsPreimage: fixture.inputsPreimage,
          badInputIndex: fixture.badInputIndex,
          awaitConfirmation: true,
        }),
    );
    const step02Result = step02ResultCapture.result;
    proofFit["step-02"] = step02ResultCapture.measurement;
    expect(step02Result.missingInput.tx_id).toBe(fixture.missingInputTxId);

    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        neSubmitStep03({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(thirdStepUtxo),
          ledgerNonMembershipProofCbor: fixture.ledgerNonMembershipProofCbor,
          awaitConfirmation: true,
        }),
    );
    const step03Result = step03ResultCapture.result;
    proofFit["step-03"] = step03ResultCapture.measurement;
    expect(step03Result.missingInputTxId).toBe(fixture.missingInputTxId);

    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    const step04ResultCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        neSubmitStep04({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(fourthStepUtxo),
          txsNonMembershipProofCbor: fixture.txsNonMembershipProofCbor,
          awaitConfirmation: true,
        }),
    );
    const step04Result = step04ResultCapture.result;
    proofFit["step-04"] = step04ResultCapture.measurement;
    expect(step04Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );

    const proverPaymentKeyHash = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential!.hash;
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

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
          fraudCategory: "nonExistentInput",
          fraudulentHeaderHash: headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
          validTo: removeNow + 300_000n,
        }),
    );
    const removeResult = removeResultCapture.result;
    proofFit["remove"] = removeResultCapture.measurement;
    expect(removeResult.fraudCategory).toBe("nonExistentInput");
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(retainedFraudProof.assets[step04Result.fraudProofUnit]).toBe(1n);

    // non-existent-input proof fit (GOAL_SPEC.md 3.3). Every transaction of the complete
    // correction path is measured, not just the largest one.
    expect(Object.keys(proofFit)).toEqual([
      "init",
      "step-01",
      "step-02",
      "step-03",
      "step-04",
      "remove",
    ]);
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({
        stage: `non-existent-input ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    if (process.env["MIDGARD_PRINT_PROOF_FIT"] === "1") {
      console.log(
        `non-existent-input proof fit: ${JSON.stringify(
          Object.fromEntries(
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
          null,
          2,
        )}`,
      );
    }
  }, 180_000);
});
