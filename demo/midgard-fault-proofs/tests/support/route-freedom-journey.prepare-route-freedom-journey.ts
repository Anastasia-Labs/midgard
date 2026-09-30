import { outRefLabel } from "@al-ft/midgard-core";
import {
  buildValidationTraceDisputeFaultProofContracts,
  parseFaultProofBlueprint,
  validationMachineStateDataFromCore,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import { getAddressDetails, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  submitValidationDisputeVerifySource,
} from "../../src/index.js";
import { createReferenceScriptPublisher } from "./emulator/reference-script-publisher.js";
import { submitInit } from "./legacy-submit-emulator.js";
import {
  type CapturedLifecycleStage,
  type RouteFreedomJourney,
} from "./route-freedom-journey.print-route-freedom-campaign-table.js";
import { buildInvalidForcedValidationDisputeFixture } from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  cloneBlueprint,
  createRealL1TargetLucids,
  createValidationDisputeParties,
  expectSingleUtxoWithUnit,
  network,
  publishFaultProofWitnessReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  runEmulatorLifecycleStage,
  stageAuthenticatedValidationDisputePublication,
  submitSetupTx,
  withRealL1MaxTxSize,
} from "./submit-init-emulator-shared.js";

/**
 * Stages one full dispute up to (and including) prepare-selected and hands
 * back the semantic-resolution leg as a function of its routing inputs.
 */
export const prepareRouteFreedomJourney = async ({
  inlineDatumPayloadBytes,
  minimumCompleteItemBytes,
}: {
  readonly inlineDatumPayloadBytes: number;
  readonly minimumCompleteItemBytes: number;
}): Promise<RouteFreedomJourney> => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const {
    emulator,
    operator,
    challenger,
    operatorLucid,
    challengerLucid,
    operatorSigner,
    challengerSigner,
    validityRange,
  } = await createValidationDisputeParties();
  // #622: every staged lifecycle stage is measured through the same
  // `captureEmulatorSubmission` seam the semantic leg already uses, so the
  // measurement campaign reads per-stage complete signed bytes and execution
  // units off the same journeys the #621 suites drive — capture only, no
  // transaction is shaped by it.
  const lifecycleMeasurements: CapturedLifecycleStage[] = [];
  const runCapturedLifecycleStage = async <T>(
    label: string,
    operation: () => Promise<T>,
  ): Promise<T> => {
    const captured = await runEmulatorLifecycleStage(label, () =>
      captureEmulatorSubmission(emulator, operation),
    );
    lifecycleMeasurements.push({
      label,
      measurements: captured.measurements,
    });
    return captured.result;
  };

  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const { nonceUtxo, referenceScriptAuth, referenceScriptPublisher } =
    await createReferenceScriptPublisher(operatorLucid, emulator.now());
  const baseContracts = {
    ...(await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      {
        referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
        realValidationTraceDispute: true,
        alwaysFraudProofCatalogue: true,
      },
    )),
    referenceScriptAuth,
    referenceScriptPublisher,
  };
  // Operator registration and activation source their four directory
  // validators from published reference scripts, so the roster has to exist
  // before the setup transaction samples the header clock.
  const contracts = {
    ...baseContracts,
    operatorLifecycleReferenceScripts:
      await publishOperatorLifecycleReferenceScripts({
        lucid: challengerLucid,
        contracts: baseContracts,
      }),
  };
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: challengerLucid,
      realBlueprint,
      computationThreadMintingScript: contracts.computationThread.mintingScript,
      fraudProofMintingScript: contracts.fraudProof.mintingScript,
    });
  const validationDisputeSdkContracts = await Effect.runPromise(
    buildValidationTraceDisputeFaultProofContracts({
      blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
      network,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      referenceScriptAuthPolicyId: contracts.referenceScriptAuth.policyId,
    }),
  );
  const itemSemanticContract =
    validationDisputeSdkContracts.validationTraceDispute.semanticResolvers[1];
  const itemObserveContract =
    validationDisputeSdkContracts.validationTraceDispute
      .canonicalDecodeItemStages.observe;
  const canonicalDecodePrepareContract =
    validationDisputeSdkContracts.validationTraceDispute.prepareResolvers[0];
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const operatorPaymentCredential = getAddressDetails(
    await operatorLucid.wallet().address(),
  ).paymentCredential;
  if (
    operatorPaymentCredential === undefined ||
    operatorPaymentCredential.type !== "Key"
  ) {
    throw new Error("Expected operator wallet to expose a payment key hash");
  }
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(
      operatorLucid,
      emulator.now() + 120_000,
    ) - 1;
  const fixture = await buildInvalidForcedValidationDisputeFixture({
    operatorVkey: operatorPaymentCredential.hash,
    now: headerStartTime,
    inlineDatumPayloadBytes,
    minimumCompleteItemBytes,
  });
  const setup = await runCapturedLifecycleStage("setup", () =>
    submitSetupTx({
      lucid: operatorLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fixture.header,
    }),
  );
  const {
    referenceScriptPublisherLucid,
    validationDisputePublication,
    validationDisputeControlPublications,
  } = await stageAuthenticatedValidationDisputePublication({
    emulator,
    operatorLucid,
    operatorSeedPhrase: challenger.seedPhrase,
    contracts,
    authPolicy: referenceScriptAuth,
    publisher: referenceScriptPublisher,
    runStage: runCapturedLifecycleStage,
  });
  const publishPlain = (label: string, script: Script) =>
    withRealL1MaxTxSize(emulator, () =>
      publishPlainReferenceScriptUtxo({
        lucid: referenceScriptPublisherLucid,
        script,
        label,
      }),
    );
  const itemSemanticPublication = await runCapturedLifecycleStage(
    "reference-script.publish-item-semantic",
    () =>
      publishPlain(
        "validation item-semantic",
        itemSemanticContract.spendingScript,
      ),
  );
  const itemObservePublication = await runCapturedLifecycleStage(
    "reference-script.publish-item-observe",
    () =>
      publishPlain(
        "validation item-observe",
        itemObserveContract.spendingScript,
      ),
  );
  const canonicalDecodePreparePublication = await runCapturedLifecycleStage(
    "reference-script.publish-canonical-decode-prepare",
    () =>
      publishPlain(
        "validation canonical-decode prepare",
        canonicalDecodePrepareContract.spendingScript,
      ),
  );
  const canonicalDecodeItemStages =
    validationDisputeSdkContracts.validationTraceDispute
      .canonicalDecodeItemStages;
  const canonicalDecodeStageReferenceScriptUtxos = {
    canonicalDecodeItemSource: (
      await runCapturedLifecycleStage(
        "reference-script.publish-canonical-decode-item-source",
        () =>
          publishPlain(
            "validation canonical-decode item source",
            canonicalDecodeItemStages.source.spendingScript,
          ),
      )
    ).utxo,
    canonicalDecodeItemProof: (
      await runCapturedLifecycleStage(
        "reference-script.publish-canonical-decode-item-proof",
        () =>
          publishPlain(
            "validation canonical-decode item proof",
            canonicalDecodeItemStages.proof.spendingScript,
          ),
      )
    ).utxo,
    canonicalDecodeItemSettlement: (
      await runCapturedLifecycleStage(
        "reference-script.publish-canonical-decode-item-settlement",
        () =>
          publishPlain(
            "validation canonical-decode item settlement",
            canonicalDecodeItemStages.settlement.spendingScript,
          ),
      )
    ).utxo,
  };
  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
    validationDisputePublication,
    validationItemSemanticReference: {
      scriptHash: itemSemanticContract.spendingScriptHash,
      utxo: itemSemanticPublication.utxo,
    },
    validationItemObserveReference: {
      scriptHash: itemObserveContract.spendingScriptHash,
      utxo: itemObservePublication.utxo,
    },
    validationCanonicalDecodePrepareReference: {
      scriptHash: canonicalDecodePrepareContract.spendingScriptHash,
      utxo: canonicalDecodePreparePublication.utxo,
    },
  });
  const initResult = await runCapturedLifecycleStage("init", () =>
    submitInit({
      lucid: challengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      fraudCategory: "validationTraceDispute",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts,
      awaitConfirmation: true,
    }),
  );
  const { targetOperatorLucid, targetChallengerLucid } =
    await createRealL1TargetLucids({
      emulator,
      sourceLucid: challengerLucid,
      operatorSeedPhrase: operator.seedPhrase,
      challengerSeedPhrase: challenger.seedPhrase,
    });
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    targetChallengerLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const openResult = await runCapturedLifecycleStage("open", () =>
    submitValidationDisputeOpen({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      claim: fixture.claim,
      challengerDescriptor: fixture.challengerDescriptor,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );
  const sourceResult = await runCapturedLifecycleStage("source", () =>
    submitValidationDisputeVerifySource({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: openResult.nextThreadOutRef,
      sourceReferenceScriptUtxo:
        validationDisputeControlPublications.source.utxo,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );

  let threadOutRef = sourceResult.nextThreadOutRef;
  for (const move of fixture.evidence.moves) {
    const revealResult = await runCapturedLifecycleStage(
      `reveal.${move.role}`,
      () =>
        submitValidationDisputeReveal({
          lucid:
            move.role === "operator"
              ? targetOperatorLucid
              : targetChallengerLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: move.role === "operator" ? operatorSigner : challengerSigner,
          threadOutRef,
          role: move.role,
          proof: move.proof,
          gameReferenceScriptUtxo:
            validationDisputeControlPublications.game.utxo,
          validityRange: validityRange(),
          awaitConfirmation: true,
        }),
    );
    threadOutRef = revealResult.nextThreadOutRef;
  }

  const resolutionResult = await runCapturedLifecycleStage(
    "enter-resolution",
    () =>
      submitValidationDisputeEnterResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef,
        gameReferenceScriptUtxo: validationDisputeControlPublications.game.utxo,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  const { lowIndex, highIndex } = fixture.evidence.finalDispute;
  const prepareResult = await runCapturedLifecycleStage(
    "prepare-resolution",
    () =>
      submitValidationDisputePrepareResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: resolutionResult.nextThreadOutRef,
        preState: validationMachineStateDataFromCore(
          fixture.operatorTrace.states[lowIndex]!,
        ),
        operatorPost: validationTraceProofDataFromCore(
          fixture.operatorTrace.tree.proofs[highIndex]!,
        ),
        challengerPost: validationTraceProofDataFromCore(
          fixture.challengerTrace.tree.proofs[highIndex]!,
        ),
        boundaryReferenceScriptUtxo:
          validationDisputeControlPublications.boundary.utxo,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  const selectedResult = await runCapturedLifecycleStage(
    "prepare-selected",
    () =>
      submitValidationDisputePrepareSelected({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: prepareResult.nextThreadOutRef,
        oneStepArgument: fixture.evidence.oneStepArgument,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );

  const stagedThreadOutRef = selectedResult.nextThreadOutRef;
  const parseOutRefLabel = (label: string) => {
    const [txHash, outputIndex] = label.split("#");
    if (txHash === undefined || outputIndex === undefined) {
      throw new Error(`malformed out-ref label "${label}"`);
    }
    return { txHash, outputIndex: Number(outputIndex) };
  };

  return {
    emulator,
    realBlueprint,
    challengerLucid: targetChallengerLucid,
    stagedThreadOutRef,
    validityRange,
    completeItemBytes: fixture.completeItemBytes,
    lifecycleMeasurements,
    submitSemanticResolution: (routing = {}) =>
      runEmulatorLifecycleStage("semantic-resolution", () =>
        captureEmulatorSubmission(emulator, () =>
          submitValidationDisputeSemanticResolution({
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef: stagedThreadOutRef,
            oneStepArgument: fixture.evidence.oneStepArgument,
            stageReferenceScriptUtxos: canonicalDecodeStageReferenceScriptUtxos,
            validityRange: validityRange(),
            awaitConfirmation: true,
            ...routing,
          }),
        ),
      ),
    submitAward: (awardThreadOutRef: string) =>
      runEmulatorLifecycleStage("award", () =>
        captureEmulatorSubmission(emulator, () =>
          submitValidationDisputeAward({
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef: awardThreadOutRef,
            awardReferenceScriptUtxo:
              validationDisputeControlPublications.award.utxo,
            witnessReferenceScripts,
            validityRange: validityRange(),
            awaitConfirmation: true,
          }),
        ),
      ),
    expectStagedThreadUnspent: async () => {
      const { txHash, outputIndex } = parseOutRefLabel(stagedThreadOutRef);
      const live = await targetChallengerLucid.utxosByOutRef([
        { txHash, outputIndex },
      ]);
      expect(
        live,
        "a refused semantic-resolution attempt must leave the staged thread unspent",
      ).toHaveLength(1);
    },
    // The prepare-selected step consumed the prepare-resolution thread output,
    // so this out-ref existed on this very ledger and is now spent — the
    // honest shape of a publication another dispute consumed first.
    spentOutRef: selectedResult.threadOutRef,
  };
};
