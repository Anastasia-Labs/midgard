import { outRefLabel } from "@al-ft/midgard-core";
import {
  validationMachineStateDataFromCore,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  getAddressDetails,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";

import {
  resolveValidationTraceDisputeDeploymentContracts,
  submitRemoveFraudulentBlock,
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  submitValidationDisputeVerifySource,
  VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX_V1,
  validationSemanticResolverGlobalIndexV1,
  validationValueAndMintSemanticReferenceScriptDeploymentEntryV1,
} from "../../../src/index.js";
import { submitInit } from "../legacy-submit-emulator.js";
import {
  alwaysSucceedsBlueprintPath,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./blueprints.js";
import { buildCatalogueDeploymentInfo } from "./catalogue.js";
import { buildMinimalFaultProofContracts } from "./contracts.js";
import {
  createRealL1TargetLucids,
  createValidationDisputeParties,
  stageAuthenticatedValidationDisputePublication,
  withRealL1MaxTxSize,
} from "./dispute-staging.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  expectSingleUtxoWithUnit,
  registerPhasMembershipRewardAccount,
  runEmulatorLifecycleStage,
} from "./emulator-context.js";
import {
  attributeTransactionBytes,
  captureEmulatorSubmission,
  midgardScriptHashNames,
} from "./measurement.js";
import {
  publishFaultProofWitnessReferenceScriptsV1,
  publishOperatorLifecycleReferenceScriptsV1,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./removal-deployment.js";
import { submitSetupTx } from "./setup-tx.js";
import { type ForcedValidationDisputeFixture } from "./validation-dispute-fixtures.js";

/**
 * VM-DEFECT-2 dispute-level regression
 * (`docs/exec-plans/evidence/vm-defect-decision-memo.md` §2).
 *
 * The shipped defect made `rejected_successor_is_exact` demand that the
 * rejecting terminal *write* `ledger_delta_root = frontier_commitment(0, [])`
 * while `immutable_context_matches` pins that same field pre == post on every
 * transition. The two are jointly unsatisfiable from any pre-state whose
 * claimed delta is non-empty -- which is every adversarially interesting
 * pre-state, because the challenger is the party who must exhibit a
 * one-step-valid rejection successor to win
 * (`validation-resolver-v1.ak` -> `challenger_wins_with_valid_successor`) and
 * a real invalid transaction always claims a non-empty delta.
 *
 * It shipped because no test ever drove a challenger to an actual win: every
 * rejection fixture pinned the claimed delta to the empty commitment, the one
 * pre-state in which the contradiction vanishes. These tests close that gap
 * end to end on the emulator, against the compiled validators, in both
 * directions (GOAL_SPEC §3 invariant 9 -- soundness is symmetric).
 */
export const runForcedValidationDisputeScenario = async (
  buildFixture: (input: {
    readonly operatorVkey: string;
    readonly now: number;
  }) => Promise<ForcedValidationDisputeFixture>,
  {
    stopAfter,
    onRemovalReferenceScriptPublicationAttempt,
  }: {
    readonly stopAfter?:
      | "prepare-resolution"
      | "prepare-selected"
      | "semantic-resolution";
    readonly onRemovalReferenceScriptPublicationAttempt?: () => void;
  } = {},
) => {
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

  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a nonce UTxO");
  }
  const baseContracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    {
      realValidationTraceDispute: true,
      alwaysFraudProofCatalogue: true,
    },
  );
  // Operator registration and activation source their four directory
  // validators from published reference scripts, so the roster has to exist
  // before the setup transaction samples the header clock.
  const contracts = {
    ...baseContracts,
    operatorLifecycleReferenceScripts:
      await publishOperatorLifecycleReferenceScriptsV1({
        lucid: challengerLucid,
        contracts: baseContracts,
      }),
  };
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScriptsV1({
      lucid: challengerLucid,
      realBlueprint,
      computationThreadMintingScript: contracts.computationThread.mintingScript,
      fraudProofMintingScript: contracts.fraudProof.mintingScript,
    });
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
  const fixture = await buildFixture({
    operatorVkey: operatorPaymentCredential.hash,
    now: headerStartTime,
  });
  const setup = await runEmulatorLifecycleStage("setup", () =>
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
    operatorSeedPhrase: operator.seedPhrase,
    contracts,
    runStage: runEmulatorLifecycleStage,
  });
  const prepareResolverContract =
    contracts.fraudProofContracts.validationTraceDispute.prepareResolvers[
      fixture.evidence.oneStepArgument.resolverIndex
    ];
  if (prepareResolverContract === undefined) {
    throw new Error("Selected validation prepare resolver is not deployed");
  }
  const prepareResolverPublication = await runEmulatorLifecycleStage(
    "reference-script.publish-prepare-resolver",
    () =>
      withRealL1MaxTxSize(emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: referenceScriptPublisherLucid,
          script: prepareResolverContract.spendingScript,
          label: "validation prepare resolver",
        }),
      ),
  );
  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
    validationDisputePublication,
  });
  const initResult = await runEmulatorLifecycleStage("init", () =>
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
  const {
    functionalProtocolParameters,
    functionalSlotConfig,
    targetOperatorLucid,
    targetChallengerLucid,
  } = await createRealL1TargetLucids({
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
  const openResult = await runEmulatorLifecycleStage("open", () =>
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
  const sourceResult = await runEmulatorLifecycleStage("source", () =>
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
    const revealResult = await runEmulatorLifecycleStage(
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

  const resolutionResult = await runEmulatorLifecycleStage(
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
  const prepareResult = await runEmulatorLifecycleStage(
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
  if (stopAfter === "prepare-resolution") {
    return { fixture, contracts, initResult, lowIndex, highIndex };
  }
  const selectedResult = await runEmulatorLifecycleStage(
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
        referenceScriptUtxo: prepareResolverPublication.utxo,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  if (stopAfter === "prepare-selected") {
    return { fixture, contracts, initResult, lowIndex, highIndex };
  }
  // Publish the exact selected semantic resolver once. Oversized ValueAndMint
  // bodies use the deployment-time host limit; all other semantic bodies are
  // published inside the real L1 envelope.
  const stagedResolverIndex = fixture.evidence.oneStepArgument.resolverIndex;
  const stagedSemanticIndex =
    fixture.evidence.oneStepArgument.semanticResolverIndex;
  // Resolved through the very helper the submit path uses, so the published
  // body is byte-identical to the one the resolution will hash-check.
  const semanticContract = (
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint: realBlueprint,
      deploymentInfo,
      network,
    })
  ).contracts.validationTraceDispute.semanticResolvers[
    validationSemanticResolverGlobalIndexV1(
      stagedResolverIndex,
      stagedSemanticIndex,
    )
  ];
  if (semanticContract === undefined) {
    throw new Error("Selected validation semantic resolver is not deployed");
  }
  const valueAndMintSemanticContract =
    stagedResolverIndex === VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX_V1
      ? semanticContract
      : undefined;
  const valueAndMintSemanticEntryName =
    valueAndMintSemanticContract === undefined
      ? undefined
      : validationValueAndMintSemanticReferenceScriptDeploymentEntryV1(
          stagedSemanticIndex,
        );
  const semanticIsOversized =
    semanticContract.spendingScript.script.length / 2 >
    PROTOCOL_PARAMETERS_DEFAULT.maxTxSize;
  const semanticPublication = await runEmulatorLifecycleStage(
    `reference-script.publish.${
      valueAndMintSemanticEntryName ??
      `validationSemanticResolver${validationSemanticResolverGlobalIndexV1(
        stagedResolverIndex,
        stagedSemanticIndex,
      ).toString()}`
    }`,
    async () => {
      if (semanticIsOversized) {
        const prePublicationProtocolParameters = emulator.protocolParameters;
        emulator.protocolParameters = functionalProtocolParameters;
        try {
          const oversizedPublisherLucid = await Lucid(emulator, "Custom", {
            slotConfig: functionalSlotConfig,
          });
          oversizedPublisherLucid.selectWallet.fromSeed(operator.seedPhrase);
          return await publishPlainReferenceScriptUtxo({
            lucid: oversizedPublisherLucid,
            script: semanticContract.spendingScript,
            label:
              valueAndMintSemanticEntryName ?? "validation semantic resolver",
            oversized: true,
          });
        } finally {
          emulator.protocolParameters = prePublicationProtocolParameters;
        }
      }
      return await withRealL1MaxTxSize(emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: referenceScriptPublisherLucid,
          script: semanticContract.spendingScript,
          label:
            valueAndMintSemanticEntryName ?? "validation semantic resolver",
        }),
      );
    },
  );
  const valueAndMintSemanticPublication =
    valueAndMintSemanticContract !== undefined && semanticIsOversized
      ? semanticPublication
      : undefined;
  const semanticDeploymentInfo =
    valueAndMintSemanticPublication === undefined
      ? deploymentInfo
      : buildRemovalDeploymentInfo(contracts, catalogue, {
          validationDisputePublication,
          validationValueAndMintSemanticReferences: [
            {
              semanticResolverIndex: stagedSemanticIndex,
              scriptHash: valueAndMintSemanticContract!.spendingScriptHash,
              utxo: valueAndMintSemanticPublication.utxo,
            },
          ],
        });
  const semanticCapture = await captureEmulatorSubmission(emulator, () =>
    runEmulatorLifecycleStage("semantic-resolution", () =>
      submitValidationDisputeSemanticResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo: semanticDeploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: selectedResult.nextThreadOutRef,
        oneStepArgument: fixture.evidence.oneStepArgument,
        referenceScriptUtxo: semanticPublication.utxo,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
    ),
  );
  const semanticResult = semanticCapture.result;
  if (stopAfter === "semantic-resolution") {
    return {
      fixture,
      contracts,
      initResult,
      lowIndex,
      highIndex,
      semanticResult,
      semanticMeasurement: semanticCapture.measurement,
      valueAndMintSemanticReferencePublication:
        valueAndMintSemanticPublication === undefined ||
        valueAndMintSemanticContract === undefined ||
        valueAndMintSemanticEntryName === undefined
          ? undefined
          : {
              entryName: valueAndMintSemanticEntryName,
              appliedResolverBytes:
                valueAndMintSemanticContract.spendingScript.script.length / 2,
              appliedResolverHash:
                valueAndMintSemanticContract.spendingScriptHash,
              utxo: valueAndMintSemanticPublication.utxo,
              publicationMeasurement:
                valueAndMintSemanticPublication.publicationMeasurement,
            },
    };
  }
  const awardResult = await runEmulatorLifecycleStage("award", () =>
    submitValidationDisputeAward({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: semanticResult.nextThreadOutRef,
      awardReferenceScriptUtxo: validationDisputeControlPublications.award.utxo,
      witnessReferenceScripts,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );
  // Block removal needs the state-queue, operator-directory and scheduler
  // validators. Publishing them as reference-script UTxOs is what the deployed
  // node does; `publishPlainReferenceScriptUtxo` refuses any publication that
  // does not itself fit the literal 16,384-byte L1 envelope, so this also
  // proves each of these validators is publishable on L1. Defer the eight
  // submissions until the route has actually reached removal so validation-only
  // and negative scenarios do not mutate the emulator first.
  //
  // The stage no longer runs under `withRealL1MaxTxSize`: the 12-parameter
  // `state_queue.mint` compiles to 16,835 bytes, past the 16,384-byte L1
  // envelope, so its publication cannot be built against the real limit at
  // all. This raises the stage's publication budget to the same raised
  // deployment-time parameters the R5 semantic resolvers publish under, and
  // `publishRemovalReferenceScripts` marks that one entry `oversized` so its
  // measurement is returned unasserted. The other seven keep their real
  // envelope check, which lives in `publishPlainReferenceScriptUtxo` itself
  // rather than in the emulator pin. The stage-level real-envelope pin is
  // suspended until the validator shrinks; deployability of the oversized
  // script on real L1 parameters is tracked in Anastasia-Labs/midgard#649.
  const removalReferenceScriptPublications = await runEmulatorLifecycleStage(
    "reference-script.publish-removal",
    async () => {
      onRemovalReferenceScriptPublicationAttempt?.();
      const prePublicationProtocolParameters = emulator.protocolParameters;
      emulator.protocolParameters = functionalProtocolParameters;
      try {
        const oversizedPublisherLucid = await Lucid(emulator, "Custom", {
          slotConfig: functionalSlotConfig,
        });
        oversizedPublisherLucid.selectWallet.fromSeed(operator.seedPhrase);
        return await publishRemovalReferenceScripts({
          lucid: oversizedPublisherLucid,
          contracts,
        });
      } finally {
        emulator.protocolParameters = prePublicationProtocolParameters;
      }
    },
  );
  const removalDeploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    {
      validationDisputePublication,
      removalReferenceScripts: removalReferenceScriptPublications.published,
    },
  );
  const removeNow = BigInt(emulator.now());
  // Block removal runs under the same literal 16,384-byte L1 envelope as every
  // dispute transaction above: `targetChallengerLucid` was constructed with
  // `maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize`, and every validator the
  // correction needs is sourced from a published reference-script UTxO instead
  // of being attached inline. Attaching them instead costs 35,634 bytes of
  // witness set and puts the correction 2.3x over the limit.
  const removalCapture = await captureEmulatorSubmission(emulator, () =>
    runEmulatorLifecycleStage("remove-fraudulent-block", () =>
      submitRemoveFraudulentBlock({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo: removalDeploymentInfo,
        network,
        signer: challengerSigner,
        fraudCategory: "validationTraceDispute",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    ),
  );
  const removal = removalCapture.result;
  if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
    const removalScriptNames = midgardScriptHashNames(contracts);
    removalCapture.transactionCbors.forEach((cbor, index) => {
      attributeTransactionBytes(
        `remove-fraudulent-block tx[${index.toString()}]`,
        cbor,
        removalScriptNames,
      );
    });
  }
  return {
    fixture,
    contracts,
    initResult,
    lowIndex,
    highIndex,
    awardResult,
    removal,
    removalMeasurements: removalCapture.measurements,
    removalReferenceScriptMeasurements:
      removalReferenceScriptPublications.measurements,
    challengerLucid: targetChallengerLucid,
    setup,
  };
};
