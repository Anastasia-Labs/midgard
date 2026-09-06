import { outRefLabel } from "@al-ft/midgard-core";
import {
  CEK_CORE_STAGE_REFERENCES,
  CEK_MATERIAL_TASK_YIELD_ROLES,
  CEK_SELECTION_YIELD_ROLES,
  createReferenceScriptAuthPolicy,
  validationMachineStateDataFromCore,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  CML,
  getAddressDetails,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";

import {
  cancelValidationCekCore,
  cancelValidationCekMaterialTraversal,
  cancelValidationSemanticResolution,
  resolveValidationTraceDisputeDeploymentContracts,
  resumeValidationCekCore,
  resumeValidationCekMaterialTraversal,
  submitRemoveFraudulentBlock,
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  type SubmitValidationDisputeSemanticResolutionResult,
  submitValidationDisputeVerifySource,
  VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX,
  validationPhaseASemanticReferenceScriptDeploymentEntry,
  validationScriptSourcesSemanticReferenceScriptDeploymentEntry,
  validationSemanticResolverGlobalIndex,
  validationValueAndMintSemanticReferenceScriptDeploymentEntry,
} from "../../../src/index.js";
import { SCRIPT_SOURCES_MIDDLE_YIELD_ROLES } from "../../../src/validation-dispute/script-sources-yields.js";
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
  measureCompleteSignedTransaction,
  midgardScriptHashNames,
} from "./measurement.js";
import {
  PHASE_A_ITEM_YIELD_SPECS,
  preparePhaseAItemCarriage,
} from "./phase-a-item-carriage.js";
import {
  publishAuthenticatedValidationDisputeControl,
  publishFaultProofWitnessReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
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
    readonly prepareFieldCarriage?: NonNullable<
      Parameters<
        typeof import("./validation-dispute-fixtures.js").buildForgedOperatorSuccessorValidationDisputeFixture
      >[0]["prepareFieldCarriage"]
    >;
  }) => Promise<ForcedValidationDisputeFixture>,
  {
    stopAfter,
    cekMaterialTraversalBatchSize,
    phaseANativeItemYieldKind,
    phaseANativeItemMaximum = false,
    phaseAObserverItemMaximum = false,
    cancelPreparedSemantic = false,
    restartCekMaterialTraversal = false,
    restartCekCore = false,
    cancelCekCore = false,
    cancelCekMaterialTraversal = false,
    onRemovalReferenceScriptPublicationAttempt,
    onSubmittedTransaction,
  }: {
    readonly cekMaterialTraversalBatchSize?: number;
    readonly phaseANativeItemYieldKind?: "native" | "foreign";
    readonly phaseANativeItemMaximum?: boolean;
    readonly phaseAObserverItemMaximum?: boolean;
    readonly cancelPreparedSemantic?: boolean;
    readonly restartCekMaterialTraversal?: boolean;
    readonly restartCekCore?: boolean;
    readonly cancelCekCore?: boolean;
    readonly cancelCekMaterialTraversal?: boolean;
    readonly stopAfter?:
      | "prepare-resolution"
      | "prepare-selected"
      | "semantic-resolution";
    readonly onRemovalReferenceScriptPublicationAttempt?: () => void;
    readonly onSubmittedTransaction?: (
      measurement: ReturnType<typeof measureCompleteSignedTransaction>,
      transactionCbor: string,
    ) => void;
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

  if (onSubmittedTransaction !== undefined) {
    const submit = emulator.submitTx.bind(emulator);
    emulator.submitTx = async (transaction) => {
      const hash = await submit(transaction);
      onSubmittedTransaction(
        measureCompleteSignedTransaction(transaction),
        transaction,
      );
      return hash;
    };
  }
  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a nonce UTxO");
  }
  const referenceScriptAuth = createReferenceScriptAuthPolicy(
    challengerLucid,
    emulator.now(),
  );
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
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
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
      emulator.now() +
        120_000 +
        (phaseANativeItemMaximum || phaseAObserverItemMaximum ? 8 * 20_000 : 0),
    ) - 1;
  let phaseAItemCarriage:
    | Awaited<ReturnType<typeof preparePhaseAItemCarriage>>
    | undefined;
  const fixture = await buildFixture({
    ...(phaseANativeItemMaximum || phaseAObserverItemMaximum
      ? {
          prepareFieldCarriage: async (
            input: Pick<
              Parameters<typeof preparePhaseAItemCarriage>[0],
              "trace" | "stateIndex" | "source"
            >,
          ) => {
            phaseAItemCarriage = await preparePhaseAItemCarriage({
              ...input,
              lucid: challengerLucid,
              signer: challengerSigner,
              chain: contracts.fraudProofContracts.validationTraceDispute,
              certificate: contracts.fieldPreimageCertificate,
              authPolicy: referenceScriptAuth,
              kind: phaseAObserverItemMaximum
                ? "observer"
                : (phaseANativeItemYieldKind ?? "native"),
            });
            return phaseAItemCarriage.resolveFieldCarriage;
          },
        }
      : {}),
    operatorVkey: operatorPaymentCredential.hash,
    now: headerStartTime,
  });
  emulator.awaitSlot(
    Math.max(0, Math.ceil((headerStartTime - 120_000 - emulator.now()) / 1000)),
  );
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
    operatorSeedPhrase: challenger.seedPhrase,
    contracts,
    authPolicy: referenceScriptAuth,
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
  // Publish the exact selected semantic resolver once. An over-limit body is
  // not a runnable lifecycle: it must be physically split before this harness
  // can claim production-fit acceptance.
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
    validationSemanticResolverGlobalIndex(
      stagedResolverIndex,
      stagedSemanticIndex,
    )
  ];
  if (semanticContract === undefined) {
    throw new Error("Selected validation semantic resolver is not deployed");
  }
  const valueAndMintSemanticContract =
    stagedResolverIndex === VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX
      ? semanticContract
      : undefined;
  const valueAndMintSemanticEntryName =
    valueAndMintSemanticContract === undefined
      ? undefined
      : validationValueAndMintSemanticReferenceScriptDeploymentEntry(
          stagedSemanticIndex,
        );
  const semanticIsOversized =
    semanticContract.spendingScript.script.length / 2 >
    PROTOCOL_PARAMETERS_DEFAULT.maxTxSize;
  if (semanticIsOversized) {
    throw new Error(
      `validation semantic resolver ${validationSemanticResolverGlobalIndex(
        stagedResolverIndex,
        stagedSemanticIndex,
      ).toString()} is unpublishable under the Van Rossem maxTxSize; lifecycle excluded until the resolver is split`,
    );
  }
  const semanticPublication =
    phaseAItemCarriage?.semanticPublication ??
    (await runEmulatorLifecycleStage(
      `reference-script.publish.${
        valueAndMintSemanticEntryName ??
        `validationSemanticResolver${validationSemanticResolverGlobalIndex(
          stagedResolverIndex,
          stagedSemanticIndex,
        ).toString()}`
      }`,
      async () =>
        await withRealL1MaxTxSize(emulator, () =>
          publishPlainReferenceScriptUtxo({
            lucid: referenceScriptPublisherLucid,
            script: semanticContract.spendingScript,
            label:
              valueAndMintSemanticEntryName ?? "validation semantic resolver",
          }),
        ),
    ));
  if (cancelPreparedSemantic) {
    const cancellation = await cancelValidationSemanticResolution({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: selectedResult.nextThreadOutRef,
      referenceScriptUtxo: semanticPublication.utxo,
      witnessReferenceScripts,
    });
    return {
      fixture,
      contracts,
      initResult,
      lowIndex,
      highIndex,
      cancellation,
    };
  }
  const valueAndMintSemanticPublication =
    valueAndMintSemanticContract !== undefined
      ? semanticPublication
      : undefined;
  const assetFoldYield =
    stagedResolverIndex === 12 && [3, 6, 8].includes(stagedSemanticIndex)
      ? contracts.fraudProofContracts.validationTraceDispute.yields
          .valueAndMintAssetFold
      : undefined;
  const assetFoldPublication =
    assetFoldYield === undefined
      ? undefined
      : await publishAuthenticatedValidationDisputeControl({
          lucid: challengerLucid,
          authPolicy: referenceScriptAuth,
          target: {
            control: "value-and-mint asset-fold",
            name: "V1 validation-trace value-and-mint asset-fold yield",
            script: assetFoldYield.withdrawalScript,
          },
        });
  const baseSemanticDeploymentInfo =
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
  let semanticDeploymentInfo =
    assetFoldPublication === undefined || assetFoldYield === undefined
      ? baseSemanticDeploymentInfo
      : {
          ...baseSemanticDeploymentInfo,
          contracts: {
            ...baseSemanticDeploymentInfo.contracts,
            validationTraceDisputeValueAndMintAssetFoldWithdraw: {
              scriptHash: assetFoldYield.withdrawalScriptHash,
              refScriptUTxO: {
                txHash: assetFoldPublication.utxo.txHash,
                outputIndex: assetFoldPublication.utxo.outputIndex,
              },
            },
          },
        };
  const publishedSemanticEntry =
    validationPhaseASemanticReferenceScriptDeploymentEntry(
      stagedResolverIndex,
      stagedSemanticIndex,
    ) ??
    validationScriptSourcesSemanticReferenceScriptDeploymentEntry(
      stagedResolverIndex,
      stagedSemanticIndex,
    );
  if (publishedSemanticEntry !== undefined) {
    semanticDeploymentInfo = {
      ...semanticDeploymentInfo,
      contracts: {
        ...semanticDeploymentInfo.contracts,
        [publishedSemanticEntry]: {
          scriptHash: semanticContract.spendingScriptHash,
          refScriptUTxO: {
            txHash: semanticPublication.utxo.txHash,
            outputIndex: semanticPublication.utxo.outputIndex,
          },
        },
      },
    };
  }
  if (stagedResolverIndex === 5 && stagedSemanticIndex === 1) {
    for (const spec of PHASE_A_ITEM_YIELD_SPECS) {
      const contract =
        contracts.fraudProofContracts.validationTraceDispute.yields[
          spec.contract
        ];
      const publication =
        phaseAItemCarriage?.yields.find(
          (entry) => entry.spec.contract === spec.contract,
        )?.publication ??
        (await publishAuthenticatedValidationDisputeControl({
          lucid: challengerLucid,
          authPolicy: referenceScriptAuth,
          target: {
            control: spec.contract,
            name: spec.role,
            script: contract.withdrawalScript,
          },
        }));
      semanticDeploymentInfo = {
        ...semanticDeploymentInfo,
        contracts: {
          ...semanticDeploymentInfo.contracts,
          [spec.deployment]: {
            scriptHash: contract.withdrawalScriptHash,
            refScriptUTxO: {
              txHash: publication.utxo.txHash,
              outputIndex: publication.utxo.outputIndex,
            },
          },
        },
      };
    }
  }
  if (stagedResolverIndex === 8 && stagedSemanticIndex === 0) {
    for (const spec of SCRIPT_SOURCES_MIDDLE_YIELD_ROLES) {
      const contract =
        contracts.fraudProofContracts.validationTraceDispute.yields[
          spec.contract
        ];
      const publication = await publishAuthenticatedValidationDisputeControl({
        lucid: challengerLucid,
        authPolicy: referenceScriptAuth,
        target: {
          control: spec.contract,
          name: spec.role,
          script: contract.withdrawalScript,
        },
      });
      semanticDeploymentInfo = {
        ...semanticDeploymentInfo,
        contracts: {
          ...semanticDeploymentInfo.contracts,
          [spec.deployment]: {
            scriptHash: contract.withdrawalScriptHash,
            refScriptUTxO: {
              txHash: publication.utxo.txHash,
              outputIndex: publication.utxo.outputIndex,
            },
          },
        },
      };
    }
  }
  if (stagedResolverIndex === 11 && stagedSemanticIndex === 1) {
    for (const spec of [
      ...CEK_SELECTION_YIELD_ROLES,
      ...CEK_MATERIAL_TASK_YIELD_ROLES,
    ]) {
      const contract =
        contracts.fraudProofContracts.validationTraceDispute.yields[
          spec.contract
        ];
      const publication = await publishAuthenticatedValidationDisputeControl({
        lucid: challengerLucid,
        authPolicy: referenceScriptAuth,
        target: {
          control: spec.contract,
          name: spec.role,
          script: contract.withdrawalScript,
        },
      });
      semanticDeploymentInfo = {
        ...semanticDeploymentInfo,
        contracts: {
          ...semanticDeploymentInfo.contracts,
          [spec.deployment]: {
            scriptHash: contract.withdrawalScriptHash,
            refScriptUTxO: {
              txHash: publication.utxo.txHash,
              outputIndex: publication.utxo.outputIndex,
            },
          },
        },
      };
    }
  }
  if (stagedResolverIndex === 11 && stagedSemanticIndex === 3) {
    for (const [key, spec] of Object.entries(CEK_CORE_STAGE_REFERENCES)) {
      const contract =
        contracts.fraudProofContracts.validationTraceDispute.cekCoreStages[
          key as keyof typeof CEK_CORE_STAGE_REFERENCES
        ];
      const publication = await publishAuthenticatedValidationDisputeControl({
        lucid: challengerLucid,
        authPolicy: referenceScriptAuth,
        target: {
          control: `CEK core ${key}`,
          name: spec.role,
          script: contract.spendingScript,
        },
      });
      semanticDeploymentInfo = {
        ...semanticDeploymentInfo,
        contracts: {
          ...semanticDeploymentInfo.contracts,
          [spec.deployment]: {
            scriptHash: contract.spendingScriptHash,
            refScriptUTxO: {
              txHash: publication.utxo.txHash,
              outputIndex: publication.utxo.outputIndex,
            },
          },
        },
      };
    }
  }
  if (stagedResolverIndex === 11 && stagedSemanticIndex === 1) {
    const contract =
      contracts.fraudProofContracts.validationTraceDispute.cekMaterialTraversal;
    const publication = await publishAuthenticatedValidationDisputeControl({
      lucid: challengerLucid,
      authPolicy: referenceScriptAuth,
      target: {
        control: "CEK material traversal",
        name: "V1 validation-trace CEK material traversal",
        script: contract.spendingScript,
      },
    });
    semanticDeploymentInfo = {
      ...semanticDeploymentInfo,
      contracts: {
        ...semanticDeploymentInfo.contracts,
        validationTraceDisputeCekMaterialTraversal: {
          scriptHash: contract.spendingScriptHash,
          refScriptUTxO: {
            txHash: publication.utxo.txHash,
            outputIndex: publication.utxo.outputIndex,
          },
        },
      },
    };
  }
  let checkpointJson: string | undefined;
  let coreCheckpointJson: string | undefined;
  let traversalOutputs = 0;
  const submitBeforeRestart = emulator.submitTx.bind(emulator);
  if (restartCekMaterialTraversal || cancelCekMaterialTraversal)
    emulator.submitTx = async (cbor) => {
      const hash = await submitBeforeRestart(cbor);
      const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
      for (let index = 0; index < outputs.len(); index++) {
        if (
          outputs.get(index).address().to_bech32() !==
          contracts.fraudProofContracts.validationTraceDispute
            .cekMaterialTraversal.spendingScriptAddress
        )
          continue;
        traversalOutputs++;
        if (traversalOutputs !== 2) continue;
        const material = fixture.evidence.oneStepArgument.cekRouteMaterial;
        if (material === undefined)
          throw new Error("restart fixture has no CEK material");
        checkpointJson = JSON.stringify({
          threadOutRef: `${hash}#${index}`,
          material: {
            envelopeCborHex: material.envelopeCbor.toString("hex"),
            programMaterialSidecarCborHex:
              material.programMaterialSidecarCbor.toString("hex"),
          },
          deploymentInfo: semanticDeploymentInfo,
        });
        throw new Error(
          "simulated process loss after accepted material checkpoint",
        );
      }
      return hash;
    };
  if (restartCekCore || cancelCekCore) {
    let coreOutputs = 0;
    const addresses = new Set(
      Object.values(
        contracts.fraudProofContracts.validationTraceDispute.cekCoreStages,
      ).map((stage) => stage.spendingScriptAddress),
    );
    emulator.submitTx = async (cbor) => {
      const hash = await submitBeforeRestart(cbor);
      const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
      for (let index = 0; index < outputs.len(); index++) {
        if (!addresses.has(outputs.get(index).address().to_bech32())) continue;
        coreOutputs++;
        if (coreOutputs !== 2) continue;
        coreCheckpointJson = JSON.stringify({
          threadOutRef: `${hash}#${index}`,
          deploymentInfo: semanticDeploymentInfo,
          transitionCborHex: Buffer.from(
            fixture.evidence.oneStepArgument.transitionCbor,
          ).toString("hex"),
          auxiliaryCborHex: Buffer.from(
            fixture.evidence.oneStepArgument.auxiliaryCbor,
          ).toString("hex"),
        });
        throw new Error(
          "simulated process loss after accepted core checkpoint",
        );
      }
      return hash;
    };
  }
  const semanticCapture = await captureEmulatorSubmission(emulator, () =>
    runEmulatorLifecycleStage(
      "semantic-resolution",
      async (): Promise<
        | SubmitValidationDisputeSemanticResolutionResult
        | {
            cancellation: Awaited<
              ReturnType<typeof cancelValidationCekMaterialTraversal>
            >;
          }
      > => {
        try {
          return await submitValidationDisputeSemanticResolution({
            phaseANativeItemYieldKind,
            ...(phaseAItemCarriage === undefined
              ? {}
              : { carriageMaterial: phaseAItemCarriage.material }),
            cekMaterialTraversalBatchSize,
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo: semanticDeploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef: selectedResult.nextThreadOutRef,
            oneStepArgument: fixture.evidence.oneStepArgument,
            ...(publishedSemanticEntry === undefined
              ? { referenceScriptUtxo: semanticPublication.utxo }
              : {}),
            validityRange: validityRange(),
            awaitConfirmation: true,
          });
        } catch (cause) {
          if (coreCheckpointJson !== undefined) {
            emulator.submitTx = submitBeforeRestart;
            const checkpoint: {
              threadOutRef: string;
              deploymentInfo: unknown;
              transitionCborHex: string;
              auxiliaryCborHex: string;
            } = JSON.parse(coreCheckpointJson);
            await targetChallengerLucid.awaitTx(
              checkpoint.threadOutRef.split("#")[0]!,
            );
            if (cancelCekCore)
              return {
                cancellation: await cancelValidationCekCore({
                  lucid: targetChallengerLucid,
                  blueprint: realBlueprint,
                  deploymentInfo: checkpoint.deploymentInfo,
                  network,
                  signer: challengerSigner,
                  threadOutRef: checkpoint.threadOutRef,
                  witnessReferenceScripts,
                }),
              };
            const resumed = await resumeValidationCekCore({
              lucid: targetChallengerLucid,
              blueprint: realBlueprint,
              deploymentInfo: checkpoint.deploymentInfo,
              network,
              signer: challengerSigner,
              threadOutRef: checkpoint.threadOutRef,
              oneStepArgument: {
                resolverIndex: 11,
                semanticResolverIndex: 3,
                transitionCbor: Buffer.from(
                  checkpoint.transitionCborHex,
                  "hex",
                ),
                auxiliaryCbor: Buffer.from(checkpoint.auxiliaryCborHex, "hex"),
              },
              validityRange: validityRange(),
            });
            if (!resumed.completed)
              throw new Error("Restart did not finish CEK core chain");
            const last = resumed.transactions.at(-1)!;
            return {
              txHash: last.txHash,
              threadOutRef: selectedResult.nextThreadOutRef,
              nextThreadOutRef: last.nextThreadOutRef,
              proofItemCarriage: "direct",
              resolverIndex: stagedResolverIndex,
              semanticResolverIndex: stagedSemanticIndex,
              semanticResolverGlobalIndex:
                validationSemanticResolverGlobalIndex(
                  stagedResolverIndex,
                  stagedSemanticIndex,
                ),
              inputIndex: last.inputIndex,
              outputIndex: last.outputIndex,
              awaitedConfirmation: true,
              stageTransactions: resumed.transactions,
            };
          }
          if (checkpointJson === undefined) throw cause;
          emulator.submitTx = submitBeforeRestart;
          const checkpoint: {
            threadOutRef: string;
            material: {
              envelopeCborHex: string;
              programMaterialSidecarCborHex: string;
            };
            deploymentInfo: unknown;
          } = JSON.parse(checkpointJson);
          await targetChallengerLucid.awaitTx(
            checkpoint.threadOutRef.split("#")[0]!,
          );
          if (cancelCekMaterialTraversal)
            return {
              cancellation: await cancelValidationCekMaterialTraversal({
                lucid: targetChallengerLucid,
                blueprint: realBlueprint,
                deploymentInfo: checkpoint.deploymentInfo,
                network,
                signer: challengerSigner,
                threadOutRef: checkpoint.threadOutRef,
                witnessReferenceScripts,
              }),
            };
          const resumed = await resumeValidationCekMaterialTraversal({
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo: checkpoint.deploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef: checkpoint.threadOutRef,
            material: checkpoint.material,
            validityRange: validityRange(),
          });
          if (!resumed.completed)
            throw new Error("restart did not finish material traversal");
          const last = resumed.transactions.at(-1)!;
          return {
            txHash: last.txHash,
            threadOutRef: selectedResult.nextThreadOutRef,
            nextThreadOutRef: last.nextThreadOutRef,
            proofItemCarriage: "direct",
            resolverIndex: stagedResolverIndex,
            semanticResolverIndex: stagedSemanticIndex,
            semanticResolverGlobalIndex: validationSemanticResolverGlobalIndex(
              stagedResolverIndex,
              stagedSemanticIndex,
            ),
            inputIndex: last.inputIndex,
            outputIndex: last.outputIndex,
            awaitedConfirmation: true,
            cekRoute: "authenticatedMaterialTraversal",
            stageTransactions: resumed.transactions,
          };
        }
      },
    ),
  );
  if ("cancellation" in semanticCapture.result)
    return {
      fixture,
      contracts,
      initResult,
      lowIndex,
      highIndex,
      cancellation: semanticCapture.result.cancellation,
    };
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
  // The stage no longer runs under `withRealL1MaxTxSize`: the ten-parameter
  // `state_queue.mint` is 16,498 bytes after applying this harness deployment,
  // past the 16,384-byte L1 envelope, so its publication cannot be built at
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
