/**
 * #621 route-freedom journey harness: the full validation-dispute lifecycle
 * (setup → init → open → source → bisection reveals → enter-resolution →
 * prepare-resolution → prepare-selected) staged once, so each test drives the
 * semantic-resolution leg with its own build-time delivery routing — forced
 * routes, refusal probes against the same staged thread, and the recoveries.
 *
 * The recipe is the one `submit-init-emulator-validation-dispute.test.ts`
 * runs; it is duplicated here rather than refactored out of that file because
 * that file holds the two recorded expected-red rows that stand until #617's
 * regeneration, and this ticket does not reshape them.
 *
 * Everything here speaks the **Option B wire** (#619/#620/#621): the
 * committed evidence is transition-only and the two-parameter item-semantic
 * validator enforces it. Against the stale deployed blueprint these journeys
 * would all die at prepare-selected with the same `Spend[0] unexpected empty
 * list` signature as the recorded rows — an unfalsifiable red that proves
 * nothing — so suites built on this harness must gate themselves with
 * {@link blueprintSpeaksOptionBCompleteItemWireV1} and skip loudly instead.
 */

import { outRefLabel } from "@al-ft/midgard-core";
import {
  buildValidationTraceDisputeFaultProofContracts,
  parseFaultProofBlueprint,
  validationMachineStateDataFromCore,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  type Emulator,
  getAddressDetails,
  type LucidEvolution,
  type Script,
} from "@lucid-evolution/lucid";
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
  validationDisputeValidityRange,
  type ValidationProofItemDeliveryV1,
} from "../../src/index.js";
import { submitInit } from "./legacy-submit-emulator.js";
import { buildInvalidForcedValidationDisputeFixture } from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  type Blueprint,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  cloneBlueprint,
  type CompleteSignedTransactionMeasurement,
  createRealL1TargetLucids,
  createValidationDisputeParties,
  expectSingleUtxoWithUnit,
  network,
  publishFaultProofWitnessReferenceScriptsV1,
  publishOperatorLifecycleReferenceScriptsV1,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  runEmulatorLifecycleStage,
  stageAuthenticatedValidationDisputePublication,
  submitSetupTx,
  withRealL1MaxTxSize,
} from "./submit-init-emulator-shared.js";

const ITEM_SEMANTIC_SPEND_TITLE =
  "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend";

/**
 * Whether the blueprint under test compiles the Option B complete-item wire.
 *
 * The discriminator is structural, not a hash pin: #620 removed the carriage
 * parameter from `canonical_decode_item_semantic_v1`, taking its declared
 * parameter list from three entries to two. A blueprint still declaring three
 * is the deployed pre-Option-B build, against which every journey in this
 * harness reds out at prepare-selected exactly like the two recorded
 * expected-red rows — so suites skip on it rather than manufacture an
 * unfalsifiable failure.
 */
export const blueprintSpeaksOptionBCompleteItemWireV1 = (
  blueprint: Blueprint,
): boolean => {
  const itemSemantic = blueprint.validators.find(
    (validator) => validator.title === ITEM_SEMANTIC_SPEND_TITLE,
  );
  if (itemSemantic === undefined) {
    throw new Error(
      `blueprint has no "${ITEM_SEMANTIC_SPEND_TITLE}" validator to probe`,
    );
  }
  return (itemSemantic.parameters ?? []).length === 2;
};

/** The gate the route-freedom suites share, probed once at collection time. */
export const realBlueprintSpeaksOptionBV1 = (): boolean =>
  blueprintSpeaksOptionBCompleteItemWireV1(readBlueprint(realBlueprintPath));

export const OPTION_B_SKIP_REASON =
  "SKIPPED (#621): the blueprint at MIDGARD_REAL_BLUEPRINT_PATH (or " +
  "onchain/aiken/plutus.json) predates Option B — " +
  "canonical_decode_item_semantic_v1 still declares the retired carriage " +
  "parameter, so every route-freedom journey would red out at " +
  "prepare-selected with the recorded `Spend[0] unexpected empty list` " +
  "signature instead of testing anything. Rebuild the blueprint with the " +
  "pinned Aiken fork (#617 regeneration) to run these journeys.";

export type CapturedSemanticSubmission = Awaited<
  ReturnType<typeof captureEmulatorSubmission<SemanticResolutionResult>>
>;

type SemanticResolutionResult = Awaited<
  ReturnType<typeof submitValidationDisputeSemanticResolution>
>;

/**
 * One staged lifecycle stage's captured submissions (#622): the stage label
 * the journey already logs, plus every transaction the stage submitted, in
 * submission order, measured by `captureEmulatorSubmission`.
 */
export type CapturedLifecycleStageV1 = {
  readonly label: string;
  readonly measurements: readonly CompleteSignedTransactionMeasurement[];
};

export type RouteFreedomJourneyV1 = {
  readonly emulator: Emulator;
  readonly realBlueprint: Blueprint;
  readonly challengerLucid: LucidEvolution;
  readonly stagedThreadOutRef: string;
  readonly validityRange: () => ReturnType<
    typeof validationDisputeValidityRange
  >;
  /** The measured §5.1 complete-item byte length the fixture staged. */
  readonly completeItemBytes: number;
  /**
   * Per-stage measurements for everything staged before the semantic leg
   * (#622's measurement campaign): setup, the reference-script publications,
   * init, open, source, every bisection reveal, enter-resolution,
   * prepare-resolution, and prepare-selected — labels as logged, one entry
   * per staged call, in staging order. The semantic-resolution and award
   * legs are captured by their own submit functions.
   */
  readonly lifecycleMeasurements: readonly CapturedLifecycleStageV1[];
  /**
   * One semantic-resolution attempt against the staged thread, with this
   * call's routing inputs. A refusal leaves the thread untouched, so failed
   * attempts and the eventual green run all target the same
   * {@link stagedThreadOutRef}.
   */
  readonly submitSemanticResolution: (routing?: {
    readonly proofItemDelivery?: ValidationProofItemDeliveryV1;
    readonly proofItemReferenceOutRef?: string;
  }) => Promise<CapturedSemanticSubmission>;
  readonly submitAward: (
    threadOutRef: string,
  ) => Promise<
    Awaited<
      ReturnType<
        typeof captureEmulatorSubmission<
          Awaited<ReturnType<typeof submitValidationDisputeAward>>
        >
      >
    >
  >;
  /**
   * Asserts the staged thread UTxO is still live — the pin that a refused
   * attempt submitted nothing and spent nothing.
   */
  readonly expectStagedThreadUnspent: () => Promise<void>;
  /** An out-ref this journey genuinely created and then spent on chain. */
  readonly spentOutRef: string;
};

/**
 * #622: one-line JSON dump of a campaign journey's complete measured table —
 * every staged lifecycle transaction plus the semantic leg, bytes, pre-sign
 * projections, and execution units — gated on MIDGARD_PRINT_PROOF_FIT=1 like
 * every other proof-fit print. The measurement-campaign suites call this
 * BEFORE their pins, so one red run still surrenders every number; the
 * committed pins were read off exactly this print, and re-measuring after a
 * shape change is one env var away.
 */
export const printRouteFreedomCampaignTableV1 = (
  headline: string,
  journey: RouteFreedomJourneyV1,
  semantic: CapturedSemanticSubmission,
): void => {
  if (process.env["MIDGARD_PRINT_PROOF_FIT"] !== "1") {
    return;
  }
  const measurementRow = (
    measurement: CompleteSignedTransactionMeasurement,
  ) => ({
    bytes: measurement.completeSignedBytes,
    mem: measurement.executionMemory.toString(),
    cpu: measurement.executionSteps.toString(),
    refInputs: measurement.referenceInputCount,
  });
  const table = {
    completeItemBytes: journey.completeItemBytes,
    lifecycle: journey.lifecycleMeasurements.map((stage) => ({
      label: stage.label,
      transactions: stage.measurements.map(measurementRow),
    })),
    stages: (semantic.result.stageTransactions ?? []).map((stage) => ({
      kind: stage.kind,
      bytes: stage.completeSignedBytes,
      projected: stage.projectedSignedBytes,
    })),
    transactions: semantic.measurements.map(measurementRow),
    refusal:
      semantic.result.proofItemInlineEnvelopeRefusal === undefined
        ? undefined
        : {
            projectedSignedBytes:
              semantic.result.proofItemInlineEnvelopeRefusal
                .projectedSignedBytes,
            maxTransactionBytes:
              semantic.result.proofItemInlineEnvelopeRefusal
                .maxTransactionBytes,
          },
  };
  console.log(`${headline} ${JSON.stringify(table)}`);
};

/**
 * #622: execution-unit band pin for stages whose bill is run-dependent. The
 * journey ledger's txids and addresses vary run to run (the emulator starts
 * at wall-clock time and the accounts are generated), and on-chain out-ref
 * lookups compare those values byte by byte, short-circuiting at the first
 * difference — so stages that walk reference inputs by out-ref bill within
 * a small band rather than exactly (measured on the reference route:
 * authenticate 163,390 vs 166,458 memory units, the by-reference observe
 * door 931,806 vs 928,938, in consecutive runs). The sweep fixture pins
 * exactly because its basis fixes `now`; journey suites pin such stages to
 * a measured anchor with 3% tolerance — wide enough for the observed <2%
 * wobble, regression-tight against Option B's 40-70% deltas.
 */
export const expectExecutionWithinBandV1 = (
  label: string,
  measurement: CompleteSignedTransactionMeasurement,
  anchor: { readonly memoryUnits: bigint; readonly stepUnits: bigint },
): void => {
  const within = (actual: bigint, expected: bigint): boolean => {
    const delta = actual > expected ? actual - expected : expected - actual;
    return delta * 100n <= expected * 3n;
  };
  expect(
    within(measurement.executionMemory, anchor.memoryUnits),
    `${label} memory ${measurement.executionMemory.toString()} strays more ` +
      `than 3% from the measured anchor ${anchor.memoryUnits.toString()}`,
  ).toBe(true);
  expect(
    within(measurement.executionSteps, anchor.stepUnits),
    `${label} steps ${measurement.executionSteps.toString()} strays more ` +
      `than 3% from the measured anchor ${anchor.stepUnits.toString()}`,
  ).toBe(true);
};

/**
 * Stages one full dispute up to (and including) prepare-selected and hands
 * back the semantic-resolution leg as a function of its routing inputs.
 */
export const prepareRouteFreedomJourneyV1 = async ({
  inlineDatumPayloadBytes,
  minimumCompleteItemBytes,
}: {
  readonly inlineDatumPayloadBytes: number;
  readonly minimumCompleteItemBytes: number;
}): Promise<RouteFreedomJourneyV1> => {
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
  const lifecycleMeasurements: CapturedLifecycleStageV1[] = [];
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
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScriptsV1({
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
    operatorSeedPhrase: operator.seedPhrase,
    contracts,
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
