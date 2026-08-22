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
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  resolveProverSigner,
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
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  network,
  publishPlainReferenceScriptUtxo,
  publishValidationDisputeReferenceScript,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  runEmulatorLifecycleStage,
  submitSetupTx,
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
  const operator = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const challenger = generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const feeUtxoCount = 12;
  const feeUtxoLovelace = 100_000_000n;
  const emulator = new Emulator(
    [
      {
        ...operator,
        assets: {
          lovelace:
            operator.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...operator,
        assets: { lovelace: feeUtxoLovelace },
      })),
      {
        ...challenger,
        assets: {
          lovelace:
            challenger.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...challenger,
        assets: { lovelace: feeUtxoLovelace },
      })),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const operatorLucid = await Lucid(emulator, "Custom");
  const challengerLucid = await Lucid(emulator, "Custom");
  operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  challengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
  const operatorSigner = resolveProverSigner({
    network,
    walletSeedPhrase: operator.seedPhrase,
  });
  const challengerSigner = resolveProverSigner({
    network,
    walletSeedPhrase: challenger.seedPhrase,
  });
  const validityRange = () => validationDisputeValidityRange(emulator.now());

  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a nonce UTxO");
  }
  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    {
      realValidationTraceDispute: true,
      alwaysFraudProofCatalogue: true,
    },
  );
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
  const setup = await runEmulatorLifecycleStage("setup", () =>
    submitSetupTx({
      lucid: operatorLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fixture.header,
    }),
  );
  const publicationSlotConfig = operatorLucid.config().slotConfig;
  if (publicationSlotConfig === undefined) {
    throw new Error(
      "Expected reference-script publisher Lucid to expose its Custom slot config",
    );
  }
  const setupProtocolParameters = emulator.protocolParameters;
  emulator.protocolParameters = {
    ...setupProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  const referenceScriptPublisherLucid = await Lucid(emulator, "Custom", {
    slotConfig: publicationSlotConfig,
  });
  referenceScriptPublisherLucid.selectWallet.fromSeed(operator.seedPhrase);
  const validationDisputePublication = await runEmulatorLifecycleStage(
    "reference-script.publish-authenticated",
    async () => {
      try {
        return await publishValidationDisputeReferenceScript({
          lucid: referenceScriptPublisherLucid,
          contracts,
          now: emulator.now(),
        });
      } finally {
        emulator.protocolParameters = setupProtocolParameters;
      }
    },
  );
  const publishPlain = async (label: string, script: Script) => {
    emulator.protocolParameters = {
      ...setupProtocolParameters,
      maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    };
    try {
      return await publishPlainReferenceScriptUtxo({
        lucid: referenceScriptPublisherLucid,
        script,
        label,
      });
    } finally {
      emulator.protocolParameters = setupProtocolParameters;
    }
  };
  const itemSemanticPublication = await runEmulatorLifecycleStage(
    "reference-script.publish-item-semantic",
    () =>
      publishPlain(
        "validation item-semantic",
        itemSemanticContract.spendingScript,
      ),
  );
  const itemObservePublication = await runEmulatorLifecycleStage(
    "reference-script.publish-item-observe",
    () =>
      publishPlain(
        "validation item-observe",
        itemObserveContract.spendingScript,
      ),
  );
  const canonicalDecodePreparePublication = await runEmulatorLifecycleStage(
    "reference-script.publish-canonical-decode-prepare",
    () =>
      publishPlain(
        "validation canonical-decode prepare",
        canonicalDecodePrepareContract.spendingScript,
      ),
  );
  const deploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    validationDisputePublication,
    {
      scriptHash: itemSemanticContract.spendingScriptHash,
      utxo: itemSemanticPublication.utxo,
    },
    {
      scriptHash: itemObserveContract.spendingScriptHash,
      utxo: itemObservePublication.utxo,
    },
    {
      scriptHash: canonicalDecodePrepareContract.spendingScriptHash,
      utxo: canonicalDecodePreparePublication.utxo,
    },
  );
  const initResult = await runEmulatorLifecycleStage("init", () =>
    submitInit({
      lucid: challengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      fraudCategory: "validationTraceDispute",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    }),
  );
  const functionalProtocolParameters = emulator.protocolParameters;
  const functionalSlotConfig = challengerLucid.config().slotConfig;
  if (functionalSlotConfig === undefined) {
    throw new Error(
      "Expected functional emulator Lucid to expose its Custom slot config",
    );
  }
  emulator.protocolParameters = {
    ...functionalProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  const targetOperatorLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  const targetChallengerLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  targetOperatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  targetChallengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
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
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
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
