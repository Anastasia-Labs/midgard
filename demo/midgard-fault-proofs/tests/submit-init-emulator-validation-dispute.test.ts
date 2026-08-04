/**
 * Validation-dispute journeys: publication of every authenticated dispute
 * control under the L1 envelope, and the full open / bisect / resolve / award
 * lifecycle for both fitting-complete-item carriages.
 *
 * Split out of `submit-init-emulator.test.ts` to keep each file's leaked wasm
 * heap far below the ~4 GiB wasm32 ceiling; see
 * tests/support/uplc-heap-guard.ts.
 */

import {
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
  outRefLabel,
} from "@al-ft/midgard-core";
import {
  buildValidationTraceDisputeFaultProofContracts,
  createReferenceScriptAuthPolicy,
  parseFaultProofBlueprint,
  referenceScriptAuthUnit,
  validationMachineStateDataFromCore,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  requireValidationCekDirectResolverReferenceScriptUtxo,
  resolveProverSigner,
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  submitValidationDisputeVerifySource,
  VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  validationDisputeValidityRange,
} from "../src/index.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import { buildInvalidForcedValidationDisputeFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  cloneBlueprint,
  type CompleteSignedTransactionMeasurement,
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  network,
  publishAuthenticatedValidationCekDirectResolver,
  publishAuthenticatedValidationDisputeControl,
  publishPlainReferenceScriptUtxo,
  publishValidationDisputeReferenceScript,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  runEmulatorLifecycleStage,
  submitSetupTx,
  type ValidationDisputeControlPublicationTarget,
  validationDisputeControlPublicationTargets,
} from "./support/submit-init-emulator-shared.js";

describe("fault-proof emulator integration", () => {
  it("publishes every authenticated validation-dispute control under the exact L1 envelope", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const publisher = generateEmulatorAccount({
      lovelace: 40_000_000_000n,
    });
    const emulator = new Emulator([publisher], {
      ...EMULATOR_PROTOCOL_PARAMETERS,
      maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    });
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected publisher wallet to expose a nonce UTxO");
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
    const targets = validationDisputeControlPublicationTargets(contracts);
    const authPolicy = createReferenceScriptAuthPolicy(lucid, emulator.now());
    const measurements = {} as Record<
      ValidationDisputeControlPublicationTarget["control"],
      CompleteSignedTransactionMeasurement
    >;

    for (const target of targets) {
      const publication = await runEmulatorLifecycleStage(
        `reference-script.publish-authenticated.${target.control}`,
        () =>
          publishAuthenticatedValidationDisputeControl({
            lucid,
            target,
            authPolicy,
          }),
      );
      measurements[target.control] = publication.publicationMeasurement;
    }

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          { validationDisputeControlPublications: measurements },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }

    expect(Object.keys(measurements)).toHaveLength(targets.length);
    for (const measurement of Object.values(measurements)) {
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        emulator.protocolParameters.maxTxExMem,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        emulator.protocolParameters.maxTxExSteps,
      );
      expect(measurement.inputCount).toBe(1);
      expect(measurement.referenceInputCount).toBe(0);
      expect(measurement.outputCount).toBe(3);
      expect(measurement.vkeyWitnessCount).toBe(1);
      expect(measurement.nativeScriptCount).toBe(1);
      expect(measurement.redeemerCount).toBe(0);
      expect(measurement.datumCount).toBe(0);
      expect(measurement.plutusV1ScriptCount).toBe(0);
      expect(measurement.plutusV2ScriptCount).toBe(0);
      expect(measurement.plutusV3ScriptCount).toBe(0);
    }
  }, 300_000);

  it("publishes and verifies the authenticated generated-blueprint CEK direct-resolver reference script", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const publisher = generateEmulatorAccount({
      lovelace: 4_000_000_000_000n,
    });
    // Deployment-time publication of the ~156 KiB applied resolver cannot fit
    // the 16,384-byte L1 proof envelope; the emulator hosts it under a raised
    // maxTxSize so the consuming finalization path can stay by-reference.
    const emulator = new Emulator([publisher], {
      ...EMULATOR_PROTOCOL_PARAMETERS,
      maxTxSize: 262_144,
    });
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const contracts = await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
        network,
        hubOraclePolicyId: "11".repeat(28),
        fraudProofCataloguePolicyId: "22".repeat(28),
      }),
    );
    const directResolver = contracts.validationTraceDispute.directResolvers[0];
    const appliedResolverBytes =
      directResolver.spendingScript.script.length / 2;
    expect(appliedResolverBytes).toBeGreaterThan(
      PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    );

    const authPolicy = createReferenceScriptAuthPolicy(lucid, emulator.now());
    const publication = await runEmulatorLifecycleStage(
      "reference-script.publish-authenticated.cekDirectResolver",
      () =>
        publishAuthenticatedValidationCekDirectResolver({
          lucid,
          script: directResolver.spendingScript,
          authPolicy,
        }),
    );
    // Honest deployment-time measurement: the publication itself exceeds the
    // L1 proof envelope precisely because the resolver body does.
    expect(publication.publicationMeasurement.l1ByteMargin).toBeLessThan(0);
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            cekDirectResolverReferencePublication: {
              appliedResolverBytes,
              appliedResolverHash: directResolver.spendingScriptHash,
              measurement: publication.publicationMeasurement,
            },
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }
    expect(publication.utxo.scriptRef).toBeDefined();
    expect(validatorToScriptHash(publication.utxo.scriptRef!)).toBe(
      directResolver.spendingScriptHash,
    );
    expect(
      publication.utxo.assets[
        referenceScriptAuthUnit(
          authPolicy.policyId,
          "V1 validation-trace CEK direct resolver",
        )
      ],
    ).toBe(1n);

    const publishedEntry = {
      scriptHash: directResolver.spendingScriptHash,
      refScriptUTxO: {
        txHash: publication.utxo.txHash,
        outputIndex: publication.utxo.outputIndex,
      },
    };

    // Complete verification path used by CEK finalization submission.
    const verified =
      await requireValidationCekDirectResolverReferenceScriptUtxo({
        lucid,
        deploymentInfo: {
          [VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]:
            publishedEntry,
        },
        expectedScriptHash: directResolver.spendingScriptHash,
        authPolicyId: authPolicy.policyId,
      });
    expect(outRefLabel(verified)).toBe(outRefLabel(publication.utxo));

    // Missing registration rejects before any transaction is constructed.
    await expect(
      requireValidationCekDirectResolverReferenceScriptUtxo({
        lucid,
        deploymentInfo: {},
        expectedScriptHash: directResolver.spendingScriptHash,
        authPolicyId: authPolicy.policyId,
      }),
    ).rejects.toThrow(/missing "validationTraceDisputeCekDirectResolver"/u);

    // Wrong reference: a UTxO without any reference script rejects.
    const plainUtxo = (await lucid.wallet().getUtxos()).find(
      (candidate) => candidate.scriptRef == null,
    );
    expect(plainUtxo).toBeDefined();
    await expect(
      requireValidationCekDirectResolverReferenceScriptUtxo({
        lucid,
        deploymentInfo: {
          [VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash: directResolver.spendingScriptHash,
            refScriptUTxO: {
              txHash: plainUtxo!.txHash,
              outputIndex: plainUtxo!.outputIndex,
            },
          },
        },
        expectedScriptHash: directResolver.spendingScriptHash,
        authPolicyId: authPolicy.policyId,
      }),
    ).rejects.toThrow(/does not carry a reference script/u);

    // Wrong reference: the published award control carries a different
    // validator, so the resolver-hash check rejects it.
    const awardPublication = await publishAuthenticatedValidationDisputeControl(
      {
        lucid,
        target: {
          control: "award",
          name: "V1 validation-trace award",
          script: contracts.validationTraceDispute.award.spendingScript,
        } as ValidationDisputeControlPublicationTarget,
        authPolicy,
      },
    );
    await expect(
      requireValidationCekDirectResolverReferenceScriptUtxo({
        lucid,
        deploymentInfo: {
          [VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash: directResolver.spendingScriptHash,
            refScriptUTxO: {
              txHash: awardPublication.utxo.txHash,
              outputIndex: awardPublication.utxo.outputIndex,
            },
          },
        },
        expectedScriptHash: directResolver.spendingScriptHash,
        authPolicyId: authPolicy.policyId,
      }),
    ).rejects.toThrow(/reference script hash mismatch/u);

    // Wrong role: the exact resolver published under another role token
    // rejects, so an attacker cannot substitute a differently-authorized
    // publication.
    const wrongRolePublication =
      await publishAuthenticatedValidationCekDirectResolver({
        lucid,
        script: directResolver.spendingScript,
        authPolicy,
        roleName: "V1 validation-trace award",
      });
    await expect(
      requireValidationCekDirectResolverReferenceScriptUtxo({
        lucid,
        deploymentInfo: {
          [VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash: directResolver.spendingScriptHash,
            refScriptUTxO: {
              txHash: wrongRolePublication.utxo.txHash,
              outputIndex: wrongRolePublication.utxo.outputIndex,
            },
          },
        },
        expectedScriptHash: directResolver.spendingScriptHash,
        authPolicyId: authPolicy.policyId,
      }),
    ).rejects.toThrow(/must carry exactly one .* auth-role token/u);
  }, 300_000);

  it.each([
    {
      name: "direct",
      inlineDatumPayloadBytes: 7_976,
      minimumCompleteItemBytes: 0,
      expectedCarriage: "direct" as const,
    },
    {
      name: "reference",
      inlineDatumPayloadBytes: 13_600,
      minimumCompleteItemBytes:
        MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes,
      expectedCarriage: "reference" as const,
    },
  ])(
    "opens, bisects, resolves a fitting complete item by $name, and awards a validation dispute",
    async ({
      inlineDatumPayloadBytes,
      minimumCompleteItemBytes,
      expectedCarriage,
    }) => {
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
                operator.assets.lovelace -
                BigInt(feeUtxoCount) * feeUtxoLovelace,
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
                challenger.assets.lovelace -
                BigInt(feeUtxoCount) * feeUtxoLovelace,
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
      const validityRange = () =>
        validationDisputeValidityRange(emulator.now());

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
      // Re-derive the applied canonical-decode item-semantic validator (the
      // same deterministic build the submit path performs) so its reference
      // script can be published and its body pinned as absent from the proof
      // transactions.
      const validationDisputeSdkContracts = await Effect.runPromise(
        buildValidationTraceDisputeFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: contracts.hubOracle.policyId,
          fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
        }),
      );
      const itemSemanticContract =
        validationDisputeSdkContracts.validationTraceDispute
          .semanticResolvers[1];
      const catalogue = await buildCatalogueDeploymentInfo(
        contracts.fraudProofs,
      );
      const operatorPaymentCredential = getAddressDetails(
        await operatorLucid.wallet().address(),
      ).paymentCredential;
      if (
        operatorPaymentCredential === undefined ||
        operatorPaymentCredential.type !== "Key"
      ) {
        throw new Error(
          "Expected operator wallet to expose a payment key hash",
        );
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
      const itemSemanticPublication = await runEmulatorLifecycleStage(
        "reference-script.publish-item-semantic",
        async () => {
          emulator.protocolParameters = {
            ...setupProtocolParameters,
            maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
          };
          try {
            return await publishPlainReferenceScriptUtxo({
              lucid: referenceScriptPublisherLucid,
              script: itemSemanticContract.spendingScript,
              label: "validation item-semantic",
            });
          } finally {
            emulator.protocolParameters = setupProtocolParameters;
          }
        },
      );
      const deploymentInfo = buildRemovalDeploymentInfo(
        contracts,
        catalogue,
        validationDisputePublication,
        {
          scriptHash: itemSemanticContract.spendingScriptHash,
          utxo: itemSemanticPublication.utxo,
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
      const openSubmission = await runEmulatorLifecycleStage("open", () =>
        captureEmulatorSubmission(emulator, () =>
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
        ),
      );
      const openResult = openSubmission.result;
      const publicationMeasurement = openSubmission.measurement;
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
              signer:
                move.role === "operator" ? operatorSigner : challengerSigner,
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
      const semanticSubmission = await runEmulatorLifecycleStage(
        "semantic-resolution",
        () =>
          captureEmulatorSubmission(emulator, () =>
            submitValidationDisputeSemanticResolution({
              lucid: targetChallengerLucid,
              blueprint: realBlueprint,
              deploymentInfo,
              network,
              signer: challengerSigner,
              threadOutRef: selectedResult.nextThreadOutRef,
              oneStepArgument: fixture.evidence.oneStepArgument,
              validityRange: validityRange(),
              awaitConfirmation: true,
            }),
          ),
      );
      const semanticResult = semanticSubmission.result;
      const awardSubmission = await runEmulatorLifecycleStage("award", () =>
        captureEmulatorSubmission(emulator, () =>
          submitValidationDisputeAward({
            lucid: targetChallengerLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: challengerSigner,
            threadOutRef: semanticResult.nextThreadOutRef,
            validityRange: validityRange(),
            awaitConfirmation: true,
          }),
        ),
      );
      const awardResult = awardSubmission.result;
      const proofTransactionMeasurements = {
        referenceScriptPublication:
          validationDisputePublication.publicationMeasurement,
        publication: publicationMeasurement,
        resolution: semanticSubmission.measurement,
        resolutionTransactions: semanticSubmission.measurements,
        award: awardSubmission.measurement,
      };
      const allProofTransactionMeasurements = [
        validationDisputePublication.publicationMeasurement,
        publicationMeasurement,
        ...semanticSubmission.measurements,
        awardSubmission.measurement,
      ];
      if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
        console.info(
          JSON.stringify(
            {
              completeItemBytes: fixture.completeItemBytes,
              transactions: proofTransactionMeasurements,
            },
            (_key, value: unknown) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          ),
        );
      }

      expect(fixture.evidence.finalDispute.turn).toEqual({
        type: "readyForOneStep",
      });
      expect(fixture.evidence.moves.length).toBeGreaterThan(0);
      expect(prepareResult.resolverIndex).toBe(
        fixture.evidence.oneStepArgument.resolverIndex,
      );
      expect(selectedResult.semanticResolverIndex).toBe(
        fixture.evidence.oneStepArgument.semanticResolverIndex,
      );
      expect(semanticResult.proofItemCarriage).toBe(expectedCarriage);
      if (expectedCarriage === "reference") {
        expect(semanticResult.proofItemReferenceOutRef).toBe(
          semanticResult.proofItemPublication?.outRef,
        );
        expect(semanticResult.proofItemPublication).toMatchObject({
          awaitedConfirmation: true,
        });
        expect(
          semanticResult.proofItemPublication?.completeSignedBytes,
        ).toBeLessThanOrEqual(PROTOCOL_PARAMETERS_DEFAULT.maxTxSize);
        expect(
          semanticResult.proofItemPublication?.lovelace ?? 0n,
        ).toBeGreaterThan(0n);
      } else {
        expect(semanticResult.proofItemReferenceOutRef).toBeUndefined();
        expect(semanticResult.proofItemPublication).toBeUndefined();
      }
      expect(semanticResult.stageTransactions).toHaveLength(5);
      expect(semanticSubmission.measurements).toHaveLength(
        expectedCarriage === "reference" ? 6 : 5,
      );
      // The semantic-resolution (authentication) proof transaction sources
      // the item-semantic validator from the published reference script: one
      // extra reference input beside the direct route, two beside the
      // published proof item on the reference route.
      expect(
        semanticSubmission.measurements.map(
          (measurement) => measurement.referenceInputCount,
        ),
      ).toEqual(
        expectedCarriage === "reference" ? [0, 2, 0, 1, 0, 0] : [1, 0, 0, 0, 0],
      );
      expect(
        semanticSubmission.measurements.every(
          (measurement) =>
            measurement.completeSignedBytes <=
            PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
        ),
      ).toBe(true);
      // C21-DISPUTE-SUBMIT defect 2: the representative complete-item
      // semantic-resolution transaction stays at or below the literal
      // 16,384-byte L1 envelope and does not embed the ~3.4 KiB applied
      // item-semantic validator body — no Plutus script witness at all; the
      // validator arrives via the published reference script.
      const resolutionMeasurements = semanticSubmission.measurements.slice(
        expectedCarriage === "reference" ? 1 : 0,
      );
      const resolutionCbors = semanticSubmission.transactionCbors.slice(
        expectedCarriage === "reference" ? 1 : 0,
      );
      const authenticationMeasurement = resolutionMeasurements[0]!;
      const authenticationCbor = resolutionCbors[0]!;
      expect(authenticationMeasurement.completeSignedBytes).toBeLessThanOrEqual(
        16_384,
      );
      expect(authenticationMeasurement.plutusV3ScriptCount).toBe(0);
      expect(authenticationMeasurement.plutusV2ScriptCount).toBe(0);
      expect(authenticationMeasurement.plutusV1ScriptCount).toBe(0);
      expect(authenticationMeasurement.nativeScriptCount).toBe(0);
      expect(itemSemanticContract.spendingScript.script.length).toBeGreaterThan(
        0,
      );
      expect(
        authenticationCbor.includes(itemSemanticContract.spendingScript.script),
      ).toBe(false);
      expect(
        semanticResult.stageTransactions?.map(
          (transaction) => transaction.completeSignedBytes,
        ),
      ).toEqual(
        semanticSubmission.measurements
          .slice(expectedCarriage === "reference" ? 1 : 0)
          .map((measurement) => measurement.completeSignedBytes),
      );
      expect(semanticResult.nextThreadOutRef).toBe(awardResult.threadOutRef);
      expect(awardResult.txHash).toHaveLength(64);
      expect(awardResult.fraudProofUnit).toBe(
        toUnit(
          contracts.fraudProof.policyId,
          initResult.computationThreadAssetName,
        ),
      );
      expect(publicationMeasurement.l1ByteMargin).toBeGreaterThan(0);
      expect(publicationMeasurement.referenceInputCount).toBe(3);
      expect(
        validationDisputePublication.publicationMeasurement.nativeScriptCount,
      ).toBe(1);
      expect(publicationMeasurement.plutusV3ScriptCount).toBe(0);
      expect(semanticSubmission.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(awardSubmission.measurement.l1ByteMargin).toBeGreaterThan(0);
      for (const measurement of allProofTransactionMeasurements) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          emulator.protocolParameters.maxTxExMem,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          emulator.protocolParameters.maxTxExSteps,
        );
      }
      await expect(
        targetChallengerLucid.utxosAtWithUnit(
          contracts.fraudProof.spendingScriptAddress,
          awardResult.fraudProofUnit,
        ),
      ).resolves.toHaveLength(1);
    },
    // 600s, matching the sibling full-lifecycle dispute journeys in
    // `submit-init-emulator-soundness.test.ts` (600s/600s/900s); 300s was the
    // outlier. Measured locally at 137.6s ('direct') and 142.4s ('reference')
    // in a fresh worker. GitHub Actions run 30766168392 timed both out at
    // exactly 300s where the same tests measured 143.5s/150.8s locally at the
    // same heap position, which pins the 2-core-runner factor at >=2.0x; that
    // projects to ~290-300s here, i.e. no margin at all against a 300s
    // budget. 600s restores ~4x headroom without weakening anything.
    600_000,
  );
});
