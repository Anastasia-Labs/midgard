import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  buildFaultProofContracts,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  completeFraudProofCategoryRecord,
  DEFAULT_FAULT_PROOF_NETWORK,
  type InspectContractsOutput,
  type InspectContractsParams,
  type InspectContractsProofCategory,
  type InspectContractsStepOutput,
} from "./inspect-contracts.inspect-contracts-output.js";
import { inspectFraudProofCatalogue } from "./inspect-contracts.inspect-fraud-proof-catalogue.js";
import {
  deploymentEntryBaseForCategory,
  expectScriptHash,
  inspectEmbeddedDeploymentScriptIdentity,
  optionalDeploymentScriptHash,
  parseContractDeploymentReferenceScriptAuthPolicyId,
  requireDeploymentScriptHash,
} from "./inspect-contracts.inspect-fraud-proof-catalogue-category-readiness.js";
import {
  contractDeploymentHistoryBounds,
  parseContractDeploymentInfo,
} from "./inspect-contracts.parse-contract-deployment-info.js";

export const inspectContracts = ({
  blueprint,
  deploymentInfo,
  network = DEFAULT_FAULT_PROOF_NETWORK,
}: InspectContractsParams): Effect.Effect<InspectContractsOutput, Error> =>
  Effect.gen(function* () {
    const parsedBlueprint = parseFaultProofBlueprint(blueprint);
    const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);

    const hubOraclePolicyId = requireDeploymentScriptHash(
      parsedDeploymentInfo,
      "hubOracleMint",
    );
    const fraudProofCataloguePolicyId = requireDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofCatalogueMint",
    );
    const deploymentFraudProofPolicyId = requireDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofMint",
    );
    const deploymentFraudProofSpendingHash = requireDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofSpend",
    );
    const deploymentDoubleSpendScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofDoubleSpend",
    );
    const deploymentNonExistentInputScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofNonExistentInput",
    );
    const nonExistentInputNoIndexIdentity =
      inspectEmbeddedDeploymentScriptIdentity(
        parsedDeploymentInfo,
        "fraudProofNonExistentInputNoIndex",
      );
    const deploymentInvalidRangeScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofInvalidRange",
    );
    const deploymentNoReferenceInputScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofNoReferenceInput",
    );
    const deploymentReferenceInputNoIdxScriptHash =
      optionalDeploymentScriptHash(
        parsedDeploymentInfo,
        "fraudProofReferenceInputNoIdx",
      );
    const deploymentInvalidSignatureScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofInvalidSignature",
    );
    const deploymentZeroInputScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofZeroInput",
    );
    const deploymentTransitionTraceScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofTransitionTrace",
    );
    const deploymentDaHashPreimageScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofDaHashPreimage",
    );
    const deploymentValidationTraceDisputeScriptHash =
      optionalDeploymentScriptHash(
        parsedDeploymentInfo,
        "validationTraceDispute",
      );
    const deployedFraudProofCatalogue =
      parsedDeploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;

    const eventHistoryBounds = contractDeploymentHistoryBounds(
      parsedDeploymentInfo,
      "fabricatedDeposit",
    );
    const withdrawalBounds = contractDeploymentHistoryBounds(
      parsedDeploymentInfo,
      "fabricatedWithdrawal",
    );
    const transitionBounds = contractDeploymentHistoryBounds(
      parsedDeploymentInfo,
      "transitionTrace",
    );
    if (
      eventHistoryBounds.inlineLimitBytes !==
        transitionBounds.inlineLimitBytes ||
      eventHistoryBounds.maxPayloadBytes !== transitionBounds.maxPayloadBytes ||
      eventHistoryBounds.maxPayloadNodes !== transitionBounds.maxPayloadNodes ||
      eventHistoryBounds.inlineLimitBytes !==
        withdrawalBounds.inlineLimitBytes ||
      eventHistoryBounds.maxPayloadBytes !== withdrawalBounds.maxPayloadBytes ||
      eventHistoryBounds.maxPayloadNodes !== withdrawalBounds.maxPayloadNodes
    )
      throw new Error(
        "Full catalogue construction requires matching event history payload bounds across proof families",
      );
    const contracts = yield* buildFaultProofContracts({
      eventHistoryBounds,
      blueprint: parsedBlueprint,
      network,
      hubOraclePolicyId,
      fraudProofCataloguePolicyId,
      referenceScriptAuthPolicyId:
        parseContractDeploymentReferenceScriptAuthPolicyId(
          deploymentInfo,
          "V1 fraud-proof min-ada step-02",
        ),
    });

    for (const [category, entry] of [
      ["fabricatedDeposit", "fraudProofFabricatedDeposit"],
      ["fabricatedWithdrawal", "fraudProofFabricatedWithdrawal"],
    ] as const) {
      if (
        contracts[category].history.retentionAddress !==
        parsedDeploymentInfo[entry]?.eventHistoryRetentionAddress
      ) {
        throw new Error(
          `${entry} history retention address does not match applied parameters`,
        );
      }
    }

    const transitionRetention =
      parsedDeploymentInfo.fraudProofTransitionTrace
        ?.eventHistoryRetentionAddresses;
    if (
      contracts.transitionTrace.history.retentionAddresses.deposit !==
        transitionRetention?.deposit ||
      contracts.transitionTrace.history.retentionAddresses.withdrawal !==
        transitionRetention?.withdrawal
    )
      throw new Error(
        "fraudProofTransitionTrace history retention addresses do not match applied parameters",
      );

    expectScriptHash(
      "fraudProofMint.scriptHash",
      contracts.fraudProof.policyId,
      deploymentFraudProofPolicyId,
    );
    expectScriptHash(
      "fraudProofSpend.scriptHash",
      contracts.fraudProof.spendingScriptHash,
      deploymentFraudProofSpendingHash,
    );

    const [step01, step02, step03, step04] = contracts.doubleSpend.steps;
    const [
      nonExistentInputStep01,
      nonExistentInputStep02,
      nonExistentInputStep03,
      nonExistentInputStep04,
    ] = contracts.nonExistentInput.steps;
    const stepOutput = (
      name: InspectContractsStepOutput["name"],
      step: typeof step01,
    ): InspectContractsStepOutput => {
      const standaloneScriptBytes = Buffer.from(
        step.spendingScriptCBOR,
        "hex",
      ).byteLength;
      return {
        name,
        scriptHash: step.spendingScriptHash,
        address: step.spendingScriptAddress,
        standaloneScriptBytes,
        withinL1TransactionByteEnvelopeNecessaryCondition:
          standaloneScriptBytes <
          MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
      };
    };
    const registeredCategories = completeFraudProofCategoryRecord(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => {
        const chain = contracts[category];
        const inspectedSteps = chain.steps.map((step, stepIndex) =>
          stepOutput(
            `step${(stepIndex + 1).toString().padStart(2, "0")}` as InspectContractsStepOutput["name"],
            step,
          ),
        );
        const deploymentFirstStepScriptHash =
          parsedDeploymentInfo[deploymentEntryBaseForCategory(category)]
            ?.scriptHash ?? null;
        const deploymentFirstStepMatches =
          deploymentFirstStepScriptHash === null
            ? null
            : deploymentFirstStepScriptHash ===
              chain.firstStep.spendingScriptHash;
        return [
          category,
          {
            categoryFirstStepHash: chain.firstStep.spendingScriptHash,
            deploymentFirstStepScriptHash,
            deploymentFirstStepMatches,
            steps: inspectedSteps,
          },
        ] as const;
      }),
    );
    const steps: InspectContractsOutput["doubleSpend"]["steps"] = [
      stepOutput("step01", step01),
      stepOutput("step02", step02),
      stepOutput("step03", step03),
      stepOutput("step04", step04),
    ];
    const nonExistentInputSteps: InspectContractsOutput["nonExistentInput"]["steps"] =
      [
        stepOutput("step01", nonExistentInputStep01),
        stepOutput("step02", nonExistentInputStep02),
        stepOutput("step03", nonExistentInputStep03),
        stepOutput("step04", nonExistentInputStep04),
      ];
    const invalidRangeSteps: InspectContractsOutput["invalidRange"]["steps"] = [
      stepOutput("step01", contracts.invalidRange.steps[0]),
      stepOutput("step02", contracts.invalidRange.steps[1]),
    ];
    const zeroInputSteps: InspectContractsOutput["zeroInput"]["steps"] = [
      stepOutput("step01", contracts.zeroInput.steps[0]),
      stepOutput("step02", contracts.zeroInput.steps[1]),
    ];
    const nonExistentInputNoIndexSteps: InspectContractsOutput["nonExistentInputNoIndex"]["steps"] =
      [
        stepOutput("step01", contracts.nonExistentInputNoIndex.steps[0]),
        stepOutput("step02", contracts.nonExistentInputNoIndex.steps[1]),
        stepOutput("step03", contracts.nonExistentInputNoIndex.steps[2]),
        stepOutput("step04", contracts.nonExistentInputNoIndex.steps[3]),
      ];
    const daHashPreimageSteps: InspectContractsOutput["daHashPreimage"]["steps"] =
      [
        stepOutput("step01", contracts.daHashPreimage.steps[0]),
        stepOutput("step02", contracts.daHashPreimage.steps[1]),
      ];
    const noReferenceInputSteps: InspectContractsOutput["noReferenceInput"]["steps"] =
      [
        stepOutput("step01", contracts.noReferenceInput.steps[0]),
        stepOutput("step02", contracts.noReferenceInput.steps[1]),
        stepOutput("step03", contracts.noReferenceInput.steps[2]),
        stepOutput("step04", contracts.noReferenceInput.steps[3]),
      ];
    const referenceInputNoIdxSteps: InspectContractsOutput["referenceInputNoIdx"]["steps"] =
      [
        stepOutput("step01", contracts.referenceInputNoIdx.steps[0]),
        stepOutput("step02", contracts.referenceInputNoIdx.steps[1]),
        stepOutput("step03", contracts.referenceInputNoIdx.steps[2]),
        stepOutput("step04", contracts.referenceInputNoIdx.steps[3]),
      ];
    const invalidSignatureSteps: InspectContractsOutput["invalidSignature"]["steps"] =
      [
        stepOutput("step01", contracts.invalidSignature.steps[0]),
        stepOutput("step02", contracts.invalidSignature.steps[1]),
      ];
    const transitionTraceSteps: InspectContractsOutput["transitionTrace"]["steps"] =
      [
        stepOutput("route", contracts.transitionTrace.route),
        stepOutput("control", contracts.transitionTrace.finals[0]),
        stepOutput("source", contracts.transitionTrace.finals[1]),
        stepOutput("withdrawal", contracts.transitionTrace.finals[2]),
        stepOutput("forced", contracts.transitionTrace.finals[3]),
        stepOutput("accepted", contracts.transitionTrace.finals[4]),
        stepOutput("deposit", contracts.transitionTrace.finals[5]),
        stepOutput("l1Event", contracts.transitionTrace.finals[6]),
        stepOutput("duplicate", contracts.transitionTrace.finals[7]),
      ];
    const validationTraceDisputeSteps: InspectContractsOutput["validationTraceDispute"]["steps"] =
      [
        stepOutput("dispute", contracts.validationTraceDispute.opener),
        stepOutput("source", contracts.validationTraceDispute.source),
        stepOutput("game", contracts.validationTraceDispute.game),
        stepOutput("boundary", contracts.validationTraceDispute.boundary),
        stepOutput("timeout", contracts.validationTraceDispute.timeout),
        stepOutput("award", contracts.validationTraceDispute.award),
        ...contracts.validationTraceDispute.semanticResolvers.map(
          (resolver, resolverIndex) =>
            stepOutput(`semantic-resolver-${resolverIndex}`, resolver),
        ),
        ...contracts.validationTraceDispute.prepareResolvers.map(
          (resolver, resolverIndex) =>
            stepOutput(`prepare-resolver-${resolverIndex}`, resolver),
        ),
      ];
    const categorizedAppliedSpendingScripts: readonly {
      readonly category: InspectContractsProofCategory;
      readonly step: InspectContractsStepOutput;
    }[] = [
      ...steps.map((step) => ({ category: "doubleSpend" as const, step })),
      ...nonExistentInputSteps.map((step) => ({
        category: "nonExistentInput" as const,
        step,
      })),
      ...invalidRangeSteps.map((step) => ({
        category: "invalidRange" as const,
        step,
      })),
      ...zeroInputSteps.map((step) => ({
        category: "zeroInput" as const,
        step,
      })),
      ...daHashPreimageSteps.map((step) => ({
        category: "daHashPreimage" as const,
        step,
      })),
      ...nonExistentInputNoIndexSteps.map((step) => ({
        category: "nonExistentInputNoIndex" as const,
        step,
      })),
      ...noReferenceInputSteps.map((step) => ({
        category: "noReferenceInput" as const,
        step,
      })),
      ...referenceInputNoIdxSteps.map((step) => ({
        category: "referenceInputNoIdx" as const,
        step,
      })),
      ...invalidSignatureSteps.map((step) => ({
        category: "invalidSignature" as const,
        step,
      })),
      ...transitionTraceSteps.map((step) => ({
        category: "transitionTrace" as const,
        step,
      })),
      ...validationTraceDisputeSteps.map((step) => ({
        category: "validationTraceDispute" as const,
        step,
      })),
      ...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(11).flatMap((category) =>
        registeredCategories[category].steps.map((step) => ({
          category,
          step,
        })),
      ),
    ];
    const oversizedAppliedSpendingScripts =
      categorizedAppliedSpendingScripts.flatMap(({ category, step }) =>
        step.withinL1TransactionByteEnvelopeNecessaryCondition
          ? []
          : [
              {
                category,
                name: step.name,
                scriptHash: step.scriptHash,
                standaloneScriptBytes: step.standaloneScriptBytes,
              },
            ],
      );
    const categoryFirstStepHash =
      contracts.doubleSpend.firstStep.spendingScriptHash;
    const nonExistentInputCategoryFirstStepHash =
      contracts.nonExistentInput.firstStep.spendingScriptHash;
    const invalidRangeCategoryFirstStepHash =
      contracts.invalidRange.firstStep.spendingScriptHash;
    const zeroInputCategoryFirstStepHash =
      contracts.zeroInput.firstStep.spendingScriptHash;
    const daHashPreimageCategoryFirstStepHash =
      contracts.daHashPreimage.firstStep.spendingScriptHash;
    const nonExistentInputNoIndexCategoryFirstStepHash =
      contracts.nonExistentInputNoIndex.firstStep.spendingScriptHash;
    const noReferenceInputCategoryFirstStepHash =
      contracts.noReferenceInput.firstStep.spendingScriptHash;
    const referenceInputNoIdxCategoryFirstStepHash =
      contracts.referenceInputNoIdx.firstStep.spendingScriptHash;
    const invalidSignatureCategoryFirstStepHash =
      contracts.invalidSignature.firstStep.spendingScriptHash;
    const transitionTraceCategoryFirstStepHash =
      contracts.transitionTrace.firstStep.spendingScriptHash;
    const validationTraceDisputeCategoryFirstStepHash =
      contracts.validationTraceDispute.firstStep.spendingScriptHash;
    const deploymentDoubleSpendMatchesFirstStep =
      deploymentDoubleSpendScriptHash === null
        ? null
        : deploymentDoubleSpendScriptHash === categoryFirstStepHash;
    const deploymentNonExistentInputMatchesFirstStep =
      deploymentNonExistentInputScriptHash === null
        ? null
        : deploymentNonExistentInputScriptHash ===
          nonExistentInputCategoryFirstStepHash;
    const deploymentInvalidRangeMatchesFirstStep =
      deploymentInvalidRangeScriptHash === null
        ? null
        : deploymentInvalidRangeScriptHash ===
          invalidRangeCategoryFirstStepHash;
    const deploymentZeroInputMatchesFirstStep =
      deploymentZeroInputScriptHash === null
        ? null
        : deploymentZeroInputScriptHash === zeroInputCategoryFirstStepHash;
    const deploymentDaHashPreimageMatchesFirstStep =
      deploymentDaHashPreimageScriptHash === null
        ? null
        : deploymentDaHashPreimageScriptHash ===
          daHashPreimageCategoryFirstStepHash;
    const deploymentNonExistentInputNoIndexMatchesFirstStep =
      nonExistentInputNoIndexIdentity.deploymentScriptHash === null
        ? null
        : nonExistentInputNoIndexIdentity.deploymentScriptHash ===
          nonExistentInputNoIndexCategoryFirstStepHash;
    const deploymentNoReferenceInputMatchesFirstStep =
      deploymentNoReferenceInputScriptHash === null
        ? null
        : deploymentNoReferenceInputScriptHash ===
          noReferenceInputCategoryFirstStepHash;
    const deploymentReferenceInputNoIdxMatchesFirstStep =
      deploymentReferenceInputNoIdxScriptHash === null
        ? null
        : deploymentReferenceInputNoIdxScriptHash ===
          referenceInputNoIdxCategoryFirstStepHash;
    const deploymentInvalidSignatureMatchesFirstStep =
      deploymentInvalidSignatureScriptHash === null
        ? null
        : deploymentInvalidSignatureScriptHash ===
          invalidSignatureCategoryFirstStepHash;
    const deploymentTransitionTraceMatchesFirstStep =
      deploymentTransitionTraceScriptHash === null
        ? null
        : deploymentTransitionTraceScriptHash ===
          transitionTraceCategoryFirstStepHash;
    const deploymentValidationTraceDisputeMatchesFirstStep =
      deploymentValidationTraceDisputeScriptHash === null
        ? null
        : deploymentValidationTraceDisputeScriptHash ===
          validationTraceDisputeCategoryFirstStepHash;
    const expectedFirstStepHashes = completeFraudProofCategoryRecord(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
        (category) =>
          [
            category,
            registeredCategories[category].categoryFirstStepHash,
          ] as const,
      ),
    );
    const fraudProofCatalogue = yield* inspectFraudProofCatalogue(
      deployedFraudProofCatalogue,
      expectedFirstStepHashes,
    );

    return {
      network,
      l1SpendingScriptEnvelopeNecessaryCondition: {
        maxTransactionBytes: MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
        appliedSpendingScriptCount: categorizedAppliedSpendingScripts.length,
        allAppliedSpendingScriptsWithinEnvelope:
          oversizedAppliedSpendingScripts.length === 0,
        oversizedAppliedSpendingScripts,
      },
      computationThread: {
        policyId: contracts.computationThread.policyId,
      },
      fraudProof: {
        policyId: contracts.fraudProof.policyId,
        address: contracts.fraudProof.spendingScriptAddress,
        spendingScriptHash: contracts.fraudProof.spendingScriptHash,
      },
      fraudProofCatalogue,
      registeredCategories,
      doubleSpend: {
        categoryFirstStepHash,
        deploymentDoubleSpendScriptHash,
        deploymentDoubleSpendMatchesFirstStep,
        steps,
      },
      nonExistentInput: {
        categoryFirstStepHash: nonExistentInputCategoryFirstStepHash,
        deploymentNonExistentInputScriptHash,
        deploymentNonExistentInputMatchesFirstStep,
        steps: nonExistentInputSteps,
      },
      invalidRange: {
        categoryFirstStepHash: invalidRangeCategoryFirstStepHash,
        deploymentInvalidRangeScriptHash,
        deploymentInvalidRangeMatchesFirstStep,
        steps: invalidRangeSteps,
      },
      zeroInput: {
        categoryFirstStepHash: zeroInputCategoryFirstStepHash,
        deploymentZeroInputScriptHash,
        deploymentZeroInputMatchesFirstStep,
        steps: [
          stepOutput("step01", contracts.zeroInput.steps[0]),
          stepOutput("step02", contracts.zeroInput.steps[1]),
        ],
      },
      daHashPreimage: {
        categoryFirstStepHash: daHashPreimageCategoryFirstStepHash,
        deploymentDaHashPreimageScriptHash,
        deploymentDaHashPreimageMatchesFirstStep,
        steps: daHashPreimageSteps,
      },
      nonExistentInputNoIndex: {
        categoryFirstStepHash: nonExistentInputNoIndexCategoryFirstStepHash,
        deploymentNonExistentInputNoIndexScriptHash:
          nonExistentInputNoIndexIdentity.deploymentScriptHash,
        deploymentNonExistentInputNoIndexMatchesFirstStep,
        deploymentMatchesEmbeddedScriptBytes:
          nonExistentInputNoIndexIdentity.deploymentMatchesScriptBytes,
        steps: nonExistentInputNoIndexSteps,
      },
      noReferenceInput: {
        categoryFirstStepHash: noReferenceInputCategoryFirstStepHash,
        deploymentNoReferenceInputScriptHash,
        deploymentNoReferenceInputMatchesFirstStep,
        steps: noReferenceInputSteps,
      },
      referenceInputNoIdx: {
        categoryFirstStepHash: referenceInputNoIdxCategoryFirstStepHash,
        deploymentReferenceInputNoIdxScriptHash,
        deploymentReferenceInputNoIdxMatchesFirstStep,
        steps: referenceInputNoIdxSteps,
      },
      invalidSignature: {
        categoryFirstStepHash: invalidSignatureCategoryFirstStepHash,
        deploymentInvalidSignatureScriptHash,
        deploymentInvalidSignatureMatchesFirstStep,
        steps: invalidSignatureSteps,
      },
      transitionTrace: {
        semanticYields: Object.entries(contracts.transitionTrace.yields).map(
          ([name, validator]) => {
            const standaloneScriptBytes = Buffer.from(
              validator.withdrawalScriptCBOR,
              "hex",
            ).byteLength;
            return {
              name,
              scriptHash: validator.withdrawalScriptHash,
              standaloneScriptBytes,
              withinL1TransactionByteEnvelopeNecessaryCondition:
                standaloneScriptBytes <
                MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
            };
          },
        ),
        categoryFirstStepHash: transitionTraceCategoryFirstStepHash,
        deploymentTransitionTraceScriptHash,
        deploymentTransitionTraceMatchesFirstStep,
        steps: transitionTraceSteps,
      },
      validationTraceDispute: {
        categoryFirstStepHash: validationTraceDisputeCategoryFirstStepHash,
        deploymentValidationTraceDisputeScriptHash,
        deploymentValidationTraceDisputeMatchesFirstStep,
        steps: validationTraceDisputeSteps,
      },
    };
  });
