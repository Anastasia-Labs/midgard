import {
  buildDoubleSpendFaultProofContracts,
  buildFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  DOUBLE_SPEND_FAULT_PROOF_TITLES,
  FAULT_PROOF_SHARED_TITLES,
  type FraudProofCatalogueDeploymentInfo,
  INVALID_RANGE_FAULT_PROOF_TITLES,
  TRANSITION_TRACE_FAULT_PROOF_TITLES,
  TRANSITION_TRACE_YIELD_TITLES,
  VALIDATION_TRACE_DISPUTE_STEP_COUNT,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  resolveDoubleSpendDeploymentContracts,
  resolveInvalidRangeDeploymentContracts,
  resolveTransitionTraceDeploymentContracts,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../src/index.js";
import { resolveNonExistentInputNoIndexInit } from "../src/submit-init.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "../src/transition-trace/yield-references.js";
import {
  catalogueFor,
  deploymentManifest,
  filterBlueprint,
  h28,
  h28b,
  placeholderInvalidRange,
  readBlueprint,
  referenceScriptAuthNativeScript,
} from "./submit-init.submit-init-signer-resolution.js";
import { submitInit } from "./support/legacy-submit-emulator.js";

describe("fault-proof deployment contract resolution", () => {
  it("preflights no-index init from exact deployed bytes and catalogue membership", async () => {
    const blueprint = readBlueprint();
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
        referenceScriptAuthPolicyId: h28b,
      }),
    );
    // Q13/F20-01: the no-index first step is now derived from the compiled
    // blueprint like every other family, so the deployment must record exactly
    // the applied `input_no_idx/step_01` script.
    const noIndexFirstStep = contracts.nonExistentInputNoIndex.firstStep;
    const noIndexContract = {
      type: "PlutusV3" as const,
      cborHex: noIndexFirstStep.spendingScript.script,
    };
    const noIndexScriptHash = noIndexFirstStep.spendingScriptHash;
    const fraudProofCatalogue = await catalogueFor({
      nonExistentInput: contracts.nonExistentInput.firstStep.spendingScriptHash,
      nonExistentInputNoIndex: noIndexScriptHash,
    });
    const fraudProofCatalogueMintEntry = {
      scriptHash: h28b,
      fraudProofCatalogue,
    };
    const noIndexDeploymentEntry = {
      scriptHash: noIndexScriptHash,
      contract: noIndexContract,
    };
    const deploymentInfo = deploymentManifest({
      hubOracleMint: { scriptHash: h28 },
      fraudProofCatalogueMint: fraudProofCatalogueMintEntry,
      fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
      fraudProofNonExistentInput: {
        scriptHash: contracts.nonExistentInput.firstStep.spendingScriptHash,
      },
      fraudProofNonExistentInputNoIndex: noIndexDeploymentEntry,
      stateQueueMint: { scriptHash: "66".repeat(28) },
    });

    const resolved = await resolveNonExistentInputNoIndexInit({
      blueprint,
      deploymentInfo,
      network: "Preprod",
    });
    expect(resolved.firstStepHash).toBe(noIndexScriptHash);
    expect(resolved.category).toEqual(
      fraudProofCatalogue.categories.nonExistentInputNoIndex,
    );
    expect(resolved.category.categoryId).toBe("00000002");
    expect(resolved.stateQueuePolicyId).toBe("66".repeat(28));

    expect(resolved.firstStepAddress).toBe(
      noIndexFirstStep.spendingScriptAddress,
    );

    const { contract: _contract, ...withoutContract } = noIndexDeploymentEntry;
    await expect(
      resolveNonExistentInputNoIndexInit({
        blueprint: {},
        deploymentInfo: {
          ...deploymentInfo,
          contracts: {
            ...deploymentInfo.contracts,
            fraudProofNonExistentInputNoIndex: withoutContract,
          },
        },
        network: "Preprod",
      }),
    ).rejects.toThrow(/missing embedded contract bytes/u);

    await expect(
      resolveNonExistentInputNoIndexInit({
        blueprint: {},
        deploymentInfo: {
          ...deploymentInfo,
          contracts: {
            ...deploymentInfo.contracts,
            fraudProofNonExistentInputNoIndex: {
              ...noIndexDeploymentEntry,
              contract: {
                ...noIndexContract,
                cborHex: "02",
              },
            },
          },
        },
        network: "Preprod",
      }),
    ).rejects.toThrow(/script hash mismatch/u);

    // Self-consistent embedded bytes that are not the applied step-01 script
    // must still fail closed.
    const foreignContract = { type: "PlutusV3" as const, cborHex: "01" };
    const foreignScriptHash = validatorToScriptHash({
      type: foreignContract.type,
      script: foreignContract.cborHex,
    });
    await expect(
      resolveNonExistentInputNoIndexInit({
        blueprint,
        deploymentInfo: {
          ...deploymentInfo,
          contracts: {
            ...deploymentInfo.contracts,
            fraudProofNonExistentInputNoIndex: {
              scriptHash: foreignScriptHash,
              contract: foreignContract,
            },
          },
        },
        network: "Preprod",
      }),
    ).rejects.toThrow(
      /fraudProofNonExistentInputNoIndex step-01 script mismatch/u,
    );

    await expect(
      resolveNonExistentInputNoIndexInit({
        blueprint,
        deploymentInfo: {
          ...deploymentInfo,
          contracts: {
            ...deploymentInfo.contracts,
            fraudProofCatalogueMint: {
              ...fraudProofCatalogueMintEntry,
              fraudProofCatalogue: {
                ...fraudProofCatalogue,
                categories: {
                  ...fraudProofCatalogue.categories,
                  nonExistentInputNoIndex: {
                    ...fraudProofCatalogue.categories.nonExistentInputNoIndex,
                    membershipProofCbor:
                      fraudProofCatalogue.categories.doubleSpend
                        .membershipProofCbor,
                  },
                },
              },
            },
          },
        },
        network: "Preprod",
      }),
    ).rejects.toThrow(
      /nonExistentInputNoIndex\.membershipProofCbor does not match/u,
    );
  }, 30_000);

  it("resolves double-spend without requiring invalid-range validators in the blueprint", async () => {
    const blueprint = filterBlueprint(readBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(DOUBLE_SPEND_FAULT_PROOF_TITLES),
    ]);
    const contracts = await Effect.runPromise(
      buildDoubleSpendFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
        referenceScriptAuthPolicyId: h28b,
      }),
    );
    const fraudProofCatalogue = await catalogueFor({
      doubleSpend: contracts.doubleSpend.firstStep.spendingScriptHash,
    });

    const resolved = await resolveDoubleSpendDeploymentContracts({
      blueprint,
      deploymentInfo: deploymentManifest({
        hubOracleMint: { scriptHash: h28 },
        fraudProofCatalogueMint: {
          scriptHash: h28b,
          fraudProofCatalogue,
        },
        fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
        fraudProofDoubleSpend: {
          scriptHash: contracts.doubleSpend.firstStep.spendingScriptHash,
        },
      }),
      network: "Preprod",
    });

    expect(resolved.doubleSpendCategory.categoryId).toBe("00000000");
    expect(resolved.contracts.doubleSpend.steps).toHaveLength(4);
  });

  it("requires the invalid-range deployment entry for invalid-range resolution", async () => {
    const blueprint = readBlueprint();
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
        referenceScriptAuthPolicyId: h28b,
      }),
    );
    const fraudProofCatalogue = await catalogueFor({
      invalidRange: contracts.invalidRange.firstStep.spendingScriptHash,
    });

    await expect(
      resolveInvalidRangeDeploymentContracts({
        blueprint: filterBlueprint(blueprint, [
          ...Object.values(FAULT_PROOF_SHARED_TITLES),
          ...Object.values(INVALID_RANGE_FAULT_PROOF_TITLES),
        ]),
        deploymentInfo: deploymentManifest({
          hubOracleMint: { scriptHash: h28 },
          fraudProofCatalogueMint: {
            scriptHash: h28b,
            fraudProofCatalogue,
          },
          fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
        }),
        network: "Preprod",
      }),
    ).rejects.toThrow('Deployment info is missing "fraudProofInvalidRange"');
  }, 30_000);

  it("rejects invalid-range resolution when the catalogue membership proof is stale", async () => {
    const blueprint = readBlueprint();
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
        referenceScriptAuthPolicyId: h28b,
      }),
    );
    const fraudProofCatalogue = await catalogueFor({
      invalidRange: contracts.invalidRange.firstStep.spendingScriptHash,
    });
    const staleCatalogue: FraudProofCatalogueDeploymentInfo = {
      ...fraudProofCatalogue,
      categories: {
        ...fraudProofCatalogue.categories,
        invalidRange: {
          ...fraudProofCatalogue.categories.invalidRange,
          membershipProofCbor:
            fraudProofCatalogue.categories.doubleSpend.membershipProofCbor,
        },
      },
    };

    await expect(
      resolveInvalidRangeDeploymentContracts({
        blueprint: filterBlueprint(blueprint, [
          ...Object.values(FAULT_PROOF_SHARED_TITLES),
          ...Object.values(INVALID_RANGE_FAULT_PROOF_TITLES),
        ]),
        deploymentInfo: deploymentManifest({
          hubOracleMint: { scriptHash: h28 },
          fraudProofCatalogueMint: {
            scriptHash: h28b,
            fraudProofCatalogue: staleCatalogue,
          },
          fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
          fraudProofInvalidRange: {
            scriptHash: contracts.invalidRange.firstStep.spendingScriptHash,
          },
        }),
        network: "Preprod",
      }),
    ).rejects.toThrow(
      "fraudProofCatalogue.categories.invalidRange.membershipProofCbor does not match",
    );
  }, 30_000);

  it("resolves transition-trace without requiring staged fault-proof validators in the blueprint", async () => {
    const blueprint = filterBlueprint(readBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(TRANSITION_TRACE_FAULT_PROOF_TITLES),
      ...Object.values(TRANSITION_TRACE_YIELD_TITLES),
      "user_events/history_data.retention.spend",
    ]);
    const contracts = await Effect.runPromise(
      buildTransitionTraceFaultProofContracts({
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
        referenceScriptAuthPolicyId: validatorToScriptHash({
          type: "Native",
          script: referenceScriptAuthNativeScript,
        }),
      }),
    );
    const fraudProofCatalogue = await catalogueFor({
      transitionTrace: contracts.transitionTrace.firstStep.spendingScriptHash,
    });
    const resolved = await resolveTransitionTraceDeploymentContracts({
      blueprint,
      deploymentInfo: deploymentManifest({
        hubOracleMint: { scriptHash: h28 },
        fraudProofCatalogueMint: {
          scriptHash: h28b,
          fraudProofCatalogue,
        },
        fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
        ...Object.fromEntries(
          Object.entries(TRANSITION_TRACE_YIELD_REFERENCES).map(
            ([key, reference]) => [
              reference.entry,
              {
                scriptHash:
                  contracts.transitionTrace.yields[
                    key as keyof typeof TRANSITION_TRACE_YIELD_REFERENCES
                  ].withdrawalScriptHash,
              },
            ],
          ),
        ),
        fraudProofTransitionTrace: {
          eventHistoryBounds: {
            inlineLimitBytes: "512",
            maxPayloadBytes: "5000",
            maxPayloadNodes: "512",
          },
          eventHistoryRetentionAddresses:
            contracts.transitionTrace.history.retentionAddresses,
          scriptHash: contracts.transitionTrace.firstStep.spendingScriptHash,
        },
      }),
      network: "Preprod",
    });

    expect(resolved.transitionTraceCategory.categoryId).toBe("00000004");
    expect(resolved.contracts.transitionTrace.steps).toHaveLength(9);
    expect(resolved.contracts.transitionTrace.finals).toHaveLength(8);
    expect(resolved.contracts.transitionTrace.steps[0]).toBe(
      resolved.contracts.transitionTrace.route,
    );
  });

  it("resolves the required V1 validation-dispute category and rejects an incomplete catalogue", async () => {
    const blueprint = readBlueprint();
    const contracts = await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
        referenceScriptAuthPolicyId: validatorToScriptHash({
          type: "Native",
          script: referenceScriptAuthNativeScript,
        }),
      }),
    );
    const proofCatalogue = await catalogueFor({
      validationTraceDispute:
        contracts.validationTraceDispute.firstStep.spendingScriptHash,
    });
    const deploymentInfo = deploymentManifest({
      hubOracleMint: { scriptHash: h28 },
      fraudProofCatalogueMint: {
        scriptHash: h28b,
        fraudProofCatalogue: proofCatalogue,
      },
      fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
      validationTraceDispute: {
        scriptHash:
          contracts.validationTraceDispute.firstStep.spendingScriptHash,
      },
      cekProgramMaterialSpend: {
        scriptHash:
          contracts.validationTraceDispute.cekProgramMaterial
            .spendingScriptHash,
      },
    });

    const resolved = await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network: "Preprod",
    });
    expect(resolved.validationTraceDisputeCategory.categoryId).toBe("00000006");
    expect(resolved.contracts.validationTraceDispute.steps).toHaveLength(
      VALIDATION_TRACE_DISPUTE_STEP_COUNT,
    );
    expect(
      resolved.contracts.validationTraceDispute.semanticResolvers,
    ).toHaveLength(91);
    expect(
      resolved.contracts.validationTraceDispute.prepareResolvers,
    ).toHaveLength(14);
    expect(resolved.contracts.validationTraceDispute.resolvers).toHaveLength(
      14,
    );
    expect(resolved.cekProgramMaterialScriptHash).toBe(
      contracts.validationTraceDispute.cekProgramMaterial.spendingScriptHash,
    );
    expect(resolved.cekProgramMaterialAddress).toBe(
      contracts.validationTraceDispute.cekProgramMaterial.spendingScriptAddress,
    );

    await expect(
      resolveValidationTraceDisputeDeploymentContracts({
        blueprint,
        deploymentInfo: deploymentManifest({
          ...deploymentInfo.contracts,
          cekProgramMaterialSpend: { scriptHash: "ff".repeat(28) },
        }),
        network: "Preprod",
      }),
    ).rejects.toThrow(/cekProgramMaterialSpend script mismatch/u);

    const { validationTraceDispute: _omitted, ...incompleteCategories } =
      proofCatalogue.categories;
    await expect(
      resolveValidationTraceDisputeDeploymentContracts({
        blueprint,
        deploymentInfo: deploymentManifest({
          ...deploymentInfo.contracts,
          fraudProofCatalogueMint: {
            scriptHash: h28b,
            fraudProofCatalogue: {
              ...proofCatalogue,
              categories: incompleteCategories,
            },
          },
        }),
        network: "Preprod",
      }),
    ).rejects.toThrow(/categories\.validationTraceDispute/u);
    // This row resolves the full V1 validation-dispute catalogue twice, which
    // measures ~22 s uncontended but timed out at its previous 60,000 ms
    // budget in 2 of 4 Midgard Node CI runs (#616) on a step that itself runs
    // ~46 minutes on shared runners. That is runner contention, not a
    // slowdown, so the budget follows midgard-node's 420 s precedent for
    // contention-exposed steps rather than being absorbed into an accepted-red
    // map. If it still times out here, #616 asks for a diagnosis instead of
    // another raise.
  }, 420_000);

  it("does not gate double-spend submit-init on stale invalid-range deployment readiness", async () => {
    const blueprint = readBlueprint();
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
        referenceScriptAuthPolicyId: h28b,
      }),
    );
    const fraudProofCatalogue = await catalogueFor({
      doubleSpend: contracts.doubleSpend.firstStep.spendingScriptHash,
      invalidRange: contracts.invalidRange.firstStep.spendingScriptHash,
    });
    const fakeLucid = {
      utxosAtWithUnit: async () => {
        throw new Error("fetch attempted after double-spend readiness");
      },
      utxosByOutRef: async () => [],
    };

    await expect(
      submitInit({
        lucid: fakeLucid as never,
        blueprint,
        deploymentInfo: deploymentManifest({
          hubOracleMint: { scriptHash: h28 },
          stateQueueMint: { scriptHash: "33".repeat(28) },
          fraudProofCatalogueMint: {
            scriptHash: h28b,
            fraudProofCatalogue,
          },
          fraudProofCatalogueSpend: { scriptHash: "44".repeat(28) },
          fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
          fraudProofSpend: {
            scriptHash: contracts.fraudProof.spendingScriptHash,
          },
          fraudProofDoubleSpend: {
            scriptHash: contracts.doubleSpend.firstStep.spendingScriptHash,
          },
          fraudProofInvalidRange: {
            scriptHash: placeholderInvalidRange,
          },
        }),
        network: "Preprod",
        signer: {
          source: "test",
          address:
            "addr_test1vqz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3d7t0s7uqthvq7n",
          paymentKeyHash: h28,
          selectWallet: () => undefined,
        },
        fraudulentBlockOutRef: `${"aa".repeat(32)}#0`,
        awaitConfirmation: false,
      }),
    ).rejects.toThrow("fetch attempted after double-spend readiness");
  }, 30_000);

  it("does not gate invalid-range submit-init on stale double-spend deployment readiness", async () => {
    const blueprint = readBlueprint();
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
        referenceScriptAuthPolicyId: h28b,
      }),
    );
    const fraudProofCatalogue = await catalogueFor({
      doubleSpend: contracts.doubleSpend.firstStep.spendingScriptHash,
      invalidRange: contracts.invalidRange.firstStep.spendingScriptHash,
    });
    const fakeLucid = {
      utxosAtWithUnit: async () => {
        throw new Error("fetch attempted after invalid-range readiness");
      },
      utxosByOutRef: async () => [],
    };

    await expect(
      submitInit({
        lucid: fakeLucid as never,
        blueprint,
        deploymentInfo: deploymentManifest({
          hubOracleMint: { scriptHash: h28 },
          stateQueueMint: { scriptHash: "33".repeat(28) },
          fraudProofCatalogueMint: {
            scriptHash: h28b,
            fraudProofCatalogue,
          },
          fraudProofCatalogueSpend: { scriptHash: "44".repeat(28) },
          fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
          fraudProofSpend: {
            scriptHash: contracts.fraudProof.spendingScriptHash,
          },
          fraudProofDoubleSpend: {
            scriptHash: placeholderInvalidRange,
          },
          fraudProofInvalidRange: {
            scriptHash: contracts.invalidRange.firstStep.spendingScriptHash,
          },
        }),
        network: "Preprod",
        signer: {
          source: "test",
          address:
            "addr_test1vqz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3d7t0s7uqthvq7n",
          paymentKeyHash: h28,
          selectWallet: () => undefined,
        },
        fraudCategory: "invalidRange",
        fraudulentBlockOutRef: `${"aa".repeat(32)}#0`,
        awaitConfirmation: false,
      }),
    ).rejects.toThrow("fetch attempted after invalid-range readiness");
  }, 30_000);
});
