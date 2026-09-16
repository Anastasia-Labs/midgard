import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { inspectContracts } from "@al-ft/midgard-fault-proofs";
import {
  createReferenceScriptAuthPolicy,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueDeploymentInfo,
  referenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { buildContractDeploymentInfoFromContracts } from "../src/commands/contract-deployment-info.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "../src/transactions/initialization.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repoRoot, "onchain/aiken/plutus.json");

const buildInspectionFixture = async () => {
  const blueprintJson: unknown = JSON.parse(
    readFileSync(blueprintPath, "utf8"),
  );
  const publisher = generateEmulatorAccount({ lovelace: 10_000_000n });
  const lucid = await Lucid(new Emulator([publisher]), "Preprod");
  lucid.selectWallet.fromSeed(publisher.seedPhrase);
  const authPolicy = await createReferenceScriptAuthPolicy(
    lucid,
    1_900_000_000_000,
  );
  const appliedContracts = await loadRealMidgardContractsForTest(
    { txHash: "ab".repeat(32), outputIndex: 0 },
    authPolicy,
  );
  const fraudProofCatalogue = await Effect.runPromise(
    buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(appliedContracts.fraudProofs),
    ),
  );
  // This is the production output before reference publication. The inspector
  // reports identities; live publication admission is tested separately.
  const deploymentInfo = buildContractDeploymentInfoFromContracts(
    appliedContracts,
    referenceScriptAuthPolicyDeploymentInfo(authPolicy),
    undefined,
    fraudProofCatalogue,
  );
  return {
    blueprintJson,
    contracts: appliedContracts.fraudProofContracts,
    fraudProofCatalogue,
    deploymentInfo,
  };
};

let inspectionFixture: Awaited<ReturnType<typeof buildInspectionFixture>>;

beforeAll(async () => {
  inspectionFixture = await buildInspectionFixture();
}, 30_000);

describe("inspect-contracts", { timeout: 30_000 }, () => {
  it("reports identities and script sizes from the current build", async () => {
    const fixture = inspectionFixture;
    const { blueprintJson, contracts, fraudProofCatalogue } = fixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: blueprintJson,
        network: "Preprod",
        deploymentInfo: fixture.deploymentInfo,
      }),
    );

    expect(output.network).toBe("Preprod");
    expect(output.computationThread.policyId).toBe(
      fixture.deploymentInfo.contracts.computationThreadMint!.scriptHash,
    );
    expect(output.fraudProof.policyId).toBe(
      fixture.deploymentInfo.contracts.fraudProofMint!.scriptHash,
    );
    expect(Object.keys(output.registeredCategories)).toEqual([
      ...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    ]);
    for (const category of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
      expect(
        output.registeredCategories[category].steps.map(
          (step) => step.scriptHash,
        ),
      ).toEqual(
        contracts[category].steps.map((step) => step.spendingScriptHash),
      );
      expect(output.fraudProofCatalogue.categories[category]).toEqual({
        categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category],
        expectedCategoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category],
        categoryIdMatchesExpected: true,
        scriptHash: contracts[category].firstStep.spendingScriptHash,
        scriptHashMatchesFirstStep: true,
        membershipProofCbor:
          fraudProofCatalogue.categories[category].membershipProofCbor,
        membershipProofMatchesDerived: true,
      });
    }
    const appliedSpendingScripts = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.flatMap(
      (category) => {
        const steps =
          category === "transitionTrace"
            ? output.transitionTrace.steps
            : category === "validationTraceDispute"
              ? output.validationTraceDispute.steps
              : output.registeredCategories[category].steps;
        return steps.map((step) => ({
          category,
          step,
        }));
      },
    );
    const selectedParameterizedValidators =
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.flatMap((category) =>
        category === "validationTraceDispute"
          ? [
              contracts.validationTraceDispute.opener,
              contracts.validationTraceDispute.source,
              contracts.validationTraceDispute.game,
              contracts.validationTraceDispute.boundary,
              contracts.validationTraceDispute.timeout,
              contracts.validationTraceDispute.award,
              ...contracts.validationTraceDispute.semanticResolvers,
              ...contracts.validationTraceDispute.prepareResolvers,
            ]
          : contracts[category].steps,
      );
    expect(appliedSpendingScripts).toHaveLength(
      selectedParameterizedValidators.length,
    );
    appliedSpendingScripts.forEach(({ step }, index) => {
      const selectedValidator = selectedParameterizedValidators[index];
      expect(selectedValidator).toBeDefined();
      const standaloneScriptBytes = Buffer.from(
        selectedValidator?.spendingScriptCBOR ?? "",
        "hex",
      ).byteLength;
      expect(step.standaloneScriptBytes).toBe(standaloneScriptBytes);
      expect(step.withinL1TransactionByteEnvelopeNecessaryCondition).toBe(
        standaloneScriptBytes <
          MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
      );
    });
    const expectedOversized = appliedSpendingScripts.flatMap(
      ({ category, step }) =>
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
    expect(output.l1SpendingScriptEnvelopeNecessaryCondition).toEqual({
      maxTransactionBytes: MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
      appliedSpendingScriptCount: appliedSpendingScripts.length,
      allAppliedSpendingScriptsWithinEnvelope: expectedOversized.length === 0,
      oversizedAppliedSpendingScripts: expectedOversized,
    });
    expect(output.fraudProofCatalogue.root).toBe(fraudProofCatalogue.root);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
  });

  it("reports a catalogue entry-point mismatch", async () => {
    const fixture = inspectionFixture;
    const catalogue = fixture.fraudProofCatalogue;
    const deploymentInfo = {
      ...fixture.deploymentInfo,
      contracts: {
        ...fixture.deploymentInfo.contracts,
        fraudProofCatalogueMint: {
          ...fixture.deploymentInfo.contracts.fraudProofCatalogueMint,
          fraudProofCatalogue: {
            ...catalogue,
            categories: {
              ...catalogue.categories,
              zeroInput: {
                ...catalogue.categories.zeroInput,
                scriptHash: "03".repeat(28),
              },
            },
          },
        },
      },
    };

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo,
      }),
    );

    expect(output.zeroInput.deploymentZeroInputMatchesFirstStep).toBe(true);
    expect(
      output.fraudProofCatalogue.zeroInput.scriptHashMatchesFirstStep,
    ).toBe(false);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(false);
  });

  it("reports an absent deployment entry point", async () => {
    const fixture = inspectionFixture;
    const deploymentInfo = fixture.deploymentInfo;
    const contractsWithoutZeroInput = Object.fromEntries(
      Object.entries(deploymentInfo.contracts).filter(
        ([name]) => name !== "fraudProofZeroInput",
      ),
    );

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: {
          ...deploymentInfo,
          contracts: contractsWithoutZeroInput,
        },
      }),
    );

    expect(output.zeroInput.deploymentZeroInputScriptHash).toBeNull();
    expect(output.zeroInput.deploymentZeroInputMatchesFirstStep).toBeNull();
  });

  it("rejects deployment info with a mismatched fraud-proof policy", async () => {
    const fixture = inspectionFixture;
    const deploymentInfo = {
      ...fixture.deploymentInfo,
      contracts: {
        ...fixture.deploymentInfo.contracts,
        fraudProofMint: {
          ...fixture.deploymentInfo.contracts.fraudProofMint,
          scriptHash: "33".repeat(28),
        },
      },
    };
    await expect(
      Effect.runPromise(
        inspectContracts({
          blueprint: fixture.blueprintJson,
          network: "Preprod",
          deploymentInfo,
        }),
      ),
    ).rejects.toThrow("fraudProofMint.scriptHash mismatch");
  });

  it("rejects a contracts-only deployment-info object", async () => {
    const fixture = inspectionFixture;
    await expect(
      Effect.runPromise(
        inspectContracts({
          blueprint: fixture.blueprintJson,
          network: "Preprod",
          deploymentInfo: fixture.deploymentInfo.contracts,
        }),
      ),
    ).rejects.toThrow(
      "Contract deployment info is missing referenceScriptAuthPolicy.",
    );
  });

  it("reports when deployed script bytes disagree with their declared hash", async () => {
    const fixture = inspectionFixture;
    const deploymentInfo = {
      ...fixture.deploymentInfo,
      contracts: {
        ...fixture.deploymentInfo.contracts,
        fraudProofNonExistentInputNoIndex: {
          ...fixture.deploymentInfo.contracts.fraudProofNonExistentInputNoIndex,
          scriptHash: "77".repeat(28),
        },
      },
    };

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo,
      }),
    );

    expect(
      output.nonExistentInputNoIndex.deploymentMatchesEmbeddedScriptBytes,
    ).toBe(false);
    expect(
      output.nonExistentInputNoIndex
        .deploymentNonExistentInputNoIndexMatchesFirstStep,
    ).toBe(false);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
  });

  it("rejects deployment info with non-canonical fraud-proof category IDs", async () => {
    const fixture = inspectionFixture;
    const deploymentInfo = fixture.deploymentInfo;
    const invalidCatalogue: FraudProofCatalogueDeploymentInfo = {
      ...fixture.fraudProofCatalogue,
      categories: {
        ...fixture.fraudProofCatalogue.categories,
        invalidRange: {
          ...fixture.fraudProofCatalogue.categories.invalidRange,
          categoryId: "ffffffff",
        },
      },
    };

    await expect(
      Effect.runPromise(
        inspectContracts({
          blueprint: fixture.blueprintJson,
          network: "Preprod",
          deploymentInfo: {
            ...deploymentInfo,
            contracts: {
              ...deploymentInfo.contracts,
              fraudProofCatalogueMint: {
                ...deploymentInfo.contracts.fraudProofCatalogueMint,
                fraudProofCatalogue: invalidCatalogue,
              },
            },
          },
        }),
      ),
    ).rejects.toThrow(
      "fraudProofCatalogue.categories.invalidRange.categoryId must be 00000003",
    );
  });

  it("rejects deployment info with duplicated fraud-proof category IDs", async () => {
    const fixture = inspectionFixture;
    const deploymentInfo = fixture.deploymentInfo;
    const invalidCatalogue: FraudProofCatalogueDeploymentInfo = {
      ...fixture.fraudProofCatalogue,
      categories: {
        ...fixture.fraudProofCatalogue.categories,
        invalidRange: {
          ...fixture.fraudProofCatalogue.categories.invalidRange,
          categoryId:
            fixture.fraudProofCatalogue.categories.doubleSpend.categoryId,
        },
      },
    };

    await expect(
      Effect.runPromise(
        inspectContracts({
          blueprint: fixture.blueprintJson,
          network: "Preprod",
          deploymentInfo: {
            ...deploymentInfo,
            contracts: {
              ...deploymentInfo.contracts,
              fraudProofCatalogueMint: {
                ...deploymentInfo.contracts.fraudProofCatalogueMint,
                fraudProofCatalogue: invalidCatalogue,
              },
            },
          },
        }),
      ),
    ).rejects.toThrow(
      "fraudProofCatalogue.categories.invalidRange.categoryId duplicates",
    );
  });
});
