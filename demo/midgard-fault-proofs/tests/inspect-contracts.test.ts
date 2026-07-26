import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildFaultProofContracts,
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueDeploymentInfo,
  parseFaultProofBlueprint,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { inspectContracts } from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const blueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");

const h28 = "11".repeat(28);
const h28b = "22".repeat(28);
const placeholderDoubleSpend = "00".repeat(28);
const placeholderNonExistentInput = "02".repeat(28);
const placeholderInvalidRange = "01".repeat(28);
const categoryIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});
type LucidDataSchema = Parameters<typeof Data.to>[1];

const deploymentManifest = (contracts: Record<string, unknown>) => ({
  referenceScriptAuthPolicy: {},
  contracts,
});

const categoryId = (index: number): string => {
  const buf = Buffer.alloc(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT);
  buf.writeUInt32BE(index);
  return buf.toString("hex");
};

const encodeCatalogueKey = (id: string): Buffer =>
  Buffer.from(
    Data.to(id, categoryIdSchema as unknown as LucidDataSchema),
    "hex",
  );

const encodeCatalogueValue = (scriptHash: string): Buffer =>
  Buffer.from(
    Data.to(scriptHash, ScriptHashSchema as unknown as LucidDataSchema),
    "hex",
  );

const trieRootHex = (trie: Trie): string =>
  trie.hash == null
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

const readBlueprintJson = (): unknown =>
  JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown;

const buildCatalogueFixture = async (
  scriptHashes: Partial<Record<string, string>>,
): Promise<FraudProofCatalogueDeploymentInfo> => {
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name, index) => [
      name,
      {
        categoryId: categoryId(index),
        scriptHash:
          scriptHashes[name] ??
          `${(index + 3).toString(16).padStart(2, "0")}`.repeat(28),
        membershipProofCbor: "",
      },
    ]),
  ) as FraudProofCatalogueDeploymentInfo["categories"];

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    await trie.insert(
      encodeCatalogueKey(category.categoryId),
      encodeCatalogueValue(category.scriptHash),
    );
  }

  const withProofs = { ...categories };
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
    withProofs[name] = {
      ...category,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }

  return {
    root: trieRootHex(trie),
    categories: withProofs,
  };
};

const buildInspectionFixture = async () => {
  const blueprintJson = readBlueprintJson();
  const contracts = await Effect.runPromise(
    buildFaultProofContracts({
      blueprint: parseFaultProofBlueprint(blueprintJson),
      network: "Preprod",
      hubOraclePolicyId: h28,
      fraudProofCataloguePolicyId: h28b,
    }),
  );
  const fraudProofCatalogue = await buildCatalogueFixture({
    doubleSpend: contracts.doubleSpend.firstStep.spendingScriptHash,
    nonExistentInput: contracts.nonExistentInput.firstStep.spendingScriptHash,
    invalidRange: contracts.invalidRange.firstStep.spendingScriptHash,
    transitionTrace: contracts.transitionTrace.firstStep.spendingScriptHash,
    validationTraceDispute:
      contracts.validationTraceDispute.firstStep.spendingScriptHash,
  });
  return { blueprintJson, contracts, fraudProofCatalogue };
};

let inspectionFixture: Awaited<ReturnType<typeof buildInspectionFixture>>;

beforeAll(async () => {
  inspectionFixture = await buildInspectionFixture();
}, 30_000);

const deploymentInfoFor = (
  {
    contracts,
    fraudProofCatalogue,
  }: Awaited<ReturnType<typeof buildInspectionFixture>>,
  doubleSpendScriptHash = contracts.doubleSpend.firstStep.spendingScriptHash,
  invalidRangeScriptHash = contracts.invalidRange.firstStep.spendingScriptHash,
  transitionTraceScriptHash = contracts.transitionTrace.firstStep
    .spendingScriptHash,
  nonExistentInputScriptHash = contracts.nonExistentInput.firstStep
    .spendingScriptHash,
) => ({
  referenceScriptAuthPolicy: {},
  contracts: {
    hubOracleMint: { scriptHash: h28 },
    fraudProofCatalogueMint: {
      scriptHash: h28b,
      fraudProofCatalogue,
    },
    fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
    fraudProofSpend: {
      scriptHash: contracts.fraudProof.spendingScriptHash,
    },
    fraudProofDoubleSpend: { scriptHash: doubleSpendScriptHash },
    fraudProofNonExistentInput: {
      scriptHash: nonExistentInputScriptHash,
    },
    fraudProofInvalidRange: { scriptHash: invalidRangeScriptHash },
    fraudProofTransitionTrace: { scriptHash: transitionTraceScriptHash },
    validationTraceDispute: {
      scriptHash: contracts.validationTraceDispute.firstStep.spendingScriptHash,
    },
  },
});

describe("inspect-contracts", () => {
  it("emits stable implemented-category inspection JSON with catalogue readiness", async () => {
    const fixture = inspectionFixture;
    const { blueprintJson, contracts, fraudProofCatalogue } = fixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(fixture),
      }),
    );

    expect(output.network).toBe("Preprod");
    expect(output.computationThread.policyId).toBe(
      contracts.computationThread.policyId,
    );
    expect(output.fraudProof.policyId).toBe(contracts.fraudProof.policyId);
    expect(output.doubleSpend.categoryFirstStepHash).toBe(
      contracts.doubleSpend.firstStep.spendingScriptHash,
    );
    expect(output.doubleSpend.steps.map((step) => step.name)).toEqual([
      "step01",
      "step02",
      "step03",
      "step04",
    ]);
    expect(output.doubleSpend.deploymentDoubleSpendScriptHash).toBe(
      contracts.doubleSpend.firstStep.spendingScriptHash,
    );
    expect(output.doubleSpend.deploymentDoubleSpendMatchesFirstStep).toBe(true);
    expect(output.nonExistentInput.categoryFirstStepHash).toBe(
      contracts.nonExistentInput.firstStep.spendingScriptHash,
    );
    expect(output.nonExistentInput.steps.map((step) => step.name)).toEqual([
      "step01",
      "step02",
      "step03",
      "step04",
    ]);
    expect(output.nonExistentInput.deploymentNonExistentInputScriptHash).toBe(
      contracts.nonExistentInput.firstStep.spendingScriptHash,
    );
    expect(
      output.nonExistentInput.deploymentNonExistentInputMatchesFirstStep,
    ).toBe(true);
    expect(output.invalidRange.categoryFirstStepHash).toBe(
      contracts.invalidRange.firstStep.spendingScriptHash,
    );
    expect(output.invalidRange.steps.map((step) => step.name)).toEqual([
      "step01",
      "step02",
    ]);
    expect(output.invalidRange.deploymentInvalidRangeScriptHash).toBe(
      contracts.invalidRange.firstStep.spendingScriptHash,
    );
    expect(output.invalidRange.deploymentInvalidRangeMatchesFirstStep).toBe(
      true,
    );
    expect(output.transitionTrace.categoryFirstStepHash).toBe(
      contracts.transitionTrace.firstStep.spendingScriptHash,
    );
    expect(output.transitionTrace.steps.map((step) => step.name)).toEqual([
      "route",
      "control",
      "source",
      "withdrawal",
      "forced",
      "accepted",
      "deposit",
      "l1Event",
      "duplicate",
    ]);
    expect(output.transitionTrace.deploymentTransitionTraceScriptHash).toBe(
      contracts.transitionTrace.firstStep.spendingScriptHash,
    );
    expect(
      output.transitionTrace.deploymentTransitionTraceMatchesFirstStep,
    ).toBe(true);
    expect(
      output.validationTraceDispute.steps.map((step) => step.name),
    ).toEqual([
      "dispute",
      "source",
      "game",
      "boundary",
      "timeout",
      "award",
      ...Array.from({ length: 37 }, (_, index) => `semantic-resolver-${index}`),
      ...Array.from({ length: 9 }, (_, index) => `prepare-resolver-${index}`),
      ...Array.from({ length: 5 }, (_, index) => `direct-resolver-${index}`),
    ]);
    expect(output.fraudProofCatalogue.root).toBe(fraudProofCatalogue.root);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.doubleSpend.categoryId).toBe("00000000");
    expect(output.fraudProofCatalogue.doubleSpend.expectedCategoryId).toBe(
      "00000000",
    );
    expect(
      output.fraudProofCatalogue.doubleSpend.categoryIdMatchesExpected,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.doubleSpend.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.doubleSpend.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.doubleSpend.ready).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInput.categoryId).toBe(
      "00000001",
    );
    expect(output.fraudProofCatalogue.nonExistentInput.expectedCategoryId).toBe(
      "00000001",
    );
    expect(
      output.fraudProofCatalogue.nonExistentInput.categoryIdMatchesExpected,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.nonExistentInput.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.nonExistentInput.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.invalidRange.categoryId).toBe("00000003");
    expect(output.fraudProofCatalogue.invalidRange.expectedCategoryId).toBe(
      "00000003",
    );
    expect(
      output.fraudProofCatalogue.invalidRange.categoryIdMatchesExpected,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.invalidRange.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.invalidRange.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.invalidRange.ready).toBe(true);
    expect(output.fraudProofCatalogue.transitionTrace.categoryId).toBe(
      "00000004",
    );
    expect(output.fraudProofCatalogue.transitionTrace.expectedCategoryId).toBe(
      "00000004",
    );
    expect(
      output.fraudProofCatalogue.transitionTrace.categoryIdMatchesExpected,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.transitionTrace.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.transitionTrace.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.transitionTrace.ready).toBe(true);
    expect(output.fraudProofCatalogue.validationTraceDispute.ready).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(true);
  });

  it("marks catalogue init as not ready when deployment still points at the placeholder", async () => {
    const fixture = inspectionFixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(fixture, placeholderDoubleSpend),
      }),
    );

    expect(output.doubleSpend.deploymentDoubleSpendMatchesFirstStep).toBe(
      false,
    );
    expect(output.fraudProofCatalogue.doubleSpend.ready).toBe(false);
    expect(output.fraudProofCatalogue.nonExistentInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.invalidRange.ready).toBe(true);
    expect(output.fraudProofCatalogue.transitionTrace.ready).toBe(true);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("rejects deployment info with a mismatched fraud-proof policy", async () => {
    const blueprintJson = readBlueprintJson();
    await expect(
      Effect.runPromise(
        inspectContracts({
          blueprint: blueprintJson,
          network: "Preprod",
          deploymentInfo: deploymentManifest({
            hubOracleMint: { scriptHash: h28 },
            fraudProofCatalogueMint: { scriptHash: h28b },
            fraudProofMint: { scriptHash: "33".repeat(28) },
            fraudProofSpend: { scriptHash: "44".repeat(28) },
            fraudProofDoubleSpend: { scriptHash: "55".repeat(28) },
            fraudProofInvalidRange: { scriptHash: "66".repeat(28) },
          }),
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
          deploymentInfo: deploymentInfoFor(fixture).contracts,
        }),
      ),
    ).rejects.toThrow(
      "Contract deployment info is missing referenceScriptAuthPolicy.",
    );
  });

  it("marks catalogue init as not ready when invalid-range deployment is stale", async () => {
    const fixture = inspectionFixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(
          fixture,
          fixture.contracts.doubleSpend.firstStep.spendingScriptHash,
          placeholderInvalidRange,
        ),
      }),
    );

    expect(output.doubleSpend.deploymentDoubleSpendMatchesFirstStep).toBe(true);
    expect(output.invalidRange.deploymentInvalidRangeMatchesFirstStep).toBe(
      false,
    );
    expect(output.fraudProofCatalogue.doubleSpend.ready).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.invalidRange.ready).toBe(false);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("marks catalogue init as not ready when non-existent-input deployment is stale", async () => {
    const fixture = inspectionFixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(
          fixture,
          fixture.contracts.doubleSpend.firstStep.spendingScriptHash,
          fixture.contracts.invalidRange.firstStep.spendingScriptHash,
          fixture.contracts.transitionTrace.firstStep.spendingScriptHash,
          placeholderNonExistentInput,
        ),
      }),
    );

    expect(
      output.nonExistentInput.deploymentNonExistentInputMatchesFirstStep,
    ).toBe(false);
    expect(output.fraudProofCatalogue.doubleSpend.ready).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInput.ready).toBe(false);
    expect(output.fraudProofCatalogue.invalidRange.ready).toBe(true);
    expect(output.fraudProofCatalogue.transitionTrace.ready).toBe(true);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("rejects deployment info with non-canonical fraud-proof category IDs", async () => {
    const fixture = inspectionFixture;
    const deploymentInfo = deploymentInfoFor(fixture);
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
    const deploymentInfo = deploymentInfoFor(fixture);
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
