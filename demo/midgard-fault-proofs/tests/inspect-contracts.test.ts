import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildDoubleSpendFaultProofContracts,
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueDeploymentInfo,
  parseFaultProofBlueprint,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { inspectContracts } from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const blueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");

const h28 = "11".repeat(28);
const h28b = "22".repeat(28);
const placeholderDoubleSpend = "00".repeat(28);
const categoryIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});
type LucidDataSchema = Parameters<typeof Data.to>[1];

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
  doubleSpendScriptHash: string,
): Promise<FraudProofCatalogueDeploymentInfo> => {
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name, index) => [
      name,
      {
        categoryId: categoryId(index),
        scriptHash:
          name === "doubleSpend"
            ? doubleSpendScriptHash
            : `${(index + 3).toString(16).padStart(2, "0")}`.repeat(28),
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
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(blueprintJson),
      network: "Preprod",
      hubOraclePolicyId: h28,
      fraudProofCataloguePolicyId: h28b,
    }),
  );
  const fraudProofCatalogue = await buildCatalogueFixture(
    contracts.doubleSpend.firstStep.spendingScriptHash,
  );
  return { blueprintJson, contracts, fraudProofCatalogue };
};

const deploymentInfoFor = (
  {
    contracts,
    fraudProofCatalogue,
  }: Awaited<ReturnType<typeof buildInspectionFixture>>,
  doubleSpendScriptHash = contracts.doubleSpend.firstStep.spendingScriptHash,
) => ({
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
});

describe("inspect-contracts", () => {
  it("emits stable double-spend contract inspection JSON with catalogue readiness", async () => {
    const fixture = await buildInspectionFixture();
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
    expect(output.fraudProofCatalogue.root).toBe(fraudProofCatalogue.root);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.doubleSpend.categoryId).toBe("00000000");
    expect(
      output.fraudProofCatalogue.doubleSpend.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.doubleSpend.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(true);
  });

  it("marks catalogue init as not ready when deployment still points at the placeholder", async () => {
    const fixture = await buildInspectionFixture();

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
          deploymentInfo: {
            hubOracleMint: { scriptHash: h28 },
            fraudProofCatalogueMint: { scriptHash: h28b },
            fraudProofMint: { scriptHash: "33".repeat(28) },
            fraudProofSpend: { scriptHash: "44".repeat(28) },
          },
        }),
      ),
    ).rejects.toThrow("fraudProofMint.scriptHash mismatch");
  });
});
