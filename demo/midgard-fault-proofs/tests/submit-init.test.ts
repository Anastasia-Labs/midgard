import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "@al-ft/midgard-sdk";
import {
  buildDoubleSpendFaultProofContracts,
  buildFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  DOUBLE_SPEND_FAULT_PROOF_TITLES,
  EMPTY_MERKLE_TREE_ROOT,
  FAULT_PROOF_SHARED_TITLES,
  type FaultProofBlueprint,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueDeploymentInfo,
  INVALID_RANGE_FAULT_PROOF_TITLES,
  parseFaultProofBlueprint,
  ScriptHashSchema,
  TRANSITION_TRACE_FAULT_PROOF_TITLES,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
} from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  resolveDoubleSpendDeploymentContracts,
  resolveFraudulentHeaderHash,
  resolveInvalidRangeDeploymentContracts,
  resolveProverSigner,
  resolveTransitionTraceDeploymentContracts,
  resolveValidationTraceDisputeDeploymentContracts,
  submitInit,
} from "../src/index.js";

const seedPhrase =
  "test test test test test test test test test test test junk";
const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const blueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");
const h28 = "11".repeat(28);
const h28b = "22".repeat(28);
const placeholderInvalidRange = "55".repeat(28);
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

const readBlueprint = (): FaultProofBlueprint =>
  parseFaultProofBlueprint(
    JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown,
  );

const filterBlueprint = (
  blueprint: FaultProofBlueprint,
  titles: readonly string[],
): FaultProofBlueprint => {
  const titleSet = new Set(titles);
  return {
    validators: blueprint.validators.filter((validator) =>
      titleSet.has(validator.title),
    ),
  };
};

const encodeCatalogueKey = (id: string): Buffer =>
  Buffer.from(
    Data.to(
      id,
      Data.Bytes({
        minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
        maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
      }) as unknown as LucidDataSchema,
    ),
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

const catalogueFor = async (
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

const expectedSeedPaymentKeyHash = (): string =>
  CML.PrivateKey.from_bech32(
    walletFromSeed(seedPhrase, { network: "Preprod" }).paymentKey,
  )
    .to_public()
    .hash()
    .to_hex();

const makeUtxo = (assets: Record<string, bigint>): UTxO =>
  ({
    txHash: "aa".repeat(32),
    outputIndex: 0,
    address: "addr_test1vqz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3d7t0s7uqthvq7n",
    assets,
  }) as UTxO;

describe("submit-init signer resolution", () => {
  it("uses USER_WALLET as the default seed phrase source like submit-withdrawal", () => {
    const signer = resolveProverSigner(
      { network: "Preprod" },
      { USER_WALLET: seedPhrase },
    );

    expect(signer.source).toBe("USER_WALLET");
    expect(signer.address).toBe(
      walletFromSeed(seedPhrase, { network: "Preprod" }).address,
    );
    expect(signer.paymentKeyHash).toBe(expectedSeedPaymentKeyHash());
  });

  it("uses a direct seed phrase before the configured seed env var", () => {
    const signer = resolveProverSigner(
      {
        network: "Preprod",
        walletSeedPhrase: seedPhrase,
        walletSeedPhraseEnv: "OTHER_WALLET",
      },
      { OTHER_WALLET: "env env env env env env env env env env env env" },
    );

    expect(signer.source).toBe("direct-seed-phrase");
    expect(signer.paymentKeyHash).toBe(expectedSeedPaymentKeyHash());
  });

  it("accepts a direct private key even when USER_WALLET is present", () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const signer = resolveProverSigner(
      {
        network: "Preprod",
        walletPrivateKey: privateKey.to_bech32(),
      },
      { USER_WALLET: seedPhrase },
    );

    expect(signer.source).toBe("direct-private-key");
    expect(signer.paymentKeyHash).toBe(privateKey.to_public().hash().to_hex());
    expect(signer.address).toContain("addr_test");
  });

  it("rejects ambiguous direct seed and private-key signer inputs", () => {
    expect(() =>
      resolveProverSigner(
        {
          network: "Preprod",
          walletSeedPhrase: seedPhrase,
          walletPrivateKey: CML.PrivateKey.generate_ed25519().to_bech32(),
        },
        {},
      ),
    ).toThrow("wallet seed phrase or a wallet private key");
  });
});

describe("submit-init state queue header resolution", () => {
  it("derives the fraudulent header hash from the selected state-queue block UTxO", () => {
    const stateQueuePolicyId = "11".repeat(28);
    const headerHash = "22".repeat(28);
    const unit = `${stateQueuePolicyId}${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;

    expect(
      resolveFraudulentHeaderHash({
        stateQueuePolicyId,
        fraudulentBlockUtxo: makeUtxo({ lovelace: 5_000_000n, [unit]: 1n }),
      }),
    ).toBe(headerHash);
  });

  it("rejects a configured header hash that does not match the UTxO block token", () => {
    const stateQueuePolicyId = "33".repeat(28);
    const headerHash = "44".repeat(28);
    const unit = `${stateQueuePolicyId}${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;

    expect(() =>
      resolveFraudulentHeaderHash({
        stateQueuePolicyId,
        fraudulentBlockUtxo: makeUtxo({ [unit]: 1n }),
        configuredHeaderHash: "55".repeat(28),
      }),
    ).toThrow("--fraudulent-header-hash mismatch");
  });
});

describe("fault-proof deployment contract resolution", () => {
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
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
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
  });

  it("rejects invalid-range resolution when the catalogue membership proof is stale", async () => {
    const blueprint = readBlueprint();
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
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
  });

  it("resolves transition-trace without requiring staged fault-proof validators in the blueprint", async () => {
    const blueprint = filterBlueprint(readBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(TRANSITION_TRACE_FAULT_PROOF_TITLES),
    ]);
    const contracts = await Effect.runPromise(
      buildTransitionTraceFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
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
        fraudProofTransitionTrace: {
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
    const blueprint = filterBlueprint(readBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.dispute,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.source,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.game,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.boundary,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.timeout,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.award,
      ...Object.values(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares),
      ...Object.values(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics),
      ...Object.values(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.directResolvers,
      ),
    ]);
    const contracts = await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
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
    });

    const resolved = await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network: "Preprod",
    });
    expect(resolved.validationTraceDisputeCategory.categoryId).toBe("00000005");
    expect(resolved.contracts.validationTraceDispute.steps).toHaveLength(74);
    expect(
      resolved.contracts.validationTraceDispute.semanticResolvers,
    ).toHaveLength(54);
    expect(
      resolved.contracts.validationTraceDispute.prepareResolvers,
    ).toHaveLength(11);
    expect(
      resolved.contracts.validationTraceDispute.directResolvers,
    ).toHaveLength(3);
    expect(resolved.contracts.validationTraceDispute.resolvers).toHaveLength(
      14,
    );

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
  }, 15_000);

  it("does not gate double-spend submit-init on stale invalid-range deployment readiness", async () => {
    const blueprint = readBlueprint();
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
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
  });

  it("does not gate invalid-range submit-init on stale double-spend deployment readiness", async () => {
    const blueprint = readBlueprint();
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28,
        fraudProofCataloguePolicyId: h28b,
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
  });
});
