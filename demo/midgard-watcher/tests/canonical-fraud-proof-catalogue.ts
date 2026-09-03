import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY,
  type DeploymentManifestFraudProofCatalogueIdentity,
  verifyDeploymentManifestFraudProofCatalogueIdentity,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { validatorToScriptHash } from "@lucid-evolution/lucid";

type ContractIdentity = Readonly<{ scriptHash: string }>;

/**
 * Deterministic synthetic Plutus bytes used by watcher deployment-authority
 * fixtures. The even-width normalization is required once the deployment
 * registry grows beyond 255 contracts.
 */
export const positionalContractScriptCbor = (contractName: string): string => {
  const index = DEPLOYMENT_MANIFEST_CONTRACT_NAMES.indexOf(
    contractName as (typeof DEPLOYMENT_MANIFEST_CONTRACT_NAMES)[number],
  );
  if (index < 0) throw new Error(`Unknown positional contract ${contractName}`);
  const ordinal = (index + 1).toString(16);
  return ordinal.length % 2 === 0 ? ordinal : `0${ordinal}`;
};

export const positionalContractScriptHash = (contractName: string): string =>
  validatorToScriptHash({
    type: "PlutusV3",
    script: positionalContractScriptCbor(contractName),
  });

const catalogueKey = (categoryId: string): Buffer =>
  Buffer.concat([Buffer.from([0x44]), Buffer.from(categoryId, "hex")]);

const positionalCategories =
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
    (categoryName) => {
      const categoryId =
        DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName];
      const contractName =
        DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY[categoryName];
      const scriptHash = positionalContractScriptHash(contractName);
      return {
        categoryName,
        categoryId,
        scriptHash,
        key: catalogueKey(categoryId),
        value: Buffer.from(`581c${scriptHash}`, "hex"),
      } as const;
    },
  );

const positionalTrie = await Trie.fromList(
  positionalCategories.map(({ key, value }) => ({ key, value })),
);
const positionalCategoryProofs = Object.fromEntries(
  await Promise.all(
    positionalCategories.map(
      async ({ categoryName, categoryId, scriptHash, key }) => [
        categoryName,
        {
          categoryId,
          scriptHash,
          membershipProofCbor: (await positionalTrie.prove(key))
            .toCBOR()
            .toString("hex"),
        },
      ],
    ),
  ),
) as DeploymentManifestFraudProofCatalogueIdentity["categories"];

/** Exact positional identity for the current canonical deployment registry. */
export const POSITIONAL_FRAUD_PROOF_CATALOGUE_ROOT = Buffer.from(
  positionalTrie.hash,
).toString("hex");

const POSITIONAL_SCRIPT_CATALOGUE =
  verifyDeploymentManifestFraudProofCatalogueIdentity({
    root: POSITIONAL_FRAUD_PROOF_CATALOGUE_ROOT,
    categories: positionalCategoryProofs,
  });

const matchesDeployedScripts = (
  catalogue: DeploymentManifestFraudProofCatalogueIdentity,
  contracts: Readonly<Record<string, ContractIdentity>>,
): boolean =>
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.every(
    (category) =>
      catalogue.categories[category].scriptHash ===
      contracts[DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY[category]]
        ?.scriptHash,
  );

export const canonicalFraudProofCatalogueFixture = (
  contracts: Readonly<Record<string, ContractIdentity>>,
): DeploymentManifestFraudProofCatalogueIdentity => {
  if (!matchesDeployedScripts(POSITIONAL_SCRIPT_CATALOGUE, contracts)) {
    throw new Error(
      "No canonical watcher fraud-proof catalogue fixture matches the deployed scripts",
    );
  }
  return structuredClone(POSITIONAL_SCRIPT_CATALOGUE);
};
