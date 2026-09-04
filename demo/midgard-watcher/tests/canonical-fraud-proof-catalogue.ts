import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY,
  type DeploymentManifestFraudProofCatalogueIdentity,
  verifyDeploymentManifestFraudProofCatalogueIdentity,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { ordinalHex } from "@al-ft/midgard-test-support/hex";
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
  return ordinalHex(index + 1);
};

/**
 * Memoized because it is not cheap: `validatorToScriptHash` runs the script
 * through Lucid's double-CBOR normalization and the CML hasher, which costs
 * roughly 40ms per contract. The canonical registry now holds 287 of them, so
 * rebuilding a whole synthetic deployment cost about twelve seconds a call,
 * and the watcher authority fixtures build several per suite — enough to blow
 * a two-minute `beforeAll` budget on the fixture rather than the test. The
 * mapping is a pure function of the contract name, so one cache entry per name
 * is exact.
 */
const positionalContractScriptHashes = new Map<string, string>();

export const positionalContractScriptHash = (contractName: string): string => {
  const memoized = positionalContractScriptHashes.get(contractName);
  if (memoized !== undefined) {
    return memoized;
  }
  const scriptHash = validatorToScriptHash({
    type: "PlutusV3",
    script: positionalContractScriptCbor(contractName),
  });
  positionalContractScriptHashes.set(contractName, scriptHash);
  return scriptHash;
};

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
