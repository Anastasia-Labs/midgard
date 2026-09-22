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

/** A definite-length CBOR byte string (major type 2) around short hex. */
const cborBytes = (hex: string): string => {
  const byteLength = hex.length / 2;
  if (byteLength >= 24) throw new Error(`positional payload too long: ${hex}`);
  return (0x40 + byteLength).toString(16) + hex;
};

/**
 * Deterministic synthetic Plutus bytes used by watcher deployment-authority
 * fixtures: the contract's one-based ordinal (even-width, so the registry may
 * grow beyond 255 contracts) carried as an already double-CBOR-wrapped script.
 *
 * The wrapping is load-bearing for speed, not meaning. Lucid normalizes every
 * script through `applyDoubleCborEncoding`, which speculatively CBOR-decodes
 * the bytes twice to tell single from double wrapping. Handing it the bare
 * ordinal made two of the 287 ordinals (`9f`, `bf`: truncated indefinite-length
 * array and map heads) spin inside cbor-x for about twelve seconds each before
 * failing, which is where the "40ms per contract" this fixture used to be
 * blamed for actually lived. A well-formed double wrap decodes cleanly at both
 * layers, so the whole registry hashes in a few milliseconds.
 */
export const positionalContractScriptCbor = (contractName: string): string => {
  const index = DEPLOYMENT_MANIFEST_CONTRACT_NAMES.indexOf(
    contractName as (typeof DEPLOYMENT_MANIFEST_CONTRACT_NAMES)[number],
  );
  if (index < 0) throw new Error(`Unknown positional contract ${contractName}`);
  return cborBytes(cborBytes(ordinalHex(index + 1)));
};

/**
 * Memoized because the watcher authority fixtures rebuild whole synthetic
 * deployments several times per suite, and the mapping is a pure function of
 * the contract name, so one cache entry per name is exact.
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
