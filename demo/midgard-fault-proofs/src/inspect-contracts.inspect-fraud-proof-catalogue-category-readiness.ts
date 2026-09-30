import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryDeploymentInfo,
  type FraudProofCatalogueCategoryName,
  type FraudProofCatalogueDeploymentInfo,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthTokenTarget,
} from "@al-ft/midgard-sdk";
import {
  mintingPolicyToId,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  type ContractDeploymentInfo,
  expectedFraudProofCategoryId,
  type InspectContractsCatalogueCategoryOutput,
} from "./inspect-contracts.inspect-contracts-output.js";
import {
  encodeCatalogueKey,
  encodeCatalogueValue,
  fraudProofCatalogueCategories,
  normalizeHex,
  trieRootHex,
} from "./inspect-contracts.parse-contract-deployment-info.js";
import { requireRecord } from "./json-file.js";

export const parseContractDeploymentReferenceScriptAuthPolicyId = (
  value: unknown,
  target: ReferenceScriptAuthTokenTarget,
): string => {
  const manifest = requireRecord(value, "Contract deployment info");
  const policy = requireRecord(
    manifest.referenceScriptAuthPolicy,
    "Contract deployment info.referenceScriptAuthPolicy",
  );
  const policyId = normalizeHex(
    policy.policyId,
    "Contract deployment info.referenceScriptAuthPolicy.policyId",
    28,
  );
  const nativeScript = requireRecord(
    policy.nativeScript,
    "Contract deployment info.referenceScriptAuthPolicy.nativeScript",
  );
  if (nativeScript.type !== "Native") {
    throw new Error(
      "Contract deployment info.referenceScriptAuthPolicy.nativeScript.type must be Native",
    );
  }
  const cborHex = normalizeHex(
    nativeScript.cborHex,
    "Contract deployment info.referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  let derivedPolicyId: string;
  try {
    derivedPolicyId = mintingPolicyToId({
      type: "Native",
      script: cborHex,
    });
  } catch (cause) {
    throw new Error(
      `Contract deployment info.referenceScriptAuthPolicy.nativeScript.cborHex is invalid: ${formatUnknownError(cause)}`,
    );
  }
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Contract deployment info.referenceScriptAuthPolicy.policyId mismatch: declared=${policyId}, derived=${derivedPolicyId}`,
    );
  }
  const tokenNames = requireRecord(
    policy.tokenNames,
    "Contract deployment info.referenceScriptAuthPolicy.tokenNames",
  );
  const expectedTokenName = REFERENCE_SCRIPT_AUTH_TOKEN_NAMES[target];
  if (tokenNames[target] !== expectedTokenName) {
    throw new Error(
      `Contract deployment info.referenceScriptAuthPolicy.tokenNames.${target} must equal ${expectedTokenName}`,
    );
  }
  return policyId;
};

export const requireDeploymentScriptHash = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): string => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  return entry.scriptHash;
};

export const optionalDeploymentScriptHash = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): string | null => deploymentInfo[name]?.scriptHash ?? null;

export const deploymentEntryBaseForCategory = (
  category: FraudProofCatalogueCategoryName,
): string => {
  if (category === "validationTraceDispute") {
    return "validationTraceDispute";
  }
  return `fraudProof${category[0]!.toUpperCase()}${category.slice(1)}`;
};

export const inspectEmbeddedDeploymentScriptIdentity = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): {
  readonly deploymentScriptHash: string | null;
  readonly expectedScriptHash: string;
  readonly deploymentMatchesScriptBytes: boolean | null;
} => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    return {
      deploymentScriptHash: null,
      expectedScriptHash: "",
      deploymentMatchesScriptBytes: null,
    };
  }
  if (entry.contract === undefined) {
    return {
      deploymentScriptHash: entry.scriptHash,
      expectedScriptHash: entry.scriptHash,
      deploymentMatchesScriptBytes: false,
    };
  }
  const derivedScriptHash = validatorToScriptHash({
    type: entry.contract.type,
    script: entry.contract.cborHex,
  });
  return {
    deploymentScriptHash: entry.scriptHash,
    expectedScriptHash: derivedScriptHash,
    deploymentMatchesScriptBytes: derivedScriptHash === entry.scriptHash,
  };
};

export const expectScriptHash = (
  label: string,
  actual: string,
  expected: string,
): void => {
  if (actual !== expected) {
    throw new Error(
      `${label} mismatch: derived=${actual} deployment=${expected}`,
    );
  }
};

export const emptyCatalogueCategoryInspection =
  (): InspectContractsCatalogueCategoryOutput => ({
    categoryId: null,
    scriptHash: null,
    scriptHashMatchesFirstStep: null,
    membershipProofCbor: null,
    membershipProofMatchesDerived: null,
    expectedCategoryId: null,
    categoryIdMatchesExpected: null,
  });

export type FraudProofCatalogueCategoryReadiness = {
  readonly category: FraudProofCatalogueCategoryDeploymentInfo;
  readonly expectedCategoryId: string;
  readonly categoryIdMatchesExpected: boolean;
  readonly scriptHashMatchesFirstStep: boolean;
  readonly membershipProofMatchesDerived: boolean;
  readonly ready: boolean;
};

export const inspectFraudProofCatalogueCategoryReadiness = async ({
  catalogue,
  categoryName,
  expectedFirstStepHash,
  deploymentMatchesFirstStep,
}: {
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly categoryName: FraudProofCatalogueCategoryName;
  readonly expectedFirstStepHash: string;
  readonly deploymentMatchesFirstStep: boolean | null;
}): Promise<{
  readonly derivedRoot: string;
  readonly rootMatchesDerived: boolean;
  readonly categoryReadiness: FraudProofCatalogueCategoryReadiness;
}> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const categories = fraudProofCatalogueCategories(catalogue);
  for (const currentCategoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const currentCategory = categories[currentCategoryName];
    if (currentCategory === undefined) {
      throw new Error(
        `fraudProofCatalogue.categories.${currentCategoryName} is missing`,
      );
    }
    await trie.insert(
      encodeCatalogueKey(currentCategory.categoryId),
      encodeCatalogueValue(currentCategory.scriptHash),
    );
  }

  const derivedRoot = trieRootHex(trie);
  const rootMatchesDerived = catalogue.root === derivedRoot;
  const category = categories[categoryName];
  if (category === undefined) {
    throw new Error(
      `fraudProofCatalogue.categories.${categoryName} is missing`,
    );
  }
  const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
  const derivedProofCbor = proof.toCBOR().toString("hex");
  const expectedCategoryId = expectedFraudProofCategoryId(categoryName);
  const categoryIdMatchesExpected = category.categoryId === expectedCategoryId;
  const scriptHashMatchesFirstStep =
    category.scriptHash === expectedFirstStepHash;
  const membershipProofMatchesDerived =
    category.membershipProofCbor === derivedProofCbor;

  return {
    derivedRoot,
    rootMatchesDerived,
    categoryReadiness: {
      category,
      expectedCategoryId,
      categoryIdMatchesExpected,
      scriptHashMatchesFirstStep,
      membershipProofMatchesDerived,
      ready:
        deploymentMatchesFirstStep === true &&
        rootMatchesDerived &&
        categoryIdMatchesExpected &&
        scriptHashMatchesFirstStep &&
        membershipProofMatchesDerived,
    },
  };
};

export const assertFraudProofCatalogueCategoryReady = async ({
  catalogue,
  categoryName,
  expectedFirstStepHash,
  deploymentMatchesFirstStep,
}: {
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly categoryName: FraudProofCatalogueCategoryName;
  readonly expectedFirstStepHash: string;
  readonly deploymentMatchesFirstStep: boolean | null;
}): Promise<FraudProofCatalogueCategoryDeploymentInfo> => {
  const { rootMatchesDerived, categoryReadiness } =
    await inspectFraudProofCatalogueCategoryReadiness({
      catalogue,
      categoryName,
      expectedFirstStepHash,
      deploymentMatchesFirstStep,
    });
  if (!rootMatchesDerived) {
    throw new Error(
      `Fraud-proof catalogue root mismatch: deployment=${catalogue.root}, derived from categories differs.`,
    );
  }
  if (!categoryReadiness.categoryIdMatchesExpected) {
    throw new Error(
      `fraudProofCatalogue.categories.${categoryName}.categoryId must be ${categoryReadiness.expectedCategoryId}, got ${categoryReadiness.category.categoryId}`,
    );
  }
  if (!categoryReadiness.scriptHashMatchesFirstStep) {
    throw new Error(
      `fraudProofCatalogue.categories.${categoryName}.scriptHash mismatch: deployment=${categoryReadiness.category.scriptHash}, derived=${expectedFirstStepHash}.`,
    );
  }
  if (!categoryReadiness.membershipProofMatchesDerived) {
    throw new Error(
      `fraudProofCatalogue.categories.${categoryName}.membershipProofCbor does not match the derived catalogue proof.`,
    );
  }
  if (!categoryReadiness.ready) {
    throw new Error(
      `Fraud-proof catalogue category ${categoryName} is not ready for Init.`,
    );
  }
  return categoryReadiness.category;
};
