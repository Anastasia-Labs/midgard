import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { bytesToHex } from "@noble/hashes/utils.js";

import { verifyDeploymentManifestFraudProofCatalogueIdentity } from "./catalogue-proof.js";
import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY,
  type DeploymentManifestFraudProofCatalogueCategory,
  type DeploymentManifestFraudProofCatalogueCategoryIdentity,
} from "./catalogue-roles.js";
import {
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRecipe,
  parseDeploymentManifestEventHistoryRetentionAddress,
  parseDeploymentManifestEventHistoryRetentionAddresses,
} from "./event-history.js";
import {
  requireExactKeys,
  requireFinalOutRef,
  requireHex,
  requireRecord,
} from "./primitives.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "./reference-script-contracts.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES } from "./reference-script-tokens.js";

// Script-hash derivation is a pure function of (type, cborHex), but each
// derivation pays a full CBOR decode of the compiled script. A manifest
// carries every deployment contract, and callers re-verify the manifest on
// every authority check, so uncached derivation is quadratic in practice.
// A cache hit returns exactly what re-derivation would, including for
// tampered manifests: a changed script changes the key.
const SCRIPT_HASH_DERIVATION_CACHE_LIMIT = 4096;

export const scriptHashDerivationCache = new Map<string, string>();

export const deriveScriptHashCached = (
  type: "Native" | "PlutusV1" | "PlutusV2" | "PlutusV3",
  cborHex: string,
): string => {
  const key = `${type}:${cborHex}`;
  const cached = scriptHashDerivationCache.get(key);
  if (cached !== undefined) {
    return cached;
  }
  const derived = validatorToScriptHash({ type, script: cborHex });
  if (scriptHashDerivationCache.size >= SCRIPT_HASH_DERIVATION_CACHE_LIMIT) {
    const oldest = scriptHashDerivationCache.keys().next().value;
    if (oldest !== undefined) {
      scriptHashDerivationCache.delete(oldest);
    }
  }
  scriptHashDerivationCache.set(key, derived);
  return derived;
};

export const validateFinalizedContracts = (
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    contracts,
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
    [],
    "contracts",
  );
  const referenceScriptContractNames = new Set<string>(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const scriptHashByName = new Map<string, string>();
  for (const contractName of DEPLOYMENT_MANIFEST_CONTRACT_NAMES) {
    const field = `contracts.${contractName}`;
    const entry = requireRecord(contracts[contractName], field);
    const historyFamily =
      contractName === "fraudProofFabricatedDeposit" ||
      contractName === "fraudProofFabricatedWithdrawal";
    const transitionHistory = contractName === "fraudProofTransitionTrace";
    const historyList =
      contractName === "depositMint" || contractName === "withdrawalMint";
    requireExactKeys(
      entry,
      [
        "refScriptUTxO",
        "contract",
        "scriptHash",
        ...(historyList ? ["eventHistoryRecipe"] : []),
        ...(historyFamily
          ? ["eventHistoryBounds", "eventHistoryRetentionAddress"]
          : transitionHistory
            ? ["eventHistoryBounds", "eventHistoryRetentionAddresses"]
            : []),
      ],
      contractName === "fraudProofCatalogueMint" ? ["fraudProofCatalogue"] : [],
      field,
    );
    if (historyList)
      parseDeploymentManifestEventHistoryRecipe(
        entry.eventHistoryRecipe,
        `${field}.eventHistoryRecipe`,
      );
    if (historyFamily || transitionHistory) {
      parseDeploymentManifestEventHistoryBounds(
        entry.eventHistoryBounds,
        `${field}.eventHistoryBounds`,
      );
      if (transitionHistory)
        parseDeploymentManifestEventHistoryRetentionAddresses(
          entry.eventHistoryRetentionAddresses,
        );
      else
        parseDeploymentManifestEventHistoryRetentionAddress(
          entry.eventHistoryRetentionAddress,
        );
    }
    if (referenceScriptContractNames.has(contractName)) {
      requireFinalOutRef(entry.refScriptUTxO, `${field}.refScriptUTxO`);
    } else if (entry.refScriptUTxO !== null) {
      throw new Error(
        `Deployment manifest ${field}.refScriptUTxO must be null because the contract has no reference-script role`,
      );
    }
    const contract = requireRecord(entry.contract, `${field}.contract`);
    requireExactKeys(contract, ["type", "cborHex"], [], `${field}.contract`);
    if (
      contract.type !== "Native" &&
      contract.type !== "PlutusV1" &&
      contract.type !== "PlutusV2" &&
      contract.type !== "PlutusV3"
    ) {
      throw new Error(
        `Deployment manifest ${field}.contract.type is unsupported`,
      );
    }
    const cborHex = requireHex(
      contract.cborHex,
      undefined,
      `${field}.contract.cborHex`,
    );
    const scriptHash = requireHex(entry.scriptHash, 28, `${field}.scriptHash`);
    let derivedScriptHash: string;
    try {
      derivedScriptHash = deriveScriptHashCached(contract.type, cborHex);
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field}.contract.cborHex is invalid: ${String(cause)}`,
      );
    }
    if (derivedScriptHash !== scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash mismatch: expected ${derivedScriptHash}`,
      );
    }
    scriptHashByName.set(contractName, scriptHash);
  }

  const catalogueMint = requireRecord(
    contracts.fraudProofCatalogueMint,
    "contracts.fraudProofCatalogueMint",
  );
  const catalogue = requireRecord(
    catalogueMint.fraudProofCatalogue,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireExactKeys(
    catalogue,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireHex(
    catalogue.root,
    32,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
  );
  const categories = requireRecord(
    catalogue.categories,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  requireExactKeys(
    categories,
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const parsedCategories = {} as Record<
    DeploymentManifestFraudProofCatalogueCategory,
    DeploymentManifestFraudProofCatalogueCategoryIdentity
  >;
  for (const categoryName of DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const contractName =
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY[categoryName];
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
    const category = requireRecord(categories[categoryName], field);
    requireExactKeys(
      category,
      ["categoryId", "scriptHash", "membershipProofCbor"],
      [],
      field,
    );
    const categoryId = requireHex(
      category.categoryId,
      4,
      `${field}.categoryId`,
    );
    const scriptHash = requireHex(
      category.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    const membershipProofCbor = requireHex(
      category.membershipProofCbor,
      undefined,
      `${field}.membershipProofCbor`,
    );
    if (scriptHash !== scriptHashByName.get(contractName)) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    parsedCategories[categoryName] = {
      categoryId,
      scriptHash,
      membershipProofCbor,
    };
  }
  verifyDeploymentManifestFraudProofCatalogueIdentity({
    root: requireHex(
      catalogue.root,
      32,
      "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
    ),
    categories: parsedCategories,
  });
};

export const validateFinalizedReferenceScripts = (
  referenceScripts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  const roles = Object.keys(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  );
  requireExactKeys(referenceScripts, roles, [], "referenceScripts");
  const policyId = requireHex(
    referenceScriptAuthPolicy.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  for (const role of roles) {
    const field = `referenceScripts.${role}`;
    const reference = requireRecord(referenceScripts[role], field);
    requireExactKeys(
      reference,
      ["status", "roleUnit", "scriptHash", "outRef"],
      [],
      field,
    );
    if (reference.status !== "confirmed") {
      throw new Error(`Deployment manifest ${field}.status must be confirmed`);
    }
    const tokenName =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
      ];
    const expectedRoleUnit =
      policyId + bytesToHex(new TextEncoder().encode(tokenName));
    if (reference.roleUnit !== expectedRoleUnit) {
      throw new Error(
        `Deployment manifest ${field}.roleUnit mismatch: expected ${expectedRoleUnit}`,
      );
    }
    const contractName =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
      ];
    const contract = requireRecord(
      contracts[contractName],
      `contracts.${contractName}`,
    );
    const scriptHash = requireHex(
      reference.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    if (scriptHash !== contract.scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    const contractOutRef = requireFinalOutRef(
      contract.refScriptUTxO,
      `contracts.${contractName}.refScriptUTxO`,
    );
    const expectedOutRef = `${contractOutRef.txHash}#${contractOutRef.outputIndex.toString()}`;
    if (reference.outRef !== expectedOutRef) {
      throw new Error(
        `Deployment manifest ${field}.outRef must equal ${expectedOutRef}`,
      );
    }
  }
};
