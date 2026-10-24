import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRecipe,
  parseDeploymentManifestEventHistoryRetentionAddress,
  parseDeploymentManifestEventHistoryRetentionAddresses,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  referenceScriptAuthUnit,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "./deployment-manifest.deployment-manifest-reference-script-contract-by-role.js";
import {
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES,
  DEPLOYMENT_MANIFEST_STEP_NAMES,
  DEPLOYMENT_MANIFEST_STEP_STATUSES,
  requireExactKeys,
  requireLowercaseHex,
  requireLowercaseVariableHex,
  requireNonEmptyString,
  requireObject,
  requireOutRef,
  requireOutRefString,
  requireScriptType,
} from "./deployment-manifest.require-out-ref-string.js";
import { validateFraudProofCatalogue } from "./deployment-manifest.validate-fraud-proof-catalogue.js";

export const validateContracts = (contracts: Record<string, unknown>): void => {
  requireExactKeys(
    contracts,
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
    [],
    "contracts",
  );
  for (const contractName of DEPLOYMENT_MANIFEST_CONTRACT_NAMES) {
    const field = `contracts.${contractName}`;
    const entry = requireObject(contracts[contractName], field);
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
    const refScriptUTxO =
      entry.refScriptUTxO === null
        ? null
        : requireOutRef(entry.refScriptUTxO, `${field}.refScriptUTxO`);
    if (
      refScriptUTxO !== null &&
      `${refScriptUTxO.txHash}#${refScriptUTxO.outputIndex.toString()}` !==
        `${(entry.refScriptUTxO as Record<string, unknown>).txHash as string}#${(entry.refScriptUTxO as Record<string, unknown>).outputIndex as number}`
    ) {
      throw new Error(
        `Deployment manifest ${field}.refScriptUTxO must be canonical`,
      );
    }
    const contract = requireObject(entry.contract, `${field}.contract`);
    requireExactKeys(contract, ["type", "cborHex"], [], `${field}.contract`);
    const scriptType = requireScriptType(
      contract.type,
      `${field}.contract.type`,
    );
    const cborHex = requireLowercaseVariableHex(
      contract.cborHex,
      `${field}.contract.cborHex`,
    );
    const scriptHash = requireLowercaseHex(
      entry.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    let derivedScriptHash: string;
    try {
      derivedScriptHash = validatorToScriptHash({
        type: scriptType,
        script: cborHex,
      });
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
  }
  const fraudProofCatalogueMint = requireObject(
    contracts.fraudProofCatalogueMint,
    "contracts.fraudProofCatalogueMint",
  );
  if (fraudProofCatalogueMint.fraudProofCatalogue !== undefined) {
    validateFraudProofCatalogue(
      requireObject(
        fraudProofCatalogueMint.fraudProofCatalogue,
        "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
      ),
      contracts,
    );
  }
};

export const validateReferenceScripts = (
  referenceScripts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    referenceScripts,
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES,
    [],
    "referenceScripts",
  );
  const policyId = requireNonEmptyString(
    referenceScriptAuthPolicy.policyId,
    "referenceScriptAuthPolicy.policyId",
  );
  for (const role of DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES) {
    const field = `referenceScripts.${role}`;
    const record = requireObject(referenceScripts[role], field);
    requireExactKeys(
      record,
      ["status", "roleUnit", "scriptHash", "outRef"],
      [],
      field,
    );
    if (record.status !== "confirmed") {
      throw new Error(`Deployment manifest ${field}.status must be confirmed`);
    }
    const expectedRoleUnit = referenceScriptAuthUnit(
      policyId,
      role as keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    );
    if (record.roleUnit !== expectedRoleUnit) {
      throw new Error(
        `Deployment manifest ${field}.roleUnit mismatch: expected ${expectedRoleUnit}`,
      );
    }
    const contractName =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
      ];
    const contract = requireObject(
      contracts[contractName],
      `contracts.${contractName}`,
    );
    if (record.scriptHash !== contract.scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    const contractOutRef = requireOutRef(
      contract.refScriptUTxO,
      `contracts.${contractName}.refScriptUTxO`,
    );
    const outRef = requireOutRefString(record.outRef, `${field}.outRef`);
    if (
      outRef.txHash !== contractOutRef.txHash ||
      outRef.outputIndex !== contractOutRef.outputIndex
    ) {
      throw new Error(
        `Deployment manifest ${field}.outRef must match contracts.${contractName}.refScriptUTxO`,
      );
    }
  }
};

export const validateSteps = (steps: Record<string, unknown>): void => {
  requireExactKeys(steps, DEPLOYMENT_MANIFEST_STEP_NAMES, [], "steps");
  for (const stepName of DEPLOYMENT_MANIFEST_STEP_NAMES) {
    const field = `steps.${stepName}`;
    const step = requireObject(steps[stepName], field);
    requireExactKeys(step, ["status"], ["txHash"], field);
    if (
      typeof step.status !== "string" ||
      !DEPLOYMENT_MANIFEST_STEP_STATUSES.some(
        (status) => status === step.status,
      )
    ) {
      throw new Error(`Deployment manifest ${field}.status is unsupported`);
    }
    if (step.txHash !== undefined) {
      requireLowercaseHex(step.txHash, 32, `${field}.txHash`);
    }
  }
};
