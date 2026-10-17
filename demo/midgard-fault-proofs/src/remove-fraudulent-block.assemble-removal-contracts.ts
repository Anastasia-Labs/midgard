import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Network,
  type SpendingValidator,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  type ContractDeploymentInfo,
  parseContractDeploymentInfo,
} from "./inspect-contracts.js";
import {
  type ReferenceScriptName,
  type RemoveFraudulentBlockCategoryLabel,
  type RemoveFraudulentBlockContracts,
  type RemoveFraudulentBlockFraudCategory,
} from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import {
  type RemoveFraudulentBlockExplicitCategory,
  requireDeploymentScript,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  requireDeploymentScriptHash,
  requireMatchingScriptHash,
  resolveFaultProofDeploymentContracts,
} from "./runtime.js";

export const buildRemovalContracts = async ({
  blueprint,
  deploymentInfo,
  network,
  fraudCategory,
}: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly fraudCategory: RemoveFraudulentBlockFraudCategory;
}): Promise<RemoveFraudulentBlockContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    categoryName: fraudCategory,
    requireFraudProofSpend: true,
  });

  return assembleRemovalContracts({
    deploymentInfo: resolved.deploymentInfo,
    network,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    fraudProofPolicyId: resolved.contracts.fraudProof.policyId,
    fraudProofAddress: resolved.contracts.fraudProof.spendingScriptAddress,
    fraudCategoryId: resolved.category.categoryId,
    fraudCategory,
  });
};

/**
 * Explicit-category counterpart of `buildRemovalContracts`: a
 * pre-registration family has no SDK builder and no catalogue entry, so the
 * caller's already-resolved facts stand in for the canonical resolution —
 * while every fail-closed cross-check the canonical path performs still runs
 * against the deployment manifest: the shared fraud-proof pair against the
 * `fraudProofMint`/`fraudProofSpend` entries, the step-01 hash against the
 * entry the record names, and the category id against every canonical
 * registered id (a collision would mean the "pre-registration" id actually
 * belongs to a registered family, which must resolve canonically).
 */
export const buildExplicitRemovalContracts = ({
  deploymentInfo,
  network,
  category,
}: {
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly category: RemoveFraudulentBlockExplicitCategory;
}): RemoveFraudulentBlockContracts => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const hubOraclePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "hubOracleMint",
  );
  const deployedFraudProofPolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofMint",
  );
  if (category.fraudProof.policyId !== deployedFraudProofPolicyId) {
    throw new Error(
      `${category.name} explicit category names fraud-proof policy ` +
        `${category.fraudProof.policyId}, but the deployment's fraudProofMint ` +
        `entry pins ${deployedFraudProofPolicyId}.`,
    );
  }
  const deployedFraudProofSpendHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofSpend",
  );
  if (category.fraudProof.spendingScriptHash !== deployedFraudProofSpendHash) {
    throw new Error(
      `${category.name} explicit category names fraud-proof spending script ` +
        `${category.fraudProof.spendingScriptHash}, but the deployment's ` +
        `fraudProofSpend entry pins ${deployedFraudProofSpendHash}.`,
    );
  }
  const deployedFirstStepHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    category.firstStepDeploymentEntry,
  );
  if (category.firstStepScriptHash !== deployedFirstStepHash) {
    throw new Error(
      `${category.name} explicit category names step-01 script ` +
        `${category.firstStepScriptHash}, but the deployment's ` +
        `${category.firstStepDeploymentEntry} entry pins ` +
        `${deployedFirstStepHash}.`,
    );
  }
  for (const [registeredName, registeredId] of Object.entries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  )) {
    if (registeredId === category.categoryId) {
      throw new Error(
        `${category.name} explicit category id ${category.categoryId} ` +
          `collides with the registered ${registeredName} category; a ` +
          `registered family must resolve through the canonical catalogue.`,
      );
    }
  }
  return assembleRemovalContracts({
    deploymentInfo: parsedDeploymentInfo,
    network,
    hubOraclePolicyId,
    fraudProofPolicyId: category.fraudProof.policyId,
    fraudProofAddress: category.fraudProof.spendingScriptAddress,
    fraudCategoryId: category.categoryId,
    fraudCategory: category.name,
  });
};

/**
 * The category-independent half of removal-contract resolution: every
 * script, address and policy id here comes straight out of the deployment
 * manifest, with the already-verified category facts passed through.
 */
const assembleRemovalContracts = ({
  deploymentInfo,
  network,
  hubOraclePolicyId,
  fraudProofPolicyId,
  fraudProofAddress,
  fraudCategoryId,
  fraudCategory,
}: {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly fraudCategoryId: string;
  readonly fraudCategory: RemoveFraudulentBlockCategoryLabel;
}): RemoveFraudulentBlockContracts => {
  const stateQueueSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueSpend",
  );
  const stateQueueMintingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueMint",
  );
  const stateQueueFraudRemovalWithdrawalScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueFraudRemovalWithdraw",
  );
  const correctionLockSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "correctionLockSpend",
  );
  const activeOperatorsSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "activeOperatorsSpend",
  );
  const activeOperatorsMintingScript = requireDeploymentScript(
    deploymentInfo,
    "activeOperatorsMint",
  );
  const retiredOperatorsSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "retiredOperatorsSpend",
  );
  const retiredOperatorsMintingScript = requireDeploymentScript(
    deploymentInfo,
    "retiredOperatorsMint",
  );
  const schedulerSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "schedulerSpend",
  );
  const activeOperatorsPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "activeOperatorsMint",
  );
  const retiredOperatorsPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "retiredOperatorsMint",
  );
  const schedulerPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "schedulerMint",
  );
  const registeredOperatorsPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "registeredOperatorsMint",
  );

  return {
    correctionLockAddress: validatorToAddress(
      network,
      correctionLockSpendingScript as SpendingValidator,
    ),
    correctionLockSpendingScript,
    stateQueuePolicyId: requireDeploymentScriptHash(
      deploymentInfo,
      "stateQueueMint",
    ),
    stateQueueAddress: validatorToAddress(
      network,
      stateQueueSpendingScript as SpendingValidator,
    ),
    stateQueueSpendingScript,
    stateQueueMintingScript,
    stateQueueFraudRemovalWithdrawalScript,
    activeOperatorsPolicyId,
    activeOperatorsAddress: validatorToAddress(
      network,
      activeOperatorsSpendingScript as SpendingValidator,
    ),
    activeOperatorsSpendingScript,
    activeOperatorsMintingScript,
    retiredOperatorsPolicyId,
    retiredOperatorsAddress: validatorToAddress(
      network,
      retiredOperatorsSpendingScript as SpendingValidator,
    ),
    retiredOperatorsSpendingScript,
    retiredOperatorsMintingScript,
    schedulerPolicyId,
    schedulerAddress: validatorToAddress(
      network,
      schedulerSpendingScript as SpendingValidator,
    ),
    schedulerSpendingScript,
    hubOraclePolicyId,
    registeredOperatorsPolicyId,
    registeredOperatorsAddress: validatorToAddress(
      network,
      requireDeploymentScript(
        deploymentInfo,
        "registeredOperatorsSpend",
      ) as SpendingValidator,
    ),
    fraudProofPolicyId,
    fraudProofAddress,
    fraudCategoryId,
    fraudCategory,
  };
};

export const requireDeploymentReferenceScript = async ({
  lucid,
  deploymentInfo,
  name,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly name: ReferenceScriptName;
}): Promise<UTxO> => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${name}" is missing refScriptUTxO; publish reference scripts and regenerate deployment info before live removal.`,
    );
  }
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef: entry.refScriptUTxO,
    label: `${name} reference-script UTxO`,
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `${name} reference-script UTxO ${outRefLabel(utxo)} does not carry a reference script.`,
    );
  }
  const scriptRef = utxo.scriptRef;
  requireMatchingScriptHash({
    label: `${name} reference script`,
    deployed: entry.scriptHash,
    derived: validatorToScriptHash(scriptRef),
  });
  return utxo;
};
