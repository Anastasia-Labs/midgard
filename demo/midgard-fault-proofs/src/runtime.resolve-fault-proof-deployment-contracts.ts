import {
  type DoubleSpendFaultProofContracts,
  type FraudProofCatalogueCategoryDeploymentInfo,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { type Network } from "@lucid-evolution/lucid";

import {
  assertFraudProofCatalogueCategoryReady,
  type ContractDeploymentInfo,
  parseContractDeploymentInfo,
  parseContractDeploymentReferenceScriptAuthPolicyId,
} from "./inspect-contracts.js";
import { buildOneCategoryFaultProofContracts } from "./runtime.build-one-category-fault-proof-contracts.js";
import {
  categoryLabel,
  type OneCategoryFaultProofContracts,
} from "./runtime.category-label.js";
import { FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY } from "./runtime.fraud-proof-deployment-entries-by-category.js";
import {
  NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY,
  NETWORK_ID_FORCED_STEP_DEPLOYMENT_ENTRY,
  type ResolvedDoubleSpendDeploymentContracts,
  type SupportedFaultProofCategoryName,
} from "./runtime.require-fault-proof-step-reference-script.js";
import {
  requireDeploymentScriptHash,
  requireMatchingScriptHash,
} from "./runtime.resolve-prover-signer.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "./transition-trace/yield-references.js";

export const resolveFaultProofDeploymentContracts = async ({
  blueprint,
  deploymentInfo,
  network,
  categoryName,
  requireStateQueueMint = false,
  requireFraudProofSpend = false,
}: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly categoryName: SupportedFaultProofCategoryName;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<{
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly category: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: OneCategoryFaultProofContracts;
}> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const catalogue =
    parsedDeploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined) {
    throw new Error(
      "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.",
    );
  }
  const category = (
    catalogue.categories as Readonly<
      Partial<
        Record<
          SupportedFaultProofCategoryName,
          FraudProofCatalogueCategoryDeploymentInfo
        >
      >
    >
  )[categoryName];
  if (category === undefined) {
    throw new Error(
      `Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}.`,
    );
  }

  const stateQueuePolicyId = requireStateQueueMint
    ? requireDeploymentScriptHash(parsedDeploymentInfo, "stateQueueMint")
    : undefined;
  const fraudProofCataloguePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofCatalogueMint",
  );
  const hubOraclePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "hubOracleMint",
  );
  const deployedFraudProofPolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofMint",
  );
  const deployedFraudProofSpendHash = requireFraudProofSpend
    ? requireDeploymentScriptHash(parsedDeploymentInfo, "fraudProofSpend")
    : undefined;
  const parsedBlueprint = parseFaultProofBlueprint(blueprint);
  const referenceScriptAuthPolicyId =
    parseContractDeploymentReferenceScriptAuthPolicyId(
      deploymentInfo,
      "reference-script-auth minting",
    );
  const contracts = await buildOneCategoryFaultProofContracts({
    deploymentInfo: parsedDeploymentInfo,
    blueprint: parsedBlueprint,
    network,
    hubOraclePolicyId,
    fraudProofCataloguePolicyId,
    referenceScriptAuthPolicyId,
    categoryName,
  });
  const categoryContracts = contracts[categoryName];
  if (categoryContracts === undefined) {
    throw new Error(
      `${categoryLabel(categoryName)} builder did not return its category chain.`,
    );
  }
  if (
    categoryName === "fabricatedDeposit" ||
    categoryName === "fabricatedWithdrawal"
  ) {
    const historyChain =
      categoryName === "fabricatedDeposit"
        ? contracts.fabricatedDeposit
        : contracts.fabricatedWithdrawal;
    const entry =
      categoryName === "fabricatedDeposit"
        ? "fraudProofFabricatedDeposit"
        : "fraudProofFabricatedWithdrawal";
    if (
      historyChain?.history.retentionAddress !==
      parsedDeploymentInfo[entry]?.eventHistoryRetentionAddress
    )
      throw new Error(
        `${entry} retained-data address does not match the reapplied history validator`,
      );
  }
  if (categoryName === "transitionTrace") {
    const transition = contracts.transitionTrace;
    if (transition === undefined)
      throw new Error("transitionTrace deployed chain absent");
    for (const [key, reference] of Object.entries(
      TRANSITION_TRACE_YIELD_REFERENCES,
    )) {
      requireMatchingScriptHash({
        label: reference.entry,
        deployed: requireDeploymentScriptHash(
          parsedDeploymentInfo,
          reference.entry,
        ),
        derived:
          transition.yields[
            key as keyof typeof TRANSITION_TRACE_YIELD_REFERENCES
          ].withdrawalScriptHash,
      });
    }
    const expected = contracts.transitionTrace?.history.retentionAddresses;
    const declared =
      parsedDeploymentInfo.fraudProofTransitionTrace
        ?.eventHistoryRetentionAddresses;
    if (
      expected?.deposit !== declared?.deposit ||
      expected?.withdrawal !== declared?.withdrawal
    )
      throw new Error(
        "fraudProofTransitionTrace retained-data addresses do not match the reapplied history validators",
      );
  }
  const derivedFirstStepHash = categoryContracts.firstStep.spendingScriptHash;
  requireMatchingScriptHash({
    label: "fraudProofMint policy",
    deployed: deployedFraudProofPolicyId,
    derived: contracts.fraudProof.policyId,
  });
  if (deployedFraudProofSpendHash !== undefined) {
    requireMatchingScriptHash({
      label: "fraudProofSpend script",
      deployed: deployedFraudProofSpendHash,
      derived: contracts.fraudProof.spendingScriptHash,
    });
  }
  const deploymentEntries =
    FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY[categoryName];
  if (deploymentEntries.length > categoryContracts.steps.length) {
    throw new Error(
      `${categoryLabel(categoryName)} manifest declares ${deploymentEntries.length.toString()} step entries, but the compiled chain has only ${categoryContracts.steps.length.toString()} steps.`,
    );
  }
  for (const [stepIndex, deploymentEntry] of deploymentEntries.entries()) {
    const derivedStep = categoryContracts.steps[stepIndex];
    if (derivedStep === undefined) {
      throw new Error(
        `${categoryLabel(categoryName)} is missing compiled step ${(stepIndex + 1).toString()}.`,
      );
    }
    const deployedStepHash =
      stepIndex === 0
        ? requireDeploymentScriptHash(parsedDeploymentInfo, deploymentEntry)
        : parsedDeploymentInfo[deploymentEntry]?.scriptHash;
    if (deployedStepHash === undefined) {
      continue;
    }
    requireMatchingScriptHash({
      label: `${deploymentEntry} step-${(stepIndex + 1).toString().padStart(2, "0")} script`,
      deployed: deployedStepHash,
      derived: derivedStep.spendingScriptHash,
    });
  }
  if (categoryName === "valueNotPreserved") {
    const value = contracts.valueNotPreserved;
    if (value === undefined)
      throw new Error("valueNotPreserved deployed chain absent");
    for (const [name, step] of [
      [
        "fraudProofValueNotPreservedUnionAcceptedSource",
        value.unionAcceptedSource,
      ],
      ["fraudProofValueNotPreservedUnionForcedSource", value.unionForcedSource],
      ["fraudProofValueNotPreservedUnionEvent", value.unionEvent],
      ["fraudProofValueNotPreservedUnionPreState", value.unionPreState],
      ["fraudProofValueNotPreservedUnionInputs", value.unionInputs],
      ["fraudProofValueNotPreservedUnionInputValue", value.unionInputValue],
      ["fraudProofValueNotPreservedUnionAssets", value.unionAssets],
      ["fraudProofValueNotPreservedUnionFieldGrammar", value.unionFieldGrammar],
      ["fraudProofValueNotPreservedUnionOutputs", value.unionOutputs],
      ["fraudProofValueNotPreservedUnionOutputScan", value.unionOutputScan],
      ["fraudProofValueNotPreservedUnionMint", value.unionMint],
      ["fraudProofValueNotPreservedUnionUpdate", value.unionUpdate],
      ["fraudProofValueNotPreservedUnionTerminal", value.unionTerminal],
    ] as const) {
      const deployed = parsedDeploymentInfo[name]?.scriptHash;
      if (deployed !== undefined)
        requireMatchingScriptHash({
          label: name,
          deployed,
          derived: step.spendingScriptHash,
        });
    }
  }
  if (categoryName === "missingSignature") {
    const missing = contracts.missingSignature;
    if (missing === undefined)
      throw new Error("missingSignature deployed chain absent");
    for (const [name, step] of [
      ["fraudProofMissingSignatureForcedStep", missing.forcedStep],
      ["fraudProofMissingSignatureForcedSigner", missing.forcedSigner],
      ["fraudProofMissingSignatureForcedWitness", missing.forcedWitness],
    ] as const) {
      const deployed = parsedDeploymentInfo[name]?.scriptHash;
      if (deployed !== undefined)
        requireMatchingScriptHash({
          label: name,
          deployed,
          derived: step.spendingScriptHash,
        });
    }
  }
  if (categoryName === "networkId") {
    const auxiliary = categoryContracts as {
      readonly forcedStep?: { readonly spendingScriptHash: string };
      readonly forcedScan?: { readonly spendingScriptHash: string };
    };
    const forcedStep = auxiliary.forcedStep;
    const deployedForcedStepHash =
      parsedDeploymentInfo[NETWORK_ID_FORCED_STEP_DEPLOYMENT_ENTRY]?.scriptHash;
    if (forcedStep !== undefined && deployedForcedStepHash !== undefined) {
      requireMatchingScriptHash({
        label: `${NETWORK_ID_FORCED_STEP_DEPLOYMENT_ENTRY} forced-step script`,
        deployed: deployedForcedStepHash,
        derived: forcedStep.spendingScriptHash,
      });
    }
    const forcedScan = auxiliary.forcedScan;
    const deployedForcedScanHash =
      parsedDeploymentInfo[NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY]?.scriptHash;
    if (forcedScan !== undefined && deployedForcedScanHash !== undefined) {
      requireMatchingScriptHash({
        label: `${NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY} forced-scan script`,
        deployed: deployedForcedScanHash,
        derived: forcedScan.spendingScriptHash,
      });
    }
  }
  const readyCategory = await assertFraudProofCatalogueCategoryReady({
    catalogue,
    categoryName,
    expectedFirstStepHash: derivedFirstStepHash,
    deploymentMatchesFirstStep: true,
  });

  return {
    deploymentInfo: parsedDeploymentInfo,
    category: readyCategory,
    stateQueuePolicyId,
    fraudProofCataloguePolicyId,
    hubOraclePolicyId,
    contracts,
  };
};

export const resolveDoubleSpendDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedDoubleSpendDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "doubleSpend",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    doubleSpendCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as DoubleSpendFaultProofContracts,
  };
};
