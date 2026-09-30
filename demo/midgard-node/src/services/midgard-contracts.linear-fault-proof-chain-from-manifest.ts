import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRetentionAddress,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Network } from "@lucid-evolution/lucid";

import {
  faultProofStepContractName,
  type LegacyFaultProofFamily,
  type RegisteredLinearFaultProofCategory,
} from "../deployable-scripts.js";
import {
  faultProofStepsFromManifest,
  spendingValidatorFromManifest,
  withdrawalValidatorFromManifest,
} from "./midgard-contracts.assert-deployment-manifest-matches-config.js";

export const linearFaultProofChainFromManifest = <
  Category extends RegisteredLinearFaultProofCategory,
>(
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  category: Category,
): SDK.FaultProofContractChains[Category] => {
  const steps = faultProofStepsFromManifest(
    network,
    manifest,
    sourcePath,
    category,
  );
  const firstStep = steps[0];
  if (firstStep === undefined) {
    throw new Error(`Fault-proof chain has no first step: ${category}`);
  }
  if (category === "fabricatedDeposit" || category === "fabricatedWithdrawal") {
    const [, secondStep, thirdStep, fourthStep] = steps;
    if (
      steps.length !== 4 ||
      secondStep === undefined ||
      thirdStep === undefined ||
      fourthStep === undefined
    ) {
      throw new Error(`Expected four history proof steps for ${category}`);
    }
    const entry = manifest.contracts[faultProofStepContractName(category, 0)];
    const bounds = parseDeploymentManifestEventHistoryBounds(
      entry?.eventHistoryBounds,
    );
    return {
      firstStep,
      steps: [firstStep, secondStep, thirdStep, fourthStep] as const,
      history: {
        inlineLimitBytes: BigInt(bounds.inlineLimitBytes),
        maxPayloadBytes: BigInt(bounds.maxPayloadBytes),
        maxPayloadNodes: BigInt(bounds.maxPayloadNodes),
        retentionAddress: parseDeploymentManifestEventHistoryRetentionAddress(
          entry?.eventHistoryRetentionAddress,
        ),
      },
    } as SDK.FaultProofContractChains[Category];
  }
  if (category === "valueNotPreserved") {
    return {
      firstStep,
      steps,
      unionAcceptedSource: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionAcceptedSource",
      ),
      unionForcedSource: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionForcedSource",
      ),
      unionEvent: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionEvent",
      ),
      unionPreState: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionPreState",
      ),
      unionInputs: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionInputs",
      ),
      unionInputValue: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionInputValue",
      ),
      unionAssets: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionAssets",
      ),
      unionFieldGrammar: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionFieldGrammar",
      ),
      unionOutputs: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionOutputs",
      ),
      unionOutputScan: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionOutputScan",
      ),
      unionMint: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionMint",
      ),
      unionUpdate: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionUpdate",
      ),
      unionTerminal: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionTerminal",
      ),
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  if (category === "missingSignature") {
    return {
      firstStep,
      steps,
      forcedStep: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofMissingSignatureForcedStep",
      ),
      forcedSigner: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofMissingSignatureForcedSigner",
      ),
      forcedWitness: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofMissingSignatureForcedWitness",
      ),
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  if (category === "networkId") {
    // The forced (wrongful-rejection) door and the resumable output scan it
    // hands off to are side entrances into step 02, not third and fourth links
    // in the chain, so `buildNetworkIdChain` returns them outside `steps`.
    // Restoring either by step index would silently bind step 02's script to
    // an auxiliary role, so both are resolved by their own manifest names.
    return {
      firstStep,
      steps,
      forcedStep: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofNetworkIdForcedStep",
      ),
      forcedScan: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofNetworkIdForcedScan",
      ),
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  if (category === "minAda") {
    return {
      firstStep,
      steps,
      yields: {
        tx: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "fraudProofMinAdaStep02TxWithdraw",
        ),
        utxo: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "fraudProofMinAdaStep02UtxoWithdraw",
        ),
      },
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  if (category === "fieldPreimageLengthMismatch") {
    return {
      firstStep,
      steps,
      acceptedStep02: steps[1],
      forcedStep02: steps[2],
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  if (category === "scriptIntegrityHashMissing") {
    return {
      firstStep,
      steps,
      scriptGrammar: steps[3],
      scriptScan: steps[4],
      redeemerGrammar: steps[5],
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  return {
    firstStep,
    steps,
  } as unknown as SDK.FaultProofContractChains[Category];
};

export const legacyFaultProofChainFromManifest = <
  Family extends LegacyFaultProofFamily,
>(
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  family: Family,
): SDK.FaultProofContractChains[Family] => {
  const steps = faultProofStepsFromManifest(
    network,
    manifest,
    sourcePath,
    family,
  );
  const firstStep = steps[0];
  if (firstStep === undefined) {
    throw new Error(`Legacy fault-proof chain has no first step`);
  }
  return {
    firstStep,
    steps,
  } as unknown as SDK.FaultProofContractChains[Family];
};
