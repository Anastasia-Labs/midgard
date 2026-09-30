import { createHash } from "node:crypto";

import { parseDeploymentManifestEconomics } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  parseContractDeploymentInfo,
  parseContractDeploymentReferenceScriptAuthPolicyId,
} from "../inspect-contracts.js";
import { resolveFaultProofDeploymentContracts } from "../runtime.js";
import {
  assertDeploymentInfoMatchesManifest,
  finalizedManifest,
  FRAUD_PROOF_WORKFLOW_DEPLOYMENT_BINDING,
  type FraudProofWorkflowDeploymentBinding,
  HEX_28,
  isFieldPreimageCertificateContract,
  type LucidDataSchema,
  manifestContract,
  releasePolicies,
  scriptOf,
} from "./deployment-manifest-binding.assert-deployment-info-matches-manifest.js";
import type { FraudProofRawL1TerminalDefinition } from "./raw-l1-family-derivation.js";
import type { FraudProofRawL1ComputationStepRole } from "./raw-l1-snapshot.js";

/**
 * Builds the family observation identity from one finalized deployment
 * manifest and the exact blueprint bytes committed by that manifest. The same
 * parsed blueprint/deployment-info pair is returned for transaction builders,
 * preventing a caller-selected network or parallel contract identity.
 */
const bindFraudProofDeployment = async <
  Category extends FraudProofCatalogueCategoryName,
>({
  manifest: manifestValue,
  blueprintJson,
  deploymentInfo: deploymentInfoValue,
  category,
  headerHash,
  proverCredential,
  stepDatumSchemas,
}: {
  readonly manifest: unknown;
  readonly blueprintJson: string;
  readonly deploymentInfo: unknown;
  readonly category: Category;
  readonly headerHash: string;
  readonly proverCredential: string;
  readonly stepDatumSchemas: readonly LucidDataSchema[] | null;
}): Promise<FraudProofWorkflowDeploymentBinding<Category>> => {
  const manifest = finalizedManifest(manifestValue);
  const blueprintHash = createHash("sha256")
    .update(blueprintJson)
    .digest("hex");
  if (blueprintHash !== manifest.artifacts.blueprintHash) {
    throw new Error(
      `blueprint SHA-256 does not match the finalized deployment manifest: expected=${manifest.artifacts.blueprintHash} actual=${blueprintHash}`,
    );
  }
  let blueprint: unknown;
  try {
    blueprint = JSON.parse(blueprintJson) as unknown;
  } catch {
    throw new Error("deployment-manifest blueprint is not valid JSON");
  }
  const suppliedDocument: unknown = structuredClone(deploymentInfoValue);
  assertDeploymentInfoMatchesManifest({
    manifest,
    deploymentInfo: parseContractDeploymentInfo(suppliedDocument),
  });
  if (
    parseContractDeploymentReferenceScriptAuthPolicyId(
      suppliedDocument,
      "reference-script-auth minting",
    ) !==
    parseContractDeploymentReferenceScriptAuthPolicyId(
      manifest,
      "reference-script-auth minting",
    )
  ) {
    throw new Error(
      "contract deployment info changed the finalized reference-script authority",
    );
  }
  if (
    typeof suppliedDocument === "object" &&
    suppliedDocument !== null &&
    "economics" in suppliedDocument &&
    JSON.stringify(
      parseDeploymentManifestEconomics(suppliedDocument.economics),
    ) !== JSON.stringify(parseDeploymentManifestEconomics(manifest.economics))
  ) {
    throw new Error(
      "contract deployment info changed the finalized manifest economics",
    );
  }
  // Builders consume the complete verified release, including its economics
  // and reference authority, rather than a parallel contract-only document.
  const deploymentDocument = manifest;
  const deploymentInfo = parseContractDeploymentInfo(deploymentDocument);
  const resolvedContracts = await resolveFaultProofDeploymentContracts({
    blueprint,
    deploymentInfo: deploymentDocument,
    network: manifest.network,
    categoryName: category,
    requireStateQueueMint: true,
    requireFraudProofSpend: true,
  });
  const chain = resolvedContracts.contracts[category];
  if (
    chain === undefined ||
    (stepDatumSchemas !== null &&
      chain.steps.length !== stepDatumSchemas.length)
  ) {
    throw new Error(
      `${category} deployment binding expected ${stepDatumSchemas?.length.toString() ?? "published"} computation steps`,
    );
  }
  const categoryIdentity = manifestContract(manifest, "fraudProofCatalogueMint")
    .fraudProofCatalogue?.categories[category];
  if (
    categoryIdentity === undefined ||
    categoryIdentity.categoryId !== resolvedContracts.category.categoryId ||
    categoryIdentity.scriptHash !== chain.firstStep.spendingScriptHash
  ) {
    throw new Error(`${category} deployment catalogue identity changed`);
  }
  if (!HEX_28.test(headerHash) || !HEX_28.test(proverCredential)) {
    throw new Error(
      "workflow header and prover credential must be canonical 28-byte hex",
    );
  }
  const stateQueueSpend = manifestContract(manifest, "stateQueueSpend");
  const stateQueueMint = manifestContract(manifest, "stateQueueMint");
  const fraudProofSpend = manifestContract(manifest, "fraudProofSpend");
  const fraudProofMint = manifestContract(manifest, "fraudProofMint");
  const catalogueSpend = manifestContract(manifest, "fraudProofCatalogueSpend");
  const activeSpend = manifestContract(manifest, "activeOperatorsSpend");
  const activeMint = manifestContract(manifest, "activeOperatorsMint");
  const retiredSpend = manifestContract(manifest, "retiredOperatorsSpend");
  const retiredMint = manifestContract(manifest, "retiredOperatorsMint");
  const schedulerSpend = manifestContract(manifest, "schedulerSpend");
  const policies = releasePolicies(manifest);
  const fieldPreimageCertificateCandidate =
    "fieldPreimageCertificate" in resolvedContracts.contracts
      ? resolvedContracts.contracts.fieldPreimageCertificate
      : undefined;
  if (
    fieldPreimageCertificateCandidate !== undefined &&
    !isFieldPreimageCertificateContract(fieldPreimageCertificateCandidate)
  ) {
    throw new Error(
      "resolved field-preimage certificate contract has an invalid shape",
    );
  }
  const fieldPreimageCertificate = fieldPreimageCertificateCandidate;
  if (fieldPreimageCertificate !== undefined) {
    const deployed = manifestContract(manifest, "fieldPreimageCertificateMint");
    if (
      fieldPreimageCertificate.policyId !== deployed.scriptHash ||
      validatorToScriptHash(fieldPreimageCertificate.mintingScript) !==
        deployed.scriptHash
    ) {
      throw new Error(
        "field-preimage certificate policy differs from the finalized manifest",
      );
    }
  }
  return {
    bindingVersion: FRAUD_PROOF_WORKFLOW_DEPLOYMENT_BINDING,
    deploymentFingerprint: manifest.manifestId,
    blueprintHash: manifest.artifacts.blueprintHash,
    network: manifest.network,
    blueprint,
    deploymentInfo: deploymentDocument,
    contractEntries: deploymentInfo,
    ...policies,
    cardanoProtocolParameters: manifest.cardanoProtocolParameters.snapshot,
    catalogue: {
      policyId: resolvedContracts.fraudProofCataloguePolicyId,
      spendingScriptAddress: validatorToAddress(
        manifest.network,
        scriptOf(catalogueSpend),
      ),
      root: manifestContract(manifest, "fraudProofCatalogueMint")
        .fraudProofCatalogue!.root,
    },
    fieldPreimageCertificate:
      fieldPreimageCertificate === undefined
        ? null
        : {
            policyId: fieldPreimageCertificate.policyId,
            mintingScript: fieldPreimageCertificate.mintingScript,
          },
    referenceScriptsByContract: Object.freeze(
      Object.fromEntries(
        Object.entries(manifest.contracts).flatMap(([name, entry]) =>
          entry.refScriptUTxO === null
            ? []
            : [
                [
                  name,
                  {
                    outRef: `${entry.refScriptUTxO.txHash}#${entry.refScriptUTxO.outputIndex.toString()}`,
                    scriptHash: entry.scriptHash,
                  },
                ] as const,
              ],
        ),
      ),
    ),
    definition: {
      category,
      categoryId: categoryIdentity.categoryId,
      headerHash,
      proverCredential,
      stateQueue: {
        policyId: stateQueueMint.scriptHash,
        address: validatorToAddress(
          manifest.network,
          scriptOf(stateQueueSpend),
        ),
      },
      computationThread: {
        policyId: resolvedContracts.contracts.computationThread.policyId,
        steps: chain.steps.map((step, index) => ({
          role: `computation_thread_step_${(index + 1).toString().padStart(2, "0")}` as FraudProofRawL1ComputationStepRole,
          address: step.spendingScriptAddress,
          datumSchema: stepDatumSchemas?.[index],
        })),
      },
      proofToken: {
        policyId: fraudProofMint.scriptHash,
        address: validatorToAddress(
          manifest.network,
          scriptOf(fraudProofSpend),
        ),
      },
      operatorDirectory: {
        activePolicyId: activeMint.scriptHash,
        activeAddress: validatorToAddress(
          manifest.network,
          scriptOf(activeSpend),
        ),
        retiredPolicyId: retiredMint.scriptHash,
        retiredAddress: validatorToAddress(
          manifest.network,
          scriptOf(retiredSpend),
        ),
      },
      schedulerAddress: validatorToAddress(
        manifest.network,
        scriptOf(schedulerSpend),
      ),
    },
    resolvedContracts,
  };
};

/** Full executable binding retains the exact per-step datum schema contract. */
export const bindFraudProofWorkflowDeployment = <
  Category extends FraudProofCatalogueCategoryName,
>(
  input: Omit<
    Parameters<typeof bindFraudProofDeployment<Category>>[0],
    "stepDatumSchemas"
  > & {
    readonly stepDatumSchemas: readonly LucidDataSchema[];
  },
): Promise<FraudProofWorkflowDeploymentBinding<Category>> =>
  bindFraudProofDeployment(input);

export type FraudProofTerminalDeploymentBinding = Pick<
  FraudProofWorkflowDeploymentBinding<FraudProofCatalogueCategoryName>,
  "deploymentFingerprint" | "releaseFinality" | "releaseEconomics"
> & { readonly definition: FraudProofRawL1TerminalDefinition };

/** Completion reads have no live-thread decoder or transaction capability. */
export const bindFraudProofTerminalDeployment = async (
  input: Omit<
    Parameters<typeof bindFraudProofDeployment>[0],
    "stepDatumSchemas"
  >,
): Promise<FraudProofTerminalDeploymentBinding> => {
  const binding = await bindFraudProofDeployment({
    ...input,
    stepDatumSchemas: null,
  });
  return {
    deploymentFingerprint: binding.deploymentFingerprint,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: {
      ...binding.definition,
      computationThread: {
        policyId: binding.definition.computationThread.policyId,
        steps: binding.definition.computationThread.steps.map(
          ({ role, address }) => ({ role, address }),
        ),
      },
    },
  };
};
