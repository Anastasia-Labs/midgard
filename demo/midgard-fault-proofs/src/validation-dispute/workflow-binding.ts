import { createHash } from "node:crypto";

import {
  type DeploymentManifestCardanoProtocolParameters,
  type DeploymentManifestEconomics,
  type DeploymentManifestL1Finality,
  parseDeploymentManifestEconomics,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type FraudProofCatalogueCategoryName,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import {
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  type ContractDeploymentInfo,
  parseContractDeploymentInfo,
} from "../inspect-contracts.js";
import {
  type ResolvedValidationTraceDisputeDeploymentContracts,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../runtime.js";
import {
  FRAUD_PROOF_WORKFLOW_DEPLOYMENT_BINDING,
  type FraudProofWorkflowDeploymentBinding,
} from "../workflow/deployment-manifest-binding.js";
import type { FraudProofRawL1FamilyDefinition } from "../workflow/raw-l1-family-derivation.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  type VerifiedFraudProofReleaseEconomicsPolicy,
} from "../workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type ReleaseL1FinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../workflow/release-finality-policy.js";
import {
  assertValidationTraceDisputeRosterIsManifestBound,
  VALIDATION_TRACE_DISPUTE_CATEGORY,
  VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
} from "./workflow-family.js";

/**
 * Ruling R1: the family-specific deployment binding for the sole interactive
 * fault-proof family. It mirrors the shared
 * `bindFraudProofWorkflowDeployment` checks exactly, except that the linear
 * chain's `steps.length === stepDatumSchemas.length` gate and the nine-role
 * computation-step synthesis are replaced by the family resolver
 * (`resolveValidationTraceDisputeDeploymentContracts`) and a single-step raw
 * observation definition anchored at the dispute opener — the only chain
 * position whose datum the shared raw derivation can authenticate. Thread
 * positions across the interactive chain's addresses are derived family-
 * locally (see workflow-chain-state.ts); the shared 17-step caps are never
 * raised.
 */
export type ValidationTraceDisputeWorkflowDeploymentBinding = Omit<
  FraudProofWorkflowDeploymentBinding<"validationTraceDispute">,
  "resolvedContracts"
> & {
  readonly resolvedContracts: ResolvedValidationTraceDisputeDeploymentContracts;
};

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;

type ManifestContract = {
  readonly scriptHash: string;
  readonly contract: {
    readonly type: Script["type"];
    readonly cborHex: string;
  };
  readonly refScriptUTxO: {
    readonly txHash: string;
    readonly outputIndex: number;
  } | null;
  readonly fraudProofCatalogue?: {
    readonly root: string;
    readonly categories: Readonly<
      Record<
        FraudProofCatalogueCategoryName,
        {
          readonly categoryId: string;
          readonly scriptHash: string;
          readonly membershipProofCbor: string;
        }
      >
    >;
  };
};

type FinalizedWorkflowManifest = {
  readonly manifestId: string;
  readonly network: Network;
  readonly proofEvidence: {
    readonly digest: string;
    readonly blueprintHash: string;
  };
  readonly l1Finality: DeploymentManifestL1Finality;
  readonly economics: DeploymentManifestEconomics;
  readonly cardanoProtocolParameters: {
    readonly snapshot: DeploymentManifestCardanoProtocolParameters;
  };
  readonly contracts: Readonly<Record<string, ManifestContract>>;
};

const finalizedManifest = (value: unknown): FinalizedWorkflowManifest => {
  const verified = verifyFinalizedDeploymentManifest(value);
  const manifest = verified as unknown as FinalizedWorkflowManifest;
  if (
    !HEX_32.test(manifest.manifestId) ||
    !HEX_32.test(manifest.proofEvidence.digest) ||
    !HEX_32.test(manifest.proofEvidence.blueprintHash)
  ) {
    throw new Error(
      "finalized deployment manifest has invalid release or blueprint identity",
    );
  }
  return manifest;
};

const manifestContract = (
  manifest: FinalizedWorkflowManifest,
  name: string,
): ManifestContract => {
  const entry = manifest.contracts[name];
  if (entry === undefined) {
    throw new Error(`deployment manifest omitted ${name}`);
  }
  return entry;
};

const scriptOf = (entry: ManifestContract): Script => ({
  type: entry.contract.type,
  script: entry.contract.cborHex,
});

const sameOutRef = (
  left: ManifestContract["refScriptUTxO"] | undefined,
  right: ManifestContract["refScriptUTxO"] | undefined,
): boolean =>
  left === right ||
  (left !== null &&
    left !== undefined &&
    right !== null &&
    right !== undefined &&
    left.txHash === right.txHash &&
    left.outputIndex === right.outputIndex);

const assertDeploymentInfoMatchesManifest = ({
  manifest,
  deploymentInfo,
}: {
  readonly manifest: FinalizedWorkflowManifest;
  readonly deploymentInfo: ContractDeploymentInfo;
}): void => {
  const manifestNames = Object.keys(manifest.contracts).sort();
  const infoNames = Object.keys(deploymentInfo).sort();
  if (
    manifestNames.length !== infoNames.length ||
    manifestNames.some((name, index) => name !== infoNames[index])
  ) {
    throw new Error(
      "contract deployment info does not enumerate the finalized manifest contracts",
    );
  }
  for (const name of manifestNames) {
    const expected = manifestContract(manifest, name);
    const actual = deploymentInfo[name]!;
    if (
      actual.scriptHash !== expected.scriptHash ||
      actual.contract?.type !== expected.contract.type ||
      actual.contract.cborHex !== expected.contract.cborHex ||
      !sameOutRef(actual.refScriptUTxO, expected.refScriptUTxO)
    ) {
      throw new Error(
        `contract deployment info changed finalized manifest contract ${name}`,
      );
    }
  }
  const expectedCatalogue = manifestContract(
    manifest,
    "fraudProofCatalogueMint",
  ).fraudProofCatalogue;
  const actualCatalogue =
    deploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (
    expectedCatalogue === undefined ||
    actualCatalogue === undefined ||
    JSON.stringify(actualCatalogue) !== JSON.stringify(expectedCatalogue)
  ) {
    throw new Error(
      "contract deployment info changed the finalized fraud-proof catalogue",
    );
  }
};

const releasePolicies = (
  manifest: FinalizedWorkflowManifest,
): {
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
} => {
  const finalityPolicy = manifest.l1Finality as ReleaseL1FinalityPolicy;
  const releaseFinality: VerifiedFraudProofReleaseFinalityPolicy = {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: manifest.manifestId,
    releaseIdentityDigest: manifest.proofEvidence.digest,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
    policy: finalityPolicy,
  };
  const compiled = parseDeploymentManifestEconomics(manifest.economics);
  const economicsPolicy = {
    profile: compiled.profile,
    requiredBondLovelace: compiled.requiredBondLovelace.toString(),
    slashingPenaltyLovelace: compiled.slashingPenaltyLovelace.toString(),
    fraudProverRewardLovelace: compiled.fraudProverRewardLovelace.toString(),
    inactivitySlashingPenaltyLovelace:
      compiled.inactivitySlashingPenaltyLovelace.toString(),
    proverCollateralFloorLovelace:
      compiled.proverCollateralFloorLovelace.toString(),
  };
  const releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy = {
    schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: manifest.manifestId,
    releaseIdentityDigest: manifest.proofEvidence.digest,
    policyDigest:
      computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
    policy: economicsPolicy,
  };
  return { releaseFinality, releaseEconomics };
};

/**
 * Verifies one finalized deployment manifest against the exact blueprint
 * bytes it committed, resolves the family chain through the same resolver
 * every dispute submitter uses, and returns the closed identity the
 * production workflow observes and actuates under.
 */
export const bindValidationTraceDisputeWorkflowDeployment = async ({
  manifest: manifestValue,
  blueprintJson,
  deploymentInfo: deploymentInfoValue,
  headerHash,
  proverCredential,
}: {
  readonly manifest: unknown;
  readonly blueprintJson: string;
  readonly deploymentInfo: unknown;
  readonly headerHash: string;
  readonly proverCredential: string;
}): Promise<ValidationTraceDisputeWorkflowDeploymentBinding> => {
  assertValidationTraceDisputeRosterIsManifestBound();
  const manifest = finalizedManifest(manifestValue);
  const blueprintHash = createHash("sha256")
    .update(blueprintJson)
    .digest("hex");
  if (blueprintHash !== manifest.proofEvidence.blueprintHash) {
    throw new Error(
      `blueprint SHA-256 does not match the finalized deployment manifest: expected=${manifest.proofEvidence.blueprintHash} actual=${blueprintHash}`,
    );
  }
  let blueprint: unknown;
  try {
    blueprint = JSON.parse(blueprintJson) as unknown;
  } catch {
    throw new Error("deployment-manifest blueprint is not valid JSON");
  }
  const deploymentInfo = parseContractDeploymentInfo(deploymentInfoValue);
  assertDeploymentInfoMatchesManifest({ manifest, deploymentInfo });
  const resolvedContracts =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo: deploymentInfoValue,
      network: manifest.network,
      requireStateQueueMint: true,
      requireFraudProofSpend: true,
    });
  const chain = resolvedContracts.contracts.validationTraceDispute;
  const categoryIdentity = manifestContract(manifest, "fraudProofCatalogueMint")
    .fraudProofCatalogue?.categories[VALIDATION_TRACE_DISPUTE_CATEGORY];
  if (
    categoryIdentity === undefined ||
    categoryIdentity.categoryId !==
      resolvedContracts.validationTraceDisputeCategory.categoryId ||
    categoryIdentity.categoryId !== VALIDATION_TRACE_DISPUTE_CATEGORY_ID ||
    categoryIdentity.scriptHash !== chain.firstStep.spendingScriptHash
  ) {
    throw new Error(
      "validationTraceDispute deployment catalogue identity changed",
    );
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
  for (const [label, entry] of [
    ["stateQueueSpend", stateQueueSpend],
    ["fraudProofSpend", fraudProofSpend],
    ["fraudProofCatalogueSpend", catalogueSpend],
    ["activeOperatorsSpend", activeSpend],
    ["retiredOperatorsSpend", retiredSpend],
    ["schedulerSpend", schedulerSpend],
  ] as const) {
    if (validatorToScriptHash(scriptOf(entry)) !== entry.scriptHash) {
      throw new Error(
        `deployment manifest ${label} script bytes/hash disagree`,
      );
    }
  }
  const policies = releasePolicies(manifest);
  const definition: FraudProofRawL1FamilyDefinition & {
    readonly category: "validationTraceDispute";
  } = {
    category: VALIDATION_TRACE_DISPUTE_CATEGORY,
    categoryId: categoryIdentity.categoryId,
    headerHash,
    proverCredential,
    stateQueue: {
      policyId: stateQueueMint.scriptHash,
      address: validatorToAddress(manifest.network, scriptOf(stateQueueSpend)),
    },
    computationThread: {
      policyId: resolvedContracts.contracts.computationThread.policyId,
      // The single raw-scoped position: the opener is where init parks the
      // thread with a `FraudProofComputationThreadStepDatum`, the only datum
      // shape the shared derivation's prover-ownership check can decode for
      // this family. Later interactive positions are derived family-locally.
      steps: [
        {
          role: "computation_thread_step_01",
          address: chain.opener.spendingScriptAddress,
          datumSchema: FraudProofComputationThreadStepDatum,
        },
      ],
    },
    proofToken: {
      policyId: fraudProofMint.scriptHash,
      address: validatorToAddress(manifest.network, scriptOf(fraudProofSpend)),
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
  };
  return {
    bindingVersion: FRAUD_PROOF_WORKFLOW_DEPLOYMENT_BINDING,
    deploymentFingerprint: manifest.manifestId,
    releaseIdentityDigest: manifest.proofEvidence.digest,
    network: manifest.network,
    blueprint,
    deploymentInfo,
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
    fieldPreimageCertificate: null,
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
    definition,
    resolvedContracts,
  };
};
