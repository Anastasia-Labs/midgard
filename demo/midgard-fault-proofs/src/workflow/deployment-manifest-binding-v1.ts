import { createHash } from "node:crypto";

import {
  type DeploymentManifestV1CardanoProtocolParameters,
  type DeploymentManifestV1Economics,
  type DeploymentManifestV1L1Finality,
  parseDeploymentManifestV1Economics,
  verifyFinalizedDeploymentManifestV1,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type Network,
  type Script,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  type ContractDeploymentInfo,
  parseContractDeploymentInfo,
} from "../inspect-contracts.js";
import { resolveFaultProofDeploymentContracts } from "../runtime.js";
import type { FraudProofRawL1FamilyDefinitionV1 } from "./raw-l1-family-derivation-v1.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigestV1,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_V1_SCHEMA_VERSION,
  type VerifiedFraudProofReleaseEconomicsPolicyV1,
} from "./release-economics-policy-v1.js";
import {
  computeFraudProofReleaseFinalityPolicyDigestV1,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthorityV1,
  type ReleaseL1FinalityPolicyV1,
  type VerifiedFraudProofReleaseFinalityPolicyV1,
} from "./release-finality-policy-v1.js";

export const FRAUD_PROOF_WORKFLOW_DEPLOYMENT_BINDING_V1 =
  "midgard-fraud-proof-workflow-deployment-binding-v1" as const;

type ManifestContractV1 = {
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

type FinalizedWorkflowManifestV1 = {
  readonly manifestId: string;
  readonly network: Network;
  readonly proofEvidence: {
    readonly digest: string;
    readonly blueprintHash: string;
  };
  readonly l1Finality: DeploymentManifestV1L1Finality;
  readonly economics: DeploymentManifestV1Economics;
  readonly cardanoProtocolParameters: {
    readonly snapshot: DeploymentManifestV1CardanoProtocolParameters;
  };
  readonly contracts: Readonly<Record<string, ManifestContractV1>>;
};

type LucidDataSchema =
  FraudProofRawL1FamilyDefinitionV1["computationThread"]["steps"][number]["datumSchema"];

export type FraudProofWorkflowDeploymentBindingV1<
  Category extends FraudProofCatalogueCategoryName,
> = {
  readonly bindingVersion: typeof FRAUD_PROOF_WORKFLOW_DEPLOYMENT_BINDING_V1;
  readonly deploymentFingerprint: string;
  readonly releaseIdentityDigest: string;
  readonly network: Network;
  readonly blueprint: unknown;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicyV1;
  readonly cardanoProtocolParameters: DeploymentManifestV1CardanoProtocolParameters;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly fieldPreimageCertificate: {
    readonly policyId: string;
    readonly mintingScript: Script;
  } | null;
  readonly referenceScriptsByContract: Readonly<
    Record<
      string,
      {
        readonly outRef: string;
        readonly scriptHash: string;
      }
    >
  >;
  readonly definition: FraudProofRawL1FamilyDefinitionV1 & {
    readonly category: Category;
  };
  readonly resolvedContracts: Awaited<
    ReturnType<typeof resolveFaultProofDeploymentContracts>
  >;
};

/** Closed authority view over the already verified finalized manifest. */
export const releaseFinalityAuthorityFromDeploymentBindingV1 = (
  binding: FraudProofWorkflowDeploymentBindingV1<FraudProofCatalogueCategoryName>,
): FraudProofReleaseFinalityAuthorityV1 => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
  verifyForWorkflow: async ({ deploymentFingerprint }) => {
    if (deploymentFingerprint !== binding.deploymentFingerprint) {
      throw new Error(
        "workflow deployment fingerprint differs from the finalized manifest",
      );
    }
    return binding.releaseFinality;
  },
});

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;

export const assertManifestBoundWorkflowSignerV1 = ({
  network,
  address,
  paymentKeyHash,
}: {
  readonly network: Network;
  readonly address: string;
  readonly paymentKeyHash: string;
}): void => {
  if (!HEX_28.test(paymentKeyHash)) {
    throw new Error("workflow signer payment credential is not 28-byte hex");
  }
  const expected = credentialToAddress(network, {
    type: "Key",
    hash: paymentKeyHash,
  });
  if (address !== expected) {
    throw new Error(
      "workflow signer address is not the manifest-network enterprise address for its payment credential",
    );
  }
};

export const requireManifestBoundReferenceScriptUtxoV1 = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: Pick<
    FraudProofWorkflowDeploymentBindingV1<FraudProofCatalogueCategoryName>,
    "referenceScriptsByContract"
  >;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO => {
  const expected = binding.referenceScriptsByContract[contractName];
  if (expected === undefined) {
    throw new Error(
      `finalized manifest has no published reference-script identity for ${contractName}`,
    );
  }
  const actualOutRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
  if (actualOutRef !== expected.outRef || utxo.scriptRef == null) {
    throw new Error(
      `${contractName} reference UTxO differs from finalized manifest identity`,
    );
  }
  const actualHash = validatorToScriptHash(utxo.scriptRef);
  if (actualHash !== expected.scriptHash) {
    throw new Error(
      `${contractName} reference UTxO script differs from finalized manifest identity`,
    );
  }
  return utxo;
};

const isScript = (value: unknown): value is Script => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return false;
  }
  const candidate = value as Readonly<Record<string, unknown>>;
  return (
    typeof candidate.script === "string" &&
    (candidate.type === "Native" ||
      candidate.type === "PlutusV1" ||
      candidate.type === "PlutusV2" ||
      candidate.type === "PlutusV3")
  );
};

const isFieldPreimageCertificateContract = (
  value: unknown,
): value is { readonly policyId: string; readonly mintingScript: Script } => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return false;
  }
  const candidate = value as Readonly<Record<string, unknown>>;
  return (
    typeof candidate.policyId === "string" && isScript(candidate.mintingScript)
  );
};

const manifestContract = (
  manifest: FinalizedWorkflowManifestV1,
  name: string,
): ManifestContractV1 => {
  const entry = manifest.contracts[name];
  if (entry === undefined) {
    throw new Error(`deployment manifest omitted ${name}`);
  }
  return entry;
};

const scriptOf = (entry: ManifestContractV1): Script => ({
  type: entry.contract.type,
  script: entry.contract.cborHex,
});

const sameOutRef = (
  left: ManifestContractV1["refScriptUTxO"] | undefined,
  right: ManifestContractV1["refScriptUTxO"] | undefined,
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
  readonly manifest: FinalizedWorkflowManifestV1;
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

const finalizedManifest = (value: unknown): FinalizedWorkflowManifestV1 => {
  const verified = verifyFinalizedDeploymentManifestV1(value);
  const manifest = verified as unknown as FinalizedWorkflowManifestV1;
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

const releasePolicies = (
  manifest: FinalizedWorkflowManifestV1,
): {
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicyV1;
} => {
  const finalityPolicy = manifest.l1Finality as ReleaseL1FinalityPolicyV1;
  const releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1 = {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
    deploymentIdentityDigest: manifest.manifestId,
    releaseIdentityDigest: manifest.proofEvidence.digest,
    policyDigest:
      computeFraudProofReleaseFinalityPolicyDigestV1(finalityPolicy),
    policy: finalityPolicy,
  };
  const compiled = parseDeploymentManifestV1Economics(manifest.economics);
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
  const releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicyV1 = {
    schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_V1_SCHEMA_VERSION,
    deploymentIdentityDigest: manifest.manifestId,
    releaseIdentityDigest: manifest.proofEvidence.digest,
    policyDigest:
      computeFraudProofReleaseEconomicsPolicyDigestV1(economicsPolicy),
    policy: economicsPolicy,
  };
  return { releaseFinality, releaseEconomics };
};

/**
 * Builds the family observation identity from one finalized deployment
 * manifest and the exact blueprint bytes committed by that manifest. The same
 * parsed blueprint/deployment-info pair is returned for transaction builders,
 * preventing a caller-selected network or parallel contract identity.
 */
export const bindFraudProofWorkflowDeploymentV1 = async <
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
  readonly stepDatumSchemas: readonly LucidDataSchema[];
}): Promise<FraudProofWorkflowDeploymentBindingV1<Category>> => {
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
  const resolvedContracts = await resolveFaultProofDeploymentContracts({
    blueprint,
    deploymentInfo: deploymentInfoValue,
    network: manifest.network,
    categoryName: category,
    requireStateQueueMint: true,
    requireFraudProofSpend: true,
  });
  const chain = resolvedContracts.contracts[category];
  if (chain === undefined || chain.steps.length !== stepDatumSchemas.length) {
    throw new Error(
      `${category} deployment binding expected ${stepDatumSchemas.length.toString()} computation steps`,
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
    bindingVersion: FRAUD_PROOF_WORKFLOW_DEPLOYMENT_BINDING_V1,
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
          role: `computation_thread_step_0${(index + 1).toString()}` as
            | "computation_thread_step_01"
            | "computation_thread_step_02"
            | "computation_thread_step_03"
            | "computation_thread_step_04"
            | "computation_thread_step_05"
            | "computation_thread_step_06"
            | "computation_thread_step_07"
            | "computation_thread_step_08"
            | "computation_thread_step_09",
          address: step.spendingScriptAddress,
          datumSchema: stepDatumSchemas[index]!,
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
