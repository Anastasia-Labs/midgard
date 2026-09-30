import {
  type DeploymentManifest,
  type DeploymentManifestCardanoProtocolParameters,
  type DeploymentManifestContractEntry,
  parseDeploymentManifestEconomics,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type Network,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { type ContractDeploymentInfo } from "../inspect-contracts.js";
import { resolveFaultProofDeploymentContracts } from "../runtime.js";
import type { FraudProofRawL1FamilyDefinition } from "./raw-l1-family-derivation.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  type VerifiedFraudProofReleaseEconomicsPolicy,
} from "./release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthority,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy.js";

export const FRAUD_PROOF_WORKFLOW_DEPLOYMENT_BINDING =
  "midgard-fraud-proof-workflow-deployment-binding-v1" as const;

export type LucidDataSchema =
  FraudProofRawL1FamilyDefinition["computationThread"]["steps"][number]["datumSchema"];

export type FraudProofWorkflowDeploymentBinding<
  Category extends FraudProofCatalogueCategoryName,
> = {
  readonly bindingVersion: typeof FRAUD_PROOF_WORKFLOW_DEPLOYMENT_BINDING;
  readonly deploymentFingerprint: string;
  readonly blueprintHash: string;
  readonly network: Network;
  readonly blueprint: unknown;
  /** Complete verified document consumed by transaction builders. */
  readonly deploymentInfo: unknown;
  readonly contractEntries: ContractDeploymentInfo;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly cardanoProtocolParameters: DeploymentManifestCardanoProtocolParameters;
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
  readonly definition: FraudProofRawL1FamilyDefinition & {
    readonly category: Category;
  };
  readonly resolvedContracts: Awaited<
    ReturnType<typeof resolveFaultProofDeploymentContracts>
  >;
};

/** Closed authority view over the already verified finalized manifest. */
export const releaseFinalityAuthorityFromDeploymentBinding = (
  binding: FraudProofWorkflowDeploymentBinding<FraudProofCatalogueCategoryName>,
): FraudProofReleaseFinalityAuthority => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async ({ deploymentFingerprint }) => {
    if (deploymentFingerprint !== binding.deploymentFingerprint) {
      throw new Error(
        "workflow deployment fingerprint differs from the finalized manifest",
      );
    }
    return binding.releaseFinality;
  },
});

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const assertManifestBoundWorkflowSigner = ({
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

export const requireManifestBoundReferenceScriptUtxo = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: Pick<
    FraudProofWorkflowDeploymentBinding<FraudProofCatalogueCategoryName>,
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

export const isFieldPreimageCertificateContract = (
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

export const manifestContract = (
  manifest: DeploymentManifest,
  name: string,
): DeploymentManifestContractEntry => {
  const entry = manifest.contracts[name];
  if (entry === undefined) {
    throw new Error(`deployment manifest omitted ${name}`);
  }
  return entry;
};

export const scriptOf = (entry: DeploymentManifestContractEntry): Script => ({
  type: entry.contract.type,
  script: entry.contract.cborHex,
});

const sameOutRef = (
  left: DeploymentManifestContractEntry["refScriptUTxO"] | undefined,
  right: DeploymentManifestContractEntry["refScriptUTxO"] | undefined,
): boolean =>
  left === right ||
  (left !== null &&
    left !== undefined &&
    right !== null &&
    right !== undefined &&
    left.txHash === right.txHash &&
    left.outputIndex === right.outputIndex);

export const assertDeploymentInfoMatchesManifest = ({
  manifest,
  deploymentInfo,
}: {
  readonly manifest: DeploymentManifest;
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

const freezeManifestDocument = <Value>(value: Value): Value => {
  if (value !== null && typeof value === "object") {
    for (const child of Object.values(value)) freezeManifestDocument(child);
    Object.freeze(value);
  }
  return value;
};

export const finalizedManifest = (value: unknown): DeploymentManifest =>
  freezeManifestDocument(
    structuredClone(verifyFinalizedDeploymentManifest(value)),
  );

export const releasePolicies = (
  manifest: DeploymentManifest,
): {
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
} => {
  const finalityPolicy = manifest.l1Finality;
  const releaseFinality: VerifiedFraudProofReleaseFinalityPolicy = {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: manifest.manifestId,
    blueprintHash: manifest.artifacts.blueprintHash,
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
    blueprintHash: manifest.artifacts.blueprintHash,
    policyDigest:
      computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
    policy: economicsPolicy,
  };
  return { releaseFinality, releaseEconomics };
};
