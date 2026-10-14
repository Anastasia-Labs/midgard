import { createHash } from "node:crypto";

import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifest,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { releaseRecord } from "@al-ft/midgard-core/deployment-release";
import {
  type FraudProofCatalogueCategoryName,
  parseFaultProofBlueprint,
  verifyBlueprintDeploymentRegistry,
} from "@al-ft/midgard-sdk";

import {
  digestField,
  PAYMENT_KEY_HASH,
} from "./funding-requirements.workflow-funding-controlled-output.js";
import { computeFraudProofReleaseEconomicsPolicyDigest } from "./release-economics-policy.js";

export const WORKFLOW_FUNDING_MEASUREMENT_TOOL =
  "midgard-workflow-funding-measurement-v1" as const;

export type WorkflowFundingMeasurementIdentity = Readonly<{
  schemaVersion: typeof WORKFLOW_FUNDING_MEASUREMENT_TOOL;
  environment: "emulator" | "local-node";
  manifestId: string;
  blueprintSha256: string;
  aikenSourceSha256: string;
  compilerVersion: string;
  sourceSnapshotSha256: string;
  deploymentProfileDigest: string;
  protocolParametersDigest: string;
  economicsPolicyDigest: string;
  deploymentParameterRegistryDigest: string;
  fundingPaymentKeyHash: string;
  categories: readonly FraudProofCatalogueCategoryName[];
}>;

export interface WorkflowFundingMeasurementContext {
  readonly schemaVersion: typeof WORKFLOW_FUNDING_MEASUREMENT_TOOL;
}
const contexts = new WeakMap<
  object,
  Readonly<{
    manifest: DeploymentManifest;
    identity: WorkflowFundingMeasurementIdentity;
  }>
>();

/**
 * Snapshots the actual finalized deployment and its build record. These are
 * measurement inputs, not a new release authority or a compiler attestation.
 * Release authoring must still authenticate their retained source/build evidence.
 */
export const createWorkflowFundingMeasurementContext = (input: {
  readonly environment: "emulator" | "local-node";
  readonly manifest: unknown;
  readonly blueprintBytes: Uint8Array;
  readonly deploymentParameterRegistry: unknown;
  readonly buildRecord: Readonly<{
    blueprintHash: string;
    sourceHash: string;
    compiler: string;
    profile: Readonly<{ name: string }>;
    profileDigest: string;
  }>;
  readonly expectedCompilerVersion: string;
  readonly sourceSnapshotSha256: string;
  readonly fundingPaymentKeyHash: string;
}): WorkflowFundingMeasurementContext => {
  if (input.environment !== "emulator" && input.environment !== "local-node")
    throw new Error("funding measurement environment is unsupported");
  const manifest = verifyFinalizedDeploymentManifest(
    structuredClone(input.manifest),
  );
  const hash = createHash("sha256").update(input.blueprintBytes).digest("hex");
  const record = input.buildRecord;
  if (
    hash !== manifest.artifacts.blueprintHash ||
    hash !== record.blueprintHash
  )
    throw new Error(
      "funding measurement blueprint differs from deployment/build record",
    );
  const registry = releaseRecord(
    input.deploymentParameterRegistry,
    "funding measurement registry",
    [
      "schemaVersion",
      "manifestId",
      "blueprintHash",
      "deployment",
      "referenceScripts",
    ],
  );
  if (
    registry.schemaVersion !== "midgard-deployment-parameter-registry-v1" ||
    registry.manifestId !== manifest.manifestId ||
    registry.blueprintHash !== hash ||
    computeDeploymentManifestJsonDigest(registry.referenceScripts) !==
      computeDeploymentManifestJsonDigest(manifest.referenceScripts)
  )
    throw new Error(
      "funding measurement parameter registry belongs to another deployment",
    );
  const reconstructed = verifyBlueprintDeploymentRegistry(
    parseFaultProofBlueprint(
      JSON.parse(Buffer.from(input.blueprintBytes).toString("utf8")),
    ),
    registry.deployment,
  );
  if (
    reconstructed.contracts.length !== Object.keys(manifest.contracts).length ||
    reconstructed.contracts.some((entry) => {
      const contract = manifest.contracts[entry.contract];
      return (
        contract === undefined ||
        contract.scriptHash !== entry.scriptHash ||
        contract.contract.type !== entry.script.type ||
        contract.contract.cborHex !== entry.script.script ||
        entry.referenceScriptRole !==
          (Object.entries(
            DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
          ).find(([, name]) => name === entry.contract)?.[0] ?? null)
      );
    })
  )
    throw new Error(
      "funding measurement parameter registry differs from deployed programs",
    );
  if (
    record.compiler !== input.expectedCompilerVersion ||
    !/^aiken v[0-9]+\.[0-9]+\.[0-9]+\+[0-9a-f]+$/u.test(record.compiler)
  )
    throw new Error(
      "funding measurement compiler differs from the pinned build",
    );
  if (
    record.profile.name !== manifest.deploymentProfile.name ||
    record.profileDigest !== manifest.deploymentProfileDigest
  )
    throw new Error(
      "funding measurement profile differs from the actual selected build",
    );
  if (!PAYMENT_KEY_HASH.test(input.fundingPaymentKeyHash))
    throw new Error(
      "funding measurement credential must be a 28-byte key hash",
    );
  const catalogue =
    manifest.contracts.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined)
    throw new Error("funding measurement deployment has no catalogue");
  const categories = Object.keys(
    catalogue.categories,
  ) as FraudProofCatalogueCategoryName[];
  const identity: WorkflowFundingMeasurementIdentity = Object.freeze({
    schemaVersion: WORKFLOW_FUNDING_MEASUREMENT_TOOL,
    environment: input.environment,
    manifestId: manifest.manifestId,
    blueprintSha256: hash,
    aikenSourceSha256: digestField(
      record.sourceHash,
      "funding measurement Aiken sources",
    ),
    compilerVersion: record.compiler,
    sourceSnapshotSha256: digestField(
      input.sourceSnapshotSha256,
      "funding measurement source snapshot",
    ),
    deploymentProfileDigest: manifest.deploymentProfileDigest,
    protocolParametersDigest: manifest.cardanoProtocolParameters.digest,
    economicsPolicyDigest: computeFraudProofReleaseEconomicsPolicyDigest({
      profile: manifest.economics.profile,
      requiredBondLovelace: manifest.economics.requiredBondLovelace.toString(),
      slashingPenaltyLovelace:
        manifest.economics.slashingPenaltyLovelace.toString(),
      fraudProverRewardLovelace:
        manifest.economics.fraudProverRewardLovelace.toString(),
      inactivitySlashingPenaltyLovelace:
        manifest.economics.inactivitySlashingPenaltyLovelace.toString(),
      proverCollateralFloorLovelace:
        manifest.economics.proverCollateralFloorLovelace.toString(),
    }),
    deploymentParameterRegistryDigest:
      computeDeploymentManifestJsonDigest(registry),
    fundingPaymentKeyHash: input.fundingPaymentKeyHash,
    categories: Object.freeze(categories),
  });
  const context = Object.freeze({
    schemaVersion: WORKFLOW_FUNDING_MEASUREMENT_TOOL,
  });
  contexts.set(context, { manifest, identity });
  return context;
};

export const readWorkflowFundingMeasurementContext = (
  context: WorkflowFundingMeasurementContext,
) => {
  const cell = contexts.get(context);
  if (cell === undefined)
    throw new Error("funding measurement context is not admitted");
  // No caller can mutate the retained manifest by changing a returned view.
  return { identity: cell.identity, manifest: structuredClone(cell.manifest) };
};
