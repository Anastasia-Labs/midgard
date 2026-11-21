import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk/core";

import type { WorkflowActuationPermit } from "./actuation-permit.js";
import {
  assertWorkflowAdaptersReady,
  missingWorkflowAdapters,
  validateWorkflowAdapterCoverage,
  WORKFLOW_ADAPTER_REGISTRATIONS,
  WORKFLOW_ADAPTER_REGISTRY_SCHEMA_VERSION,
  workflowAdapterRunner,
  type WorkflowApplicationRegistry,
} from "./adapters.js";
import type { WorkflowSends } from "./in-flight.js";
import type { WalletOnlySend } from "./wallet-only-send-gate.js";

export const workflowReadinessReport = (
  launchScope: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  applicationRegistry?: WorkflowApplicationRegistry,
) => {
  const registryRegistrations =
    applicationRegistry?.registrations ?? WORKFLOW_ADAPTER_REGISTRATIONS;
  validateWorkflowAdapterCoverage(registryRegistrations);
  const registrations = registryRegistrations.filter((registration) =>
    launchScope.includes(registration.category),
  );
  return {
    schemaVersion: WORKFLOW_ADAPTER_REGISTRY_SCHEMA_VERSION,
    deploymentFingerprint: applicationRegistry?.deploymentFingerprint ?? null,
    installedCategoryCount:
      applicationRegistry?.installedCategories.length ?? 0,
    registeredCategoryCount: registryRegistrations.length,
    requestedCategoryCount: launchScope.length,
    readyCategoryCount: registrations.filter(
      (registration) => registration.status === "ready",
    ).length,
    missingCategoryCount: missingWorkflowAdapters(
      launchScope,
      applicationRegistry,
    ).length,
    registrations,
  };
};

/**
 * Compiled run/resume boundary. A ready row must carry the actual executable
 * runner, so readiness can never be promoted independently of the command.
 * Runtime config names infrastructure and credentials only; production proof
 * evidence still comes exclusively from authenticated L1 and public DA.
 */
export const runFraudProofWorkflowCli = async ({
  category,
  deploymentFingerprint,
  headerHash,
  runtimeConfigPath,
  decisionDigest,
  actuationPermit,
  sends,
  onProgress,
  applicationRegistry,
  walletOnlySend,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly runtimeConfigPath: string;
  /** Opaque authority minted from the admitted classifier decision. */
  readonly decisionDigest?: string;
  readonly actuationPermit?: WorkflowActuationPermit;
  /** The objective's part of the application's in-flight set. */
  readonly sends?: WorkflowSends;
  readonly onProgress?: () => void;
  /** Application-installed only after signed deployment identity admission. */
  readonly applicationRegistry?: WorkflowApplicationRegistry;
  /** The caller's restart wait and latest wallet-only bound (`wallet-only-send-gate.ts`). */
  readonly walletOnlySend: WalletOnlySend;
}): Promise<unknown> => {
  normalizeDaDeploymentFingerprintHex(deploymentFingerprint);
  if (
    applicationRegistry !== undefined &&
    applicationRegistry.deploymentFingerprint !== deploymentFingerprint
  ) {
    throw new Error(
      "production workflow application registry differs from the CLI deployment fingerprint",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(headerHash)) {
    throw new Error("--header-hash must be 28-byte lowercase hex");
  }
  if (runtimeConfigPath.trim().length === 0) {
    throw new Error("--workflow-runtime-config must not be empty");
  }
  if (
    decisionDigest === undefined ||
    actuationPermit === undefined ||
    sends === undefined
  ) {
    throw new Error(
      "production workflow actuation requires an admitted decision, its actuation permit and the in-flight set from the application supervisor",
    );
  }
  assertWorkflowAdaptersReady([category], applicationRegistry);
  return await workflowAdapterRunner(category, applicationRegistry).runOrResume(
    {
      category,
      deploymentFingerprint,
      headerHash,
      decisionDigest,
      actuationPermit,
      sends,
      runtimeConfigPath,
      ...(onProgress === undefined ? {} : { onProgress }),
      walletOnlySend,
    },
  );
};
