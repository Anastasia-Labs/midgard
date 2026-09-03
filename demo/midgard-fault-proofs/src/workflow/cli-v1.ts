import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import { DirectoryFraudProofWorkflowJournalStore } from "./journal-v1.js";
import type { WorkflowActuationPermit } from "./production-actuation-permit-v1.js";
import {
  assertWorkflowAdaptersReady,
  missingWorkflowAdapters,
  validateWorkflowAdapterCoverage,
  WORKFLOW_ADAPTER_REGISTRATIONS,
  WORKFLOW_ADAPTER_REGISTRY_SCHEMA_VERSION,
  workflowAdapterRunner,
  type WorkflowApplicationRegistry,
} from "./production-adapters-v1.js";
import type { WorkflowFundingReservationPermit } from "./production-funding-reservation-permit-v1.js";

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
  mode,
  category,
  deploymentFingerprint,
  headerHash,
  journalDirectory,
  runtimeConfigPath,
  decisionDigest,
  actuationPermit,
  fundingReservationPermit,
  applicationRegistry,
}: {
  readonly mode: "run" | "resume";
  readonly category: FraudProofCatalogueCategoryName;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly journalDirectory: string;
  readonly runtimeConfigPath: string;
  /** Opaque authority minted from the admitted classifier decision. */
  readonly decisionDigest?: string;
  readonly actuationPermit?: WorkflowActuationPermit;
  readonly fundingReservationPermit?: WorkflowFundingReservationPermit;
  /** Application-installed only after signed deployment identity admission. */
  readonly applicationRegistry?: WorkflowApplicationRegistry;
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
  if (journalDirectory.trim().length === 0) {
    throw new Error("--workflow-journal-dir must not be empty");
  }
  if (runtimeConfigPath.trim().length === 0) {
    throw new Error("--workflow-runtime-config must not be empty");
  }
  if (
    decisionDigest === undefined ||
    actuationPermit === undefined ||
    fundingReservationPermit === undefined
  ) {
    throw new Error(
      "production workflow actuation requires admitted decision and funding reservation permits from the application supervisor",
    );
  }
  // Construction is side-effect free. The directory is created only by an
  // actual append after a complete adapter has admitted canonical evidence.
  void new DirectoryFraudProofWorkflowJournalStore(journalDirectory);
  assertWorkflowAdaptersReady([category], applicationRegistry);
  return await workflowAdapterRunner(category, applicationRegistry).runOrResume(
    {
      mode,
      category,
      deploymentFingerprint,
      headerHash,
      decisionDigest,
      actuationPermit,
      fundingReservationPermit,
      journalDirectory,
      runtimeConfigPath,
    },
  );
};
