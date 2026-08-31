import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import { DirectoryFraudProofWorkflowJournalStoreV1 } from "./journal-v1.js";
import type { ProductionWorkflowActuationPermitV1 } from "./production-actuation-permit-v1.js";
import {
  assertProductionWorkflowAdaptersReadyV1,
  missingProductionWorkflowAdaptersV1,
  PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1,
  PRODUCTION_WORKFLOW_ADAPTER_REGISTRY_V1_SCHEMA_VERSION,
  productionWorkflowAdapterRunnerV1,
  type ProductionWorkflowApplicationRegistryV1,
  validateProductionWorkflowAdapterCoverageV1,
} from "./production-adapters-v1.js";
import type { ProductionWorkflowFundingReservationPermitV1 } from "./production-funding-reservation-permit-v1.js";

export const productionWorkflowReadinessReportV1 = (
  launchScope: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  applicationRegistry?: ProductionWorkflowApplicationRegistryV1,
) => {
  const registryRegistrations =
    applicationRegistry?.registrations ??
    PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1;
  validateProductionWorkflowAdapterCoverageV1(registryRegistrations);
  const registrations = registryRegistrations.filter((registration) =>
    launchScope.includes(registration.category),
  );
  return {
    schemaVersion: PRODUCTION_WORKFLOW_ADAPTER_REGISTRY_V1_SCHEMA_VERSION,
    deploymentFingerprint: applicationRegistry?.deploymentFingerprint ?? null,
    installedCategoryCount:
      applicationRegistry?.installedCategories.length ?? 0,
    registeredCategoryCount: registryRegistrations.length,
    requestedCategoryCount: launchScope.length,
    readyCategoryCount: registrations.filter(
      (registration) => registration.status === "ready",
    ).length,
    missingCategoryCount: missingProductionWorkflowAdaptersV1(
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
export const runProductionFraudProofWorkflowCliV1 = async ({
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
  readonly actuationPermit?: ProductionWorkflowActuationPermitV1;
  readonly fundingReservationPermit?: ProductionWorkflowFundingReservationPermitV1;
  /** Application-installed only after signed deployment identity admission. */
  readonly applicationRegistry?: ProductionWorkflowApplicationRegistryV1;
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
  void new DirectoryFraudProofWorkflowJournalStoreV1(journalDirectory);
  assertProductionWorkflowAdaptersReadyV1([category], applicationRegistry);
  return await productionWorkflowAdapterRunnerV1(
    category,
    applicationRegistry,
  ).runOrResume({
    mode,
    category,
    deploymentFingerprint,
    headerHash,
    decisionDigest,
    actuationPermit,
    fundingReservationPermit,
    journalDirectory,
    runtimeConfigPath,
  });
};
