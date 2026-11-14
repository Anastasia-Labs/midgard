import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import {
  freezeRegistration,
  type MissingWorkflowAdapterRegistration,
  WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION,
  type WorkflowAdapterRegistration,
  type WorkflowAdapterRunner,
  type WorkflowApplicationRegistry,
  type WorkflowApplicationRunnerInstallation,
} from "./adapters.freeze-registration.js";
import { workflowAdapterRegistrationRows } from "./adapters.workflow-adapter-registration-rows.js";
import {
  isAdmittedWorkflowRunner,
  WORKFLOW_ADAPTER_RUNNER,
} from "./runner-admission.js";

export const WORKFLOW_ADAPTER_REGISTRATIONS = Object.freeze(
  workflowAdapterRegistrationRows.map(freezeRegistration),
);

export const validateWorkflowAdapterCoverage = (
  registrations: readonly {
    readonly category: unknown;
    readonly status?: unknown;
    readonly runner?: {
      readonly runnerVersion?: unknown;
      readonly runOrResume?: unknown;
    };
  }[],
  catalogue: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
): void => {
  if (registrations.length !== catalogue.length) {
    throw new Error(
      `production workflow registration cardinality mismatch: expected=${catalogue.length.toString()} actual=${registrations.length.toString()}`,
    );
  }
  const seen = new Set<unknown>();
  for (const [index, registration] of registrations.entries()) {
    if (seen.has(registration.category)) {
      throw new Error(
        `production workflow registration duplicates ${String(registration.category)}`,
      );
    }
    seen.add(registration.category);
    const expected = catalogue[index];
    if (registration.category !== expected) {
      throw new Error(
        `production workflow registration order mismatch at ${index.toString()}: expected=${String(expected)} actual=${String(registration.category)}`,
      );
    }
    if (registration.status !== "missing" && registration.status !== "ready") {
      throw new Error(
        `production workflow registration ${String(registration.category)} has an unknown status`,
      );
    }
    if (
      registration.status === "ready" &&
      (registration.runner?.runnerVersion !== WORKFLOW_ADAPTER_RUNNER ||
        typeof registration.runner.runOrResume !== "function" ||
        !isAdmittedWorkflowRunner({
          category: expected!,
          runner: registration.runner,
        }))
    ) {
      throw new Error(
        `production workflow registration ${String(registration.category)} has no compiled executable runner admitted for its exact category`,
      );
    }
  }
};

validateWorkflowAdapterCoverage(WORKFLOW_ADAPTER_REGISTRATIONS);

const admittedApplicationRegistries = new WeakSet<object>();

const assertCanonicalLaunchScope = (
  launchScope: readonly FraudProofCatalogueCategoryName[],
  label: string,
): void => {
  if (new Set(launchScope).size !== launchScope.length) {
    throw new Error(`${label} contains a duplicate category`);
  }
  for (const [index, category] of launchScope.entries()) {
    if (!FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.includes(category)) {
      throw new Error(`${label} contains unknown category ${String(category)}`);
    }
    if (
      index > 0 &&
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(launchScope[index - 1]!) >=
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(category)
    ) {
      throw new Error(`${label} is not in canonical catalogue order`);
    }
  }
};

/**
 * Installs executable runners into an immutable, deployment-bound application
 * overlay. The canonical registry above is never mutated. Applications must
 * call this only after their own opaque signed-deployment verifier has admitted
 * the identity; runner admission and exact category coverage are rechecked
 * here so metadata alone can never become ready.
 */
export const installWorkflowApplicationRegistry = ({
  deploymentFingerprint,
  requiredInstalledCategories,
  installations,
}: {
  readonly deploymentFingerprint: string;
  readonly requiredInstalledCategories: readonly FraudProofCatalogueCategoryName[];
  readonly installations: readonly WorkflowApplicationRunnerInstallation[];
}): WorkflowApplicationRegistry => {
  const normalizedDeploymentFingerprint = normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  );
  if (normalizedDeploymentFingerprint !== deploymentFingerprint) {
    throw new Error(
      "production workflow application deployment fingerprint is not canonical",
    );
  }
  assertCanonicalLaunchScope(
    requiredInstalledCategories,
    "production workflow application installation scope",
  );
  if (requiredInstalledCategories.length === 0) {
    throw new Error(
      "production workflow application installation scope must not be empty",
    );
  }
  if (installations.length !== requiredInstalledCategories.length) {
    throw new Error(
      `production workflow application installation cardinality mismatch: expected=${requiredInstalledCategories.length.toString()} actual=${installations.length.toString()}`,
    );
  }
  const installationByCategory = new Map<
    FraudProofCatalogueCategoryName,
    WorkflowApplicationRunnerInstallation
  >();
  for (const [index, installation] of installations.entries()) {
    const expected = requiredInstalledCategories[index];
    if (installationByCategory.has(installation.category)) {
      throw new Error(
        `production workflow application installation duplicates ${String(installation.category)}`,
      );
    }
    if (installation.category !== expected) {
      throw new Error(
        `production workflow application installation order mismatch at ${index.toString()}: expected=${String(expected)} actual=${String(installation.category)}`,
      );
    }
    if (installation.deploymentFingerprint !== deploymentFingerprint) {
      throw new Error(
        `production workflow application installation ${String(installation.category)} has an unrecognized deployment identity`,
      );
    }
    if (
      installation.runner.runnerVersion !== WORKFLOW_ADAPTER_RUNNER ||
      typeof installation.runner.runOrResume !== "function" ||
      !isAdmittedWorkflowRunner({
        category: installation.category,
        runner: installation.runner,
      })
    ) {
      throw new Error(
        `production workflow application installation ${String(installation.category)} has no module-admitted category-bound runner`,
      );
    }
    installationByCategory.set(installation.category, installation);
  }
  const registrations = Object.freeze(
    WORKFLOW_ADAPTER_REGISTRATIONS.map((registration) => {
      const installation = installationByCategory.get(registration.category);
      if (installation === undefined) return registration;
      return freezeRegistration({
        category: registration.category,
        status: "ready",
        adapterVersion: WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION,
        runner: installation.runner,
        existingSurface: registration.existingSurface,
        guarantees: [
          "runner is module-admitted for the exact catalogue category",
          "runner is installed for this exact verified deployment fingerprint",
          "runtime reconstructs proof evidence from authenticated L1 and public retained DA",
        ],
      });
    }),
  );
  validateWorkflowAdapterCoverage(registrations);
  const registry = Object.freeze({
    schemaVersion: WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION,
    deploymentFingerprint,
    installedCategories: Object.freeze([...requiredInstalledCategories]),
    registrations,
  });
  admittedApplicationRegistries.add(registry);
  return registry;
};

export const assertWorkflowApplicationRegistry = (
  registry: WorkflowApplicationRegistry,
): void => {
  if (
    !admittedApplicationRegistries.has(registry) ||
    registry.schemaVersion !== WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION
  ) {
    throw new Error(
      "production workflow application registry was not installed through the authenticated immutable boundary",
    );
  }
  validateWorkflowAdapterCoverage(registry.registrations);
};

const resolveWorkflowRegistrations = (
  applicationRegistry?: WorkflowApplicationRegistry,
): readonly WorkflowAdapterRegistration[] => {
  if (applicationRegistry === undefined) {
    return WORKFLOW_ADAPTER_REGISTRATIONS;
  }
  assertWorkflowApplicationRegistry(applicationRegistry);
  return applicationRegistry.registrations;
};

export const missingWorkflowAdapters = (
  launchScope: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  applicationRegistry?: WorkflowApplicationRegistry,
): readonly MissingWorkflowAdapterRegistration[] => {
  const registrations = resolveWorkflowRegistrations(applicationRegistry);
  validateWorkflowAdapterCoverage(registrations);
  assertCanonicalLaunchScope(launchScope, "production workflow launch scope");
  const scope = new Set(launchScope);
  return registrations.filter(
    (registration): registration is MissingWorkflowAdapterRegistration =>
      registration.status === "missing" && scope.has(registration.category),
  );
};

export class MissingWorkflowAdaptersError extends Error {
  readonly missing: readonly MissingWorkflowAdapterRegistration[];

  constructor(missing: readonly MissingWorkflowAdapterRegistration[]) {
    super(
      `production fraud-proof workflow adapters are unavailable: ${missing
        .map((entry) => `${entry.category}(${entry.reason})`)
        .join(", ")}`,
    );
    this.name = "MissingProductionWorkflowAdaptersErrorV1";
    this.missing = missing;
  }
}

/** Refuses production startup until the requested launch scope is concrete. */
export const assertWorkflowAdaptersReady = (
  launchScope: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  applicationRegistry?: WorkflowApplicationRegistry,
): void => {
  const missing = missingWorkflowAdapters(launchScope, applicationRegistry);
  if (missing.length > 0) {
    throw new MissingWorkflowAdaptersError(missing);
  }
};

export const workflowAdapterRunner = (
  category: FraudProofCatalogueCategoryName,
  applicationRegistry?: WorkflowApplicationRegistry,
): WorkflowAdapterRunner => {
  const registrations = resolveWorkflowRegistrations(applicationRegistry);
  validateWorkflowAdapterCoverage(registrations);
  const registration = registrations.find(
    (candidate) => candidate.category === category,
  );
  if (registration === undefined || registration.status !== "ready") {
    assertWorkflowAdaptersReady([category], applicationRegistry);
    throw new Error(
      `production workflow registry invariant: ${category} has no ready registration`,
    );
  }
  return registration.runner;
};
