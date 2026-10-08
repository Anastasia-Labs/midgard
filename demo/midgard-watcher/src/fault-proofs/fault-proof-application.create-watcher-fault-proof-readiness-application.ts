import { assertWatcherVerifiedDeploymentAuthority } from "../runtime/deployment-authority.js";
import { type WatcherFaultProofApplicationWithLoaderForTest } from "./fault-proof-application.build-common-infrastructure.js";
import { createApplication } from "./fault-proof-application.create-application.js";
import {
  productionDependencies,
  type WatcherFaultProofApplication,
  type WatcherFaultProofApplicationConstructionOptions,
  type WatcherFaultProofApplicationDependencies,
  type WatcherFaultProofApplicationOptions,
} from "./fault-proof-application.production-dependencies.js";

export const createWatcherFaultProofApplication = (
  options: WatcherFaultProofApplicationOptions,
): WatcherFaultProofApplication =>
  createApplication({
    options: Object.freeze({
      l1: options.l1,
      deploymentIdentity: options.deploymentAuthority.deploymentIdentity,
      deploymentAuthority: options.deploymentAuthority,
      replayTranscriptStore: options.replayTranscriptStore,
      userEvents: options.userEvents,
      infrastructure: options.infrastructure,
      historicalNativeScriptCheckpointStore:
        options.historicalNativeScriptCheckpointStore,
      fundingProfileOverlay: options.fundingProfileOverlay,
    }),
    dependencies: productionDependencies,
    environment: process.env,
    allowExecution: true,
  });

/** Bind installed production workflows without exposing execution or classification. */
export const createWatcherFaultProofReadinessApplication = (
  options: Pick<
    WatcherFaultProofApplicationOptions,
    | "l1"
    | "deploymentAuthority"
    | "infrastructure"
    | "historicalNativeScriptCheckpointStore"
    | "fundingProfileOverlay"
  >,
): Pick<
  WatcherFaultProofApplication,
  "installedCategories" | "assertStartupReady" | "close"
> => {
  assertWatcherVerifiedDeploymentAuthority(options.deploymentAuthority);
  const application = createApplication({
    options: {
      ...options,
      deploymentIdentity: options.deploymentAuthority.deploymentIdentity,
    },
    dependencies: productionDependencies,
    environment: process.env,
    allowExecution: false,
  });
  return Object.freeze({
    installedCategories: application.installedCategories,
    assertStartupReady: application.assertStartupReady,
    close: application.close,
  });
};

/**
 * Narrow test-only dependency seam. It cannot execute transactions, and it
 * exposes the one runtime loader its runners load through so a test can prove
 * what the watcher supplies to a family without holding an actuation permit.
 */
export const unsafeCreateWatcherFaultProofApplicationForTest = (
  options: WatcherFaultProofApplicationConstructionOptions,
  dependencies: WatcherFaultProofApplicationDependencies,
  environment: NodeJS.ProcessEnv = {},
): WatcherFaultProofApplicationWithLoaderForTest =>
  createApplication({
    options,
    dependencies,
    environment,
    allowExecution: false,
    unsafeExposeRuntimeLoaderForTest: true,
  });
