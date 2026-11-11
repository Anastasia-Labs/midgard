import type { WorkflowAdapterReadinessInput } from "@al-ft/midgard-fault-proofs";

import { watcherWorkflowFundingProfiles } from "../funding/workflow-funding-profile-overlay.js";
import { assertWatcherVerifiedDeploymentAuthority } from "../runtime/deployment-authority.js";
import { readWatcherRuntimeConfigFile } from "../runtime/watcher-runtime.launch-checks.js";
import { admitInfrastructure } from "./fault-proof-application.bind-watcher-deployment-authority.js";
import { type WatcherFaultProofApplicationWithLoaderForTest } from "./fault-proof-application.build-common-infrastructure.js";
import { createApplication } from "./fault-proof-application.create-application.js";
import {
  productionDependencies,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherFaultProofApplication,
  type WatcherFaultProofApplicationConstructionOptions,
  type WatcherFaultProofApplicationDependencies,
  type WatcherFaultProofApplicationOptions,
  type WatcherFaultProofStartupReadiness,
} from "./fault-proof-application.production-dependencies.js";

export const createWatcherFaultProofApplication = (
  options: WatcherFaultProofApplicationOptions,
): WatcherFaultProofApplication =>
  createApplication({
    options: Object.freeze({
      watcherConfig: options.watcherConfig,
      l1: options.l1,
      replayedLedger: options.replayedLedger,
      deploymentIdentity: options.deploymentAuthority.deploymentIdentity,
      deploymentAuthority: options.deploymentAuthority,
      replayTranscriptStore: options.replayTranscriptStore,
      infrastructure: options.infrastructure,
      fundingProfileOverlay: options.fundingProfileOverlay,
      ...(options.onValidationTraceReplayStuck === undefined
        ? {}
        : {
            onValidationTraceReplayStuck: options.onValidationTraceReplayStuck,
          }),
    }),
    dependencies: productionDependencies,
    environment: process.env,
    allowExecution: true,
  });

/**
 * Bind installed production workflows without exposing execution or
 * classification. The first check admits the runtime configuration file its
 * invocation names and builds one non-executing application on it; every
 * later check must name the same file and runs on that application.
 */
export const createWatcherFaultProofReadinessApplication = (
  options: Pick<
    WatcherFaultProofApplicationOptions,
    "l1" | "deploymentAuthority" | "infrastructure" | "fundingProfileOverlay"
  >,
): Readonly<{
  installedCategories: WatcherFaultProofApplication["installedCategories"];
  assertStartupReady(
    invocation: WorkflowAdapterReadinessInput,
  ): Promise<WatcherFaultProofStartupReadiness>;
  close(): Promise<void>;
}> => {
  assertWatcherVerifiedDeploymentAuthority(options.deploymentAuthority);
  const deploymentIdentity = options.deploymentAuthority.deploymentIdentity;
  admitInfrastructure(options.infrastructure);
  watcherWorkflowFundingProfiles({
    overlay: options.fundingProfileOverlay,
    deploymentIdentity,
    allowExecution: false,
  });
  let bound:
    | Readonly<{
        runtimeConfigPath: string;
        application: Promise<WatcherFaultProofApplication>;
      }>
    | undefined;
  let closed = false;
  return Object.freeze({
    installedCategories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    assertStartupReady: async ({ runtimeConfigPath, ...invocation }) => {
      if (closed) throw new Error("watcher fault-proof application is closed");
      bound ??= Object.freeze({
        runtimeConfigPath,
        application: readWatcherRuntimeConfigFile(runtimeConfigPath).then(
          (watcherConfig) =>
            createApplication({
              options: { ...options, deploymentIdentity, watcherConfig },
              dependencies: productionDependencies,
              environment: process.env,
              allowExecution: false,
            }),
        ),
      });
      if (runtimeConfigPath !== bound.runtimeConfigPath) {
        throw new Error(
          "watcher readiness checks name different runtime configurations",
        );
      }
      return await (await bound.application).assertStartupReady(invocation);
    },
    close: async () => {
      closed = true;
      await (await bound?.application.catch(() => undefined))?.close();
    },
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
