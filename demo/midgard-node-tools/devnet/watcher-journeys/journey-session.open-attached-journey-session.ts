import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";

import { captureJourneyWorkflowBaseline } from "./journey-session.capture-journey-workflow-baseline.js";
import { openJourneySessionCore } from "./journey-session.open-journey-session-core.js";
import { readJourneyOperations } from "./operations.js";
import type { StackJourneyContext } from "./stack-context.js";
import {
  composeWatcherEvidenceExec,
  createWatcherEvidenceReader,
  type WatcherEvidenceExec,
} from "./watcher-evidence-reader.js";

type AttachedWatcherStatus = Readonly<{
  deploymentFingerprint: string;
  liveness: string;
  readiness?: string;
  readinessReasons?: readonly string[];
  l1Readiness?: unknown;
}>;

/**
 * An attached watcher's `/v1/status` when it serves this deployment and is
 * live; refuses a watcher still starting, another deployment's watcher and a
 * stopping or blocked one.
 */
export const assertAttachedWatcherStatus = (
  status: unknown,
  deploymentFingerprint: string,
): AttachedWatcherStatus => {
  const value = status as Partial<AttachedWatcherStatus> | null | undefined;
  // A starting watcher serves `/v1/status` before it has bound a deployment.
  if (value?.deploymentFingerprint === undefined)
    throw new Error(
      `The stack watcher is still starting and not attached to a deployment (${JSON.stringify(value?.readinessReasons ?? [])})`,
    );
  if (value.deploymentFingerprint !== deploymentFingerprint)
    throw new Error(
      "The stack watcher's operations endpoint serves another deployment",
    );
  if (value.liveness !== "live")
    throw new Error(
      `The stack watcher is not live: ${String(value.liveness)} (${JSON.stringify(value.readinessReasons ?? [])})`,
    );
  return value as AttachedWatcherStatus;
};

/**
 * A journey session attached to a running e2e-stack's own watcher. It starts
 * nothing: no retained DA, no watcher configuration or process, no native
 * recorder and no prover funding. Health and readiness come from the stack
 * watcher's `/v1/status`; decision and workflow evidence from its journals,
 * read inside its container (`exec`, by default `docker compose exec`).
 */
export const openAttachedJourneySession = async (
  context: StackJourneyContext,
  options: Readonly<{ exec?: WatcherEvidenceExec }> = {},
) => {
  const { directory, isClosed, close, stage } = await openJourneySessionCore(
    context.runDirectory,
  );
  try {
    const deploymentFingerprint = context.deployment.manifest.manifestId;
    const deploymentIdentity = context.release.deploymentIdentity;
    if (deploymentIdentity.manifestId !== deploymentFingerprint)
      throw new Error(
        "The stack's watcher release belongs to another deployment",
      );
    const releaseFinality = await watcherDeploymentReleaseFinalityAuthority(
      deploymentIdentity,
    ).verifyForWorkflow({ deploymentFingerprint });
    const { operationsEndpoint, composeProject, processConfigPath } =
      context.watcher;
    const requireLive = () => {
      if (isClosed()) throw new Error("Watcher journey session is closed");
    };
    const operations = async (path: string) => {
      requireLive();
      return await readJourneyOperations(operationsEndpoint, path);
    };
    const evidence = createWatcherEvidenceReader({
      exec: options.exec ?? composeWatcherEvidenceExec(composeProject),
      hostProcessConfigPath: processConfigPath,
      deploymentFingerprint,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
    const assertHealthy = async () => {
      try {
        assertAttachedWatcherStatus(
          await operations("/v1/status"),
          deploymentFingerprint,
        );
      } catch (cause) {
        await close();
        throw cause;
      }
    };
    await stage("attached watcher status", assertHealthy);
    // The first read proves the evidence comes from this stack's watcher.
    await stage("attached watcher evidence", () => evidence.readDecisions());
    const watcher = Object.freeze({
      requireLive,
      operations,
      diagnostics: async () => ({
        status: await operations("/v1/status").catch(String),
        metrics: await operations("/v1/metrics").catch(String),
      }),
      // The watcher's own named readiness reasons, null while its operations
      // endpoint does not answer.
      watcherProgress: async (): Promise<string | null> => {
        const status = (await operations("/v1/status").catch(
          () => undefined,
        )) as Partial<AttachedWatcherStatus> | undefined;
        if (status === undefined) return null;
        return JSON.stringify({
          readinessReasons: status.readinessReasons ?? null,
          l1Readiness: status.l1Readiness ?? null,
        });
      },
    });
    return {
      mode: "attached" as const,
      context,
      directory,
      authority: Object.freeze({
        deploymentAuthority: context.release,
        manifestPath: context.layout.manifestPath,
        blueprintPath: context.deployment.blueprintPath,
      }),
      releaseFinality,
      watcher,
      evidence,
      captureWorkflowBaseline: (category: FraudProofCatalogueCategoryName) =>
        captureJourneyWorkflowBaseline(evidence, category),
      assertHealthy,
      close,
    };
  } catch (cause) {
    await close();
    throw cause;
  }
};
