import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";

import type { JourneyOperator2Node } from "./deployment-source.js";
import { captureJourneyWorkflowBaseline } from "./journey-session.capture-journey-workflow-baseline.js";
import { openJourneySessionCore } from "./journey-session.open-journey-session-core.js";
import { readJourneyOperations } from "./operations.js";
import {
  composeOperatorNodeControl,
  createOperatorPause,
  guardOperatorSignatures,
  type OperatorNodeControl,
} from "./operator-pause.js";
import type { StackJourneyContext } from "./stack-context.js";
import { openStackJourneyReplayedLedger } from "./stack-replayed-ledger.js";
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
 * Refuses a drill whose stack prover holds less than the stack
 * configuration's `wallets.prover.minimumLovelace`: the stack watcher's
 * proofs spend from it, and the drill never funds it.
 */
const assertStackProverFunded = async (context: StackJourneyContext) => {
  const address = context.layout.proverAddress;
  const minimum = BigInt(context.stackConfig.wallets.prover!.minimumLovelace);
  const balance = (await context.provider.getUtxos(address)).reduce(
    (total, utxo) => total + (utxo.assets.lovelace ?? 0n),
    0n,
  );
  if (balance < minimum)
    throw new Error(
      `The stack watcher's prover ${address} holds ${balance.toString()} lovelace, ${(minimum - balance).toString()} lovelace short of the stack configuration's wallets.prover.minimumLovelace (${minimum.toString()}); fund it before the drill`,
    );
};

/**
 * A journey session attached to a running e2e-stack's own watcher. It starts
 * no retained DA, no watcher configuration or process, no native recorder
 * and no prover funding. It holds operator-2's node while the drill signs as
 * operator-2, and starts it again when it closes. Health and readiness come
 * from the stack watcher's `/v1/status`; decision and workflow evidence from
 * its journals, read inside its container (`exec`, by default
 * `docker compose exec`). Staging reads ledgers from the stack's own replay,
 * opened on its first read. It refuses a prover below the stack's funding
 * minimum before the drill starts.
 */
export const openAttachedJourneySession = async (
  context: StackJourneyContext,
  options: Readonly<{
    /** Operator-2's node, the second stack instance the drill pauses. */
    operator2: JourneyOperator2Node;
    exec?: WatcherEvidenceExec;
    /** Operator-2's node control, by default `docker compose` on its project. */
    operator2Control?: OperatorNodeControl;
  }>,
) => {
  const { directory, onClose, isClosed, close, stage } =
    await openJourneySessionCore(context.runDirectory);
  try {
    // Pausing our own stack's node instead of operator-2's would stop the
    // honest successor; refuse a project or endpoint that is ours.
    if (options.operator2.composeProject === context.watcher.composeProject)
      throw new Error(
        "Operator-2's Compose project is this stack's own project; refusing to pause our node",
      );
    if (
      new URL(options.operator2.endpoint).origin ===
      new URL(context.stackConfig.endpoint).origin
    )
      throw new Error(
        "Operator-2's endpoint is this stack's own node endpoint; refusing to pause our node",
      );
    const operator2 = createOperatorPause(
      options.operator2Control ?? composeOperatorNodeControl(options.operator2),
    );
    // The drill's wallet and DA producer key are operator-2's: each
    // signature and each DA publication proves its node stopped first.
    guardOperatorSignatures(context.deployment.operatorLucid, operator2);
    context.daProducer.guard(operator2);
    // A run that ends with operator-2 still held starts it again; only a
    // completed run proves its liveness (`operator2.resume`).
    onClose(operator2.release);
    // Closes run in reverse order and each runs even if a later one throws,
    // so operator-2 is started again whatever the ledger close does.
    const replayedLedger = openStackJourneyReplayedLedger(context, directory);
    onClose(replayedLedger.close);
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
    await stage("attached watcher prover funding", () =>
      assertStackProverFunded(context),
    );
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
      ledgerAt: replayedLedger.ledgerAt,
      watcher,
      evidence,
      /** The drill's hold on operator-2's node; see `operator-pause.ts`. */
      operator2,
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
