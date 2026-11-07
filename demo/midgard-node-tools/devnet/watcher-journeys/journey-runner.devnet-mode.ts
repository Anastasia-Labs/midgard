import { existsSync } from "node:fs";
import { appendFile, copyFile } from "node:fs/promises";
import { join } from "node:path";

import {
  readWatcherFaultDecisionEvidence,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
} from "midgard-watcher";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyFixtureStage } from "./fixture.js";
import { devnetSuccessorPort } from "./honest-successor.js";
import { WATCHER_LAUNCH_TIMEOUT_MS } from "./journey-runner.journey-execution.js";
import type { JourneyRunMode } from "./journey-runner.journey-mode.js";
import {
  captureJourneyWorkflowBaseline,
  type JourneySession,
} from "./journey-session.js";
import { readJourneyTiming } from "./journey-timing.js";
import { verifyJourneyPublicDa } from "./public-da-preflight.js";

/**
 * A devnet journey: the session launches and owns the watcher, records the
 * chain natively, retains DA, and the journey commits the honest successor
 * itself.
 */
export const devnetJourneyMode = (session: JourneySession): JourneyRunMode => {
  const { context, authority, watcherConfig, native, retain } = session;
  const { deployment } = context;
  const reusedWatcher = session.watcherStarted();
  let config: { workflowJournalDirectory: string } | undefined;
  return {
    captureWorkflowBaseline: (category) =>
      captureJourneyWorkflowBaseline(watcherConfig.storage.path, category),
    readTiming: (category, options) =>
      readJourneyTiming(context.runDirectory, category, options),
    stageFixture: async (fixture, { directory, onStage }) => {
      const fixtureStage: JourneyFixtureStage = {
        context,
        directory,
        retain,
        ledgerAt: session.ledgerAt,
        readConfirmedTransaction: native.transaction,
        onHealthyPredecessor: async () => {
          // Binding and launch are awaited; the process then catches up while
          // the fixture prepares its fault. Baseline capture already completed.
          await session.ensureWatcher();
        },
        readSignedCommitRecovery: session.readSignedCommitRecovery,
        onStage,
      };
      const headPath = join(context.runDirectory, "work/journeys/head.json");
      if (!session.watcherStarted() && existsSync(headPath)) {
        const head = await readJourneyArtifact<{
          deploymentFingerprint: string;
        }>(headPath);
        if (head.deploymentFingerprint !== deployment.manifest.manifestId)
          throw new Error(
            "Watcher session readiness head belongs to a different deployment",
          );
        // Startup binds all runners; actual classification still observes the live queue.
        await session.ensureWatcher();
      }
      return await fixture.stage(fixtureStage);
    },
    verifyPublicDa: ({ directory, predecessor, current }) =>
      verifyJourneyPublicDa({
        directory,
        watcherConfig,
        deploymentIdentity: authority.deploymentAuthority.deploymentIdentity,
        predecessor,
        current,
      }),
    startWatcher: async ({ directory, polls, onWatcher }) => {
      const running = await session.ensureWatcher();
      const { requireLive, operations, watcherProgress } = running;
      ({ config } = running);
      onWatcher(running);
      await writeJourneyArtifact(join(directory, "session.json"), {
        sessionDirectory: session.directory,
        watcherUse: reusedWatcher ? "reused" : "started",
        configPath: running.configPath,
        bindingPreflightPath: join(
          session.directory,
          "workflow-binding-preflight.json",
        ),
        nativeEvidencePath: native.nativeEvidencePath,
        process: running.observe(),
      });
      // Keep the process config the watcher was launched with: the parsed config
      // carries derived fields (each DA peer's peerId) that the parser refuses.
      await copyFile(
        running.configPath,
        join(directory, "watcher-process.json"),
      );
      let nextStartupReport = 0;
      await polls.pollWhileReplaying(
        reusedWatcher
          ? "shared watcher availability"
          : "normal watcher launcher",
        async () => {
          requireLive();
          let operationsError: string | undefined;
          const status = await operations("/v1/status").catch((cause) => {
            operationsError =
              cause instanceof Error ? cause.message : String(cause);
            return undefined;
          });
          if (status !== undefined || Date.now() >= nextStartupReport) {
            const observation = {
              observedAt: new Date().toISOString(),
              process: running.observe(),
              operations:
                status === undefined
                  ? { reachable: false, error: operationsError }
                  : { reachable: true, status },
            };
            await appendFile(
              join(directory, "startup-observations.ndjson"),
              `${JSON.stringify(observation)}\n`,
            );
            console.info("Live watcher startup observation", {
              pid: observation.process.pid,
              processState: observation.process.state,
              startup: observation.process.startup,
              operations: observation.operations,
            });
            nextStartupReport = Date.now() + 15_000;
          }
          // While startup runs, `/v1/status` answers with a `startup:<stage>`
          // reason and every other operations route answers 503 `starting`.
          const reasons =
            (status as { readinessReasons?: readonly string[] } | undefined)
              ?.readinessReasons ?? [];
          if (reasons.includes("startup_failed"))
            throw new Error(
              `Watcher startup failed and holds: ${JSON.stringify(status)}`,
            );
          return reasons.some((reason) => reason.startsWith("startup:"))
            ? undefined
            : status;
        },
        // A watcher resuming after a long L1 outage catches up on every block it
        // missed, so the launcher is budgeted by its progress.
        watcherProgress,
        WATCHER_LAUNCH_TIMEOUT_MS,
      );
      return running;
    },
    // The watcher holds the journal key; its rows are read as evidence.
    readDecisions: async () => {
      if (config === undefined)
        throw new Error("The journey's watcher is not started");
      return existsSync(
        join(config.workflowJournalDirectory, "watcher-journals.sqlite"),
      )
        ? readWatcherFaultDecisionEvidence({
            directory: config.workflowJournalDirectory,
            deploymentFingerprint: deployment.manifest.manifestId,
            launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
          }).map((decision) => ({ decision }))
        : [];
    },
    watcherStore: watcherConfig.storage.path,
    transactions: native,
    get proverAddress() {
      return session.proverAddress();
    },
    successor: ({ directory, staged, polls }) =>
      devnetSuccessorPort({
        directory,
        staged,
        retainPayload: session.retainPayload,
        retain,
        stage: polls.stage,
      }),
  };
};
