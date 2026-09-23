import { existsSync } from "node:fs";
import { appendFile, mkdir } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { journalJsonDigest } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";
import {
  openWatcherFaultDecisionJournal,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
} from "midgard-watcher";
import { expect } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import {
  finalizePendingJourneyEvidence,
  readJourneyWorkflowEntries,
  verifyJourneyCorrection,
} from "./correction.js";
import type {
  JourneyFixture,
  JourneyFixtureStage,
  JourneySuccessor,
  JourneySuccessorCheckpoint,
} from "./fixture.js";
import { prepareDuplicateEventHistory } from "./history-settlement.js";
import {
  captureJourneyWorkflowBaseline,
  type JourneySession,
  openJourneySession,
} from "./journey-session.js";
import { readJourneyTiming } from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH, loadJourneyContext } from "./live-context.js";
import { verifyJourneyPublicDa } from "./public-da-preflight.js";
import { verifyJourneyResultEvidence } from "./readiness-evidence.js";
import { measureJourneyStage } from "./stage-timing.js";
import { prepareJourneyHistory } from "./staging.js";

const WATCHER_LAUNCH_TIMEOUT_MS = 1_800_000;

type JourneyExecution =
  | { kind: "journey"; fixture: JourneyFixture; waitForAnchors: boolean }
  | { kind: "prepare_duplicate_event_history" };

/** One shared service lifecycle for acceptance and genuine history prerequisites. */
const runJourney = async (
  runDirectory: string,
  execution: JourneyExecution,
  session: JourneySession,
) => {
  const { context } = session;
  if (context.runDirectory !== runDirectory)
    throw new Error("Journey session belongs to a different run directory");
  await session.assertHealthy();
  const { deployment, provider } = context;
  const category =
    execution.kind === "journey"
      ? execution.fixture.category
      : "crossBlockDuplicateEvent";
  const directory = join(
    context.runDirectory,
    `work/journeys/${category === "transitionTrace" ? "transition-trace" : category}`,
  );
  const runtimeDirectory = join(context.runDirectory, "work/journeys/runtime");
  await Promise.all([
    mkdir(directory, { recursive: true, mode: 0o700 }),
    mkdir(runtimeDirectory, { recursive: true, mode: 0o700 }),
  ]);
  let activeStage = "services";
  let diagnostics: (() => Promise<unknown>) | undefined;
  let succeeded = false;
  const stage = async <T>(
    name: string,
    action: () => Promise<T>,
  ): Promise<T> => {
    activeStage = name;
    return await measureJourneyStage(directory, name, action);
  };
  const poll = async <T>(
    name: string,
    action: () => Promise<T | undefined>,
    timeoutMs = 300_000,
  ): Promise<T> =>
    stage(name, async () => {
      const deadline = performance.now() + timeoutMs;
      for (;;) {
        const result = await action();
        if (result !== undefined) return result;
        if (performance.now() >= deadline)
          throw new Error(`Timed out waiting for ${name}`);
        await pause(1000);
      }
    });
  // A stage that waits on the watcher's authenticated replay is budgeted by
  // progress, not by wall clock. The watcher resumes from its last persisted
  // state-queue observation and walks every later L1 block through the
  // trusted-head authority before it can classify a header, so a long L1 gap
  // (a node outage, a frozen devnet) legitimately takes hours. The stage fails
  // only when the authority head stops advancing for the whole allowance; the
  // journey timeout still bounds the total.
  const pollWhileReplaying = async <T>(
    name: string,
    action: () => Promise<T | undefined>,
    progress: () => Promise<string | null>,
    stallTimeoutMs: number,
  ): Promise<T> =>
    stage(name, async () => {
      let lastProgress = await progress();
      let deadline = performance.now() + stallTimeoutMs;
      for (;;) {
        const result = await action();
        if (result !== undefined) return result;
        const current = await progress();
        if (current !== lastProgress) {
          lastProgress = current;
          deadline = performance.now() + stallTimeoutMs;
        }
        if (performance.now() >= deadline)
          throw new Error(`Timed out waiting for ${name}`);
        await pause(1000);
      }
    });
  try {
    const {
      authority,
      releaseFinality,
      watcherConfig,
      archives,
      native,
      retain,
    } = session;
    const reusedWatcher = session.watcherStarted();
    const baselineStartedAt = new Date().toISOString();
    const baseline =
      execution.kind === "journey"
        ? await captureJourneyWorkflowBaseline(
            session.workflowJournalDirectory,
            execution.fixture.category,
          )
        : undefined;
    if (baseline !== undefined)
      await writeJourneyArtifact(join(directory, "workflow-baseline.json"), {
        startedAt: baselineStartedAt,
        finishedAt: new Date().toISOString(),
        category,
        prefixes: [...baseline].map(([headerHash, entries]) => ({
          headerHash,
          workflowId: entries[0]?.workflowId ?? null,
          entryCount: entries.length,
          journalDigest: journalJsonDigest(entries),
        })),
      });
    const timing =
      execution.kind === "journey"
        ? await readJourneyTiming(context.runDirectory, category, {
            authenticatedConfirmationDepth:
              releaseFinality.policy.confirmationDepth,
            actionDepth: JOURNEY_ACTION_DEPTH,
          })
        : undefined;
    if (timing !== undefined) {
      await writeJourneyArtifact(join(directory, "timing-plan.json"), timing);
      console.info("Live watcher confirmation budget", timing);
    }
    const fixtureStage: JourneyFixtureStage = {
      context,
      directory,
      historicalNativeScriptProviders: archives.configuration.providers,
      retain,
      readConfirmedTransaction: native.transaction,
      onHealthyPredecessor: async () => {
        // Binding and launch are awaited; the process then catches up while
        // the fixture prepares its fault. Baseline capture already completed.
        await session.ensureWatcher();
      },
      readSignedCommitRecovery: session.readSignedCommitRecovery,
      onStage: (name) => {
        activeStage = name;
        console.info(`Live fixture: ${name}`);
      },
    };
    if (execution.kind === "prepare_duplicate_event_history") {
      const head = await stage("honest duplicate-event source history", () =>
        prepareJourneyHistory(fixtureStage, async (input) => {
          const history = await prepareDuplicateEventHistory(input);
          console.info(
            `Duplicate-event source ${history.source.headerHash} matures at ${new Date(Number(history.readyAt)).toISOString()}; settlement remains required.`,
          );
        }),
      );
      native.assertHealthy();
      await writeJourneyArtifact(join(directory, "history-preparation.json"), {
        deploymentFingerprint: deployment.manifest.manifestId,
        status: "prepared",
        head: head.headerHash,
      });
      succeeded = true;
      return;
    }
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
    const { fixture } = execution;
    const staged = await stage("invalid commitment and DA attestations", () =>
      fixture.stage(fixtureStage),
    );
    // A watcher already running from an earlier family can correct the fault
    // before its DA apply lands. That authenticated correction completes
    // staging; the journey then observes the same removal it already knows.
    await writeJourneyArtifact(join(directory, "staged-target.json"), {
      category: fixture.category,
      headerHash: staged.current.headerHash,
      target: staged.target,
    });
    if (staged.target.kind === "corrected")
      console.info(
        `Live fixture: fault ${staged.current.headerHash} corrected by ${staged.target.removalTxHash} before DA apply; continuing without restart`,
      );
    await stage("production public DA preflight", () =>
      verifyJourneyPublicDa({
        directory,
        watcherConfig,
        deploymentIdentity: authority.deploymentAuthority.deploymentIdentity,
        predecessor: staged.predecessor,
        current: staged.current,
      }),
    );
    const running = await session.ensureWatcher();
    const { config, requireLive, operations, trustedHeadRevision } = running;
    diagnostics = running.diagnostics;
    const workflowBaseline = baseline?.get(staged.current.headerHash) ?? [];
    await writeJourneyArtifact(join(directory, "session.json"), {
      sessionDirectory: session.directory,
      watcherUse: reusedWatcher ? "reused" : "started",
      authorityProcess: session.authorityObserve(),
      configPath: running.configPath,
      bindingPreflightPath: join(
        session.directory,
        "workflow-binding-preflight.json",
      ),
      nativeEvidencePath: native.nativeEvidencePath,
      process: running.observe(),
    });
    await writeJourneyArtifact(join(directory, "watcher-process.json"), config);
    let nextStartupReport = 0;
    await pollWhileReplaying(
      reusedWatcher ? "shared watcher availability" : "normal watcher launcher",
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
        return status;
      },
      // A watcher resuming after a long L1 outage replays every block it
      // missed during user-event catch-up before it serves its operations
      // endpoint, so the launcher is budgeted by trusted-head progress.
      trustedHeadRevision,
      WATCHER_LAUNCH_TIMEOUT_MS,
    );
    const readDecisions = async () =>
      (
        await openWatcherFaultDecisionJournal({
          directory: config.workflowJournalDirectory,
          deploymentFingerprint: deployment.manifest.manifestId,
          launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
        })
      ).readAll();
    await poll(
      `automatic ${fixture.category} decision`,
      async () => {
        requireLive();
        const records = await readDecisions();
        const decision = records.find(
          ({ decision }) => decision.headerHash === staged.current.headerHash,
        )?.decision;
        if (decision === undefined) return undefined;
        expect(decision).toMatchObject({
          decision: "fault_detected",
          category: fixture.category,
        });
        expect(
          records.find(
            ({ decision }) =>
              decision.headerHash === staged.predecessor.headerHash,
          )?.decision,
        ).toMatchObject({ decision: "healthy" });
        return decision;
      },
      900_000,
    );
    const completion = await verifyJourneyCorrection({
      workflowBaseline,
      actionDepth: JOURNEY_ACTION_DEPTH,
      correctionTimeoutMs: timing?.correctionTimeoutMs,
      progressAllowanceMs: timing?.transactionAllowanceMs,
      reconciliationAllowanceMs: timing?.allowances.finalizedEvidenceStampMs,
      context,
      native,
      workflowJournalDirectory: config.workflowJournalDirectory,
      directory,
      category: fixture.category,
      headerHash: staged.current.headerHash,
      predecessorHeaderHash: staged.predecessor.headerHash,
      operatorVkey: staged.current.header.operatorVkey,
      requireLive,
      poll,
      stage,
    });
    if (
      staged.target.kind === "corrected" &&
      staged.target.removalTxHash !== completion.correction.removalTxHash
    )
      throw new Error(
        `Staging reconciled removal ${staged.target.removalTxHash} but the watcher's authenticated correction is ${completion.correction.removalTxHash}`,
      );
    const { contracts } = deployment;
    const headerUnit = (hash: string) =>
      toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash,
      );
    const successorPath = join(directory, "successor.json");
    const successorProgressPath = join(directory, "successor-progress.json");
    const successor = existsSync(successorPath)
      ? await readJourneyArtifact<JourneySuccessor>(successorPath)
      : await stage("honest successor commitment", async () =>
          staged.commitHonestSuccessor({
            beforeCommit: session.retainPayload,
            resume: existsSync(successorProgressPath)
              ? await readJourneyArtifact<JourneySuccessorCheckpoint>(
                  successorProgressPath,
                )
              : undefined,
            onCheckpoint: (checkpoint) =>
              writeJourneyArtifact(successorProgressPath, checkpoint),
          }),
        );
    await writeJourneyArtifact(successorPath, successor);
    await retain(successor, successor.commitTxHash);
    await pollWhileReplaying(
      "healthy processing after correction",
      async () => {
        requireLive();
        const decision = (await readDecisions()).find(
          ({ decision }) => decision.headerHash === successor.headerHash,
        )?.decision;
        if (decision === undefined) return undefined;
        expect(decision).toMatchObject({ decision: "healthy" });
        expect(
          await provider.getUtxosWithUnit(
            contracts.stateQueue.spendingScriptAddress,
            headerUnit(successor.headerHash),
          ),
        ).toHaveLength(1);
        const status = await operations("/v1/status");
        if (status.readiness !== "ready") return undefined;
        expect(status).toMatchObject({
          liveness: "live",
          readiness: "ready",
          readinessReasons: [],
          activeAlerts: [],
          launchScope: { complete: true },
        });
        return decision;
      },
      trustedHeadRevision,
      timing?.allowances.healthySuccessorObservationMs ?? 1_800_000,
    );
    await writeJourneyArtifact(
      join(context.runDirectory, "work/journeys/head.json"),
      {
        deploymentFingerprint: deployment.manifest.manifestId,
        block: successor,
      },
    );
    const observedTerminal = (
      await readJourneyWorkflowEntries({
        workflowJournalDirectory: config.workflowJournalDirectory,
        category: fixture.category,
        headerHash: staged.current.headerHash,
      })
    ).find(
      ({ event }) =>
        (event.kind === "terminal_included" || event.kind === "completed") &&
        event.terminal.correction.removalTxHash ===
          completion.correction.removalTxHash,
    );
    if (observedTerminal === undefined)
      throw new Error(
        "Terminal observation disappeared from its immutable journal",
      );
    // Save the anchor request before the provisional verdict so restart cannot
    // lose finality tracking after releasing this family's execution slot.
    await writeJourneyArtifact(join(directory, "pending-evidence-stamp.json"), {
      category: fixture.category,
      headerHash: staged.current.headerHash,
      deploymentFingerprint: deployment.manifest.manifestId,
      terminalObservedAt: observedTerminal.recordedAt,
      successorTxHash: successor.commitTxHash,
      completedAtConfirmationDepth: completion.observedAt.confirmationDepth,
      finalityDepth: releaseFinality.policy.confirmationDepth,
      releaseFinalityPolicyDigest: releaseFinality.policyDigest,
    });
    succeeded = true;
    await writeJourneyArtifact(join(directory, "result.json"), {
      status: "passed",
      executionPolicy: "authenticated-inclusion",
      proofExecution: workflowBaseline.some(
        ({ event }) => event.kind === "completed",
      )
        ? "historical-completed-proof"
        : "current-inclusion-policy",
      evidenceStatus: "terminal-included-awaiting-anchor",
      category: fixture.category,
      deploymentFingerprint: deployment.manifest.manifestId,
      completion,
      successor: successor.headerHash,
      diagnostics: await diagnostics(),
      nativeEvidencePath: native.nativeEvidencePath,
    });
    const finalizeEvidence = () =>
      finalizePendingJourneyEvidence({
        journeysDirectory: join(context.runDirectory, "work/journeys"),
        workflowJournalDirectory: config.workflowJournalDirectory,
        deploymentFingerprint: deployment.manifest.manifestId,
        releaseFinalityPolicyDigest: releaseFinality.policyDigest,
        finalityDepth: releaseFinality.policy.confirmationDepth,
        nativeEvidencePath: native.nativeEvidencePath,
        authenticate: async (request, terminal) => {
          const transactions = await Promise.all(
            [
              terminal.proofToken.createdByTxHash,
              terminal.correction.removalTxHash,
              request.successorTxHash,
            ].map((txHash) => native.transaction(txHash)),
          );
          const observedBlockNo = native.observedBlockNo();
          return (
            observedBlockNo !== undefined &&
            transactions.every(
              ({ point }) =>
                observedBlockNo - BigInt(point.blockNo) + 1n >=
                BigInt(request.finalityDepth),
            )
          );
        },
      });
    if (execution.waitForAnchors) {
      await stage("finalized evidence stamps", () =>
        poll(
          "finalized evidence stamps",
          async () => {
            requireLive();
            return (await finalizeEvidence()) === 0 ? true : undefined;
          },
          timing?.allowances.finalizedEvidenceStampMs,
        ),
      );
    } else {
      await finalizeEvidence();
    }
  } catch (cause) {
    await writeJourneyArtifact(join(directory, "failure.json"), {
      activeStage,
      error:
        cause instanceof Error
          ? { message: cause.message, stack: cause.stack }
          : String(cause),
      diagnostics: await diagnostics?.(),
    });
    throw cause;
  } finally {
    console.info(
      `Journey ${execution.kind} evidence retained at ${directory}; succeeded=${succeeded}`,
    );
  }
};

/** One process-driven acceptance path for every non-interactive family. */
export const runAutonomousWatcherJourney = async (
  runDirectory: string,
  fixture: JourneyFixture,
  options: { waitForAnchors?: boolean; session?: JourneySession } = {},
) => {
  const session = options.session ?? (await openJourneySession(runDirectory));
  try {
    await runJourney(
      runDirectory,
      {
        kind: "journey",
        fixture,
        waitForAnchors: options.waitForAnchors ?? true,
      },
      session,
    );
  } catch (cause) {
    await session.close();
    throw cause;
  } finally {
    if (options.session === undefined) await session.close();
  }
};

/** Start the real maturity clock after the verified trace baseline has completed. */
export const prepareAutonomousWatcherHistory = async (runDirectory: string) => {
  const context = await loadJourneyContext(runDirectory);
  await verifyJourneyResultEvidence(
    context.runDirectory,
    join(context.runDirectory, "work/journeys/transition-trace"),
    "transitionTrace",
    context.deployment,
  );
  const session = await openJourneySession(runDirectory);
  try {
    await runJourney(
      runDirectory,
      { kind: "prepare_duplicate_event_history" },
      session,
    );
  } finally {
    await session.close();
  }
};
