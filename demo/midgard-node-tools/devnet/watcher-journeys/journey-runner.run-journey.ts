import { mkdir } from "node:fs/promises";
import { join } from "node:path";

import { deploymentManifestHubOraclePolicyId } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect } from "vitest";

import { journeyFamilyDirectory, writeJourneyArtifact } from "./artifacts.js";
import {
  finalizePendingJourneyEvidence,
  journeyCorrectionChain,
  verifyJourneyCorrection,
} from "./correction.js";
import type { JourneyWatcherStartStatus } from "./correction.watcher-start-deadline.js";
import type { JourneySuccessorPort } from "./honest-successor.js";
import { attachedJourneyMode } from "./journey-runner.attached-mode.js";
import {
  awaitHealthyWatcher,
  type JourneyHealthyWatcherStatus,
} from "./journey-runner.await-healthy-watcher.js";
import { devnetJourneyMode } from "./journey-runner.devnet-mode.js";
import type { JourneyExecution } from "./journey-runner.journey-execution.js";
import { createJourneyRunPolls } from "./journey-runner.polls.js";
import type {
  AttachedJourneySession,
  JourneySession,
} from "./journey-session.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import { onQueueOrMerged } from "./state-queue-root-anchor.js";
import { createJourneyVerifiedHeaders } from "./verified-headers.js";

/**
 * One shared service lifecycle for a family's acceptance journey. Every
 * stage, wait and staging step checks the family's `signal` and stops with
 * its reason once it aborts. The correction runs inside `excludingLimit`:
 * the chain's correction or the watcher's own verdict ends it, so the
 * family's time limit pauses for it (`journeyFamilyDeadline`).
 */
export const runJourney = async (
  runDirectory: string,
  execution: JourneyExecution,
  session: JourneySession | AttachedJourneySession,
  signal: AbortSignal,
  excludingLimit: <T>(work: () => Promise<T>) => Promise<T> = (work) => work(),
) => {
  const { context } = session;
  if (context.runDirectory !== runDirectory)
    throw new Error("Journey session belongs to a different run directory");
  await session.assertHealthy();
  const { deployment, provider } = context;
  const category = execution.fixture.category;
  const directory = journeyFamilyDirectory(context.runDirectory, category);
  const runtimeDirectory = join(context.runDirectory, "work/journeys/runtime");
  await Promise.all([
    mkdir(directory, { recursive: true, mode: 0o700 }),
    mkdir(runtimeDirectory, { recursive: true, mode: 0o700 }),
  ]);
  let activeStage = "services";
  let diagnostics: (() => Promise<unknown>) | undefined;
  let succeeded = false;
  const polls = createJourneyRunPolls({
    directory,
    onStage: (name) => {
      activeStage = name;
    },
    signal,
  });
  const { stage, poll, pollWhileReplaying } = polls;
  let successorPort: JourneySuccessorPort | undefined;
  let closeMode: (() => Promise<void>) | undefined;
  try {
    const mode =
      session.mode === "devnet"
        ? devnetJourneyMode(session)
        : attachedJourneyMode(session, directory);
    closeMode = mode.close;
    const { releaseFinality } = session;
    const correctionChain = (
      target: Readonly<{
        category: typeof category;
        headerHash: string;
      }>,
    ) =>
      journeyCorrectionChain({
        kupoUrl: context.kupoUrl,
        contracts: deployment.contracts,
        provider,
        transactions: mode.transactions,
        proverAddress: mode.proverAddress,
        ...target,
      });
    const timing = await mode.readTiming(execution.fixture, {
      authenticatedConfirmationDepth: releaseFinality.policy.confirmationDepth,
      actionDepth: JOURNEY_ACTION_DEPTH,
    });
    await writeJourneyArtifact(join(directory, "timing-plan.json"), timing);
    console.info("Live watcher confirmation budget", timing);
    const { fixture } = execution;
    const staged = await stage("invalid commitment and DA attestations", () =>
      mode.stageFixture(fixture, {
        directory,
        // Each step staging reports first checks the family's abort.
        onStage: (name) => {
          signal.throwIfAborted();
          activeStage = name;
          console.info(`Live fixture: ${name}`);
        },
      }),
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
    // A refused fault is never attested, so no watcher can prove it and no
    // successor builds on it: the journey ends at its age-out.
    const { target } = staged;
    if (target.kind === "refused") {
      if (mode.refusal === undefined)
        throw new Error(
          `Fault ${staged.current.headerHash} was refused by a DA committee this journey does not run`,
        );
      const refusal = await stage("committee refusal and age-out", () =>
        mode.refusal!({ fault: staged.current, refused: target, polls }),
      );
      succeeded = true;
      await writeJourneyArtifact(join(directory, "result.json"), {
        status: "passed",
        executionPolicy: "committee-refusal",
        category: fixture.category,
        hubOraclePolicyId: deploymentManifestHubOraclePolicyId(
          deployment.manifest,
        ),
        headerHash: staged.current.headerHash,
        refusal,
      });
      return;
    }
    await stage("production public DA preflight", () =>
      mode.verifyPublicDa({
        directory,
        predecessor: staged.predecessor,
        current: staged.current,
      }),
    );
    successorPort = mode.successor({ directory, staged, polls });
    await successorPort.afterFaultLanded();
    const running = await mode.startWatcher({
      directory,
      polls,
      onWatcher: (watcher) => {
        diagnostics = watcher.diagnostics;
      },
    });
    const { requireLive, operations, watcherProgress } = running;
    const verifiedHeaders = createJourneyVerifiedHeaders(
      running,
      directory,
      join(context.runDirectory, "work/journeys/head.json"),
    );
    await poll(
      `automatic ${fixture.category} decision`,
      async () => {
        requireLive();
        const records = await mode.readDecisions();
        const decision = records.find(
          ({ decision }) => decision.headerHash === staged.current.headerHash,
        )?.decision;
        if (decision === undefined) return undefined;
        expect(decision).toMatchObject({
          decision: "fault_detected",
          category: fixture.category,
        });
        return (
          (await verifiedHeaders.collect([staged.predecessor])) && decision
        );
      },
      timing.allowances.automaticDecisionMs,
    );
    const { completion, alreadyCorrected } = await excludingLimit(() =>
      verifyJourneyCorrection({
        actionDepth: JOURNEY_ACTION_DEPTH,
        context,
        transactions: mode.transactions,
        chain: correctionChain({
          category: fixture.category,
          headerHash: staged.current.headerHash,
        }),
        watcherStatus: async () =>
          (await operations("/v1/status")) as JourneyWatcherStartStatus,
        proverAddress: mode.proverAddress,
        category: fixture.category,
        headerHash: staged.current.headerHash,
        predecessorHeaderHash: staged.predecessor.headerHash,
        operatorVkey: staged.current.header.operatorVkey,
        requireLive,
        poll,
        stage,
      }),
    );
    if (
      staged.target.kind === "corrected" &&
      staged.target.removalTxHash !== completion.correction.removalTxHash
    )
      throw new Error(
        `Staging reconciled removal ${staged.target.removalTxHash} but the watcher's authenticated correction is ${completion.correction.removalTxHash}`,
      );
    const { contracts } = deployment;
    const {
      successor,
      stage: successorStage,
      faultAttestation,
    } = await successorPort.commit({
      removalTxHash: completion.correction.removalTxHash,
    });
    const healthy = await awaitHealthyWatcher({
      name: "healthy processing after correction",
      observe: async () => {
        requireLive();
        const decision = await verifiedHeaders.collectHealthy(
          staged.predecessor,
          successor,
        );
        if (decision === undefined) return undefined;
        // Our node merges the successor once it matures, which can come
        // first on a long wait or a resumed run.
        expect(
          await onQueueOrMerged({
            provider,
            kupoUrl: context.kupoUrl,
            stateQueue: contracts.stateQueue,
            headerHash: successor.headerHash,
            readConfirmedTransaction: (txHash) =>
              mode.transactions.transaction(txHash),
          }),
        ).toBe(true);
        return decision;
      },
      status: async () =>
        (await operations("/v1/status")) as JourneyHealthyWatcherStatus,
      polls: { pollWhileReplaying },
      progress: watcherProgress,
      stallTimeoutMs: timing.allowances.healthySuccessorObservationMs,
    });
    await verifiedHeaders.retain();
    await verifiedHeaders.handOn({
      hubOraclePolicyId: deploymentManifestHubOraclePolicyId(
        deployment.manifest,
      ),
      block: successor,
    });
    // Save the anchor request before the provisional verdict so restart cannot
    // lose finality tracking after releasing this family's execution slot.
    await writeJourneyArtifact(join(directory, "pending-evidence-stamp.json"), {
      category: fixture.category,
      headerHash: staged.current.headerHash,
      hubOraclePolicyId: deploymentManifestHubOraclePolicyId(
        deployment.manifest,
      ),
      // When the removal landed: its block's slot.
      terminalObservedAt: new Date(
        deployment.operatorLucid.slotToUnixTime(completion.observedAt.slot),
      ).toISOString(),
      successorTxHash: successor.commitTxHash,
      completedAtConfirmationDepth: completion.observedAt.confirmationDepth,
      finalityDepth: releaseFinality.policy.confirmationDepth,
      releaseFinalityPolicyDigest: releaseFinality.policyDigest,
    });
    succeeded = true;
    await writeJourneyArtifact(join(directory, "result.json"), {
      status: "passed",
      executionPolicy: "authenticated-inclusion",
      // Whether an earlier run's correction was already on chain at the
      // journey's first read.
      proofExecution: alreadyCorrected
        ? "historical-completed-proof"
        : "current-inclusion-policy",
      evidenceStatus: "terminal-included-awaiting-anchor",
      category: fixture.category,
      hubOraclePolicyId: deploymentManifestHubOraclePolicyId(
        deployment.manifest,
      ),
      proverAddress: mode.proverAddress,
      completion,
      successor: successor.headerHash,
      // Whether the successor stage tested our node's refusal, or the fault
      // was removed before our node's shift could reach it.
      ...successorStage,
      // How long after the fault's block time its DA attestation was
      // applied, where the successor read it from L1.
      ...(faultAttestation === undefined ? {} : { faultAttestation }),
      // The status read that proved healthy processing, which the evidence
      // verifier checks, not a later read that may carry a fresh alert.
      diagnostics: { ...(await running.diagnostics()), status: healthy.status },
      nativeEvidencePath: mode.transactions.nativeEvidencePath,
    });
    const finalizeEvidence = () =>
      finalizePendingJourneyEvidence({
        journeysDirectory: join(context.runDirectory, "work/journeys"),
        readCorrection: (request) => correctionChain(request).correction(),
        hubOraclePolicyId: deploymentManifestHubOraclePolicyId(
          deployment.manifest,
        ),
        releaseFinalityPolicyDigest: releaseFinality.policyDigest,
        finalityDepth: releaseFinality.policy.confirmationDepth,
        nativeEvidencePath: mode.transactions.nativeEvidencePath,
        authenticate: async (request, terminal) => {
          const transactions = await Promise.all(
            [
              terminal.proofToken.createdByTxHash,
              terminal.correction.removalTxHash,
              request.successorTxHash,
            ].map((txHash) => mode.transactions.transaction(txHash)),
          );
          const observedBlockNo = mode.transactions.observedBlockNo();
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
          timing.allowances.finalizedEvidenceStampMs,
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
    await successorPort?.close();
    await closeMode?.();
    console.info(
      `Journey ${execution.kind} evidence retained at ${directory}; succeeded=${succeeded}`,
    );
  }
};
