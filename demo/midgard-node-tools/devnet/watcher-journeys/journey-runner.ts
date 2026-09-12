import { createHash, randomBytes } from "node:crypto";
import { appendFileSync, existsSync } from "node:fs";
import { appendFile, mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { resolveProverSigner } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";
import { startStateQueueMutationLeaseServer } from "midgard-node/tests/helpers/state-queue-mutation-lease-server";
import {
  makeWatcherFinalityPolicy,
  openWatcherFaultDecisionJournal,
  parseWatcherConfig,
  parseWatcherProcessConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";
import { createPublishedWatcherDeploymentAuthority } from "midgard-watcher/tests/support/published-deployment-authority";
import { expect } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import {
  readJourneyWorkflowEntries,
  verifyJourneyCorrection,
} from "./correction.js";
import type {
  JourneyFixture,
  JourneyFixtureStage,
  JourneySuccessor,
  JourneySuccessorCheckpoint,
} from "./fixture.js";
import { startJourneyHistoryArchives } from "./history-archives.js";
import { prepareDuplicateEventHistory } from "./history-settlement.js";
import { readTransitionTraceJourneyTiming } from "./journey-timing.js";
import { JOURNEY_FINALITY_DEPTH, loadJourneyContext } from "./live-context.js";
import { journeyNativeNodeQuery } from "./native-node.js";
import { startJourneyNativeRecorder } from "./native-recorder.js";
import { journeyPorts, launchJourneyWatcherProcess } from "./process.js";
import { verifyJourneyPublicDa } from "./public-da-preflight.js";
import { verifyJourneyResultEvidence } from "./readiness-evidence.js";
import { startJourneyRetainedDa } from "./retained-da.js";
import { measureJourneyStage } from "./stage-timing.js";
import { prepareJourneyHistory } from "./staging.js";
import { verifyJourneyWorkflowBindings } from "./workflow-binding-preflight.js";

/** The watcher CLI's failed-closed exit status. */
const WATCHER_FAILED_CLOSED_EXIT_CODE = 70;
const WATCHER_RESTART_LIMIT = 12;
const WATCHER_LAUNCH_TIMEOUT_MS = 1_800_000;

type JourneyExecution =
  | { kind: "journey"; fixture: JourneyFixture }
  | { kind: "prepare_duplicate_event_history" };

/** One shared service lifecycle for acceptance and genuine history prerequisites. */
const runJourney = async (
  runDirectory: string,
  execution: JourneyExecution,
) => {
  const context = await loadJourneyContext(runDirectory);
  const { deployment, provider, accounts, runEnv } = context;
  if (execution.kind === "prepare_duplicate_event_history")
    await verifyJourneyResultEvidence(
      context.runDirectory,
      join(context.runDirectory, "work/journeys/transition-trace"),
      "transitionTrace",
      deployment,
    );
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
  const cleanup: (() => Promise<void>)[] = [];
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
    const authority = await stage("signed deployment authority", () =>
      createPublishedWatcherDeploymentAuthority({
        deployment,
        directory,
        fundingProfiles: [],
        programCommitments: {
          "computation-thread-policy-v1": createHash("sha256")
            .update(
              JSON.stringify({
                computationThreadPolicyId:
                  deployment.contracts.computationThread.policyId,
              }),
            )
            .digest("hex"),
        },
      }),
    );
    const releaseFinality = await watcherDeploymentReleaseFinalityAuthority(
      authority.deploymentAuthority.deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: deployment.manifest.manifestId,
    });
    const timing =
      execution.kind === "journey" && category === "transitionTrace"
        ? await readTransitionTraceJourneyTiming(context.runDirectory, {
            authenticatedConfirmationDepth:
              releaseFinality.policy.confirmationDepth,
          })
        : undefined;
    if (timing !== undefined) {
      await writeJourneyArtifact(join(directory, "timing-plan.json"), timing);
      console.info("Live watcher confirmation budget", timing);
    }
    const retainedDa = await startJourneyRetainedDa({
      runDirectory: context.runDirectory,
      runEnv,
      deploymentFingerprint: deployment.manifest.manifestId,
    });
    cleanup.push(retainedDa.close);
    const archives = await startJourneyHistoryArchives({
      releaseFinality,
      runDirectory: context.runDirectory,
      composeProject: runEnv.MIDGARD_PHASE4_COMPOSE_PROJECT,
      deploymentFingerprint: deployment.manifest.manifestId,
    });
    cleanup.push(archives.close);
    const leaseServer = await startStateQueueMutationLeaseServer({
      postgres: {
        host: "127.0.0.1",
        port: Number(runEnv.MIDGARD_PHASE4_POSTGRES_PORT),
        username: runEnv.MIDGARD_PHASE4_POSTGRES_USER,
        password: runEnv.MIDGARD_PHASE4_POSTGRES_PASSWORD,
      },
    });
    cleanup.push(async () => {
      await writeJourneyArtifact(
        join(directory, "node-lease-inspection.json"),
        await leaseServer.inspect(),
      );
      await leaseServer.close();
    });
    const secret = async (name: string, initial: string) => {
      const path = join(context.runDirectory, "secrets", name);
      if (!existsSync(path))
        await writeFile(path, initial, { mode: 0o600, flag: "wx" });
      return { kind: "file" as const, path };
    };
    const rollbackKey = await secret(
      "watcher-rollback.key",
      randomBytes(32).toString("hex"),
    );
    const proverKey = await secret(
      "watcher-prover.seed",
      accounts.publisher.seedPhrase,
    );
    const availabilityKey = await secret(
      "watcher-availability.seed",
      accounts.availability.seedPhrase,
    );
    const bearerKey = await secret(
      "watcher-trusted-bearer.key",
      randomBytes(32).toString("hex"),
    );
    const recordKey = await secret(
      "watcher-trusted-record.key",
      randomBytes(32).toString("hex"),
    );
    const nodeAdminKey = {
      kind: "file" as const,
      path: join(context.runDirectory, "secrets/journey-node-admin.key"),
    };
    await writeFile(nodeAdminKey.path, leaseServer.adminApiKey, {
      mode: 0o600,
    });
    const nativeQuery = await journeyNativeNodeQuery(context.runDirectory);
    if (nativeQuery.watcherConfig.l1.source.sourceMode !== "local_node")
      throw new Error("Native node source required");
    const watcherInput = {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "acceptance",
      targetNetwork: "Custom",
      customNetwork: context.customNetwork,
      l1: {
        source: {
          ...nativeQuery.watcherConfig.l1.source,
          queryServices: [
            {
              kind: "ogmios",
              identity: "journey-ogmios",
              endpoint: context.ogmiosUrl,
            },
            {
              kind: "kupo",
              identity: "journey-kupo",
              endpoint: context.kupoUrl,
            },
          ],
        },
        requestTimeoutMs: 30_000,
        maxConcurrency: 8,
        finality: {
          depth: JOURNEY_FINALITY_DEPTH,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: JOURNEY_FINALITY_DEPTH,
          },
        },
      },
      da: {
        peers: [retainedDa.peer],
        requestTimeoutMs: 30_000,
        maxConcurrency: 8,
      },
      storage: {
        driver: "sqlite",
        path: join(runtimeDirectory, "watcher.sqlite"),
        rollbackAuthorityKeySource: rollbackKey,
      },
      proverWallet: { keySource: proverKey },
      deadlines: {
        daFetchMs: 60_000,
        daPublishMs: 60_000,
        proofConstructMs: 300_000,
        proofSubmitMs: 120_000,
      },
    };
    const watcherConfig = parseWatcherConfig(watcherInput);
    const native = await stage("independent native chain recorder", () =>
      startJourneyNativeRecorder({
        directory,
        watcherConfig,
        binaryPath: nativeQuery.binaryPath,
        onBlock: archives.retainNativeBlock,
        onRollback: archives.rollbackNativeBlocks,
      }),
    );
    cleanup.push(native.close);
    const retain = async (
      block: { headerHash: string; payloadEnvelopeCbor: Uint8Array },
      txHash: string,
    ) => {
      const actual = await native.transaction(txHash);
      await retainedDa.retain(block);
      await archives.retain(block, actual.point);
    };
    const fixtureStage: JourneyFixtureStage = {
      context,
      directory,
      historicalNativeScriptProviders: archives.configuration.providers,
      retain,
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
    const { fixture } = execution;
    const staged = await stage("invalid commitment and DA attestations", () =>
      fixture.stage(fixtureStage),
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
    await stage("independent actor funding", async () => {
      const fundingPath = join(runtimeDirectory, "actor-funding.txt");
      if (existsSync(fundingPath)) return;
      const prover = resolveProverSigner({
        network: "Custom",
        walletSeedPhrase: accounts.publisher.seedPhrase,
      }).address;
      const availability = resolveProverSigner({
        network: "Custom",
        walletSeedPhrase: accounts.availability.seedPhrase,
      }).address;
      const publisherAddress = await deployment.publisherLucid
        .wallet()
        .address();
      deployment.publisherLucid.overrideUTxOs(
        (await provider.getUtxos(publisherAddress)).filter(
          (utxo) =>
            utxo.datum == null &&
            utxo.datumHash == null &&
            utxo.scriptRef == null &&
            Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
        ),
      );
      const built = await deployment.publisherLucid
        .newTx()
        .pay.ToAddress(prover, { lovelace: 500_000_000n })
        .pay.ToAddress(prover, { lovelace: 500_000_000n })
        .pay.ToAddress(prover, { lovelace: 100_000_000n })
        .pay.ToAddress(availability, { lovelace: 40_000_000_000n })
        .pay.ToAddress(availability, { lovelace: 10_000_000n })
        .complete({ localUPLCEval: true });
      const txHash = await (await built.sign.withWallet().complete()).submit();
      await provider.awaitTx(txHash, 500);
      await native.transaction(txHash);
      await writeFile(fundingPath, txHash);
      deployment.publisherLucid.overrideUTxOs(
        await provider.getUtxos(publisherAddress),
      );
    });
    const policy = makeWatcherFinalityPolicy(
      watcherConfig,
      authority.deploymentAuthority.deploymentIdentity,
    );
    if (policy === null) throw new Error("Finality policy was not admitted");
    const [authorityPort, operationsPort] = await journeyPorts(2);
    const trustedHeadAuthorityEndpoint = `http://127.0.0.1:${authorityPort}`;
    const operationsEndpoint = `http://127.0.0.1:${operationsPort}`;
    const authorityConfigPath = join(directory, "authority-process.json");
    await writeJourneyArtifact(authorityConfigPath, {
      schemaVersion: "midgard-watcher-trusted-head-authority-process-config-v1",
      directory: join(runtimeDirectory, "trusted-head"),
      endpoint: trustedHeadAuthorityEndpoint,
      policy,
      recordAuthenticationKeySource: recordKey,
      httpBearerSecretSource: bearerKey,
    });
    const authorityProcess = launchJourneyWatcherProcess({
      command: "authority",
      configPath: authorityConfigPath,
      directory,
      caPath: archives.caPath,
    });
    cleanup.push(authorityProcess.close);
    diagnostics = async () => ({
      authorityProcess: authorityProcess.observe(),
    });
    const bearer = (await readFile(bearerKey.path, "utf8")).trim();
    await poll(
      "trusted-head authority process",
      async () => {
        authorityProcess.assertHealthy();
        const ready = await fetch(
          `${trustedHeadAuthorityEndpoint}/v1/identity`,
          {
            headers: { authorization: `Bearer ${bearer}` },
            signal: AbortSignal.timeout(5000),
          },
        )
          .then((response) => response.ok)
          .catch(() => false);
        return ready ? true : undefined;
      },
      30_000,
    );
    const processInput = {
      schemaVersion: WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
      watcherConfig: watcherInput,
      watcherRuntimeConfigPath: join(directory, "watcher.json"),
      deploymentAuthorityPath: authority.authorityPath,
      ruleBundlePath: authority.ruleBundlePath,
      fundingProfileBundlePath: authority.fundingProfileBundlePath,
      nativeChainSyncBinaryPath: nativeQuery.binaryPath,
      trustedHeadAuthorityEndpoint,
      operationsEndpoint,
      httpBearerSecretSource: bearerKey,
      workflowJournalDirectory: join(runtimeDirectory, "workflows"),
      availability: {
        keySource: availabilityKey,
        journalPath: join(runtimeDirectory, "availability.sqlite"),
        minimumFundingLovelace: "100000000",
      },
      readinessHeaderHash: staged.current.headerHash,
      faultProofInfrastructure: {
        manifestPath: authority.manifestPath,
        blueprintPath: authority.blueprintPath,
        deploymentInfoPath: authority.deploymentInfoPath,
        midgardNodeUrl: leaseServer.url,
        midgardNodeAdminKeySource: nodeAdminKey,
        historicalNativeScriptHistory: archives.configuration,
      },
    };
    const config = parseWatcherProcessConfig(processInput);
    const configPath = join(directory, "watcher-process.json");
    await writeJourneyArtifact(config.watcherRuntimeConfigPath, watcherInput);
    await writeJourneyArtifact(configPath, processInput);
    await stage("installed workflow binding preflight", () =>
      verifyJourneyWorkflowBindings({
        directory,
        config,
      }),
    );
    const workflowBaseline = await readJourneyWorkflowEntries({
      workflowJournalDirectory: config.workflowJournalDirectory,
      category: fixture.category,
      headerHash: staged.current.headerHash,
    });
    const watcherLaunch = {
      command: "start" as const,
      configPath,
      directory,
      caPath: archives.caPath,
    };
    let watcher = launchJourneyWatcherProcess(watcherLaunch);
    cleanup.push(() => watcher.close());
    // The watcher fails closed on transient L1 conditions (Kupo checkpoint
    // churn, Ogmios disconnects, retained-DA fetches) and exits 70. Its
    // journal makes a restart resume the same workflow, so relaunch a bounded
    // number of times; a stalled workflow still ends the journey through the
    // journal, and any other exit remains fatal.
    let watcherRestarts = 0;
    const requireLive = () => {
      native.assertHealthy();
      authorityProcess.assertHealthy();
      const observed = watcher.observe();
      if (
        observed.state === "exited" &&
        observed.exitCode === WATCHER_FAILED_CLOSED_EXIT_CODE &&
        watcherRestarts < WATCHER_RESTART_LIMIT
      ) {
        watcherRestarts += 1;
        appendFileSync(
          join(directory, "watcher-restarts.ndjson"),
          `${JSON.stringify({ restartedAt: new Date().toISOString(), restart: watcherRestarts, previous: observed })}\n`,
        );
        console.warn(
          `Live watcher exited failed-closed; relaunching (restart ${watcherRestarts}/${WATCHER_RESTART_LIMIT})`,
          observed.tail.at(-1) ?? "",
        );
        watcher = launchJourneyWatcherProcess(watcherLaunch);
        return;
      }
      watcher.assertHealthy();
    };
    const operations = async (path: string) => {
      const response = await fetch(`${operationsEndpoint}${path}`, {
        signal: AbortSignal.timeout(5000),
      });
      if (!response.ok)
        throw new Error(`Operations HTTP returned ${response.status}`);
      return await response.json();
    };
    diagnostics = async () => ({
      process: watcher.observe(),
      authorityProcess: authorityProcess.observe(),
      status: await operations("/v1/status").catch(String),
      metrics: await operations("/v1/metrics").catch(String),
    });
    const trustedHeadRevision = async (): Promise<string | null> => {
      try {
        const response = await fetch(
          `${trustedHeadAuthorityEndpoint}/v1/trusted-head`,
          {
            headers: { authorization: `Bearer ${bearer}` },
            signal: AbortSignal.timeout(5000),
          },
        );
        if (!response.ok) return null;
        const body = (await response.json()) as {
          head?: { revision?: unknown };
        };
        return typeof body.head?.revision === "string"
          ? body.head.revision
          : null;
      } catch {
        return null;
      }
    };
    let nextStartupReport = 0;
    await pollWhileReplaying(
      "normal watcher launcher",
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
            process: watcher.observe(),
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
      correctionTimeoutMs: timing?.correctionTimeoutMs,
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
            beforeCommit: retainedDa.retain,
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
    succeeded = true;
    await writeJourneyArtifact(join(directory, "result.json"), {
      status: "passed",
      category: fixture.category,
      deploymentFingerprint: deployment.manifest.manifestId,
      completion,
      successor: successor.headerHash,
      diagnostics: await diagnostics(),
    });
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
    for (const close of cleanup.reverse()) await close();
    console.info(
      `Journey ${execution.kind} evidence retained at ${directory}; succeeded=${succeeded}`,
    );
  }
};

/** One process-driven acceptance path for every non-interactive family. */
export const runAutonomousWatcherJourney = (
  runDirectory: string,
  fixture: JourneyFixture,
) => runJourney(runDirectory, { kind: "journey", fixture });

/** Start the real maturity clock after the verified trace baseline has completed. */
export const prepareAutonomousWatcherHistory = (runDirectory: string) =>
  runJourney(runDirectory, { kind: "prepare_duplicate_event_history" });
