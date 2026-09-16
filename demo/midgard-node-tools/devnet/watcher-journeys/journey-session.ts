import { createHash, randomBytes } from "node:crypto";
import { appendFileSync, existsSync, readdirSync, readFileSync } from "node:fs";
import { mkdir, mkdtemp, readdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";
import { fileURLToPath } from "node:url";

import {
  createLocalKupmiosHttpOgmiosRawSource,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  resolveProverSigner,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { startStateQueueMutationLeaseServer } from "midgard-node/tests/helpers/state-queue-mutation-lease-server";
import {
  makeWatcherFinalityPolicy,
  parseWatcherConfig,
  parseWatcherProcessConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";
import { createPublishedWatcherDeploymentAuthority } from "midgard-watcher/tests/support/published-deployment-authority";

import { writeJourneyArtifact } from "./artifacts.js";
import { readJourneyWorkflowEntries } from "./correction.js";
import { startJourneyHistoryArchives } from "./history-archives.js";
import { JOURNEY_FINALITY_DEPTH, loadJourneyContext } from "./live-context.js";
import { journeyNativeNodeQuery } from "./native-node.js";
import { startJourneyNativeRecorder } from "./native-recorder.js";
import {
  isJourneyOperationsConnectionRefused,
  readJourneyOperations,
  waitForRestartedJourneyOperations,
} from "./operations.js";
import { journeyPorts, launchJourneyWatcherProcess } from "./process.js";
import { startJourneyRetainedDa } from "./retained-da.js";
import type { SignedCommitAttempt } from "./signed-commit-reconciliation.js";
import { measureJourneyStage } from "./stage-timing.js";
import { verifyJourneyWorkflowBindings } from "./workflow-binding-preflight.js";

const WATCHER_FAILED_CLOSED_EXIT_CODE = 70;
const WATCHER_RESTART_LIMIT = 12;

/** Capture immutable journal prefixes before a running observer can act on new staging. */
export const captureJourneyWorkflowBaseline = async (
  workflowJournalDirectory: string,
  category: FraudProofCatalogueCategoryName,
) => {
  const directory = join(workflowJournalDirectory, "fault-proofs", category);
  if (!existsSync(directory))
    return new Map<
      string,
      Awaited<ReturnType<typeof readJourneyWorkflowEntries>>
    >();
  const headers = (await readdir(directory, { withFileTypes: true })).filter(
    (entry) => entry.isDirectory() && /^[0-9a-f]{56}$/u.test(entry.name),
  );
  return new Map(
    await Promise.all(
      headers.map(
        async ({ name }) =>
          [
            name,
            await readJourneyWorkflowEntries({
              workflowJournalDirectory,
              category,
              headerHash: name,
            }),
          ] as const,
      ),
    ),
  );
};

/** One immutable deployment and service lifetime, explicitly owned by its suite. */
export const openJourneySession = async (runDirectory: string) => {
  const context = await loadJourneyContext(runDirectory);
  const { deployment, provider, accounts, runEnv } = context;
  const runtimeDirectory = join(context.runDirectory, "work/journeys/runtime");
  const sessionsDirectory = join(
    context.runDirectory,
    "work/journeys/sessions",
  );
  await Promise.all([
    mkdir(runtimeDirectory, { recursive: true, mode: 0o700 }),
    mkdir(sessionsDirectory, { recursive: true, mode: 0o700 }),
  ]);
  const directory = await mkdtemp(join(sessionsDirectory, "session-"));
  const cleanup: (() => Promise<void>)[] = [];
  let closed = false;
  let closePromise: Promise<void> | undefined;
  const close = () =>
    (closePromise ??= (async () => {
      closed = true;
      const failures: unknown[] = [];
      for (const stop of cleanup.reverse()) {
        try {
          await stop();
        } catch (cause) {
          failures.push(cause);
        }
      }
      if (failures.length > 0)
        throw new AggregateError(
          failures,
          "Could not close watcher journey session",
        );
    })());
  const stage = <T>(name: string, action: () => Promise<T>) =>
    measureJourneyStage(directory, name, action);
  const poll = async <T>(
    name: string,
    action: () => Promise<T | undefined>,
    timeoutMs: number,
  ) =>
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
  try {
    const authority = await stage("signed deployment authority", () =>
      createPublishedWatcherDeploymentAuthority({
        deployment,
        directory,
        // Every journey resumes the shared watcher runtime, whose saved
        // user-event history binds the attesting trust root.
        trustRootKeyPath: join(
          context.runDirectory,
          "work/journeys/deployment-trust-root.pem",
        ),
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
      try {
        await writeJourneyArtifact(
          join(directory, "node-lease-inspection.json"),
          await leaseServer.inspect(),
        );
      } finally {
        await leaseServer.close();
      }
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
    const signedCommitSource = createLocalKupmiosHttpOgmiosRawSource({
      sourceId: "journey-signed-header-recovery",
      kupoHttpUrl: context.kupoUrl,
      ogmiosUrl: context.ogmiosUrl,
      releaseFinality,
      timeoutMs: watcherConfig.l1.requestTimeoutMs,
    });
    const readSignedCommitRecovery = (attempt: SignedCommitAttempt) => {
      if (closed) throw new Error("Journey session is closed");
      return readAdmittedLocalKupmiosSignedTransactionRecovery({
        source: signedCommitSource,
        transactionHash: attempt.txHash,
        signedTransactionCborHex: attempt.signedCbor,
      });
    };

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
      transportEnvironment: archives.transportEnvironment,
    });
    cleanup.push(authorityProcess.close);
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
    const launch = async (readinessHeaderHash: string) => {
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
        readinessHeaderHash,
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
      const bindingReceipt = await stage(
        "installed workflow binding preflight",
        () =>
          verifyJourneyWorkflowBindings({
            directory,
            config,
          }),
      );
      const demoRoot = fileURLToPath(new URL("../../../", import.meta.url));
      const pinnedFiles = [
        ...bindingReceipt.configurationFiles,
        ...bindingReceipt.sourceIdentity.files.map(({ path, sha256 }) => ({
          path: join(demoRoot, path),
          sha256,
        })),
        ...[
          fileURLToPath(import.meta.url),
          fileURLToPath(new URL("./journey-runner.ts", import.meta.url)),
          configPath,
          authorityConfigPath,
          nativeQuery.binaryPath,
          process.execPath,
          archives.caPath,
          rollbackKey.path,
          proverKey.path,
          availabilityKey.path,
          bearerKey.path,
          recordKey.path,
          nodeAdminKey.path,
        ].map((path) => ({
          path,
          sha256: createHash("sha256").update(readFileSync(path)).digest("hex"),
        })),
      ];
      const readBuildIdentity = () =>
        ["midgard-watcher", "midgard-fault-proofs"].map((name) => {
          const path = join(demoRoot, name, "dist");
          return readdirSync(path)
            .filter((file) => file.endsWith(".js"))
            .sort()
            .map((file) => [
              name,
              file,
              createHash("sha256")
                .update(readFileSync(join(path, file)))
                .digest("hex"),
            ]);
        });
      const buildIdentity = JSON.stringify(readBuildIdentity());
      const assertIdentity = () => {
        for (const { path, sha256 } of pinnedFiles) {
          if (
            createHash("sha256").update(readFileSync(path)).digest("hex") !==
            sha256
          )
            throw new Error(`Watcher journey session input changed: ${path}`);
        }
        if (JSON.stringify(readBuildIdentity()) !== buildIdentity)
          throw new Error("Watcher journey session compiled artifacts changed");
      };
      assertIdentity();
      const watcherLaunch = {
        command: "start" as const,
        configPath,
        directory,
        caPath: archives.caPath,
        transportEnvironment: archives.transportEnvironment,
      };
      let watcher = launchJourneyWatcherProcess(watcherLaunch);
      cleanup.push(() => watcher.close());
      // The watcher fails closed on transient L1 conditions (Kupo checkpoint
      // churn, Ogmios disconnects, retained-DA fetches) and exits 70. Its
      // journal makes a restart resume the same workflow, so relaunch a bounded
      // number of times; a stalled workflow still ends the journey through the
      // journal, and any other exit remains fatal.
      let watcherRestarts = 0;
      let restartPending = false;
      const requireLive = () => {
        if (closed) throw new Error("Watcher journey session is closed");
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
          assertIdentity();
          watcher = launchJourneyWatcherProcess(watcherLaunch);
          restartPending = true;
          return;
        }
        watcher.assertHealthy();
      };
      const readOperations = (path: string) =>
        readJourneyOperations(operationsEndpoint, path);
      const operations = async (
        path: string,
      ): ReturnType<typeof readJourneyOperations> => {
        requireLive();
        if (restartPending) {
          const status = await stage("restarted watcher operations", () =>
            waitForRestartedJourneyOperations({
              readStatus: () => readOperations("/v1/status"),
              requireLive,
            }),
          );
          restartPending = false;
          if (path === "/v1/status") return status;
        }
        try {
          return await readOperations(path);
        } catch (cause) {
          if (!isJourneyOperationsConnectionRefused(cause)) throw cause;
          // The process can exit between the health check and this read.
          // Retry only if that exit actually authorizes a bounded restart.
          requireLive();
          if (!restartPending) throw cause;
          return await operations(path);
        }
      };
      const diagnostics = async () => ({
        process: watcher.observe(),
        authorityProcess: authorityProcess.observe(),
        status: await readOperations("/v1/status").catch(String),
        metrics: await readOperations("/v1/metrics").catch(String),
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

      return {
        config,
        configPath,
        requireLive,
        assertIdentity,
        operations,
        diagnostics,
        trustedHeadRevision,
        observe: () => watcher.observe(),
      };
    };
    let started: ReturnType<typeof launch> | undefined;
    const assertHealthy = async () => {
      if (closed) throw new Error("Watcher journey session is closed");
      try {
        native.assertHealthy();
        authorityProcess.assertHealthy();
        if (started !== undefined) {
          const running = await started;
          running.assertIdentity();
          running.requireLive();
        }
      } catch (cause) {
        await close();
        throw cause;
      }
    };
    const ensureWatcher = async (readinessHeaderHash: string) => {
      await assertHealthy();
      return await (started ??= launch(readinessHeaderHash).catch(
        async (cause: unknown) => {
          await close();
          throw cause;
        },
      ));
    };
    return {
      context,
      directory,
      runtimeDirectory,
      authority,
      releaseFinality,
      watcherConfig,
      readSignedCommitRecovery,
      archives,
      native,
      retain,
      retainPayload: retainedDa.retain,
      workflowJournalDirectory: join(runtimeDirectory, "workflows"),
      ensureWatcher,
      watcherStarted: () => started !== undefined,
      authorityObserve: authorityProcess.observe,
      assertHealthy,
      close,
    };
  } catch (cause) {
    await close();
    throw cause;
  }
};
export type JourneySession = Awaited<ReturnType<typeof openJourneySession>>;
