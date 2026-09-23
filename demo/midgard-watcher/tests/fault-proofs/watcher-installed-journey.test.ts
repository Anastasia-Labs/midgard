import { copyFile, mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { createServer, type Server, type Socket } from "node:net";
import { dirname, join } from "node:path";
import { performance } from "node:perf_hooks";
import { setTimeout as pause } from "node:timers/promises";

import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalEntry,
  resolveProverSigner,
} from "@al-ft/midgard-fault-proofs";
import { recordCrossBlockRawEmulator } from "@al-ft/midgard-fault-proofs/test-support/cross-block-raw-emulator";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  generateEmulatorAccount,
  Kupmios,
  type Provider,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeployment,
} from "midgard-node/tests/helpers/published-workflow-deployment";
import { expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import { makeWatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import { WatcherLocalKupmios } from "../../src/l1/native-reward-account.js";
import {
  parseWatcherProcessConfig,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
} from "../../src/runtime/process-config.js";
import {
  createWatcherRuntime,
  type WatcherRuntime,
} from "../../src/runtime/watcher-runtime.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import { createEmulatorChainTransport } from "../support/emulator-chain-transport.js";
import { serveEmulatorRetainedDa } from "../support/emulator-retained-da.js";
import { createPublishedWatcherDeploymentAuthority } from "../support/published-deployment-authority.js";
import { stagePublishedDepositTrace } from "../support/published-deposit-trace.js";
import { startWatcherTrustedHeadAuthorityChildForTest } from "../support/trusted-head-process-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

const transport = vi.hoisted(() => ({
  provider: undefined as Provider | undefined,
}));
vi.mock("@al-ft/midgard-fault-proofs", async (loadOriginal) => {
  const actual =
    await loadOriginal<typeof import("@al-ft/midgard-fault-proofs")>();
  return {
    ...actual,
    makeLucidForSubmit: async (
      config: Parameters<typeof actual.makeLucidForSubmit>[0],
    ) => {
      if (
        transport.provider === undefined ||
        config.provider !== "Kupmios" ||
        config.network !== "Preprod"
      ) {
        throw new Error(
          "Watcher fixture received an unexpected provider configuration",
        );
      }
      const { Lucid } = await import("@lucid-evolution/lucid");
      return await Lucid(transport.provider, config.network);
    },
  };
});

// Every proof submission reaches the actual Lucid emulator. Only the external
// chain transport is simulated; the watcher application, decision bridge,
// supervisor, journals and validators execute their normal implementations.
it("detects an invalid commitment, confirms correction, and classifies the honest successor as healthy", async () => {
  const directory = await mkdtemp("/var/tmp/watcher-installed-journey-");
  const recorder = recordCrossBlockRawEmulator();
  const cleanup: (() => void | Promise<void>)[] = [];
  let watcher: WatcherRuntime | undefined;
  let runtimeDiagnostics: (() => Promise<unknown>) | undefined;
  let nativeBlocksPath: string | undefined;
  let succeeded = false;
  let completedStage = "test setup";
  let activeStage = "deployment";
  const stage = async <T>(
    name: string,
    action: () => Promise<T>,
    deadlineMs = 120_000,
  ): Promise<T> => {
    activeStage = name;
    const startedAt = performance.now();
    console.info(`watcher journey: ${name}`);
    let timer: ReturnType<typeof setTimeout> | undefined;
    try {
      const value = await Promise.race([
        action(),
        new Promise<never>((_, reject) => {
          timer = setTimeout(
            () =>
              reject(
                new Error(
                  `Stage deadline exceeded; last completed stage: ${completedStage}; waiting for: ${activeStage}`,
                ),
              ),
            deadlineMs,
          );
        }),
      ]);
      completedStage = name;
      console.info(
        `watcher journey: completed ${name} in ${((performance.now() - startedAt) / 1_000).toFixed(1)}s`,
      );
      return value;
    } finally {
      clearTimeout(timer);
    }
  };
  const listener = async () => {
    const sockets = new Set<Socket>();
    const server = createServer((socket) => {
      sockets.add(socket);
      socket.once("close", () => sockets.delete(socket));
    });
    await new Promise<void>((resolve, reject) => {
      server.once("error", reject);
      server.listen(0, "127.0.0.1", resolve);
    });
    const address = server.address();
    if (address === null || typeof address === "string")
      throw new Error("No local transport port");
    cleanup.push(
      () =>
        new Promise<void>((resolve, reject) => {
          for (const socket of sockets) socket.destroy();
          server.close((error) => (error ? reject(error) : resolve()));
        }),
    );
    return { server, port: address.port };
  };
  const closeServer = (server: Server) =>
    new Promise<void>((resolve, reject) =>
      server.close((error) => (error ? reject(error) : resolve())),
    );
  try {
    const accounts = createPublishedWorkflowDeploymentAccounts();
    const availabilityAccount = generateEmulatorAccount({ lovelace: 0n });
    const deployment = await stage("published deployment", () =>
      publishWorkflowDeployment({ network: "Preprod", accounts }),
    );
    vi.spyOn(Date, "now").mockImplementation(() => deployment.emulator.now());
    const staged = await stage("known invalid block fixture", () =>
      stagePublishedDepositTrace(deployment, {
        daSignerConfig: {
          L1_OPERATOR_SEED_PHRASE: accounts.operator.seedPhrase,
          DA_COSIGNER_SEED_PHRASE: accounts.cosigner.seedPhrase,
          NETWORK: "Preprod",
        },
        onStage: (name) => {
          activeStage = name;
          console.info(`watcher fixture: ${name}`);
        },
      }),
    );
    await stage("prover wallet funding", async () => {
      const proverAddress = resolveProverSigner({
        network: "Preprod",
        walletSeedPhrase: accounts.publisher.seedPhrase,
      }).address;
      const availabilityAddress = resolveProverSigner({
        network: "Preprod",
        walletSeedPhrase: availabilityAccount.seedPhrase,
      }).address;
      const transfer = await deployment.publisherLucid
        .newTx()
        .pay.ToAddress(proverAddress, { lovelace: 500_000_000n })
        .pay.ToAddress(proverAddress, { lovelace: 500_000_000n })
        .pay.ToAddress(proverAddress, { lovelace: 100_000_000n })
        .pay.ToAddress(availabilityAddress, { lovelace: 40_000_000_000n })
        .pay.ToAddress(availabilityAddress, { lovelace: 10_000_000n })
        .complete();
      await deployment.publisherLucid.awaitTx(
        await (await transfer.sign.withWallet().complete()).submit(),
      );
    });
    const retainedDa = serveEmulatorRetainedDa({
      deploymentFingerprint: deployment.manifest.manifestId,
      blocks: [staged.predecessor, staged.current],
    });
    const configuration = await stage("watcher configuration", () =>
      createPublishedWatcherDeploymentAuthority({
        deployment,
        directory,
        fundingProfiles: [],
        programCommitments: {
          "computation-thread-policy-v1": watcherSha256CanonicalJson({
            computationThreadPolicyId:
              deployment.contracts.computationThread.policyId,
          }),
        },
      }),
    );
    const kupo = await listener();
    const ogmios = await listener();
    const operations = await listener();
    await closeServer(operations.server);
    cleanup.pop();
    const initializationStatus = await deployment.emulator.getTransactionStatus(
      deployment.initialization.txHash,
    );
    if (
      initializationStatus.status !== "confirmed" ||
      initializationStatus.confirmation.slot === undefined
    ) {
      throw new Error(
        "Published initialization is not confirmed by the emulator",
      );
    }
    const native = await stage("native chain fixture", () =>
      createSyntheticUserEventOriginFixture({
        queryEndpoints: {
          kupo: `http://127.0.0.1:${kupo.port}`,
          ogmios: `http://127.0.0.1:${ogmios.port}`,
        },
        nativeTipBaseDepth: 40,
        blockSlotInterval: 20,
        nativeTipMode: "controlled",
        nativeStreamInitialAcknowledgement: true,
        published: {
          deployment: configuration.nativeDeployment,
          inclusionSlot: initializationStatus.confirmation.slot,
          transactionCbor: recorder.signedCbors.get(
            deployment.initialization.txHash,
          )!,
          creatingTransactions: recorder.rows
            .slice(
              0,
              recorder.rows.findIndex(
                ({ txHash }) => txHash === deployment.initialization.txHash,
              ),
            )
            .map(({ txHash }) => ({
              transactionCbor: recorder.signedCbors.get(txHash)!,
            })),
        },
        protocolParameters: {
          minFeeCoefficient: 44,
          minFeeConstant: { ada: { lovelace: 155381 } },
          scriptExecutionPrices: { memory: "577/10000", cpu: "721/10000000" },
          minUtxoDepositCoefficient: 4310,
          collateralPercentage: 150,
          maxCollateralInputs: 3,
          maxTransactionSize: { bytes: 16384 },
          maxValueSize: { bytes: 5000 },
          maxExecutionUnitsPerTransaction: {
            memory: 16_500_000,
            cpu: 10_000_000_000,
          },
          minFeeReferenceScripts: { base: 15, range: 25_600, multiplier: 1.2 },
          maxReferenceScriptsSizePerTransaction: { bytes: 204_800 },
        },
      }),
    );
    cleanup.push(native.close);
    const chain = await stage("emulator confirmation transport", () =>
      createEmulatorChainTransport({
        emulator: deployment.emulator,
        native,
        recorder,
        initializationTxHash: deployment.initialization.txHash,
        confirmationBlocksPerSubmission:
          native.watcherConfig.l1.finality.depth + 1,
      }),
    );
    nativeBlocksPath = join(
      dirname(native.nativeChainSyncBinaryPath),
      "blocks.json",
    );
    cleanup.push(chain.close);
    await chain.grow(40);
    // Submission growth supplies the configured finality depth plus one real
    // block. Background growth matches the fixture's 20-second block interval
    // so durable ingestion need not race a chain accelerated fourfold.
    chain.start({ intervalMs: 20_000, blocksPerTick: 1 });

    const provider = deployment.emulator;
    transport.provider = provider;
    vi.spyOn(Kupmios.prototype, "getProtocolParameters").mockImplementation(
      () => provider.getProtocolParameters(),
    );
    vi.spyOn(Kupmios.prototype, "getUtxos").mockImplementation((address) =>
      provider.getUtxos(address),
    );
    vi.spyOn(Kupmios.prototype, "getUtxosWithUnit").mockImplementation(
      (address, unit) => provider.getUtxosWithUnit(address, unit),
    );
    vi.spyOn(Kupmios.prototype, "getUtxosByOutRef").mockImplementation(
      (outRefs) => provider.getUtxosByOutRef(outRefs),
    );
    vi.spyOn(Kupmios.prototype, "getUtxoByUnit").mockImplementation((unit) =>
      provider.getUtxoByUnit(unit),
    );
    vi.spyOn(Kupmios.prototype, "getDelegation").mockImplementation((address) =>
      provider.getDelegation(address),
    );
    vi.spyOn(Kupmios.prototype, "getRewardAccount").mockImplementation(
      (address) => provider.getRewardAccount(address),
    );
    vi.spyOn(
      WatcherLocalKupmios.prototype,
      "getRewardAccount",
    ).mockImplementation((address) => provider.getRewardAccount(address));
    vi.spyOn(Kupmios.prototype, "submitTx").mockImplementation((cbor) =>
      provider.submitTx(cbor),
    );
    vi.spyOn(Kupmios.prototype, "awaitTx").mockImplementation((hash) =>
      provider.awaitTx(hash),
    );

    vi.stubEnv("MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY", "17".repeat(32));
    vi.stubEnv("MIDGARD_WATCHER_PROVER_KEY", accounts.publisher.seedPhrase);
    vi.stubEnv("WATCHER_AVAILABILITY_KEY", availabilityAccount.seedPhrase);
    vi.stubEnv("MIDGARD_WATCHER_TRUSTED_HEAD_BEARER", "39".repeat(32));
    const watcherConfig = {
      ...native.watcherConfig,
      storage: {
        ...native.watcherConfig.storage,
        path: join(directory, "watcher.sqlite"),
      },
    };
    const policy = makeWatcherFinalityPolicy(
      watcherConfig,
      configuration.deploymentAuthority.deploymentIdentity,
    );
    if (policy === null)
      throw new Error("Fixture finality policy was not admitted");
    const trusted = await startWatcherTrustedHeadAuthorityChildForTest({
      config: {
        schemaVersion:
          "midgard-watcher-trusted-head-authority-process-config-v1",
        directory: join(directory, "trusted-head"),
        endpoint: "http://127.0.0.1:0",
        policy,
        recordAuthenticationKeySource: {
          kind: "environment",
          variable: "MIDGARD_TEST_RECORD_KEY",
        },
        httpBearerSecretSource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_TRUSTED_HEAD_BEARER",
        },
      },
      unsafeEnvironmentForTest: {
        MIDGARD_TEST_RECORD_KEY: "5c".repeat(32),
        MIDGARD_WATCHER_TRUSTED_HEAD_BEARER: "39".repeat(32),
      },
      unsafeAllowEphemeralPortForTest: true,
    });
    cleanup.push(trusted.close);
    const config = parseWatcherProcessConfig({
      schemaVersion: WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
      watcherConfig,
      watcherRuntimeConfigPath: join(directory, "watcher.json"),
      deploymentAuthorityPath: configuration.authorityPath,
      ruleBundlePath: configuration.ruleBundlePath,
      fundingProfileBundlePath: configuration.fundingProfileBundlePath,
      nativeChainSyncBinaryPath: native.nativeChainSyncBinaryPath,
      trustedHeadAuthorityEndpoint: trusted.server.endpoint,
      operationsEndpoint: `http://127.0.0.1:${operations.port}`,
      httpBearerSecretSource: {
        kind: "environment",
        variable: "MIDGARD_WATCHER_TRUSTED_HEAD_BEARER",
      },
      workflowJournalDirectory: join(directory, "workflows"),
      availability: {
        keySource: {
          kind: "environment",
          variable: "WATCHER_AVAILABILITY_KEY",
        },
        journalPath: join(directory, "availability.sqlite"),
        minimumFundingLovelace: "100000000",
      },
      faultProofInfrastructure: {
        manifestPath: configuration.manifestPath,
        blueprintPath: configuration.blueprintPath,
        deploymentInfoPath: configuration.deploymentInfoPath,
        historicalNativeScriptHistory: {
          sourceMode: "external_provider_quorum",
          consistencyPolicy: "exact_bytes_all_providers_v1",
          providers: [
            {
              sourceId: "history-a",
              operatorIdentitySha256: "a1".repeat(32),
              authorityEndpoint: "https://history-a.example.test",
            },
            {
              sourceId: "history-b",
              operatorIdentitySha256: "b2".repeat(32),
              authorityEndpoint: "https://history-b.example.test",
            },
          ],
        },
      },
    });
    await writeFile(
      config.watcherRuntimeConfigPath,
      JSON.stringify(watcherConfig),
    );
    watcher = await stage(
      "watcher startup",
      () => createWatcherRuntime({ config }),
      300_000,
    );
    let runtimeFailure: unknown;
    void watcher.done.catch((cause: unknown) => {
      runtimeFailure = cause;
    });
    const requireLiveRuntime = () => {
      chain.assertHealthy();
      if (runtimeFailure !== undefined) throw runtimeFailure;
      if (watcher!.status().phase !== "live")
        throw new Error("Watcher stopped while processing the fixture");
    };
    // The running service owns the journal's admitted cache. This separate
    // test observer opens a fresh durable view for each observation.
    const readDecisions = async () =>
      (
        await openWatcherFaultDecisionJournal({
          directory: config.workflowJournalDirectory,
          deploymentFingerprint: deployment.manifest.manifestId,
          launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
        })
      ).readAll();
    const latestDiagnostics = (
      kind: "l1_source" | "verification" | "da_fetch",
    ) => {
      let page = watcher!.operations.api.diagnostics({ kind, limit: 100 });
      let records = page.records.slice(-5);
      while (page.nextCursor !== null) {
        page = watcher!.operations.api.diagnostics({
          kind,
          limit: 100,
          cursor: page.nextCursor,
        });
        records = [...records, ...page.records].slice(-5);
      }
      return records;
    };
    runtimeDiagnostics = async () => ({
      runtime: watcher!.status(),
      coordinator: watcher!.coordinator.status(),
      chainTip: chain.tip(),
      decisions: (await readDecisions()).map(({ decision }) => ({
        headerHash: decision.headerHash,
        decision: decision.decision,
      })),
      operations: watcher!.operations.api.status(),
      metrics: watcher!.operations.api.metrics(),
      l1: latestDiagnostics("l1_source"),
      verification: latestDiagnostics("verification"),
      da: latestDiagnostics("da_fetch"),
    });
    await stage("recorded invalid-block decision", async () => {
      for (;;) {
        const records = await readDecisions();
        const decision = records.find(
          ({ decision }) => decision.headerHash === staged.current.headerHash,
        );
        if (decision !== undefined) {
          expect(decision.decision).toMatchObject({
            decision: "fault_detected",
            category: "transitionTrace",
          });
          expect(
            records.find(
              ({ decision }) =>
                decision.headerHash === staged.predecessor.headerHash,
            )?.decision,
          ).toMatchObject({ decision: "healthy" });
          return;
        }
        requireLiveRuntime();
        await pause(50);
      }
    });
    const workflowDirectory = join(
      config.workflowJournalDirectory,
      "fault-proofs",
      "transitionTrace",
      staged.current.headerHash,
    );
    const workflowJournal = new DirectoryFraudProofWorkflowJournalStore(
      workflowDirectory,
    );
    const readWorkflow = async (): Promise<
      readonly FraudProofWorkflowJournalEntry[]
    > => {
      let entries;
      try {
        entries = await readdir(workflowDirectory, { withFileTypes: true });
      } catch (cause) {
        if (
          typeof cause === "object" &&
          cause !== null &&
          "code" in cause &&
          cause.code === "ENOENT"
        )
          return [];
        throw cause;
      }
      const ids = entries
        .filter(
          (entry) => entry.isDirectory() && /^[0-9a-f]{64}$/u.test(entry.name),
        )
        .map(({ name }) => name);
      expect(ids.length).toBeLessThanOrEqual(1);
      return ids.length === 0 ? [] : await workflowJournal.load(ids[0]!);
    };
    let reportedWorkflowSequence = -1;
    const completion = await stage(
      "confirmed proof and correction",
      async () => {
        for (;;) {
          const entries = await readWorkflow();
          for (const { sequence, event } of entries) {
            if (sequence <= reportedWorkflowSequence) continue;
            if (event.kind === "submitted" || event.kind === "confirmed") {
              console.info(
                `watcher proof: ${event.kind} ${event.actionId.split(":")[0]}`,
              );
            }
            reportedWorkflowSequence = sequence;
          }
          const lastEvent = entries.at(-1)?.event;
          if (lastEvent?.kind === "stalled") {
            throw new Error(`Workflow stalled: ${lastEvent.reason}`);
          }
          const intents = entries.flatMap(({ event }) =>
            event.kind === "submission_intent" ? [event] : [],
          );
          const submitted = entries.flatMap(({ event }) =>
            event.kind === "submitted" ? [event] : [],
          );
          const confirmed = entries.flatMap(({ event }) =>
            event.kind === "confirmed" ? [event] : [],
          );
          for (const event of submitted) {
            expect(
              recorder.rows.filter(({ txHash }) => txHash === event.txHash),
            ).toHaveLength(1);
            expect(recorder.signedCbors.has(event.txHash)).toBe(true);
            expect(
              (await deployment.emulator.getTransactionStatus(event.txHash))
                .status,
            ).toBe("confirmed");
          }
          const terminal = entries
            .filter(({ event }) => event.kind === "completed")
            .at(-1)?.event;
          if (terminal?.kind === "completed") {
            expect(submitted.map(({ txHash }) => txHash)).toEqual(
              intents.map(({ txHash }) => txHash),
            );
            expect(
              confirmed.map(({ actionId, txHash }) => ({ actionId, txHash })),
            ).toEqual(
              intents.map(({ actionId, txHash }) => ({ actionId, txHash })),
            );
            expect(confirmed.length).toBeGreaterThan(2);
            expect(terminal.terminal).toMatchObject({
              category: "transitionTrace",
              headerHash: staged.current.headerHash,
              correction: { fraudulentHeaderAbsent: true },
              proofToken: { retainedAtFinalState: true },
            });
            expect(
              terminal.terminal.observedAt.confirmationDepth,
            ).toBeGreaterThanOrEqual(30);
            return terminal.terminal;
          }
          const next = intents[confirmed.length];
          activeStage =
            next === undefined
              ? "watcher proof construction"
              : `confirmation of ${String(next.actionInput.stage ?? next.actionId)}`;
          if (confirmed.length > 0)
            completedStage = `confirmed proof transaction ${confirmed.length}`;
          requireLiveRuntime();
          await pause(50);
        }
      },
      // The deposit proof folds through multiple on-chain checkpoints. Each
      // transaction must independently reach the normal 30-block finality.
      720_000,
    );
    await stage("corrected on-chain state", async () => {
      const { contracts, emulator } = deployment;
      const targetUnit = toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + staged.current.headerHash,
      );
      expect(
        await emulator.getUtxosWithUnit(
          contracts.stateQueue.spendingScriptAddress,
          targetUnit,
        ),
      ).toHaveLength(0);
      const predecessors = await emulator.getUtxosWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        toUnit(
          contracts.stateQueue.policyId,
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
            staged.predecessor.headerHash,
        ),
      );
      expect(predecessors).toHaveLength(1);
      expect(
        (
          await Effect.runPromise(
            SDK.getLinkedListNodeViewFromUTxO(predecessors[0]!),
          )
        ).next,
      ).toBe("Empty");
      const proofs = await emulator.getUtxosWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        completion.proofToken.unit,
      );
      expect(proofs).toHaveLength(1);
      expect(`${proofs[0]!.txHash}#${proofs[0]!.outputIndex}`).toBe(
        completion.proofToken.outRef,
      );
      expect(
        Object.values(emulator.ledger).filter(
          ({ spent, utxo }) =>
            !spent &&
            Object.keys(utxo.assets).some((unit) =>
              unit.startsWith(contracts.computationThread.policyId),
            ),
        ),
      ).toHaveLength(0);
      expect(
        await emulator.getUtxosWithUnit(
          contracts.activeOperators.spendingScriptAddress,
          toUnit(
            contracts.activeOperators.policyId,
            SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX +
              staged.current.header.operatorVkey,
          ),
        ),
      ).toHaveLength(0);
      const schedulers = await emulator.getUtxosWithUnit(
        contracts.scheduler.spendingScriptAddress,
        toUnit(contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
      );
      expect(schedulers).toHaveLength(1);
      expect(Data.from(schedulers[0]!.datum!, SDK.SchedulerDatum)).toBe(
        "NoActiveOperators",
      );
      const locks = await emulator.getUtxosWithUnit(
        contracts.correctionLock.spendingScriptAddress,
        toUnit(contracts.hubOracle.policyId, SDK.CORRECTION_LOCK_ASSET_NAME),
      );
      expect(locks).toHaveLength(1);
      expect(Data.from(locks[0]!.datum!, SDK.CorrectionLockDatum)).toBe("Idle");
    });
    requireLiveRuntime();
    // The following commitment is supplied by the fixture's second operator;
    // the watcher must observe and classify it through its running service.
    const successor = await stage("new honest commitment", () =>
      chain.withPausedBackgroundGrowth(() =>
        staged.commitHonestSuccessor({
          beforeCommit: (block) => retainedDa.addBlock(block),
        }),
      ),
    );
    await stage(
      "continued processing after correction",
      async () => {
        for (;;) {
          const records = await readDecisions();
          const decision = records.find(
            ({ decision }) => decision.headerHash === successor.headerHash,
          );
          if (decision !== undefined) {
            expect(decision.decision).toMatchObject({ decision: "healthy" });
            expect(
              await deployment.emulator.getUtxosWithUnit(
                deployment.contracts.stateQueue.spendingScriptAddress,
                toUnit(
                  deployment.contracts.stateQueue.policyId,
                  SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + successor.headerHash,
                ),
              ),
            ).toHaveLength(1);
            requireLiveRuntime();
            console.info(`watcher successor: ${successor.headerHash} healthy`);
            return;
          }

          requireLiveRuntime();
          await pause(50);
        }
      },
      // Catch-up authenticates every intervening L1 block, including the
      // confirmation blocks for all proof and replacement-operator actions.
      3_000_000,
    );
    succeeded = true;
  } catch (cause) {
    console.error(`Watcher journey stopped at ${activeStage}`, cause);
    if (runtimeDiagnostics !== undefined) {
      console.error(
        "watcher runtime diagnostics",
        JSON.stringify(await runtimeDiagnostics()),
      );
    }
    if (nativeBlocksPath !== undefined) {
      await copyFile(nativeBlocksPath, join(directory, "native-blocks.json"));
      await writeFile(
        join(directory, "accepted-transactions.json"),
        JSON.stringify([...recorder.signedCbors], null, 2),
      );
      console.error(`Watcher journey failure data retained at ${directory}`);
    }
    throw new Error(
      `Watcher integration failed; last completed stage: ${completedStage}; waiting for: ${activeStage}\n${cause instanceof Error ? (cause.stack ?? cause.message) : String(cause)}`,
      { cause },
    );
  } finally {
    await watcher?.close();
    for (const close of cleanup.reverse()) await close();
    vi.restoreAllMocks();
    transport.provider = undefined;
    vi.unstubAllEnvs();
    recorder.restore();
    if (succeeded) await rm(directory, { recursive: true, force: true });
  }
}, 3_600_000);
