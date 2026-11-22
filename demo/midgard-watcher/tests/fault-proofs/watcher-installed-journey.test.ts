import { existsSync } from "node:fs";
import { copyFile, mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { createServer, type Server, type Socket } from "node:net";
import { dirname, join } from "node:path";
import { performance } from "node:perf_hooks";
import { DatabaseSync } from "node:sqlite";
import { setTimeout as pause } from "node:timers/promises";

import { resolveProverSigner } from "@al-ft/midgard-fault-proofs/runtime";
import { recordCrossBlockRawEmulator } from "@al-ft/midgard-fault-proofs/test-support/cross-block-raw-emulator";
import { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import * as SDKUsers from "@al-ft/midgard-sdk/users";
import {
  CML,
  coreToTxOutput,
  Data,
  generateEmulatorAccount,
  Kupmios,
  type Provider,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { publishWorkflowDeployment } from "midgard-node/tests/helpers/published-workflow-deployment.publish-workflow-deployment";
import { createPublishedWorkflowDeploymentAccounts } from "midgard-node/tests/helpers/published-workflow-deployment.submit-published-initialization";
import { expect, it, vi } from "vitest";

import { readWatcherFaultDecisionEvidence } from "../../src/fault-proofs/fault-decision-journal.create-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { WATCHER_FAULT_PROOF_QUARANTINED } from "../../src/fault-proofs/fault-proof-quarantine.js";
import {
  closeWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
} from "../../src/fault-proofs/watcher-journal-database.js";
import {
  parseWatcherProcessConfig,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
} from "../../src/runtime/process-config.js";
import type { WatcherRuntime } from "../../src/runtime/watcher-runtime.launch-checks.js";
import { createWatcherRuntime } from "../../src/runtime/watcher-runtime.startup-hold.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.canonical-json.js";
import { createEmulatorChainTransport } from "../support/emulator-chain-transport.js";
import { serveEmulatorRetainedDa } from "../support/emulator-retained-da.js";
import { operationsVerifiedHeader } from "../support/operations-verified-header.js";
import { createPublishedWatcherDeploymentAuthority } from "../support/published-deployment-authority.js";
import { stagePublishedDepositTrace } from "../support/published-deposit-trace.stage-published-deposit-trace.js";
import { createTerminalRelease } from "../support/terminal-release.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.create-synthetic-user-event-origin-fixture.js";

/** The fields of an operations response these probes read. */
type ProbeBody = {
  ready?: boolean;
  reasons?: string[];
  error?: string;
  supervisor?: { journalIntegrity: string | null };
};

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
// Real watcher/validators execute against Lucid; only chain transport is simulated.
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
    // A fresh key with no deployment role proves and submits.
    const proverAccount = generateEmulatorAccount({ lovelace: 0n });
    const [prover, availability] = [proverAccount, availabilityAccount].map(
      ({ seedPhrase: walletSeedPhrase }) =>
        resolveProverSigner({ network: "Preprod", walletSeedPhrase }),
    );
    const deployment = await stage("published deployment", () =>
      publishWorkflowDeployment({ accounts }),
    );
    vi.spyOn(Date, "now").mockImplementation(() => deployment.emulator.now());
    await stage("deposit history admission readiness", async () => {
      const history = SDKUsers.eventHistoryDeploymentFromContracts(
        SDKUsers.requireEventHistoryContracts(deployment.contracts).deposit,
      );
      const nodes = SDKUsers.authenticateHistoryNodes(
        await deployment.operatorLucid.utxosAt(history.address),
        history,
      );
      expect(nodes.some(({ key }) => key === null)).toBe(true);
      const protectedUntil = nodes.reduce(
        (latest, { node }) =>
          node.protected_until > latest ? node.protected_until : latest,
        0n,
      );
      // The backdated admission lower bound must respect root protection.
      const readyAt = Number(protectedUntil) + 60_000;
      expect(Number.isSafeInteger(readyAt)).toBe(true);
      await deployment.chain.awaitLedgerTime(readyAt);
      expect(deployment.emulator.now()).toBeGreaterThanOrEqual(readyAt);
    });
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
      const transfer = await deployment.publisherLucid
        .newTx()
        .pay.ToAddress(prover!.address, { lovelace: 500_000_000n })
        .pay.ToAddress(prover!.address, { lovelace: 500_000_000n })
        .pay.ToAddress(prover!.address, { lovelace: 100_000_000n })
        .pay.ToAddress(availability!.address, { lovelace: 40_000_000_000n })
        .pay.ToAddress(availability!.address, { lovelace: 10_000_000n })
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
        // The defaults are the emulator's parameters below.
        nodeProtocolParameters: {},
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
      dirname(native.l1NodeTransportBinaryPath),
      "blocks.json",
    );
    cleanup.push(chain.close);
    // Extra setup blocks age the fraudulent commitment towards merge.
    await chain.grow(native.watcherConfig.l1.finality.depth + 1);
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
    vi.spyOn(Kupmios.prototype, "submitTx").mockImplementation((cbor) =>
      provider.submitTx(cbor),
    );
    vi.spyOn(Kupmios.prototype, "awaitTx").mockImplementation((hash) =>
      provider.awaitTx(hash),
    );
    // The follower provider's node queries and submissions reach the
    // emulator; the synthetic native transport only serves chain-sync.
    vi.spyOn(
      L1FollowerProvider.prototype,
      "getProtocolParameters",
    ).mockImplementation(() => provider.getProtocolParameters());
    vi.spyOn(L1FollowerProvider.prototype, "getUtxos").mockImplementation(
      (address) => provider.getUtxos(address),
    );
    vi.spyOn(
      L1FollowerProvider.prototype,
      "getUtxosWithUnit",
    ).mockImplementation((address, unit) =>
      provider.getUtxosWithUnit(address, unit),
    );
    vi.spyOn(
      L1FollowerProvider.prototype,
      "getUtxosByOutRef",
    ).mockImplementation((outRefs) => provider.getUtxosByOutRef(outRefs));
    vi.spyOn(L1FollowerProvider.prototype, "getUtxoByUnit").mockImplementation(
      (unit) => provider.getUtxoByUnit(unit),
    );
    vi.spyOn(L1FollowerProvider.prototype, "getDelegation").mockImplementation(
      (address) => provider.getDelegation(address),
    );
    vi.spyOn(
      L1FollowerProvider.prototype,
      "getRewardAccount",
    ).mockImplementation((address) => provider.getRewardAccount(address));
    vi.spyOn(L1FollowerProvider.prototype, "submitTx").mockImplementation(
      (cbor) => provider.submitTx(cbor),
    );
    vi.spyOn(L1FollowerProvider.prototype, "awaitTx").mockImplementation(
      (hash) => provider.awaitTx(hash),
    );
    vi.stubEnv("MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY", "17".repeat(32));
    vi.stubEnv("MIDGARD_WATCHER_PROVER_KEY", proverAccount.seedPhrase);
    vi.stubEnv("WATCHER_AVAILABILITY_KEY", availabilityAccount.seedPhrase);
    const watcherConfig = {
      ...native.watcherConfig,
      // The follower starts at the block before the protocol-init block.
      l1: {
        ...native.watcherConfig.l1,
        origin: {
          slot: Number(native.activationBlock.parentPoint.slot),
          blockHash: native.activationBlock.parentPoint.blockHash,
        },
      },
      storage: {
        ...native.watcherConfig.storage,
        path: join(directory, "watcher.sqlite"),
      },
    };
    const config = parseWatcherProcessConfig({
      schemaVersion: WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
      watcherConfig,
      watcherRuntimeConfigPath: join(directory, "watcher.json"),
      deploymentAuthorityPath: configuration.authorityPath,
      ruleBundlePath: configuration.ruleBundlePath,
      fundingProfileBundlePath: configuration.fundingProfileBundlePath,
      l1NodeTransportBinaryPath: native.l1NodeTransportBinaryPath,
      operationsEndpoint: `http://127.0.0.1:${operations.port}`,
      decisionJournalDirectory: join(directory, "workflows"),
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
    // Observe a fresh durable view, independently of the service's cache
    // and its connection: the evidence reader opens the file read-only.
    const readDecisions = async () =>
      existsSync(
        join(config.decisionJournalDirectory, WATCHER_JOURNAL_DATABASE_FILE),
      )
        ? readWatcherFaultDecisionEvidence({
            directory: config.decisionJournalDirectory,
            deploymentFingerprint: deployment.manifest.manifestId,
            launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
          }).map((decision) => ({ decision }))
        : [];
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
    const verifiedHealthy = (headerHash: string) =>
      operationsVerifiedHeader(watcher!.operations.api, headerHash);
    runtimeDiagnostics = async () => ({
      runtime: watcher!.status(),
      decisionDriver: watcher!.decisionDriver.status(),
      follower: watcher!.follower.status(),
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
            records.some(
              ({ decision }) =>
                decision.headerHash === staged.predecessor.headerHash,
            ),
          ).toBe(false);
          expect(verifiedHealthy(staged.predecessor.headerHash)).toBe(true);
          return;
        }
        requireLiveRuntime();
        await pause(50);
      }
    });
    // The target's state-queue node while the fault stands: the removal is
    // the transaction that spends it and continues none.
    const targetUnit = toUnit(
      deployment.contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + staged.current.headerHash,
    );
    expect(
      await deployment.emulator.getUtxosWithUnit(
        deployment.contracts.stateQueue.spendingScriptAddress,
        targetUnit,
      ),
    ).toHaveLength(1);
    /** The prover's accepted submissions, in order: each carries the
     * prover's key witness. */
    const proverTransactions = () =>
      recorder.rows.filter(({ witnessSetCbor }) => {
        const keys =
          CML.TransactionWitnessSet.from_cbor_hex(
            witnessSetCbor,
          ).vkeywitnesses();
        for (let index = 0; index < (keys?.len() ?? 0); index++)
          if (
            keys!.get(index).vkey().hash().to_hex() === prover!.paymentKeyHash
          )
            return true;
        return false;
      });
    let reportedSubmissions = 0;
    const terminalRelease = createTerminalRelease(chain);
    await stage(
      "confirmed proof and correction",
      async () => {
        for (;;) {
          const submissions = proverTransactions();
          for (const { txHash } of submissions.slice(reportedSubmissions))
            console.info(`watcher proof: submitted ${txHash}`);
          reportedSubmissions = submissions.length;
          const quarantined = watcher!.operations.api
            .status()
            .l1Readiness.filter(
              ({ reason }) => reason === WATCHER_FAULT_PROOF_QUARANTINED,
            );
          if (quarantined.length > 0)
            throw new Error(
              `Workflow quarantined: ${quarantined.map(({ detail }) => detail).join("; ")}`,
            );
          // The latest proof step: `confirmed` once the removal is included,
          // `completed` once it is final.
          const steps = watcher!.operations.api.metrics().proofSteps;
          await terminalRelease.observe(steps);
          if (steps.completed !== "0") {
            terminalRelease.assertCompleted();
            return;
          }
          activeStage =
            submissions.length === 0
              ? "watcher proof construction"
              : `confirmation of prover transaction ${submissions.length}`;
          if (submissions.length > 0)
            completedStage = `accepted prover transaction ${submissions.length}`;
          requireLiveRuntime();
          await pause(50);
        }
      },
      // Each proof checkpoint must independently reach finality depth.
      720_000,
    );
    // Once the chain alone completed it, its one sweep-only run spends what
    // is left at our proof-materials address back to our wallet.
    const addressOf = (outputCbor: string) =>
      coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCbor)).address;
    const isSweep = ({ resolvedInputs }: (typeof recorder.rows)[number]) =>
      resolvedInputs.length > 0 &&
      resolvedInputs.every(
        ({ outputCbor }) =>
          addressOf(outputCbor) === prover!.proofMaterialsAddress,
      );
    await stage("proof-materials sweep", async () => {
      for (;;) {
        const [sweep] = proverTransactions().filter(isSweep);
        if (
          sweep !== undefined &&
          (await deployment.emulator.getTransactionStatus(sweep.txHash))
            .status === "confirmed"
        )
          return;
        requireLiveRuntime();
        await pause(50);
      }
    });
    await stage("corrected on-chain state", async () => {
      const { contracts, emulator } = deployment;
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
      // Exactly one proof token stands, created by the prover.
      const proofs = (
        await emulator.getUtxos(contracts.fraudProof.spendingScriptAddress)
      ).filter(({ assets }) =>
        Object.keys(assets).some((unit) =>
          unit.startsWith(contracts.fraudProof.policyId),
        ),
      );
      expect(proofs).toHaveLength(1);
      expect(
        Data.from(proofs[0]!.datum!, SDKFraudProof.FraudProofTokenDatum)
          .fraud_prover,
      ).toBe(prover!.paymentKeyHash);
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
      // Each prover transaction was accepted once and is confirmed; one
      // created the proof token that stands, and a later one removed the
      // target's node citing that token.
      const rows = proverTransactions();
      const submissions = rows.map(({ txHash }) => txHash);
      expect(new Set(submissions).size).toBe(submissions.length);
      expect(submissions.length).toBeGreaterThan(2);
      for (const txHash of submissions)
        expect((await emulator.getTransactionStatus(txHash)).status).toBe(
          "confirmed",
        );
      const carries = (outputCbor: string, unit: string) =>
        (coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCbor)).assets[
          unit
        ] ?? 0n) > 0n;
      const outputsOf = ({ bodyCbor }: (typeof rows)[number]) => {
        const outputs = CML.TransactionBody.from_cbor_hex(bodyCbor).outputs();
        return Array.from({ length: outputs.len() }, (_, index) =>
          outputs.get(index).to_cbor_hex(),
        );
      };
      const proofUnit = Object.keys(proofs[0]!.assets).find((unit) =>
        unit.startsWith(contracts.fraudProof.policyId),
      )!;
      const creation = rows.findIndex((row) =>
        outputsOf(row).some((output) => carries(output, proofUnit)),
      );
      expect(creation).toBeGreaterThanOrEqual(0);
      expect(proofs[0]!.txHash).toBe(submissions[creation]);
      const removals = rows.filter(
        (row) =>
          row.resolvedInputs.some(({ outputCbor }) =>
            carries(outputCbor, targetUnit),
          ) && !outputsOf(row).some((output) => carries(output, targetUnit)),
      );
      expect(removals).toHaveLength(1);
      expect(removals[0]!.txHash).not.toBe(submissions[creation]);
      expect(submissions.indexOf(removals[0]!.txHash)).toBeGreaterThan(
        creation,
      );
      expect(
        removals[0]!.resolvedReferenceInputs.map(({ outRef }) => outRef),
      ).toContain(`${proofs[0]!.txHash}#${proofs[0]!.outputIndex}`);
      // Sixteen in all, the last the proof-materials sweep after the
      // removal: one plain output to our wallet.
      expect(rows).toHaveLength(16);
      expect(rows.filter(isSweep).map(({ txHash }) => txHash)).toEqual([
        submissions.at(-1),
      ]);
      expect(submissions.indexOf(removals[0]!.txHash)).toBe(14);
      const swept = outputsOf(rows.at(-1)!).map((output) =>
        coreToTxOutput(CML.TransactionOutput.from_cbor_hex(output)),
      );
      expect(swept).toHaveLength(1);
      expect(swept[0]!.address).toBe(prover!.address);
      expect(Object.keys(swept[0]!.assets)).toEqual(["lovelace"]);
    });
    requireLiveRuntime();
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
          if (verifiedHealthy(successor.headerHash)) {
            expect(
              (await readDecisions()).some(
                ({ decision }) => decision.headerHash === successor.headerHash,
              ),
            ).toBe(false);
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
      // Batched catch-up over the 2,161-block release horizon; fails loudly.
      600_000,
    );
    const evidenceDirectory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
    if (evidenceDirectory !== undefined) {
      await mkdir(evidenceDirectory, { recursive: true });
      await writeFile(
        join(evidenceDirectory, "watcher-installed-history-journey.json"),
        JSON.stringify(
          {
            scope:
              "Applied emulator deployment and real watcher service; synthetic native transport, not live Cardano acceptance",
            manifest: deployment.manifest,
            protocolParameters:
              deployment.operatorLucid.config().protocolParameters,
            honestSuccessorHeaderHash: successor.headerHash,
            decisions: await readDecisions(),
            proverTransactions: proverTransactions().map(
              ({ txHash }) => txHash,
            ),
            acceptedTransactions: [...recorder.signedCbors].map(
              ([txHash, signedCbor]) => ({ txHash, signedCbor }),
            ),
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
      await copyFile(
        nativeBlocksPath!,
        join(evidenceDirectory, "watcher-installed-history-native-blocks.json"),
      );
    }
    // W2-E2: a restart over journals that fail integrity binds the operations
    // server and holds the watcher live and unready; it never fails startup.
    await watcher.close();
    watcher = undefined;
    // The process's journal connection outlives the runtime; a restart is a
    // new process, so close it as an exit would.
    closeWatcherJournalDatabase(config.decisionJournalDirectory);
    // A decision row whose bytes no longer match its MAC, as a bit the disk
    // flipped and SQLite does not check would leave. Completion pruning has
    // emptied the journal tables by now, so the row is written in; deleting
    // it is the operator's repair.
    const corruptJournalRow = (corrupt: boolean) => {
      const raw = new DatabaseSync(
        join(config.decisionJournalDirectory, WATCHER_JOURNAL_DATABASE_FILE),
      );
      try {
        const { changes } = corrupt
          ? raw
              .prepare(
                "INSERT INTO watcher_fault_decisions (row_key, scope, state, revision, body, mac) VALUES ('corrupt', 'corrupt', 'fault_detected', 1, '{}', ?)",
              )
              .run("00".repeat(32))
          : raw
              .prepare(
                "DELETE FROM watcher_fault_decisions WHERE row_key = 'corrupt'",
              )
              .run();
        expect(Number(changes)).toBe(1);
      } finally {
        raw.close();
      }
    };
    corruptJournalRow(true);
    let restartFailure: unknown;
    const restarting = createWatcherRuntime({ config });
    void restarting.catch((cause: unknown) => {
      restartFailure = cause;
    });
    const probe = async (path: string) => {
      const response = await fetch(`${config.operationsEndpoint}${path}`);
      return {
        status: response.status,
        body: (await response.json()) as ProbeBody,
      };
    };
    await stage(
      "restart held unready over a refused journal",
      async () => {
        for (;;) {
          if (restartFailure !== undefined) throw restartFailure;
          const readyz = await probe("/readyz").catch(() => undefined);
          if (readyz?.body.reasons?.includes("journal_integrity") === true) {
            expect(readyz.status).toBe(503);
            break;
          }
          await pause(100);
        }
        const live = await probe("/v1/status");
        expect(live.status).toBe(200);
        expect(live.body.supervisor?.journalIntegrity).toContain("MAC differs");
        expect(await probe("/v1/metrics")).toEqual({
          status: 503,
          body: { error: "journal_integrity" },
        });
      },
      300_000,
    );
    // Repair the row and forget the refusal, as the restart that clears it
    // would; the held startup then completes on the repaired journals.
    corruptJournalRow(false);
    closeWatcherJournalDatabase(config.decisionJournalDirectory);
    watcher = await stage("startup after journal repair", () => restarting);
    expect(watcher.faultProofSupervisor.status().journalIntegrity).toBeNull();
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
