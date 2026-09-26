#!/usr/bin/env node
import { runDaZstdStartupSelfTest } from "@al-ft/midgard-core/da-compression";
import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { loadRuntimeConfig } from "@al-ft/midgard-core/runtime-config";

import { createCommitteeApiServer } from "./api/server.js";
import { availabilityResponderFromConfig } from "./availability/factory.js";
import {
  type CommitteeRetentionReadinessSnapshot,
  CommitteeService,
} from "./committee-service.js";
import { loadCommitteeConfig } from "./config.js";
import {
  l1SubmitterWalletPreflightFromConfig,
  onChainCoordinatorFromConfig,
} from "./coordinator/factory.js";
import { SubmitterReconciler } from "./coordinator/submitter-reconciler.js";
import {
  createDaLibp2pAttestationRequestHandlers,
  createDaLibp2pPayloadRequestHandlers,
  createDaLibp2pProofRequestHandlers,
  DaLibp2pAttestationExchange,
  DaLibp2pNode,
  DaLibp2pPayloadSource,
  DaPeerRegistry,
  StoreBackedDaAttestationProtocol,
} from "./da/libp2p/index.js";
import { daAttestationReaderFromConfig } from "./l1/da-attestation-reader.js";
import { providerFromConfig } from "./l1/provider.js";
import { l1SubmitterPreflightResultToJson } from "./l1/submitter.js";
import { PeerSignatureCoordinator } from "./peer/coordinator.js";
import { PeerSignaturePoller } from "./peer/poller.js";
import { resolveRemoteDaAttestationTargets } from "./peer/targets.js";
import {
  loadDaSigner,
  validateDaCommittee,
  validateDaSignerMembership,
} from "./signer.js";
import { openCommitteeStore } from "./store/factory.js";
import { type RetentionL1View, runRetentionCycle } from "./store/retention.js";
import { createCommitteeTickRunner } from "./tick-runner.js";

const main = async (): Promise<void> => {
  loadRuntimeConfig();
  if (process.argv[2] === "l1-wallet-preflight") {
    await runL1WalletPreflightCommand(process.argv.slice(3));
    return;
  }
  if (process.argv.includes("--help")) {
    printHelp();
    return;
  }
  // Decoder-first rollout means every committee node must be capable of
  // safely decoding zstd envelopes before any producer is flipped.
  await runDaZstdStartupSelfTest();
  const config = await loadCommitteeConfig();
  const startedAtMs = Date.now();
  const store = await openCommitteeStore(config.localState);
  const signer =
    config.signerKeySource === undefined
      ? undefined
      : await loadDaSigner(config.signerKeySource);
  const committeeValidation = validateDaCommittee({
    daParams: config.daParams,
  });
  const signerValidation =
    signer === undefined || config.signerIndex === undefined
      ? undefined
      : validateDaSignerMembership({
          daParams: config.daParams,
          signer,
          signerIndex: config.signerIndex,
        });
  const provider = await providerFromConfig(config);
  const daChainReader = await daAttestationReaderFromConfig(config);
  if (config.daTransport.kind !== "libp2p") {
    throw new Error("da-committee-node requires libp2p DA transport mode");
  }
  if (config.libp2pPrivateKeySource === undefined) {
    throw new Error(
      "da-committee-node libp2p mode requires DA_LIBP2P_PRIVATE_KEY_SOURCE",
    );
  }
  const daIdentity = await loadDaLibp2pIdentity(config.libp2pPrivateKeySource);
  const daPeerRegistry = DaPeerRegistry.fromConfig(config.daTransport);
  daPeerRegistry.requireKnownPeer(daIdentity.peerId);
  const daAttestationProtocol = new StoreBackedDaAttestationProtocol({
    deploymentFingerprint: config.deploymentFingerprint,
    localPeerId: daIdentity.peerId,
    committeeValidation,
    availabilityCommitmentAuthority: {
      deploymentIdentity: config.midgardNodeDeployment.hubOraclePolicyId,
      bondOwnerCredential: config.availabilityChallenge.bondOwnerCredential,
      responseGeometry: config.availabilityChallenge.responseGeometry,
    },
    store,
  });
  const requestHandlers = new Map([
    ...createDaLibp2pPayloadRequestHandlers({
      deploymentFingerprint: config.deploymentFingerprint,
      store,
      limits: config.daTransport.limits,
    }),
    ...createDaLibp2pProofRequestHandlers({
      deploymentFingerprint: config.deploymentFingerprint,
      store,
      limits: config.daTransport.limits,
      accessPolicy: {
        kind: "manifest_roles",
        registry: daPeerRegistry,
      },
    }),
    ...createDaLibp2pAttestationRequestHandlers({
      deploymentFingerprint: config.deploymentFingerprint,
      protocol: daAttestationProtocol,
      limits: config.daTransport.limits,
    }),
  ]);
  const daLibp2pNode = new DaLibp2pNode({
    config: config.daTransport,
    registry: daPeerRegistry,
    privateKeySource: config.libp2pPrivateKeySource,
    requestHandlers,
    onGossipMessageError: (error) => {
      process.stderr.write(
        `rejected DA conflict evidence gossip: ${
          error instanceof Error ? error.message : String(error)
        }\n`,
      );
    },
  });
  const payloadSource = new DaLibp2pPayloadSource({
    deploymentFingerprint: config.deploymentFingerprint,
    node: daLibp2pNode,
    registry: daPeerRegistry,
    limits: config.daTransport.limits,
  });
  const daAttestationTargets = resolveRemoteDaAttestationTargets({
    peers: config.daTransport.peers,
    localPeerId: daIdentity.peerId,
    signerIndex: config.signerIndex,
  });
  const daAttestationPeers = daAttestationTargets.remotePeers;
  const attestationExchange = new DaLibp2pAttestationExchange({
    deploymentFingerprint: config.deploymentFingerprint,
    localPeerId: daIdentity.peerId,
    node: daLibp2pNode,
    registry: daPeerRegistry,
    protocol: daAttestationProtocol,
    committeeValidation,
    store,
    requestTimeoutMs: config.peerRequestTimeoutMs,
  });
  const onChainCoordinator = config.l1SubmissionEnabled
    ? await onChainCoordinatorFromConfig(config, daChainReader, store)
    : undefined;
  const peerPoller =
    onChainCoordinator !== undefined && daAttestationPeers.length > 0
      ? new PeerSignaturePoller({
          deploymentFingerprint: config.deploymentFingerprint,
          peers: daAttestationPeers,
          localPeerId: daIdentity.peerId,
          attestationExchange,
          signerValidation: committeeValidation,
          availabilityCommitmentAuthority: {
            deploymentIdentity: config.midgardNodeDeployment.hubOraclePolicyId,
            bondOwnerCredential:
              config.availabilityChallenge.bondOwnerCredential,
            responseGeometry: config.availabilityChallenge.responseGeometry,
          },
          store,
          requestTimeoutMs: config.peerRequestTimeoutMs,
        })
      : undefined;
  const submitterReconciler =
    onChainCoordinator === undefined
      ? undefined
      : new SubmitterReconciler({
          deploymentFingerprint: config.deploymentFingerprint,
          committeeValidation,
          store,
          coordinator: onChainCoordinator,
          peerPoller,
          availabilityCommitmentAuthority: {
            deploymentIdentity: config.midgardNodeDeployment.hubOraclePolicyId,
            bondOwnerCredential:
              config.availabilityChallenge.bondOwnerCredential,
            responseGeometry: config.availabilityChallenge.responseGeometry,
          },
          submitterId: config.l1SubmitterId,
        });
  const l1SubmitterPreflight = config.l1SubmissionEnabled
    ? await l1SubmitterWalletPreflightFromConfig(config)
        .then((result) => ({
          status: result.status,
          detail: l1SubmitterPreflightResultToJson(result),
        }))
        .catch((error: unknown) => ({
          status: "failed" as const,
          error: error instanceof Error ? error.message : String(error),
        }))
    : { status: "not_required" as const };
  const coordinator =
    signer !== undefined &&
    signerValidation !== undefined &&
    config.signerIndex !== undefined &&
    (daAttestationPeers.length > 0 || onChainCoordinator !== undefined)
      ? new PeerSignatureCoordinator({
          deploymentFingerprint: config.deploymentFingerprint,
          peers: daAttestationPeers,
          localPeerId: daIdentity.peerId,
          signer,
          signerIndex: config.signerIndex,
          signerValidation,
          availabilityCommitmentAuthority: {
            deploymentIdentity: config.midgardNodeDeployment.hubOraclePolicyId,
            bondOwnerCredential:
              config.availabilityChallenge.bondOwnerCredential,
            responseGeometry: config.availabilityChallenge.responseGeometry,
          },
          store,
          attestationExchange,
          requestTimeoutMs: config.peerRequestTimeoutMs,
          retryInitialDelayMs: config.peerRetryInitialDelayMs,
          retryMaxDelayMs: config.peerRetryMaxDelayMs,
          retryMaxAttempts: config.peerRetryMaxAttempts,
          onChainCoordinator,
        })
      : undefined;
  const service = new CommitteeService({
    config,
    store,
    stateQueueProvider: provider,
    payloadSource,
    signer,
    signerValidation,
    coordinator,
    submitterReconciler,
    daChainReader,
    daLibp2pNode,
    daPeerRegistry,
  });
  await service.initialize();
  const availabilityRuntime = config.l1SubmissionEnabled
    ? await availabilityResponderFromConfig(config, store, provider)
    : undefined;
  await daLibp2pNode.start();

  const runAvailabilityResponse = async (): Promise<void> => {
    if (availabilityRuntime === undefined) return;
    const report = await availabilityRuntime.responder.tick();
    if (report.status !== "idle") {
      const stream =
        report.status === "failed" || report.status === "unavailable"
          ? process.stderr
          : process.stdout;
      stream.write(
        `${JSON.stringify({ event: "availability_responder", ...report })}\n`,
      );
    }
  };

  let retentionReadiness: CommitteeRetentionReadinessSnapshot = {
    status: "not_checked",
    scanned: 0,
    retained: 0,
    prunable: 0,
    alerting: 0,
  };
  const runRetention = async (view: RetentionL1View): Promise<void> => {
    // The exemption sets come from the L1 view the poller accepted this tick.
    const options = {
      nowMs: Date.now(),
      retentionDays: config.daTransport.retentionDays,
      deploymentFingerprint: config.deploymentFingerprint,
      minimumFinalityDepth: config.finalityDepth,
      confirmedHeadHash: view.confirmedHeadHash,
      liveQueueHeaderHashes: view.liveQueueHeaderHashes,
    };
    try {
      const { deadlines, prune } = await runRetentionCycle(store, options);
      retentionReadiness = {
        status: deadlines.alerting > 0 ? "alerting" : "ok",
        checkedAt: new Date(options.nowMs).toISOString(),
        scanned: deadlines.scanned,
        retained: deadlines.retained,
        prunable: deadlines.prunable,
        alerting: deadlines.alerting,
      };
      if (prune.prunedHeaderHashes.length > 0) {
        process.stdout.write(
          `${JSON.stringify({ event: "da_retention_pruned", ...prune })}\n`,
        );
      }
      if (deadlines.alerting > 0) {
        process.stderr.write(
          `${JSON.stringify({ event: "da_retention_deadline_alert", report: deadlines })}\n`,
        );
      }
    } catch (error) {
      retentionReadiness = {
        status: "failed",
        checkedAt: new Date(options.nowMs).toISOString(),
        scanned: 0,
        retained: 0,
        prunable: 0,
        alerting: 0,
        error: error instanceof Error ? error.message : String(error),
      };
      throw error;
    }
  };

  // Assigned after the tick runner is built: its fatal exit calls shutdown,
  // which must close whichever of these exist by then.
  // eslint-disable-next-line prefer-const
  let interval: ReturnType<typeof setInterval> | undefined;
  // eslint-disable-next-line prefer-const
  let api: ReturnType<typeof createCommitteeApiServer> | undefined;
  const shutdown = async (): Promise<void> => {
    clearInterval(interval);
    await api?.close();
    await daLibp2pNode.stop();
    availabilityRuntime?.close();
    await store.close?.();
  };
  const tickRunner = createCommitteeTickRunner({
    tick: () => service.tick(),
    runAvailabilityResponse,
    runRetention,
    latestL1View: () => service.latestL1View(),
    latestL1ProgressAtMs: () => service.latestL1ProgressAtMs(),
    setRetentionReadiness: (snapshot) => {
      retentionReadiness = snapshot;
    },
    l1ViewFatalMs: config.l1ViewFatalMs,
    startedAtMs,
    nowMs: () => Date.now(),
    write: (stream, line) => process[stream].write(line),
    shutdown,
    exit: (code) => process.exit(code),
  });

  if (process.argv.includes("--once")) {
    try {
      const tickStartedAtMs = Date.now();
      const result = await service.tick();
      await runAvailabilityResponse();
      await tickRunner.runRetentionStep(tickStartedAtMs);
      process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
    } finally {
      await daLibp2pNode.stop();
      availabilityRuntime?.close();
      await store.close?.();
    }
    return;
  }

  api = createCommitteeApiServer({
    deploymentFingerprint: config.deploymentFingerprint,
    signerIndex: config.signerIndex,
    signerValidation: committeeValidation,
    store,
    readiness: () =>
      service.readinessSnapshot({
        localPeerId: daIdentity.peerId,
        l1SubmitterPreflight,
        retention: retentionReadiness,
      }),
    manifest: config.deploymentManifest,
    peerReplayWindowMs: config.peerReplayWindowMs,
    peerMaxBodyBytes: config.peerMaxBodyBytes,
    peerRateLimitWindowMs: config.peerRateLimitWindowMs,
    peerRateLimitMaxRequests: config.peerRateLimitMaxRequests,
  });
  await api.listen(config.apiPort, config.apiHost);
  process.stdout.write(
    `da-committee-node listening on http://${config.apiHost}:${config.apiPort.toString()}\n`,
  );

  await tickRunner.runTick();
  // runTick contains its own error boundary and overlap guard.
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  interval = setInterval(tickRunner.runTick, config.pollIntervalMs);
  process.once("SIGINT", () => {
    void shutdown().then(() => process.exit(0));
  });
  process.once("SIGTERM", () => {
    void shutdown().then(() => process.exit(0));
  });
};

const runL1WalletPreflightCommand = async (
  args: readonly string[],
): Promise<void> => {
  if (args.includes("--help")) {
    printL1WalletPreflightHelp();
    return;
  }
  const unknownArgs = args.filter((arg) => arg !== "--json");
  if (unknownArgs.length > 0) {
    throw new Error(
      `unknown l1-wallet-preflight arguments: ${unknownArgs.join(", ")}`,
    );
  }
  const config = await loadCommitteeConfig();
  const result = await l1SubmitterWalletPreflightFromConfig(config);
  process.stdout.write(
    `${JSON.stringify(l1SubmitterPreflightResultToJson(result), null, 2)}\n`,
  );
  if (result.status === "failed") {
    process.exitCode = 1;
  }
};

const printHelp = (): void => {
  process.stdout.write(`da-committee-node

Usage:
  da-committee-node --once                       scan once, verify finalized unattested headers, sign
  da-committee-node l1-wallet-preflight --json   print L1 submitter wallet readiness
  da-committee-node                              run API and polling loop

Required configuration follows demo/da-committee-node/docs/da-committee-node-architecture.md in the repository.
L1 submission requires L1_SUBMITTER_KEY_SOURCE for a funded Cardano wallet.
Supported CARDANO_PROVIDER_URLS forms (Blockfrost cannot serve the state
queue: it has no authenticated ordered history source):
  kupmios:http://kupo:1442|http://ogmios:1337
  fixture:/path/to/state-queue.json (tests only; requires
    CARDANO_L1_TEST_MODE=true)

L1 source modes:
  CARDANO_L1_SOURCE_MODE=local_node
    requires CARDANO_LOCAL_NODE_AUTHORITY_ID and
    CARDANO_LOCAL_NODE_CHAIN_SYNC_URL=chain-sync:<provider> and
    CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH=/durable/path/cursor.jsonl;
    CARDANO_PROVIDER_URLS are aligned query surfaces for that node and are not
    counted as independent providers.
  CARDANO_L1_SOURCE_MODE=external_providers
    requires at least two CARDANO_PROVIDER_URLS and one distinct operational
    identity per URL in CARDANO_EXTERNAL_PROVIDER_IDENTITIES.
`);
};

const printL1WalletPreflightHelp = (): void => {
  process.stdout.write(`da-committee-node l1-wallet-preflight --json

Prints DA L1 submitter wallet readiness as JSON using the normal environment
configuration. Exits non-zero when readiness fails.
`);
};

main().catch((error) => {
  process.stderr.write(
    `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
  );
  process.exit(1);
});
