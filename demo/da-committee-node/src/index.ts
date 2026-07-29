#!/usr/bin/env node
import { runDaZstdStartupSelfTest } from "@al-ft/midgard-core/da-compression";

import { createWatcherApiServer } from "./api/server.js";
import { loadWatcherConfig } from "./config.js";
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
  loadDaLibp2pIdentity,
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
import { openWatcherStore } from "./store/factory.js";
import { WatcherService } from "./watcher.js";

const main = async (): Promise<void> => {
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
  const config = await loadWatcherConfig();
  const store = await openWatcherStore(config.localState);
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
    throw new Error("midgard-watcher requires libp2p DA transport mode");
  }
  if (config.libp2pPrivateKeySource === undefined) {
    throw new Error(
      "midgard-watcher libp2p mode requires DA_LIBP2P_PRIVATE_KEY_SOURCE",
    );
  }
  const daIdentity = await loadDaLibp2pIdentity(config.libp2pPrivateKeySource);
  const daPeerRegistry = DaPeerRegistry.fromConfig(config.daTransport);
  daPeerRegistry.requireKnownPeer(daIdentity.peerId);
  const daAttestationProtocol = new StoreBackedDaAttestationProtocol({
    deploymentFingerprint: config.deploymentFingerprint,
    localPeerId: daIdentity.peerId,
    committeeValidation,
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
      registry: daPeerRegistry,
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
          daAttestationPolicyId: config.daAttestationPolicyId,
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
          store,
          attestationExchange,
          requestTimeoutMs: config.peerRequestTimeoutMs,
          retryInitialDelayMs: config.peerRetryInitialDelayMs,
          retryMaxDelayMs: config.peerRetryMaxDelayMs,
          retryMaxAttempts: config.peerRetryMaxAttempts,
          onChainCoordinator,
        })
      : undefined;
  const service = new WatcherService({
    config,
    store,
    stateQueueProvider: provider,
    payloadSource,
    signer,
    signerValidation,
    coordinator,
    submitterReconciler,
    daChainReader,
  });
  await service.initialize();
  await daLibp2pNode.start();

  if (process.argv.includes("--once")) {
    try {
      const result = await service.tick();
      process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
    } finally {
      await daLibp2pNode.stop();
      await store.close?.();
    }
    return;
  }

  const api = createWatcherApiServer({
    deploymentFingerprint: config.deploymentFingerprint,
    signerIndex: config.signerIndex,
    signerValidation: committeeValidation,
    store,
    readiness: () =>
      service.readinessSnapshot({
        localPeerId: daIdentity.peerId,
        l1SubmitterPreflight,
      }),
    manifest: config.deploymentManifest,
    peerReplayWindowMs: config.peerReplayWindowMs,
    peerMaxBodyBytes: config.peerMaxBodyBytes,
    peerRateLimitWindowMs: config.peerRateLimitWindowMs,
    peerRateLimitMaxRequests: config.peerRateLimitMaxRequests,
  });
  await api.listen(config.apiPort, config.apiHost);
  process.stdout.write(
    `midgard-watcher listening on http://${config.apiHost}:${config.apiPort.toString()}\n`,
  );

  const runTick = async (): Promise<void> => {
    try {
      const result = await service.tick();
      if (result.errors.length > 0) {
        process.stderr.write(`${JSON.stringify(result)}\n`);
      }
    } catch (error) {
      process.stderr.write(
        `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
      );
    }
  };
  await runTick();
  const interval = setInterval(runTick, config.pollIntervalMs);
  const shutdown = async (): Promise<void> => {
    clearInterval(interval);
    await api.close();
    await daLibp2pNode.stop();
    await store.close?.();
  };
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
  const config = await loadWatcherConfig();
  const result = await l1SubmitterWalletPreflightFromConfig(config);
  process.stdout.write(
    `${JSON.stringify(l1SubmitterPreflightResultToJson(result), null, 2)}\n`,
  );
  if (result.status === "failed") {
    process.exitCode = 1;
  }
};

const printHelp = (): void => {
  process.stdout.write(`midgard-watcher

Usage:
  midgard-watcher --once                       scan once, verify finalized unattested headers, sign
  midgard-watcher l1-wallet-preflight --json   print L1 submitter wallet readiness
  midgard-watcher                              run API and polling loop

Required configuration follows docs/da-payload-attestation-watcher-plan.md.
L1 submission requires L1_SUBMITTER_KEY_SOURCE for a funded Cardano wallet.
Supported CARDANO_PROVIDER_URLS forms:
  blockfrost:https://cardano-preview.blockfrost.io/api/v0#PROJECT_ID
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
  process.stdout.write(`midgard-watcher l1-wallet-preflight --json

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
