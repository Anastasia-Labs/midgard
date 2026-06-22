#!/usr/bin/env node
import { createWatcherApiServer } from "./api/server.js";
import { loadWatcherConfig } from "./config.js";
import {
  l1SubmitterWalletPreflightFromConfig,
  onChainCoordinatorFromConfig,
} from "./coordinator/factory.js";
import { SubmitterReconciler } from "./coordinator/submitter-reconciler.js";
import { DaPayloadClient } from "./da/client.js";
import { daAttestationReaderFromConfig } from "./l1/da-attestation-reader.js";
import { providerFromConfig } from "./l1/provider.js";
import { l1SubmitterPreflightResultToJson } from "./l1/submitter.js";
import { PeerSignatureCoordinator } from "./peer/coordinator.js";
import { PeerSignaturePoller } from "./peer/poller.js";
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
  const payloadClient = new DaPayloadClient({
    endpoints: config.daPayloadEndpoints,
  });
  const onChainCoordinator = config.l1SubmissionEnabled
    ? await onChainCoordinatorFromConfig(config, daChainReader, store)
    : undefined;
  const peerPoller =
    onChainCoordinator !== undefined && config.peerEndpoints.length > 0
      ? new PeerSignaturePoller({
          deploymentFingerprint: config.deploymentFingerprint,
          peers: config.peerEndpoints,
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
          submitterId: config.l1SubmitterId,
        });
  const coordinator =
    signer !== undefined &&
    signerValidation !== undefined &&
    config.signerIndex !== undefined &&
    config.peerEndpoints.length > 0
      ? new PeerSignatureCoordinator({
          deploymentFingerprint: config.deploymentFingerprint,
          peers: config.peerEndpoints,
          signer,
          signerIndex: config.signerIndex,
          signerValidation,
          store,
          requestTimeoutMs: config.peerRequestTimeoutMs,
          retryInitialDelayMs: config.peerRetryInitialDelayMs,
          retryMaxDelayMs: config.peerRetryMaxDelayMs,
          retryMaxAttempts: config.peerRetryMaxAttempts,
        })
      : undefined;
  const service = new WatcherService({
    config,
    store,
    stateQueueProvider: provider,
    payloadClient,
    signer,
    signerValidation,
    coordinator,
    submitterReconciler,
    daChainReader,
  });
  await service.initialize();

  if (process.argv.includes("--once")) {
    const result = await service.tick();
    process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
    await store.close?.();
    return;
  }

  let ready = true;
  const api = createWatcherApiServer({
    deploymentFingerprint: config.deploymentFingerprint,
    signerIndex: config.signerIndex,
    signerValidation: committeeValidation,
    store,
    ready: () => ready,
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
      ready = true;
      const result = await service.tick();
      if (result.errors.length > 0) {
        process.stderr.write(`${JSON.stringify(result)}\n`);
      }
    } catch (error) {
      ready = false;
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
  fixture:/path/to/state-queue.json
  blockfrost:https://cardano-preview.blockfrost.io/api/v0#PROJECT_ID
  kupmios:http://kupo:1442|http://ogmios:1337
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
