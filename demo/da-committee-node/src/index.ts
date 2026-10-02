#!/usr/bin/env node
import { runDaZstdStartupSelfTest } from "@al-ft/midgard-core/da-compression";
import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { loadRuntimeConfig } from "@al-ft/midgard-core/runtime-config";

import { createCommitteeApiServer } from "./api/server.js";
import { createAvailabilityResponseLoop } from "./availability-response-loop.js";
import {
  type CommitteeRetentionReadinessSnapshot,
  retentionReadinessFromDeadlines,
} from "./committee-service.js";
import { loadCommitteeConfig, type LoadedCommitteeConfig } from "./config.js";
import { l1SubmitterWalletPreflightFromConfig } from "./coordinator/factory.js";
import { DaPeerRegistry } from "./da/libp2p/index.js";
import { l1SubmitterPreflightResultToJson } from "./l1/submitter.js";
import { createL1SubmitterPreflightMonitor } from "./l1-submitter-preflight-monitor.js";
import {
  type CommitteeNodeLocalSetup,
  openCommitteeNodeRuntime,
} from "./node-runtime.js";
import {
  loadDaSigner,
  validateDaCommittee,
  validateDaSignerMembership,
} from "./signer.js";
import { listenStartingServer, retryStartup } from "./startup.js";
import type { PostgresStoreInstanceLockEvents } from "./store/postgres.instance-lock.js";
import {
  retentionCycleOptions,
  type RetentionL1View,
  runRetentionCycle,
} from "./store/retention.js";
import {
  createCommitteeTickRunner,
  l1ViewStaleMs,
  startCommitteeTickLoop,
} from "./tick-runner.js";

/** Upper bound on the shutdown before exiting for a lost store instance lock. */
const STORE_LOCK_LOST_SHUTDOWN_GRACE_MS = 10_000;

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
  const local = await loadLocalSetup(config);
  const once = process.argv.includes("--once");
  const write = (line: string): void => {
    process.stderr.write(line);
  };

  // Bound once shutdown exists; until then a lost lock exits at once.
  // eslint-disable-next-line prefer-const
  let exitForLostStoreInstanceLock: (() => void) | undefined;
  let storeLockSuspended = false;
  const storeLockEvents: PostgresStoreInstanceLockEvents = {
    // Postgres went away: refuse work until the lock is held again.
    onInstanceLockSuspended: (error) => {
      storeLockSuspended = true;
      write(
        `${JSON.stringify({ event: "committee_store_instance_lock_suspended", error: error.message })}\n`,
      );
    },
    onInstanceLockRestored: () => {
      storeLockSuspended = false;
      write(
        `${JSON.stringify({ event: "committee_store_instance_lock_restored" })}\n`,
      );
    },
    // Another live process holds the lock now, so this one stops running
    // decision effects and exits for its supervisor to restart it.
    onInstanceLockLost: (error) => {
      write(
        `${JSON.stringify({ event: "committee_store_instance_lock_lost", error: error.message })}\n`,
      );
      if (exitForLostStoreInstanceLock === undefined) process.exit(1);
      exitForLostStoreInstanceLock();
    },
    onInstanceLockTakeover: (reason) => {
      write(
        `${JSON.stringify({ event: "committee_store_instance_lock_taken_over", reason })}\n`,
      );
    },
  };

  // Dependencies that are not up yet are waited for, not exited on.
  const starting = once
    ? undefined
    : await listenStartingServer(config.apiPort, config.apiHost);
  const runtime = await retryStartup({
    attempt: () => openCommitteeNodeRuntime(local, storeLockEvents),
    onFailure: (reason) => starting?.setReason(reason),
    write,
    ...(once ? { isFatal: () => true } : {}),
  }).catch(async (error: unknown) => {
    await starting?.close();
    throw error;
  });
  const { store, service, availabilityRuntime } = runtime;

  const responseLoop = createAvailabilityResponseLoop({
    drain: () =>
      availabilityRuntime === undefined
        ? Promise.resolve({ challenges: 0, status: "idle" as const })
        : availabilityRuntime.responder.drain(),
    write: (stream, line) => process[stream].write(line),
    pollIntervalMs: config.pollIntervalMs,
  });
  const preflight = config.l1SubmissionEnabled
    ? createL1SubmitterPreflightMonitor({
        evaluate: ({ autoFund }) =>
          l1SubmitterWalletPreflightFromConfig(
            autoFund ? config : withoutAutoFund(config),
          ),
        write,
      })
    : undefined;

  let retentionReadiness: CommitteeRetentionReadinessSnapshot = {
    status: "not_checked",
    scanned: 0,
    retained: 0,
    prunable: 0,
    alerting: 0,
  };
  const runRetention = async (view: RetentionL1View): Promise<void> => {
    // The exemption sets come from the L1 view the poller accepted this tick.
    const options = retentionCycleOptions(config, view, Date.now());
    try {
      const { deadlines, prune } = await runRetentionCycle(store, options);
      retentionReadiness = retentionReadinessFromDeadlines(deadlines);
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

  // Assigned after the tick runner is built; shutdown closes whichever of
  // these exist by then.
  // eslint-disable-next-line prefer-const
  let interval: ReturnType<typeof setInterval> | undefined;
  // eslint-disable-next-line prefer-const
  let api: ReturnType<typeof createCommitteeApiServer> | undefined;
  const shutdown = async (): Promise<void> => {
    clearInterval(interval);
    responseLoop.stop();
    preflight?.stop();
    await api?.close();
    await runtime.close();
  };
  exitForLostStoreInstanceLock = () => {
    let grace: ReturnType<typeof setTimeout> | undefined;
    void Promise.race([
      shutdown(),
      new Promise<void>((resolve) => {
        grace = setTimeout(resolve, STORE_LOCK_LOST_SHUTDOWN_GRACE_MS);
      }),
    ])
      .catch(() => undefined)
      .finally(() => {
        clearTimeout(grace);
        process.exit(1);
      });
  };
  const tickRunner = createCommitteeTickRunner({
    // No decision is attempted while the store's instance lock is being
    // taken again; the tick reports why and the next one retries.
    tick: () =>
      storeLockSuspended
        ? Promise.resolve(STORE_LOCK_REACQUIRING_TICK)
        : service.tick(),
    // The responder runs on its own interval; a scan only nudges it.
    runAvailabilityResponse: responseLoop.nudge,
    ...runtime.daBondPool.tickRunnerDeps(runtime.onChainCoordinator),
    runRetention,
    latestL1View: () => service.latestL1View(),
    latestL1ProgressAtMs: () => service.latestL1ProgressAtMs(),
    setRetentionReadiness: (update) => {
      retentionReadiness = update(retentionReadiness);
    },
    l1ViewFatalMs: config.l1ViewFatalMs,
    l1ViewStaleMs: l1ViewStaleMs(config),
    startedAtMs,
    nowMs: () => Date.now(),
    write: (stream, line) => process[stream].write(line),
    slowTickMs: config.pollIntervalMs,
  });

  if (once) {
    try {
      await preflight?.start();
      const viewBeforeTick = service.latestL1View();
      const result = await service.tick();
      await responseLoop.run();
      await tickRunner.runRetentionStep(viewBeforeTick);
      process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
    } finally {
      preflight?.stop();
      await runtime.close();
    }
    return;
  }

  await preflight?.start();
  const readiness = async () => {
    const snapshot = await service.readinessSnapshot({
      localPeerId: local.daIdentity.peerId,
      ...(preflight === undefined
        ? {}
        : { l1SubmitterPreflight: preflight.snapshot() }),
      l1SubmitterFunding: runtime.submitterFunding(),
      ...runtime.daBondPool.readiness(),
      retention: retentionReadiness,
    });
    const liveness = tickRunner.liveness();
    const reasons = [
      ...snapshot.reasons,
      ...(liveness.l1ViewUnavailable === undefined
        ? []
        : [
            `l1_view_unavailable:${liveness.l1ViewUnavailable.l1ViewAgeMs.toString()}`,
          ]),
      ...(storeLockSuspended ? ["store_instance_lock_reacquiring"] : []),
      ...(preflight?.reasons() ?? []),
      ...responseLoop.reasons(),
    ];
    return { ...snapshot, ready: reasons.length === 0, reasons, liveness };
  };
  await starting?.close();
  api = createCommitteeApiServer({
    deploymentFingerprint: config.deploymentFingerprint,
    signerIndex: config.signerIndex,
    signerValidation: local.committeeValidation,
    store,
    readiness,
    // A tick that has not settled past the L1-view deadline cannot recover
    // in-process; the supervisor's restart is the repair.
    health: () => {
      const hung = tickRunner.liveness().tickHung;
      return hung === undefined
        ? { ok: true }
        : {
            ok: false,
            reason: `committee_tick_hung:${hung.inFlightMs.toString()}`,
          };
    },
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

  responseLoop.start();
  interval = await startCommitteeTickLoop({
    runTick: tickRunner.runTick,
    pollIntervalMs: config.pollIntervalMs,
    shutdown,
    exit: (code) => process.exit(code),
  });
};

/** The tick result while the store's instance lock is being taken again. */
const STORE_LOCK_REACQUIRING_TICK = {
  scannedHeaders: 0,
  signedHeaders: 0,
  reconciledHeaders: 0,
  skippedHeaders: 0,
  payloadFetches: [],
  errors: ["store_instance_lock_reacquiring"],
} as const;

/** The configuration with automatic funding switched off. */
const withoutAutoFund = (
  config: LoadedCommitteeConfig,
): LoadedCommitteeConfig => {
  const { autoFundKeySource: _ignored, ...preflight } =
    config.l1SubmitterPreflight;
  return { ...config, l1SubmitterPreflight: preflight };
};

/**
 * Everything checked and loaded before any dependency is touched. A failure
 * here is the configuration's or the key material's, and exits at once.
 */
const loadLocalSetup = async (
  config: LoadedCommitteeConfig,
): Promise<CommitteeNodeLocalSetup> => {
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
  return {
    config,
    ...(signer === undefined ? {} : { signer }),
    committeeValidation,
    ...(signerValidation === undefined ? {} : { signerValidation }),
    daIdentity,
    daPeerRegistry,
    libp2pPrivateKeySource: config.libp2pPrivateKeySource,
  };
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
