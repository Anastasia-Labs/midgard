#!/usr/bin/env node
import { runDaZstdStartupSelfTest } from "@al-ft/midgard-core/da-compression";
import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { loadRuntimeConfig } from "@al-ft/midgard-core/runtime-config";
import { FOLLOWER_TRANSIENT_EXHAUSTED } from "@al-ft/midgard-l1-follower";

import { createCommitteeApiServer } from "./api/server.js";
import { createAvailabilityResponseLoop } from "./availability-response-loop.js";
import {
  type CommitteeRetentionReadinessSnapshot,
  retentionReadinessFromDeadlines,
} from "./committee-service.js";
import { loadCommitteeConfig, type LoadedCommitteeConfig } from "./config.js";
import { committeeFailureExit } from "./config-refusal-exit.js";
import { l1SubmitterWalletPreflightFromConfig } from "./coordinator/factory.js";
import { DaPeerRegistry } from "./da/libp2p/index.js";
import {
  committeeL1InterventionReason,
  type CommitteeL1Readiness,
  openCommitteeL1Reader,
  untilCommitteeL1SourceReady,
} from "./l1/follower/l1-follower.js";
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
import { listenStartingServer, retryStartup, startOrHold } from "./startup.js";
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
import { transientExhaustionExit } from "./transient-exhaustion.js";

/** Why the store's instance lock refuses decision effects. */
type StoreLockRefusal =
  | "store_instance_lock_reacquiring"
  | "store_instance_lock_held_elsewhere"
  | "store_instance_lock_failed";

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
  // No port serves `/readyz` before the configuration loads: a failure exits
  // non-zero, a role L1 refusal 78 with its reason (config-refusal-exit.ts).
  const config = await loadCommitteeConfig();
  const startedAtMs = Date.now();
  const once = process.argv.includes("--once");
  const write = (line: string): void => {
    process.stderr.write(line);
  };
  // eslint-disable-next-line prefer-const -- the shutdown, once it is built
  let shutdownOnExit: (() => Promise<void>) | undefined;
  const exhausted = transientExhaustionExit({
    write,
    shutdown: () => shutdownOnExit,
    exit: (code) => process.exit(code),
  });

  // Why the store's instance lock refuses work, while it does: readiness
  // names the reason, and the lock keeps trying until it fails
  // (`store_instance_lock_failed`), the process exiting only when exhausted.
  let storeLockRefusal: StoreLockRefusal | undefined;
  const storeLockEvents: PostgresStoreInstanceLockEvents = {
    // Postgres went away: refuse work until the lock is held again.
    onInstanceLockSuspended: (error) => {
      storeLockRefusal = "store_instance_lock_reacquiring";
      write(
        `${JSON.stringify({ event: "committee_store_instance_lock_suspended", error: error.message })}\n`,
      );
    },
    // Another live process holds the lock: this one is the passive member
    // and takes over when that process's session ends.
    onInstanceLockHeldElsewhere: (error) => {
      storeLockRefusal = "store_instance_lock_held_elsewhere";
      write(
        `${JSON.stringify({ event: "committee_store_instance_lock_passive", error: error.message })}\n`,
      );
    },
    // Not taken again. Postgres unreachable past the reacquire budget
    // exits non-zero; a failure that is not transient is refused, the
    // process up, until it is restarted.
    onInstanceLockFailed: (error) => {
      storeLockRefusal = "store_instance_lock_failed";
      write(
        `${JSON.stringify({ event: "committee_store_instance_lock_failed", exhausted: error.exhausted, error: error.message })}\n`,
      );
      if (error.exhausted)
        exhausted({
          source: "store_instance_lock",
          reason: "store_instance_lock_failed",
          detail: error.message,
        });
    },
    onInstanceLockRestored: () => {
      storeLockRefusal = undefined;
      write(
        `${JSON.stringify({ event: "committee_store_instance_lock_restored" })}\n`,
      );
    },
  };

  // Dependencies that are down or not up yet are waited for, within the
  // startup budget; past it the process exits non-zero and its supervisor
  // restarts it. A failure no wait is known to repair holds the process up,
  // unready (`startupFailureOutcome`); a one-shot run exits on it.
  const starting = once
    ? undefined
    : await listenStartingServer(config.apiPort, config.apiHost);
  // While the L1 follower holds the committee the attempt waits, process up,
  // naming the follower's reasons on /readyz; a one-shot run fails instead
  // on a reason no wait clears.
  const onL1Held = (reasons: readonly CommitteeL1Readiness[]): void => {
    if (once) throwOnL1Intervention(reasons);
    starting?.setReason(
      `starting:${reasons.map(({ reason, detail }) => `${reason}: ${detail}`).join("; ")}`,
    );
  };
  const onFollowerExhausted = (detail: string) =>
    exhausted({
      source: "l1_follower",
      reason: FOLLOWER_TRANSIENT_EXHAUSTED,
      detail,
    });
  const started = await startOrHold(starting, write, async () => {
    // Decoder-first rollout means every committee node must be capable of
    // safely decoding zstd envelopes before any producer is flipped.
    await runDaZstdStartupSelfTest();
    const setup = await loadLocalSetup(config);
    const opened = await retryStartup({
      attempt: () =>
        openCommitteeNodeRuntime(
          setup,
          storeLockEvents,
          onL1Held,
          onFollowerExhausted,
        ),
      onFailure: (reason) => starting?.setReason(reason),
      write,
      ...(once ? { classify: () => "fatal" as const } : {}),
    });
    return { local: setup, runtime: opened };
  });
  // Held: the starting server keeps the process up until it is restarted.
  if (started === undefined) return;
  const { local, runtime } = started;
  const { store, service, availabilityRuntime } = runtime;

  const responseLoop = createAvailabilityResponseLoop({
    drain: () =>
      availabilityRuntime === undefined
        ? Promise.resolve({ challenges: 0, status: "idle" as const })
        : availabilityRuntime.responder.drain(),
    write: (stream, line) => process[stream].write(line),
    pollIntervalMs: config.pollIntervalMs,
    enforcement: availabilityRuntime?.promiseLoopEnforcement,
  });
  const preflight = config.l1SubmissionEnabled
    ? createL1SubmitterPreflightMonitor({
        evaluate: ({ autoFund }) =>
          l1SubmitterWalletPreflightFromConfig(
            autoFund ? config : withoutAutoFund(config),
            runtime.l1Lucid,
          ),
        write,
      })
    : undefined;

  let retentionReadiness: CommitteeRetentionReadinessSnapshot =
    runtime.startupCompactionFailure === undefined
      ? {
          status: "not_checked",
          scanned: 0,
          retained: 0,
          prunable: 0,
          alerting: 0,
        }
      : {
          status: "failed",
          checkedAt: new Date().toISOString(),
          scanned: 0,
          retained: 0,
          prunable: 0,
          alerting: 0,
          error: runtime.startupCompactionFailure,
        };
  /** Retirement holds (degraded detail) and the follower's pin failures. */
  const retentionDetail = () => ({
    holds: availabilityRuntime?.retirementHolds?.() ?? [],
    pinFailures: runtime.l1Retention?.reasons() ?? [],
  });
  const runRetention = async (view: RetentionL1View): Promise<void> => {
    // The exemption sets come from the L1 view the poller accepted this tick.
    const options = retentionCycleOptions(config, view, Date.now());
    try {
      await availabilityRuntime?.compactRetainedPromises?.();
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
  shutdownOnExit = shutdown;
  const tickRunner = createCommitteeTickRunner({
    // No decision is attempted while the store's instance lock refuses
    // work; the tick reports why and the next one retries.
    tick: () =>
      storeLockRefusal === undefined
        ? service.tick()
        : Promise.resolve(storeLockRefusedTick(storeLockRefusal)),
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
      await untilCommitteeL1SourceReady(runtime.l1, {
        onHeld: throwOnL1Intervention,
      });
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
      retention: { ...retentionReadiness, ...retentionDetail() },
    });
    const liveness = tickRunner.liveness();
    const reasons = [
      ...snapshot.reasons,
      ...(liveness.l1ViewUnavailable === undefined
        ? []
        : [
            `l1_view_unavailable:${liveness.l1ViewUnavailable.l1ViewAgeMs.toString()}`,
          ]),
      ...(storeLockRefusal === undefined ? [] : [storeLockRefusal]),
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

/** The tick result while the store's instance lock refuses work. */
const storeLockRefusedTick = (reason: StoreLockRefusal) =>
  ({
    scannedHeaders: 0,
    signedHeaders: 0,
    reconciledHeaders: 0,
    skippedHeaders: 0,
    payloadFetches: [],
    errors: [reason],
  }) as const;

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
 * here is the configuration's or the key material's: no restart repairs it,
 * so the process holds, unready.
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
  const reader = await openCommitteeL1Reader(config, (line) =>
    process.stderr.write(`${line}\n`),
  );
  let result: Awaited<ReturnType<typeof l1SubmitterWalletPreflightFromConfig>>;
  try {
    result = await l1SubmitterWalletPreflightFromConfig(config, reader.lucid);
  } finally {
    await reader.close();
  }
  process.stdout.write(
    `${JSON.stringify(l1SubmitterPreflightResultToJson(result), null, 2)}\n`,
  );
  if (result.status === "failed") {
    process.exitCode = 1;
  }
};

/** A one-shot run cannot wait on an operator: it fails on such a reason. */
const throwOnL1Intervention = (
  reasons: readonly CommitteeL1Readiness[],
): void => {
  const blocking = committeeL1InterventionReason(reasons);
  if (blocking !== undefined)
    throw new Error(`${blocking.reason}: ${blocking.detail}`);
};

const printHelp = (): void => {
  process.stdout.write(`da-committee-node

Usage:
  da-committee-node --once                       once the L1 follower caught up, tick once: verify signable unattested headers, sign
  da-committee-node l1-wallet-preflight --json   print L1 submitter wallet readiness
  da-committee-node                              run API and polling loop

Required configuration follows demo/da-committee-node/docs/da-committee-node-architecture.md in the repository.
L1 submission requires L1_SUBMITTER_KEY_SOURCE for a funded Cardano wallet.
The committee reads L1 through its own chain follower on the local node:
L1_ORIGIN, the native ledger (CARDANO_LOCAL_NODE_SOCKET_PATH,
CARDANO_LOCAL_NODE_CONFIG_PATH and CARDANO_L1_NODE_TRANSPORT_BINARY_PATH) and
the deployment's hubOracleOneShot. Until all are set it stays unready with
l1_follower_unconfigured. The availability responder reads and submits
through the same follower; no chain index (Kupo, Ogmios) is configured.
`);
};

const printL1WalletPreflightHelp = (): void => {
  process.stdout.write(`da-committee-node l1-wallet-preflight --json

Prints DA L1 submitter wallet readiness as JSON using the normal environment
configuration. Reads the wallet from the committee's L1 follower facts, as
current as the running committee made them. Exits non-zero when readiness
fails.
`);
};

main().catch((error) => {
  const { code, line } = committeeFailureExit(error);
  process.stderr.write(line);
  process.exit(code);
});
