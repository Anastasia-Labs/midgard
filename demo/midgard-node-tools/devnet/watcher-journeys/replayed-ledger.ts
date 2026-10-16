import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  isFraudProofL1WaitError,
  type LedgerAt,
} from "@al-ft/midgard-fault-proofs";
import {
  EVENT_WAIT_DURATION_MS,
  MAX_EVENT_VALIDITY_RANGE_LENGTH_MS,
} from "@al-ft/midgard-sdk";
import {
  createWatcherRetainedDaRuntime,
  openWatcherDeploymentFollower,
  type VerifiedWatcherDeploymentAuthority,
  type WatcherConfig,
  watcherDeploymentAppliedScriptHashes,
  watcherDeploymentProtocolScriptAuthority,
  watcherDeploymentReleaseFinalityPolicy,
  watcherFollowedScripts,
} from "midgard-watcher";
import {
  createWatcherFollowerReplaySource,
  createWatcherFollowerUserEvents,
  createWatcherReplayedLedger,
  createWatcherReplayRetention,
  readWatcherObservation,
  readWatcherUserEventScriptBinding,
  verifyWatcherUserEventScriptBinding,
  watcherReplayAvailabilityAuthority,
} from "midgard-watcher/replayed-ledger";

import { createJourneyReplayLifetime } from "./journey-replay-lifetime.js";

/** A read-only production follower and keeper for the staging owner's ledger reads. */
export const openJourneyReplayedLedger = async (input: {
  authority: VerifiedWatcherDeploymentAuthority;
  blueprintPath: string;
  watcherConfig: WatcherConfig;
  binaryPath: string;
  directory: string;
  checkpointDirectory: string;
  networkMagic: number;
}) => {
  const deploymentIdentity = input.authority.deploymentIdentity;
  const blueprintBytes = await readFile(input.blueprintPath);
  const binding = readWatcherUserEventScriptBinding({
    binding: verifyWatcherUserEventScriptBinding({
      deploymentIdentity,
      blueprintBytes,
    }),
    deploymentIdentity,
  });
  const protocolAuthority =
    watcherDeploymentProtocolScriptAuthority(deploymentIdentity);
  const release =
    watcherDeploymentReleaseFinalityPolicy(deploymentIdentity).policy;
  const slotConfig = input.authority.ruleBundle.ruleBundle.l1SlotConfig;
  const retention = createWatcherReplayRetention({
    slotConfig,
    eventWaitMs: BigInt(EVENT_WAIT_DURATION_MS),
    maximumAdmissionRangeMs: MAX_EVENT_VALIDITY_RANGE_LENGTH_MS,
    maximumProtectionWaitMs: BigInt(binding.maximumProtectionDurationMs),
  });
  const nativeSource = input.watcherConfig.l1.source;
  if (nativeSource.sourceMode !== "local_node")
    throw new Error("Journey replay requires the native node");
  const follower = openWatcherDeploymentFollower({
    authority: protocolAuthority,
    origin: input.watcherConfig.l1.origin,
    followedScripts: watcherFollowedScripts({
      contractScriptHashes:
        watcherDeploymentAppliedScriptHashes(deploymentIdentity),
      blueprint: JSON.parse(blueprintBytes.toString("utf8")),
    }),
    storePath: join(input.directory, "replayed-ledger-follower.sqlite"),
    automaticRecoveryMaxDepth: release.automaticRecoveryMaxDepth,
    walletAddresses: [],
    eventProjection: binding.eventProjection,
    replayPruneFloor: retention.floor,
    node: {
      binaryPath: input.binaryPath,
      socketPath: nativeSource.chainSync.socketPath,
      networkMagic: input.networkMagic,
      requestTimeoutMs: input.watcherConfig.l1.requestTimeoutMs,
    },
  });
  const identity = {
    deploymentManifestId: deploymentIdentity.manifestId,
    blueprintHash: deploymentIdentity.blueprintHash,
    network: deploymentIdentity.network,
  };
  const scripts = {
    depositPolicyId: binding.deposit.policyId,
    withdrawalPolicyId: binding.withdrawal.policyId,
    forcedOrderPolicyId: binding.forcedOrder.policyId,
    forcedOrderAddressHex: binding.forcedOrder.addressHex,
  };
  const userEvents = createWatcherFollowerUserEvents({
    store: follower.store,
    rawReads: follower.rawReads,
    proofRetention: follower.proofRetention,
    identity,
    scripts,
  });
  const minimumCheckpointDepth = release.automaticRecoveryMaxDepth + 2;
  const source = createWatcherFollowerReplaySource({
    store: follower.store,
    userEvents,
    identity,
    scripts,
    retention,
    minimumCheckpointDepth,
    observationAuthority: protocolAuthority,
  });
  const keeper = createWatcherReplayedLedger({
    follower: source,
    l1SlotConfig: slotConfig,
    checkpointPath: join(
      input.checkpointDirectory,
      `replayed-ledger-${deploymentIdentity.manifestId}.json`,
    ),
    availabilityAuthority:
      watcherReplayAvailabilityAuthority(deploymentIdentity),
  });
  let da: Awaited<ReturnType<typeof createWatcherRetainedDaRuntime>>;
  try {
    da = await createWatcherRetainedDaRuntime({
      watcherConfig: input.watcherConfig,
      deploymentIdentity,
    });
  } catch (error) {
    source.close();
    userEvents.close();
    await follower.close();
    throw error;
  }
  const lifetime = createJourneyReplayLifetime();
  const ledgerAt: LedgerAt = (headerHash) =>
    lifetime.run(async () => {
      const deadline = Date.now() + 90_000;
      for (;;) {
        if (lifetime.isClosed())
          throw new Error("Journey replay host is closed");
        const status = follower.status();
        if (status !== null && status.readiness.length === 0) {
          try {
            const observation = await readWatcherObservation(follower.store, {
              authority: protocolAuthority,
              sourceId: "journey-replayed-ledger",
              depth: minimumCheckpointDepth,
              releaseDepth: release.confirmationDepth,
            });
            // Until a stable checked checkpoint exists, retention continues to hold the origin.
            if (observation.kind === "ok")
              await keeper.advance(observation.observation, da.sources);
            return await keeper.ledgerAt(da.sources)(headerHash);
          } catch (error) {
            if (!isFraudProofL1WaitError(error) || Date.now() >= deadline)
              throw error;
          }
        }
        if (Date.now() >= deadline)
          throw new Error(
            `Journey replay follower is pending: ${JSON.stringify(await follower.readiness())}`,
          );
        await pause(250);
      }
    });
  return {
    ledgerAt,
    close: () =>
      lifetime.close(async () => {
        source.close();
        userEvents.close();
        const results = await Promise.allSettled([
          da.close(),
          follower.close(),
        ]);
        const failures = results.flatMap((result) =>
          result.status === "rejected" ? [result.reason] : [],
        );
        if (failures.length > 0)
          throw new AggregateError(
            failures,
            "Could not close journey replay host",
          );
      }),
  };
};
