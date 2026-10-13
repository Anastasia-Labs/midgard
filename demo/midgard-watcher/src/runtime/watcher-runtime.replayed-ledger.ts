import { join } from "node:path";

import {
  EVENT_WAIT_DURATION_MS,
  MAX_EVENT_VALIDITY_RANGE_LENGTH_MS,
} from "@al-ft/midgard-sdk";

import { createWatcherReplayedLedger } from "../fault-proofs/replayed-ledger.js";
import type { WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import { createWatcherReplayRetention } from "../l1-follower/replay-retention.js";
import { createWatcherFollowerReplaySource } from "../l1-follower/replay-source.js";
import { createWatcherFollowerUserEvents } from "../l1-follower/user-events.js";
import type { VerifiedWatcherDeploymentAuthority } from "./deployment-authority.js";
import {
  readWatcherUserEventScriptBinding,
  watcherDeploymentProtocolScriptAuthority,
  watcherDeploymentReleaseFinalityPolicy,
} from "./deployment-identity.js";

/** Allocate the origin floor before the follower, then bind its replay facts. */
export const createWatcherRuntimeReplayHost = (
  deploymentAuthority: VerifiedWatcherDeploymentAuthority,
  userEventScripts: ReturnType<typeof readWatcherUserEventScriptBinding>,
  journalDirectory: string,
) => {
  const deploymentIdentity = deploymentAuthority.deploymentIdentity;
  const l1SlotConfig = deploymentAuthority.ruleBundle.ruleBundle.l1SlotConfig;
  const retention = createWatcherReplayRetention({
    slotConfig: l1SlotConfig,
    eventWaitMs: BigInt(EVENT_WAIT_DURATION_MS),
    maximumAdmissionRangeMs: MAX_EVENT_VALIDITY_RANGE_LENGTH_MS,
    maximumProtectionWaitMs: BigInt(
      userEventScripts.maximumProtectionDurationMs,
    ),
  });
  const identity = {
    deploymentManifestId: deploymentIdentity.manifestId,
    blueprintHash: deploymentIdentity.blueprintHash,
    network: deploymentIdentity.network,
  };
  const scripts = {
    depositPolicyId: userEventScripts.deposit.policyId,
    withdrawalPolicyId: userEventScripts.withdrawal.policyId,
    forcedOrderPolicyId: userEventScripts.forcedOrder.policyId,
    forcedOrderAddressHex: userEventScripts.forcedOrder.addressHex,
  };
  return {
    retention,
    bind: (follower: WatcherFollowerRuntime) => {
      const userEvents = createWatcherFollowerUserEvents({
        store: follower.store,
        rawReads: follower.rawReads,
        proofRetention: follower.proofRetention,
        identity,
        scripts,
      });
      const source = createWatcherFollowerReplaySource({
        store: follower.store,
        userEvents,
        identity,
        scripts,
        retention,
        minimumCheckpointDepth:
          watcherDeploymentReleaseFinalityPolicy(deploymentIdentity).policy
            .automaticRecoveryMaxDepth + 2,
        observationAuthority:
          watcherDeploymentProtocolScriptAuthority(deploymentIdentity),
      });
      const keeper = createWatcherReplayedLedger({
        follower: source,
        l1SlotConfig,
        checkpointPath: join(
          journalDirectory,
          `replayed-ledger-${deploymentIdentity.manifestId}.json`,
        ),
      });
      return { userEvents, keeper, close: () => source.close() };
    },
  };
};
