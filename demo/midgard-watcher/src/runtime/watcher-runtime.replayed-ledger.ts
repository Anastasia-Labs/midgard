import { mkdirSync } from "node:fs";
import { join } from "node:path";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { createLedgerStore } from "@al-ft/midgard-fault-proofs/ledger-store";
import { openSqliteLedgerStorage } from "@al-ft/midgard-fault-proofs/ledger-store/sqlite";
import {
  EVENT_WAIT_DURATION_MS,
  MAX_EVENT_VALIDITY_RANGE_LENGTH_MS,
} from "@al-ft/midgard-sdk/core";

import { createWatcherReplayLostRoots } from "../fault-proofs/replay-lost-roots.js";
import { createWatcherReplayRefusals } from "../fault-proofs/replay-refusals.js";
import {
  createWatcherReplayedLedger,
  watcherReplayAvailabilityAuthority,
} from "../fault-proofs/replayed-ledger.js";
import type { WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import { createWatcherReplayRetention } from "../l1-follower/replay-retention.js";
import { createWatcherFollowerReplaySource } from "../l1-follower/replay-source.js";
import { createWatcherFollowerUserEvents } from "../l1-follower/user-events.js";
import { registerWatcherDaFetcher } from "../storage/retained-da-runtime.js";
import type { VerifiedWatcherDeploymentAuthority } from "./deployment-authority.js";
import { readWatcherUserEventScriptBinding } from "./deployment-identity.js";
import { watcherDeploymentProtocolScriptAuthority } from "./deployment-identity.verify-watcher-user-event-script-binding.js";
import { watcherDeploymentReleaseFinalityPolicy } from "./deployment-identity.watcher-deployment-availability-challenge-authority.js";
import type { WatcherOperationsSink } from "./operations-observability.js";

/**
 * Allocate the origin floor before the follower, then bind its replay facts.
 * The deployment's replayed ledgers live in
 * `replayed-ledger-<manifest id>.sqlite` in the journal directory, with the
 * verified confirmed head, open from `bind` until its `close`. A confirmed
 * block that does not open its root raises `root_mismatch` on
 * `operationsSink`, and a ledger the store lost `replayed_ledger_root_lost`.
 */
export const createWatcherRuntimeReplayHost = (
  deploymentAuthority: VerifiedWatcherDeploymentAuthority,
  userEventScripts: ReturnType<typeof readWatcherUserEventScriptBinding>,
  journalDirectory: string,
  operationsSink: Pick<WatcherOperationsSink, "setAlert">,
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
    ledgerRetentionMs: BigInt(MIDGARD_RETENTION_WINDOW.ledgerRetentionMs),
    maturityMs: BigInt(MIDGARD_RETENTION_WINDOW.maturityMs),
  });
  const identity = {
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
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
      const refusals = createWatcherReplayRefusals({ sink: operationsSink });
      mkdirSync(journalDirectory, { recursive: true, mode: 0o700 });
      const storage = openSqliteLedgerStorage(
        join(
          journalDirectory,
          `replayed-ledger-${deploymentIdentity.hubOraclePolicyId}.sqlite`,
        ),
      );
      const keeper = createWatcherReplayedLedger({
        follower: source,
        l1SlotConfig,
        store: createLedgerStore(storage),
        verifiedHead: storage.verifiedHead,
        availabilityAuthority:
          watcherReplayAvailabilityAuthority(deploymentIdentity),
        refusals,
        lostRoots: createWatcherReplayLostRoots({ sink: operationsSink }),
      });
      const unregister = registerWatcherDaFetcher(
        deploymentIdentity,
        keeper.mayStillFetch,
      );
      return {
        userEvents,
        keeper,
        refusals,
        close: () => {
          unregister();
          source.close();
          storage.close();
        },
      };
    },
  };
};
