import type { FraudProofRawL1SnapshotRequest } from "@al-ft/midgard-fault-proofs/workflow/raw-l1-snapshot";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type ReleaseL1FinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs/workflow/release-finality-policy";
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk/core";
import { scriptHashToCredential } from "@lucid-evolution/lucid";
import { credentialToAddress } from "@lucid-evolution/lucid";

import {
  createWatcherFaultProofL1Source,
  WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX,
} from "../../src/l1-follower/fault-proof-l1-source.js";
import {
  createWatcherPendingRead,
  type WatcherPendingRead,
} from "../../src/l1-follower/pending-read.js";
import type { FollowerRawReads } from "../../src/l1-follower/raw-reads.types.js";
import { D, K } from "./l1-follower-raw-reads-fixture.js";

/**
 * The release policy, snapshot request, signed bytes and node double the
 * watcher's fault-proof L1 source tests (ticket W1) use over the raw-read
 * fixture chain (`l1-follower-raw-reads-fixture.ts`, k = 3).
 */

export const SOURCE_ID = `${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX}watcher-test`;

/** confirmationDepth 2; the recovery depth scaled to the fixture's k (k + 2 = 5). */
const POLICY = {
  confirmationDepth: 2,
  automaticRecoveryMaxDepth:
    K as ReleaseL1FinalityPolicy["automaticRecoveryMaxDepth"],
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const satisfies ReleaseL1FinalityPolicy;

export const RELEASE: VerifiedFraudProofReleaseFinalityPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: "d1".repeat(32),
  blueprintHash: "b1".repeat(32),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(POLICY),
  policy: POLICY,
};
export const RECOVERY_DEPTH = K + 2;

export const STATE_QUEUE_ADDRESS = credentialToAddress(
  D.network,
  scriptHashToCredential(D.stateQueueSpend),
);

export const nodeUnit = (header: string): string =>
  `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`;

/** The state-queue scope and one header's node-unit history. */
export const requestFor = (
  header: string,
  overrides: Partial<FraudProofRawL1SnapshotRequest> = {},
): FraudProofRawL1SnapshotRequest => ({
  deploymentIdentityDigest: RELEASE.deploymentIdentityDigest,
  blueprintHash: RELEASE.blueprintHash,
  finalityPolicyDigest: RELEASE.policyDigest,
  headerHash: header,
  scopes: [{ role: "state_queue", address: STATE_QUEUE_ADDRESS }],
  historyUnits: [nodeUnit(header)],
  ...overrides,
});

/**
 * The node's mempool and tip over `store`: `state.txs` is the listing
 * (empty unless set), `state.tip` the node's tip (the store's own view
 * unless set), and `state.calls` the order the read asks them in.
 */
export const mempoolDouble = (store: FactStore) => {
  const state: {
    txs: Uint8Array[];
    tip: Readonly<{ slot: number; hash: string }> | undefined;
    calls: string[];
  } = { txs: [], tip: undefined, calls: [] };
  const read: WatcherPendingRead = createWatcherPendingRead({
    store,
    node: {
      mempoolTxs: async () => {
        state.calls.push("mempoolTxs");
        return { acquiredSlot: 0n, txs: [...state.txs] };
      },
    },
    nodeTip: async () => {
      state.calls.push("nodeTip");
      if (state.tip !== undefined) return state.tip;
      const view = await store.currentView();
      // A node has a tip before the follower has a cursor.
      if (view === null) return { slot: 0, hash: "" };
      return { slot: view.point.slot, hash: view.point.hash.toString("hex") };
    },
  });
  return { read, state };
};

/** An empty mempool at the store's own view. */
export const pendingOver = (store: FactStore): WatcherPendingRead =>
  mempoolDouble(store).read;

export const sourceOver = (
  fx: Readonly<{ store: FactStore }>,
  rawReads: FollowerRawReads,
  pending: WatcherPendingRead = pendingOver(fx.store),
) =>
  createWatcherFaultProofL1Source({
    store: fx.store,
    rawReads,
    pending,
    sourceId: SOURCE_ID,
  });
