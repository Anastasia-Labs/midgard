import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import {
  l1SourceAuthorityDigest,
  type LoadedCommitteeConfig,
} from "../config.js";
import type { L1ObservedDecision } from "../store.committee-store.js";
import { persistedDecisionTransition } from "../store.persisted-decision-transition.js";
import { retirementDigest } from "../store/retirement-model.js";
import type { ChainSyncCursor } from "./provider.js";
import { LocalNodeStateQueueProvider } from "./provider.local-node-state-queue-provider.js";
import { samePersistedCursor } from "./provider.same-persisted-cursor.js";
import {
  assertUnsignedL1Incident,
  type L1RecoverySnapshot,
} from "./recovery-incident.js";
import {
  scanStateQueue,
  type StateQueueReplayAnchor,
} from "./state-queue-scanner.js";

export type L1RecoveryProofInput = Readonly<{
  snapshot: L1RecoverySnapshot;
  config: LoadedCommitteeConfig;
  provider: LocalNodeStateQueueProvider;
  scope: DaAvailabilityReadScope;
}>;

/** Every read is awaited through completion. The deadline fences writes, not latency. */
export const proveUnsignedL1Recovery = async (args: L1RecoveryProofInput) => {
  const { snapshot, config, provider, scope } = args;
  assertUnsignedL1Incident(snapshot);
  const prior = snapshot.data.chainCursor!;
  if (
    config.l1Source.sourceMode !== "local_node" ||
    !(provider instanceof LocalNodeStateQueueProvider)
  )
    throw new Error("complete_local_native_source_required");
  if (config.availabilityJournalPath || config.availabilitySubmitterKeySource)
    throw new Error("journal_exclusive_inventory_unavailable");
  if (
    prior.sourceMode !== config.l1Source.sourceMode ||
    prior.network !== config.network ||
    prior.authoritySha256 !==
      l1SourceAuthorityDigest(config.network, config.l1Source)
  )
    throw new Error("configured_source_binding_changed");
  const deployment = snapshot.data.deployment!;
  if (
    retirementDigest(deployment.marker) !==
      retirementDigest(makeDeploymentMarker(config.deploymentFingerprint)) ||
    deployment.manifestSha256 !== config.deploymentManifestSha256 ||
    deployment.contractDeploymentInfoSha256 !==
      config.contractDeploymentInfoSha256 ||
    deployment.manifestRaw !== config.deploymentManifestRaw
  )
    throw new Error("configured_deployment_binding_changed");
  scope.assertCurrent();
  let cursor: ChainSyncCursor | undefined;
  let anchor: StateQueueReplayAnchor | undefined;
  let incomplete = false;
  let deferred = false;
  let steps: ReadonlyMap<
    string,
    NonNullable<L1ObservedDecision["authenticatedSteps"]>
  > = new Map();
  const records = await scanStateQueue(provider, {
    deploymentFingerprint: config.deploymentFingerprint,
    deploymentIdentityDigest: config.deploymentFingerprint,
    stateQueuePolicyId: config.stateQueuePolicyId,
    daAttestationPolicyId: config.daAttestationPolicyId,
    finalityDepth: config.finalityDepth,
    automaticRecoveryMaxDepth: config.automaticRecoveryMaxDepth,
    consensusProfile: config.consensusProfile,
    previousHeaders: Object.values(snapshot.data.stateQueueHeaders),
    terminalReplayAnchor: prior.stateQueueReplayAnchor,
    recordChainSyncCursor: (c) => {
      cursor = c;
    },
    recordReplayAnchor: (a) => {
      anchor = a;
    },
    recordCatchUp: () => {
      incomplete = true;
    },
    recordL1View: (view) => {
      incomplete ||= view.recoveryProofUnavailable === true;
    },
    recordReplayedHeaderSteps: (replayed) => {
      deferred = replayed.deferredHeaderHashes.length > 0;
      steps = replayed.finalSteps;
    },
  });
  scope.assertCurrent();
  if (
    !cursor ||
    !anchor ||
    incomplete ||
    deferred ||
    provider.chainSyncCatchUpProgress()
  )
    throw new Error("native_replay_incomplete");
  if (
    !records.some(
      (record) =>
        !prior.observations.some((o) => o.headerHash === record.headerHash),
    )
  )
    throw new Error("new_canonical_candidate_missing");
  if (
    records.some(
      (r) =>
        !r.finalized ||
        r.status === "conflicted" ||
        r.observedChainPoint.slot === undefined ||
        r.observedChainPoint.blockHash === undefined,
    )
  )
    throw new Error("canonical_finalized_candidate_evidence_missing");
  for (const observed of prior.observations) {
    if (observed.slot === undefined || observed.blockHash === undefined)
      throw new Error("prior_original_canonical_point_missing");
    const record = records.find((r) => r.headerHash === observed.headerHash);
    if (!record) throw new Error("prior_observation_not_completely_replayed");
    const current: L1ObservedDecision = {
      headerHash: record.headerHash,
      stateQueueOutRef: record.stateQueueOutRef,
      stateQueueStatus: record.status,
      slot: record.observedChainPoint.slot,
      blockHash: record.observedChainPoint.blockHash,
      finalized: record.finalized,
      hasPersistedDecision: observed.hasPersistedDecision,
      authenticatedSteps: steps.get(record.headerHash),
    };
    if (persistedDecisionTransition(observed, current) === "unexplained")
      throw new Error("prior_observation_transition_unproved");
  }
  const consumed = await provider.loadConsumedChainSyncCursor();
  scope.assertCurrent();
  if (
    !consumed ||
    consumed.sequence > cursor.sequence ||
    consumed.rollbackGeneration > cursor.rollbackGeneration
  )
    throw new Error("durable_native_consumer_evidence_missing");
  const events = await provider.replayChainSyncEvents(consumed.sequence);
  scope.assertCurrent();
  if (
    events.length !== cursor.sequence - consumed.sequence ||
    events.some((event) => event.direction !== "roll_forward") ||
    consumed.rollbackGeneration !== cursor.rollbackGeneration
  )
    throw new Error("native_rollback_or_replay_gap_requires_reconciliation");
  const captured = cursor;
  const assertCurrent = async (): Promise<void> => {
    scope.assertCurrent();
    const fresh = await provider.fetchStateQueueSnapshot();
    scope.assertCurrent();
    if (
      !fresh.chainSyncCursor ||
      !samePersistedCursor(captured, fresh.chainSyncCursor)
    )
      throw new Error("native_source_changed_during_recovery");
    scope.assertCurrent();
    const currentConsumed = await provider.loadConsumedChainSyncCursor();
    scope.assertCurrent();
    if (!currentConsumed || !samePersistedCursor(consumed, currentConsumed))
      throw new Error("native_consumer_changed_during_recovery");
    const finalCursor = await provider.currentChainSyncCursor();
    scope.assertCurrent();
    if (!samePersistedCursor(captured, finalCursor))
      throw new Error("native_source_changed_during_recovery");
  };
  await assertCurrent();
  return { assertCurrent, assertScopeCurrent: () => scope.assertCurrent() };
};
