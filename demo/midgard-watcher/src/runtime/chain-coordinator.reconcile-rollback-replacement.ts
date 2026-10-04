import {
  evaluateWatcherFinality,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservation } from "../l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.js";
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";
import { WatcherDurableAuthorityConflict } from "../storage/durable-runtime.load-published-authority.js";
import { canonicalPathFromHistory } from "./chain-coordinator.canonical-path-from-history.js";
import { WatcherCoordinatorIntegrityHeld } from "./chain-coordinator.integrity-hold.js";
import { retainedCanonicalPrefix } from "./chain-coordinator.retained-canonical-prefix.js";

export const reconcileRollbackReplacement = async (input: {
  readonly durable: WatcherDurableRuntime;
  readonly policy: WatcherFinalityPolicy;
  readonly block: WatcherNativeBlockAdmission;
  readonly target: WatcherNativeChainSyncPoint;
  readonly observed: WatcherLocalKupmiosNativeObservation &
    Readonly<{ assertCurrent?: () => void }>;
}): Promise<
  | Readonly<{ kind: "common_prefix"; atFrontier: boolean }>
  | Readonly<{ kind: "replacement"; quarantined: boolean }>
> => {
  const { target, block, observed } = input;
  if (target.kind === "point" && block.prevHash !== target.blockHash) {
    throw new Error(
      "replacement block is not the child of the native rollback point",
    );
  }
  const before = input.durable.read();
  const previousFinalityState = before.currentFinalityState;
  const retained = retainedCanonicalPrefix(input.durable);
  const common = retained.find(
    ({ agreement }) =>
      agreement?.blockHash === block.blockHash &&
      agreement.blockNo === block.blockNo &&
      agreement.slot === block.slot,
  );
  if (common !== undefined) {
    if (
      common.agreement?.blockContentDigest !== observed.block.blockContentDigest
    )
      throw new WatcherCoordinatorIntegrityHeld(
        "rollback_evidence_rejected",
        "native common-prefix content differs from retained authority",
      );
    const persisted = await input.durable.persistObservation(observed);
    if (persisted.persistence === "conflict")
      throw new WatcherDurableAuthorityConflict(
        "watcher common-prefix persistence conflicted",
      );
    return Object.freeze({
      kind: "common_prefix",
      atFrontier: retained.at(-1) === common,
    });
  }
  await input.durable.persistObservation(observed);
  const finalityResult = evaluateWatcherFinality(
    input.policy,
    previousFinalityState,
    observed.consistency,
  );
  const rollback = await input.durable.persistRollback({
    assertCurrent: observed.assertCurrent,
    previousFinalityState,
    consistency: observed.consistency,
    finalityResult,
    transportAttestations: observed.transportAttestations,
  });
  if (rollback.persistence === "conflict") {
    throw new WatcherDurableAuthorityConflict(
      "watcher rollback persistence conflicted",
    );
  }
  if (rollback.result.action === "reject") {
    throw new WatcherCoordinatorIntegrityHeld(
      "rollback_evidence_rejected",
      "authenticated native rollback was rejected by durable recovery",
    );
  }
  let quarantined = rollback.result.protocolDecision === "quarantined";
  if (
    quarantined &&
    target.kind === "point" &&
    previousFinalityState.finalized !== null
  ) {
    const previousPath = canonicalPathFromHistory({
      history: before.authenticatedConsistencyHistory,
      store: before.currentStore,
      ancestor: target,
      terminal: previousFinalityState.finalized,
    });
    const ancestorConsistency = previousPath?.[0];
    if (previousPath !== null && ancestorConsistency !== undefined) {
      const recovery = await input.durable.persistPostFinalityRecovery({
        assertCurrent: observed.assertCurrent,
        previousCanonicalPath: previousPath,
        replacementCanonicalPath: Object.freeze([
          ancestorConsistency,
          observed.consistency,
        ]),
        transportAttestations: observed.transportAttestations,
      });
      if (recovery.persistence === "conflict") {
        throw new WatcherDurableAuthorityConflict(
          "watcher post-finality recovery persistence conflicted",
        );
      }
      quarantined = recovery.result.protocolDecision !== "resume_replay";
    }
  }
  return Object.freeze({ kind: "replacement", quarantined });
};
