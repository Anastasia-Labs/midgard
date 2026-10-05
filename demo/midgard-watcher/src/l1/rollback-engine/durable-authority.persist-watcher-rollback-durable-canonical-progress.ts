import {
  evaluateWatcherFinality,
  makeWatcherFinalityBootstrapState,
} from ".././finality-engine.js";
import {
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from ".././l1-adapter.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import { retainReleasedFinality } from "../finality-engine.retain-released-finality.js";
import {
  authenticatesCanonicalBlock,
  commitRollbackDurableAuthority,
  currentRollbackFinalityState,
  nextAuthenticatedEvidenceWithinRecoveryHorizon,
} from "./durable-authority.commit-rollback-durable-authority.js";
import {
  makeRollbackDurableTrustedHead,
  runtimeForRollbackDurableAuthority,
} from "./durable-authority.parse-rollback-durable-trusted-head.js";
import {
  ancestryLinksFinalizedToBlock,
  assertCanonicalProgressEvidence,
  type WatcherRollbackCanonicalAncestryLink,
} from "./durable-authority.persist-watcher-rollback-durable-observation.js";
import { freezeRollbackSnapshotJson } from "./durable-authority.rollback-authority-canonical.js";
import { reject } from "./records.js";
import {
  evaluateWatcherPostFinalityRecovery,
  parseWatcherPostFinalityRecoveryResult,
} from "./recovery.js";
import {
  evaluateWatcherRollbackStep,
  makeEpochBootstrapState,
} from "./state.js";
import {
  type WatcherPostFinalityRecoveryInput,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableCanonicalProgressResult,
  type WatcherRollbackDurableEvaluationResult,
  type WatcherRollbackDurableRecoveryResult,
} from "./types.js";

export const persistWatcherRollbackDurableCanonicalProgress = async (input: {
  readonly authority: WatcherRollbackDurableAuthority;
  readonly block: WatcherNormalizedL1Block;
  readonly observations: readonly WatcherNormalizedL1Block[];
  readonly consistency: WatcherMultiProviderConsistency;
  readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  readonly ancestry?: readonly WatcherRollbackCanonicalAncestryLink[];
}): Promise<WatcherRollbackDurableCanonicalProgressResult> => {
  const runtime = runtimeForRollbackDurableAuthority(input.authority);
  if (!authenticatesCanonicalBlock(input)) {
    throw new Error(
      "watcher canonical progress lacks authenticated local-node agreement",
    );
  }
  const previousFinalityState = currentRollbackFinalityState(runtime);
  let evaluationState = previousFinalityState;
  if (
    previousFinalityState.phase === "finalized" &&
    previousFinalityState.finalized?.pointDigest !==
      input.block.chainPoint.pointDigest
  ) {
    const finalized = previousFinalityState.finalized;
    if (
      finalized === null ||
      !ancestryLinksFinalizedToBlock(
        finalized,
        input.block,
        input.ancestry ?? [],
      )
    ) {
      throw new Error(
        "watcher canonical progress is not the direct child of the finalized block",
      );
    }
    evaluationState =
      makeWatcherFinalityBootstrapState(runtime.policy) ??
      (() => {
        throw new Error("watcher canonical progress bootstrap is invalid");
      })();
  }
  const finalityResult = retainReleasedFinality(
    evaluateWatcherFinality(runtime.policy, evaluationState, input.consistency),
    previousFinalityState.finalized,
  );
  if (
    finalityResult.state === null ||
    ["reject", "rewind_pending", "quarantine_incident"].includes(
      finalityResult.action,
    )
  ) {
    throw new Error(
      "watcher canonical progress is not a forward finality transition",
    );
  }
  if (finalityResult.action === "duplicate") {
    return Object.freeze({
      persistence: "unchanged",
      authority: input.authority,
      trustedHead: makeRollbackDurableTrustedHead(
        runtime.policy,
        runtime.snapshot,
        runtime.snapshotSha256,
        runtime.authenticationKey,
      ),
      finalityResult,
    });
  }
  const { store: nextStore, history: nextHistory } =
    nextAuthenticatedEvidenceWithinRecoveryHorizon({
      source: runtime.snapshot.currentStore,
      history: runtime.snapshot.consistencyHistory,
      observations: input.observations,
      consistencies: [input.consistency],
      frontier: finalityResult.state,
    });
  assertCanonicalProgressEvidence(
    runtime.policy,
    runtime.snapshot.currentStore,
    nextStore,
    input,
  );
  // This store was freshly parsed by the builder and is owned here. Reuse its
  // validated encoding for the epoch, MAC and complete-snapshot CAS.
  freezeRollbackSnapshotJson(nextStore);
  const nextRollbackState = makeEpochBootstrapState(
    runtime.policy,
    runtime.snapshot.rollbackState,
    nextStore,
    finalityResult.state,
    null,
    "canonical_progress",
  );
  const committed = await commitRollbackDurableAuthority(
    input.authority,
    nextStore,
    nextRollbackState,
    nextRollbackState,
    nextRollbackState.stateDigest,
    nextHistory,
  );
  return committed === null
    ? Object.freeze({ persistence: "conflict" })
    : Object.freeze({
        persistence: "committed",
        authority: committed.authority,
        trustedHead: committed.trustedHead,
        finalityResult,
      });
};

/**
 * Applies one W13 transition from the already authenticated, atomically loaded
 * authority, then persists store, journal, bootstrap, and anchor in one
 * expected-prior CAS. A stale/concurrent handle can compute but cannot commit.
 * A committed result is not actionable until its emitted `trustedHead` has
 * been atomically published to the independent monotonic authority.
 */
export const evaluateAndPersistWatcherRollback = async (input: {
  readonly authority: WatcherRollbackDurableAuthority;
  readonly previousFinalityState: unknown;
  readonly consistency: unknown;
  readonly finalityResult: unknown;
  readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
}): Promise<WatcherRollbackDurableEvaluationResult> => {
  const runtime = runtimeForRollbackDurableAuthority(input.authority);
  const ordinary = evaluateWatcherRollbackStep(
    runtime.policy,
    runtime.snapshot.currentStore,
    runtime.snapshot.rollbackState,
    runtime.snapshot.rollbackBootstrapState,
    input.previousFinalityState,
    input.consistency,
    input.finalityResult,
    input.transportAttestations,
  );
  const current = currentRollbackFinalityState(runtime);
  const incoming = (input.consistency as WatcherMultiProviderConsistency | null)
    ?.agreement;
  const crossesReleasedPrefix =
    current.phase === "pending" &&
    current.pending !== null &&
    typeof incoming?.blockNo === "string" &&
    /^(0|[1-9][0-9]{0,19})$/.test(incoming.blockNo) &&
    BigInt(incoming.blockNo) < BigInt(current.pending.blockNo);
  const verified =
    crossesReleasedPrefix && current.finalized === null
      ? reject("replacement_evidence_missing")
      : ordinary;
  if (
    verified.action === "reject" ||
    verified.action === "duplicate_rewind" ||
    verified.nextStore === null ||
    verified.rollbackState === null ||
    verified.rollbackBootstrapState === null
  ) {
    return Object.freeze({
      persistence: "unchanged",
      authority: input.authority,
      trustedHead: makeRollbackDurableTrustedHead(
        runtime.policy,
        runtime.snapshot,
        runtime.snapshotSha256,
        runtime.authenticationKey,
      ),
      result: verified,
    });
  }
  const committed = await commitRollbackDurableAuthority(
    input.authority,
    verified.nextStore,
    verified.rollbackState,
    verified.rollbackBootstrapState,
    verified.trustedCheckpointStateDigest ??
      runtime.snapshot.trustedCheckpointStateDigest,
  );
  return committed === null
    ? Object.freeze({ persistence: "conflict" })
    : Object.freeze({
        persistence: "committed",
        authority: committed.authority,
        trustedHead: committed.trustedHead,
        result: verified,
      });
};

/**
 * Runs post-finality recovery from the backend-owned incident state and
 * atomically installs the recovered store plus the new epoch trust anchor.
 * A committed recovery is not actionable until its emitted `trustedHead` has
 * been atomically published to the independent monotonic authority.
 */
export const evaluateAndPersistWatcherPostFinalityRecovery = async (input: {
  readonly authority: WatcherRollbackDurableAuthority;
  readonly previousCanonicalPath: unknown;
  readonly replacementCanonicalPath: unknown;
  readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
}): Promise<WatcherRollbackDurableRecoveryResult> => {
  const runtime = runtimeForRollbackDurableAuthority(input.authority);
  const recoveryInput: WatcherPostFinalityRecoveryInput = {
    policy: runtime.policy,
    sourceStore: runtime.snapshot.currentStore,
    currentStore: runtime.snapshot.currentStore,
    quarantinedRollbackState: runtime.snapshot.rollbackState,
    rollbackBootstrapState: runtime.snapshot.rollbackBootstrapState,
    trustedCheckpointAuthority: input.authority,
    previousCanonicalPath: input.previousCanonicalPath,
    replacementCanonicalPath: input.replacementCanonicalPath,
    previousRecoveryState: null,
    transportAttestations: input.transportAttestations,
  };
  const result = evaluateWatcherPostFinalityRecovery(recoveryInput);
  const verified = parseWatcherPostFinalityRecoveryResult(
    result,
    recoveryInput,
  );
  if (verified === null) {
    throw new Error("watcher rollback durable recovery verification failed");
  }
  if (
    verified.action !== "rewind_and_replay" ||
    verified.nextStore === null ||
    verified.resumableRollbackState === null ||
    verified.resumableRollbackBootstrapState === null ||
    verified.resumableTrustedCheckpointStateDigest === null
  ) {
    return Object.freeze({
      persistence: "unchanged",
      authority: input.authority,
      trustedHead: makeRollbackDurableTrustedHead(
        runtime.policy,
        runtime.snapshot,
        runtime.snapshotSha256,
        runtime.authenticationKey,
      ),
      result: verified,
    });
  }
  const committed = await commitRollbackDurableAuthority(
    input.authority,
    verified.nextStore,
    verified.resumableRollbackState,
    verified.resumableRollbackBootstrapState,
    verified.resumableTrustedCheckpointStateDigest,
  );
  return committed === null
    ? Object.freeze({ persistence: "conflict" })
    : Object.freeze({
        persistence: "committed",
        authority: committed.authority,
        trustedHead: committed.trustedHead,
        result: verified,
      });
};
