import {
  parseWatcherDurableStore,
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import {
  parseWatcherFinalityPolicy,
  parseWatcherFinalityState,
  type WatcherFinalityPolicy,
} from ".././finality-engine.js";
import { reject, sameMarker } from "./records.js";
import { decodeWatcherRollbackStateStructural } from "./state.decode-watcher-rollback-state-structural.js";
import {
  evaluateWatcherRollbackStep,
  replayWatcherRollbackState,
} from "./state.evaluate-watcher-rollback-step.js";
import {
  initialRollbackState,
  storeDigest,
} from "./state.parse-finality-transition.js";
import { rollbackStateBindingFailure } from "./state.plan-rewind.js";
import { AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE } from "./state.verify-persisted-consistency-evidence.js";
import {
  rollbackDurableAuthorityRuntime,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackResult,
  type WatcherRollbackState,
  type WatcherRollbackStateVerificationContext,
} from "./types.js";

/**
 * Creates the only valid zero-transition rollback state. Callers must persist
 * this explicit bootstrap and subsequently persist each returned state; the
 * evaluator never treats null or a compact self-hash as a fresh installation.
 */
export const makeWatcherRollbackBootstrapState = (
  policyInput: unknown,
  bootstrapStoreInput: unknown,
  bootstrapFinalityStateInput: unknown,
): WatcherRollbackState | null => {
  const policy = parseWatcherFinalityPolicy(policyInput);
  if (policy === null) {
    return null;
  }
  try {
    const bootstrapStore = parseWatcherDurableStore(bootstrapStoreInput);
    const bootstrapFinalityState = parseWatcherFinalityState(
      bootstrapFinalityStateInput,
      policy,
    );
    return sameMarker(
      bootstrapStore.deploymentMarker,
      policy.deploymentMarker,
    ) && bootstrapFinalityState !== null
      ? initialRollbackState(policy, bootstrapStore, bootstrapFinalityState)
      : null;
  } catch {
    return null;
  }
};

export const parseRollbackBootstrapStateWithTrustedDigest = (
  policy: WatcherFinalityPolicy,
  value: unknown,
  trustedCheckpointStateDigest: string | null,
  parseStore = parseWatcherDurableStore,
): WatcherRollbackState | null => {
  const candidate = decodeWatcherRollbackStateStructural(value, parseStore);
  if (
    candidate === null ||
    rollbackStateBindingFailure(policy, candidate) !== null ||
    parseWatcherFinalityState(candidate.bootstrapFinalityState, policy) === null
  ) {
    return null;
  }
  const expected = initialRollbackState(
    policy,
    candidate.bootstrapStore,
    candidate.bootstrapFinalityState,
  );
  return candidate.epoch === "0"
    ? watcherSameCanonicalJson(candidate, expected)
      ? candidate
      : null
    : candidate.epochCheckpoint !== null &&
        candidate.transitions.length === 0 &&
        candidate.incident === null &&
        trustedCheckpointStateDigest === candidate.stateDigest
      ? candidate
      : null;
};

const parseRollbackBootstrapState = (
  policy: WatcherFinalityPolicy,
  value: unknown,
  trustedCheckpointAuthorityInput: unknown = undefined,
): WatcherRollbackState | null => {
  const trustedAuthority =
    typeof trustedCheckpointAuthorityInput === "object" &&
    trustedCheckpointAuthorityInput !== null
      ? rollbackDurableAuthorityRuntime.get(
          trustedCheckpointAuthorityInput as WatcherRollbackDurableAuthority,
        )
      : undefined;
  return parseRollbackBootstrapStateWithTrustedDigest(
    policy,
    value,
    trustedAuthority !== undefined &&
      trustedAuthority.policy.policyDigest === policy.policyDigest
      ? trustedAuthority.snapshot.trustedCheckpointStateDigest
      : null,
  );
};

/**
 * Authoritative rollback-state restart parser. It deterministically replays
 * the bounded transition history from a separately persisted explicit
 * bootstrap state and requires every derived intermediate store to lead to
 * `currentStore`.
 */
export const parseWatcherRollbackState = (
  value: unknown,
  context: WatcherRollbackStateVerificationContext,
): WatcherRollbackState | null => {
  const candidate = decodeWatcherRollbackStateStructural(value);
  const policy = parseWatcherFinalityPolicy(context.policy);
  if (candidate === null || policy === null) {
    return null;
  }
  try {
    const bootstrapState = parseRollbackBootstrapState(
      policy,
      context.rollbackBootstrapState,
      context.trustedCheckpointAuthority,
    );
    const currentStore = parseWatcherDurableStore(context.currentStore);
    const authority = context.trustedCheckpointAuthority;
    const trustedRuntime =
      typeof authority === "object" && authority !== null
        ? rollbackDurableAuthorityRuntime.get(
            authority as WatcherRollbackDurableAuthority,
          )
        : undefined;
    // Only the complete already-authenticated snapshot may reuse its admitted
    // transport evidence after restart. Caller-supplied state keeps the live
    // transport validation path, even when it carries matching claimed digests.
    const authenticatedSnapshotEvidence =
      trustedRuntime !== undefined &&
      trustedRuntime.policy.policyDigest === policy.policyDigest &&
      watcherSameCanonicalJson(
        trustedRuntime.snapshot.currentStore,
        currentStore,
      ) &&
      watcherSameCanonicalJson(
        trustedRuntime.snapshot.rollbackState,
        candidate,
      ) &&
      watcherSameCanonicalJson(
        trustedRuntime.snapshot.rollbackBootstrapState,
        context.rollbackBootstrapState,
      )
        ? AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
        : null;
    return bootstrapState === null
      ? null
      : replayWatcherRollbackState(
          policy,
          bootstrapState,
          candidate,
          currentStore,
          context.transportAttestations,
          authenticatedSnapshotEvidence,
        );
  } catch {
    return null;
  }
};

/**
 * Applies exactly one W11→W12 transition to W03's canonical durable store.
 * The explicit prior rollback state is fully replayed from bootstrap before
 * use; null cannot reset the journal after restart.
 */
export const evaluateWatcherRollback = (
  policyInput: unknown,
  storeInput: unknown,
  previousFinalityStateInput: unknown,
  consistencyInput: unknown,
  finalityResultInput: unknown,
  previousRollbackStateInput: unknown,
  rollbackBootstrapStateInput: unknown,
  trustedCheckpointAuthorityInput: unknown = undefined,
  transportAttestationsInput: unknown = [],
): WatcherRollbackResult => {
  const policy = parseWatcherFinalityPolicy(policyInput);
  if (policy === null) {
    return reject(
      "malformed_policy",
      "watcher_rollback_configuration_mismatch",
    );
  }
  let store: WatcherDurableStore;
  try {
    store = parseWatcherDurableStore(storeInput);
  } catch {
    return reject("malformed_store");
  }
  if (!sameMarker(store.deploymentMarker, policy.deploymentMarker)) {
    return reject(
      "deployment_mismatch",
      "watcher_rollback_configuration_mismatch",
    );
  }
  const rollbackStateCandidate = decodeWatcherRollbackStateStructural(
    previousRollbackStateInput,
  );
  if (rollbackStateCandidate === null) {
    return reject(
      "malformed_rollback_state",
      "watcher_rollback_state_rejected",
    );
  }
  const bindingFailure = rollbackStateBindingFailure(
    policy,
    rollbackStateCandidate,
  );
  if (bindingFailure !== null) {
    return reject(bindingFailure, "watcher_rollback_configuration_mismatch");
  }
  if (rollbackStateCandidate.storeDigest !== storeDigest(store)) {
    return reject(
      "rollback_state_store_mismatch",
      "watcher_rollback_state_rejected",
    );
  }
  const rollbackBootstrapState = parseRollbackBootstrapState(
    policy,
    rollbackBootstrapStateInput,
    trustedCheckpointAuthorityInput,
  );
  if (rollbackBootstrapState === null) {
    return reject(
      "malformed_rollback_state",
      "watcher_rollback_state_rejected",
    );
  }
  const rollbackState = replayWatcherRollbackState(
    policy,
    rollbackBootstrapState,
    rollbackStateCandidate,
    store,
    transportAttestationsInput,
  );
  if (rollbackState === null) {
    return reject(
      "malformed_rollback_state",
      "watcher_rollback_state_rejected",
    );
  }
  return evaluateWatcherRollbackStep(
    policy,
    store,
    rollbackState,
    rollbackBootstrapState,
    previousFinalityStateInput,
    consistencyInput,
    finalityResultInput,
    transportAttestationsInput,
  );
};
