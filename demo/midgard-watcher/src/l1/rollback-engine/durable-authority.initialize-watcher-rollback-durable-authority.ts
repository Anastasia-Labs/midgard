import {
  compareAndSwapWatcherDurableAtomicSnapshot,
  parseWatcherDurableStore,
  readWatcherDurableAtomicSnapshot,
  watcherCanonicalJson,
  type WatcherDurableAtomicBackend,
} from "../../storage/durable-store.js";
import {
  type WatcherUserEventCheckpoint,
  type WatcherUserEventValidation,
} from "../../storage/user-event-checkpoint.js";
import {
  parseWatcherFinalityPolicy,
  parseWatcherFinalityState,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import {
  decodeRollbackDurableAuthoritySnapshot,
  makeRollbackDurableAuthorityHandle,
  makeRollbackDurableAuthoritySnapshot,
} from "./durable-authority.decode-rollback-durable-authority-snapshot.js";
import {
  assertRollbackDurableTrustedHeadMatches,
  authorityFromEncodedSnapshot,
  makeRollbackDurableTrustedHead,
  parseRollbackDurableTrustedHead,
  runtimeForRollbackDurableAuthority,
} from "./durable-authority.parse-rollback-durable-trusted-head.js";
import { parseRollbackAuthorityAuthenticationKey } from "./durable-authority.rollback-authority-canonical.js";
import { makeWatcherRollbackBootstrapState, storeDigest } from "./state.js";
import {
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableAuthorityOpenResult,
  type WatcherRollbackDurableAuthorityRead,
  type WatcherRollbackDurableAuthorityStatus,
  type WatcherRollbackDurableTrustedHeadReconciliation,
} from "./types.js";

/**
 * Prepares the only safe recovery from a crash between snapshot CAS and
 * external trusted-head publication. The result contains no authority and no
 * protocol decision. The operator must atomically compare-and-swap the
 * independent monotonic authority from `expectedTrustedHead` to
 * `nextTrustedHead`, then call `loadWatcherRollbackDurableAuthority` with
 * the externally read-back head.
 *
 * Recovery is deliberately bounded to an authenticated revision-zero snapshot
 * when the external head is null, or to one authenticated direct successor
 * whose prior snapshot digest is the protected head. A same-database head,
 * an older restored snapshot, a skipped revision, or a divergent successor
 * cannot produce a publication proposal. Re-running after external
 * publication returns `already_aligned`.
 */
export const prepareWatcherRollbackDurableTrustedHeadReconciliation =
  async (input: {
    readonly backend: WatcherDurableAtomicBackend;
    readonly policy: unknown;
    readonly authenticationKey: Uint8Array;
    readonly trustedHead: unknown | null;
  }): Promise<WatcherRollbackDurableTrustedHeadReconciliation> => {
    const policy = parseWatcherFinalityPolicy(input.policy);
    if (policy === null) {
      throw new Error("invalid watcher rollback durable authority policy");
    }
    const authenticationKey = parseRollbackAuthorityAuthenticationKey(
      input.authenticationKey,
    );
    const stored = await readWatcherDurableAtomicSnapshot(input.backend);
    if (stored === null) {
      throw new Error("watcher rollback durable authority missing");
    }
    const snapshot = decodeRollbackDurableAuthoritySnapshot(
      stored.bytes,
      policy,
      authenticationKey,
    );
    if (snapshot === null) {
      throw new Error("invalid watcher rollback durable authority");
    }

    const expectedTrustedHead =
      input.trustedHead === null
        ? null
        : parseRollbackDurableTrustedHead(
            input.trustedHead,
            policy,
            authenticationKey,
          );
    if (
      expectedTrustedHead !== null &&
      expectedTrustedHead.snapshotSha256 === stored.sha256
    ) {
      assertRollbackDurableTrustedHeadMatches(
        expectedTrustedHead,
        snapshot,
        stored.sha256,
      );
      return Object.freeze({
        action: "already_aligned",
        trustedHead: expectedTrustedHead,
      });
    }

    const isDirectSuccessor =
      expectedTrustedHead === null
        ? snapshot.revision === "0" && snapshot.priorSnapshotSha256 === null
        : BigInt(snapshot.revision) ===
            BigInt(expectedTrustedHead.revision) + 1n &&
          snapshot.priorSnapshotSha256 === expectedTrustedHead.snapshotSha256;
    if (!isDirectSuccessor) {
      throw new Error(
        "watcher rollback durable trusted head reconciliation refused",
      );
    }
    return Object.freeze({
      action: "publish_direct_successor",
      expectedTrustedHead,
      nextTrustedHead: makeRollbackDurableTrustedHead(
        policy,
        snapshot,
        stored.sha256,
        authenticationKey,
      ),
    });
  };

/**
 * Creates the epoch-zero authority exactly once. `trustedHead` must be null
 * only for a genuinely empty deployment and must otherwise be the independently
 * protected exact head. Every success emits the head that must be atomically
 * published to non-rollbackable external authority before the returned
 * capability or any resulting protocol decision is used. A same-database row
 * does not satisfy that contract.
 */
export const initializeWatcherRollbackDurableAuthority = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: unknown;
  readonly bootstrapStore: unknown;
  readonly bootstrapFinalityState: unknown;
  readonly authenticationKey: Uint8Array;
  readonly trustedHead: unknown | null;
}): Promise<WatcherRollbackDurableAuthorityOpenResult> => {
  const policy = parseWatcherFinalityPolicy(input.policy);
  if (policy === null) {
    throw new Error("invalid watcher rollback durable authority policy");
  }
  const bootstrapState = makeWatcherRollbackBootstrapState(
    policy,
    input.bootstrapStore,
    input.bootstrapFinalityState,
  );
  if (bootstrapState === null) {
    throw new Error("invalid watcher rollback durable authority bootstrap");
  }
  const bootstrapStore = parseWatcherDurableStore(input.bootstrapStore);
  const authenticationKey = parseRollbackAuthorityAuthenticationKey(
    input.authenticationKey,
  );
  const existing = await readWatcherDurableAtomicSnapshot(input.backend);
  if (existing !== null) {
    if (input.trustedHead === null) {
      throw new Error("watcher rollback durable trusted head required");
    }
    const authority = authorityFromEncodedSnapshot(
      input.backend,
      policy,
      existing.bytes,
      existing.sha256,
      authenticationKey,
      input.trustedHead,
    );
    const runtime = runtimeForRollbackDurableAuthority(authority);
    return Object.freeze({
      initialized: false,
      authority,
      trustedHead: makeRollbackDurableTrustedHead(
        policy,
        runtime.snapshot,
        runtime.snapshotSha256,
        runtime.authenticationKey,
      ),
    });
  }
  if (input.trustedHead !== null) {
    throw new Error("watcher rollback durable authority missing");
  }
  const { snapshot, encoded } = makeRollbackDurableAuthoritySnapshot(
    policy,
    "0",
    null,
    bootstrapStore,
    Object.freeze([]),
    bootstrapState,
    bootstrapState,
    bootstrapState.stateDigest,
    null,
    authenticationKey,
  );
  if (
    decodeRollbackDurableAuthoritySnapshot(
      encoded,
      policy,
      authenticationKey,
      true,
    ) === null
  ) {
    throw new Error("watcher rollback initial validation failed");
  }
  const commit = await compareAndSwapWatcherDurableAtomicSnapshot({
    backend: input.backend,
    expectedSha256: null,
    next: encoded,
    canonicalValue: snapshot,
  });
  if (!commit.committed) {
    throw new Error("watcher rollback durable authority conflict");
  }
  const authority = makeRollbackDurableAuthorityHandle(
    Object.freeze({
      backend: input.backend,
      policy,
      snapshot,
      encoded,
      snapshotSha256: commit.sha256,
      authenticationKey,
    }),
  );
  return Object.freeze({
    initialized: true,
    authority,
    trustedHead: makeRollbackDurableTrustedHead(
      policy,
      snapshot,
      commit.sha256,
      authenticationKey,
    ),
  });
};

export const watcherRollbackDurableAuthorityStatus = (
  authority: WatcherRollbackDurableAuthority,
): WatcherRollbackDurableAuthorityStatus => {
  const runtime = runtimeForRollbackDurableAuthority(authority);
  const { snapshot } = runtime;
  return Object.freeze({
    revision: snapshot.revision,
    snapshotSha256: runtime.snapshotSha256,
    authorityDigest: snapshot.authorityDigest,
    priorSnapshotSha256: snapshot.priorSnapshotSha256,
    storeDigest: storeDigest(snapshot.currentStore),
    rollbackStateDigest: snapshot.rollbackState.stateDigest,
    rollbackBootstrapStateDigest: snapshot.rollbackBootstrapState.stateDigest,
    trustedCheckpointStateDigest: snapshot.trustedCheckpointStateDigest,
    authenticationKeyId: snapshot.authenticationKeyId,
    epoch: snapshot.rollbackState.epoch,
    transitionCount: snapshot.rollbackState.transitionCount,
    incidentDigest: snapshot.rollbackState.incident?.incidentDigest ?? null,
  });
};

/** Returns detached finality state without projecting the unrelated store and
 * consistency history. The opaque handle still owns the admitted snapshot. */
export const readWatcherRollbackDurableFinalityState = (
  authority: WatcherRollbackDurableAuthority,
): WatcherFinalityState => {
  const runtime = runtimeForRollbackDurableAuthority(authority);
  const lastTransition = runtime.snapshot.rollbackState.transitions.at(-1);
  const currentInput =
    lastTransition?.finalityResult.state ??
    runtime.snapshot.rollbackState.bootstrapFinalityState;
  const currentFinalityState = parseWatcherFinalityState(
    JSON.parse(watcherCanonicalJson(currentInput)) as unknown,
    runtime.policy,
  );
  if (currentFinalityState === null) {
    throw new Error("watcher rollback durable current finality state invalid");
  }
  return currentFinalityState;
};

/**
 * Returns detached, re-parsed protocol state from an opaque durable authority.
 * Mutating the returned projection cannot mutate or authorize the underlying
 * CAS snapshot; every subsequent transition still requires the WeakMap-backed
 * authority handle.
 */
export const readWatcherRollbackDurableAuthority = (
  authority: WatcherRollbackDurableAuthority,
): WatcherRollbackDurableAuthorityRead => {
  const runtime = runtimeForRollbackDurableAuthority(authority);
  // This privately owned snapshot already carries durable validation. Reads
  // need a detached projection, not another validation of every stored record.
  const store = structuredClone(runtime.snapshot.currentStore);
  return Object.freeze({
    currentStore: store,
    currentFinalityState: readWatcherRollbackDurableFinalityState(authority),
    authenticatedConsistencyHistory: Object.freeze(
      JSON.parse(
        watcherCanonicalJson(runtime.snapshot.consistencyHistory),
      ) as WatcherMultiProviderConsistency[],
    ),
  });
};

/** Package-internal structural read; archive and published-head checks belong
 * to the serialized durable runtime before it issues a protected receipt. */
export const readWatcherRollbackDurableUserEventCheckpoint = (
  authority: WatcherRollbackDurableAuthority,
): WatcherUserEventCheckpoint | null =>
  runtimeForRollbackDurableAuthority(authority).snapshot.userEventCheckpoint;

export const readWatcherRollbackDurableUserEventValidation = (
  authority: WatcherRollbackDurableAuthority,
): WatcherUserEventValidation | null =>
  runtimeForRollbackDurableAuthority(authority).snapshot.userEventValidation;
