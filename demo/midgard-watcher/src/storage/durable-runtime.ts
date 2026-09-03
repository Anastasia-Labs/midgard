import {
  makeWatcherFinalityBootstrapState,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import type {
  WatcherL1TransportAttestationContext,
  WatcherNormalizedL1Block,
} from "../l1/l1-adapter.js";
import type { WatcherMultiProviderConsistency } from "../l1/multi-provider-consistency.js";
import {
  evaluateAndPersistWatcherPostFinalityRecovery,
  evaluateAndPersistWatcherRollback,
  initializeWatcherRollbackDurableAuthority,
  loadWatcherRollbackDurableAuthority,
  persistWatcherRollbackDurableCanonicalProgress,
  persistWatcherRollbackDurableObservation,
  prepareWatcherRollbackDurableTrustedHeadReconciliation,
  readWatcherRollbackDurableAuthority,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableAuthorityRead,
  type WatcherRollbackDurableCanonicalProgressResult,
  type WatcherRollbackDurableEvaluationResult,
  type WatcherRollbackDurableObservationResult,
  type WatcherRollbackDurableRecoveryResult,
  type WatcherRollbackDurableTrustedHead,
} from "../l1/rollback-engine.js";
import type { WatcherTrustedHeadAuthorityClient } from "../runtime/trusted-head-authority.js";
import {
  makeEmptyWatcherDurableStore,
  type WatcherDurableAtomicBackend,
} from "./durable-store.js";

export const WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION =
  "midgard-watcher-production-durable-runtime-v1" as const;

export type WatcherDurableRuntime = Readonly<{
  schemaVersion: typeof WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION;
  read(): WatcherRollbackDurableAuthorityRead;
  persistObservation(input: {
    readonly block: WatcherNormalizedL1Block;
    readonly observations: readonly WatcherNormalizedL1Block[];
    readonly consistency: WatcherMultiProviderConsistency;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  }): Promise<WatcherRollbackDurableObservationResult>;
  persistCanonicalProgress(input: {
    readonly block: WatcherNormalizedL1Block;
    readonly observations: readonly WatcherNormalizedL1Block[];
    readonly consistency: WatcherMultiProviderConsistency;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  }): Promise<WatcherRollbackDurableCanonicalProgressResult>;
  persistRollback(input: {
    readonly previousFinalityState: unknown;
    readonly consistency: unknown;
    readonly finalityResult: unknown;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  }): Promise<WatcherRollbackDurableEvaluationResult>;
  persistPostFinalityRecovery(input: {
    readonly previousCanonicalPath: unknown;
    readonly replacementCanonicalPath: unknown;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  }): Promise<WatcherRollbackDurableRecoveryResult>;
}>;

const sameHead = (
  left: WatcherRollbackDurableTrustedHead | null,
  right: WatcherRollbackDurableTrustedHead | null,
): boolean => JSON.stringify(left) === JSON.stringify(right);

const loadPublishedAuthority = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: WatcherFinalityPolicy;
  readonly authenticationKey: Uint8Array;
  readonly client: WatcherTrustedHeadAuthorityClient;
  readonly expectedHead: WatcherRollbackDurableTrustedHead;
}): Promise<
  Readonly<{
    authority: WatcherRollbackDurableAuthority;
    trustedHead: WatcherRollbackDurableTrustedHead;
  }>
> => {
  const readBack = await input.client.readCurrent();
  if (!sameHead(readBack, input.expectedHead) || readBack === null) {
    throw new Error(
      "watcher trusted-head read-back differs from the durable snapshot",
    );
  }
  const authority = await loadWatcherRollbackDurableAuthority({
    backend: input.backend,
    policy: input.policy,
    authenticationKey: input.authenticationKey,
    trustedHead: readBack,
  });
  return Object.freeze({ authority, trustedHead: readBack });
};

const publishDirectSuccessor = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: WatcherFinalityPolicy;
  readonly authenticationKey: Uint8Array;
  readonly client: WatcherTrustedHeadAuthorityClient;
  readonly expectedHead: WatcherRollbackDurableTrustedHead | null;
  readonly nextHead: WatcherRollbackDurableTrustedHead;
}): Promise<
  Readonly<{
    authority: WatcherRollbackDurableAuthority;
    trustedHead: WatcherRollbackDurableTrustedHead;
  }>
> => {
  if (
    !(await input.client.compareAndSwap({
      expectedTrustedHead: input.expectedHead,
      nextTrustedHead: input.nextHead,
    }))
  ) {
    throw new Error("watcher trusted-head direct-successor CAS conflicted");
  }
  return await loadPublishedAuthority({
    backend: input.backend,
    policy: input.policy,
    authenticationKey: input.authenticationKey,
    client: input.client,
    expectedHead: input.nextHead,
  });
};

/**
 * Reconciles the SQLite snapshot with the independently durable sidecar before
 * returning any actionable capability. The only crash recovery admitted is
 * the authenticated revision-zero head or one exact direct successor.
 */
export const createWatcherDurableRuntime = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: WatcherFinalityPolicy;
  readonly authenticationKey: Uint8Array;
  readonly client: WatcherTrustedHeadAuthorityClient;
}): Promise<WatcherDurableRuntime> => {
  let externallyProtectedHead = await input.client.readCurrent();
  let authority: WatcherRollbackDurableAuthority;
  const stored = await input.backend.read();
  if (stored === null) {
    if (externallyProtectedHead !== null) {
      throw new Error(
        "watcher SQLite snapshot is absent while trusted authority is nonempty",
      );
    }
    const bootstrapFinalityState = makeWatcherFinalityBootstrapState(
      input.policy,
    );
    if (bootstrapFinalityState === null) {
      throw new Error("watcher production finality bootstrap is invalid");
    }
    const initialized = await initializeWatcherRollbackDurableAuthority({
      backend: input.backend,
      policy: input.policy,
      bootstrapStore: makeEmptyWatcherDurableStore(
        input.policy.deploymentMarker,
      ),
      bootstrapFinalityState,
      authenticationKey: input.authenticationKey,
      trustedHead: null,
    });
    const published = await publishDirectSuccessor({
      ...input,
      expectedHead: null,
      nextHead: initialized.trustedHead,
    });
    authority = published.authority;
    externallyProtectedHead = published.trustedHead;
  } else {
    const reconciliation =
      await prepareWatcherRollbackDurableTrustedHeadReconciliation({
        backend: input.backend,
        policy: input.policy,
        authenticationKey: input.authenticationKey,
        trustedHead: externallyProtectedHead,
      });
    if (reconciliation.action === "publish_direct_successor") {
      const published = await publishDirectSuccessor({
        ...input,
        expectedHead: reconciliation.expectedTrustedHead,
        nextHead: reconciliation.nextTrustedHead,
      });
      authority = published.authority;
      externallyProtectedHead = published.trustedHead;
    } else {
      const loaded = await loadPublishedAuthority({
        ...input,
        expectedHead: reconciliation.trustedHead,
      });
      authority = loaded.authority;
      externallyProtectedHead = loaded.trustedHead;
    }
  }

  if (externallyProtectedHead === null) {
    throw new Error("watcher trusted-head authority remained empty");
  }
  let publishedHead: WatcherRollbackDurableTrustedHead =
    externallyProtectedHead;

  let serial = Promise.resolve();
  const serialized = async <Result>(operation: () => Promise<Result>) => {
    const previous = serial;
    let release!: () => void;
    serial = new Promise<void>((resolve) => {
      release = resolve;
    });
    await previous;
    try {
      return await operation();
    } finally {
      release();
    }
  };

  const admitResult = async <
    Result extends
      | WatcherRollbackDurableCanonicalProgressResult
      | WatcherRollbackDurableObservationResult
      | WatcherRollbackDurableEvaluationResult
      | WatcherRollbackDurableRecoveryResult,
  >(
    result: Result,
  ): Promise<Result> => {
    if (result.persistence === "conflict") {
      throw new Error("watcher durable snapshot CAS conflicted");
    }
    if (result.persistence === "committed") {
      const published = await publishDirectSuccessor({
        ...input,
        expectedHead: publishedHead,
        nextHead: result.trustedHead,
      });
      authority = published.authority;
      publishedHead = published.trustedHead;
    } else {
      const loaded = await loadPublishedAuthority({
        ...input,
        expectedHead: publishedHead,
      });
      authority = loaded.authority;
      publishedHead = loaded.trustedHead;
    }
    return result;
  };

  return Object.freeze({
    schemaVersion: WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION,
    read: () => readWatcherRollbackDurableAuthority(authority),
    persistObservation: async (operationInput) =>
      await serialized(
        async () =>
          await admitResult(
            await persistWatcherRollbackDurableObservation({
              authority,
              ...operationInput,
            }),
          ),
      ),
    persistCanonicalProgress: async (operationInput) =>
      await serialized(
        async () =>
          await admitResult(
            await persistWatcherRollbackDurableCanonicalProgress({
              authority,
              ...operationInput,
            }),
          ),
      ),
    persistRollback: async (operationInput) =>
      await serialized(
        async () =>
          await admitResult(
            await evaluateAndPersistWatcherRollback({
              authority,
              ...operationInput,
            }),
          ),
      ),
    persistPostFinalityRecovery: async (operationInput) =>
      await serialized(
        async () =>
          await admitResult(
            await evaluateAndPersistWatcherPostFinalityRecovery({
              authority,
              ...operationInput,
            }),
          ),
      ),
  });
};
