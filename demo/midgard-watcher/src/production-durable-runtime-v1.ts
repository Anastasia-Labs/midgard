import {
  makeEmptyWatcherDurableStoreV1,
  type WatcherDurableAtomicBackend,
} from "./durable-store.js";
import {
  makeWatcherFinalityBootstrapStateV1,
  type WatcherFinalityPolicyV1,
} from "./finality-engine.js";
import type {
  WatcherL1TransportAttestationContextV1,
  WatcherNormalizedL1BlockV1,
} from "./l1-adapter.js";
import type { WatcherMultiProviderConsistencyV1 } from "./multi-provider-consistency.js";
import {
  evaluateAndPersistWatcherPostFinalityRecoveryV1,
  evaluateAndPersistWatcherRollbackV1,
  initializeWatcherRollbackDurableAuthorityV1,
  loadWatcherRollbackDurableAuthorityV1,
  persistWatcherRollbackDurableCanonicalProgressV1,
  persistWatcherRollbackDurableObservationV1,
  prepareWatcherRollbackDurableTrustedHeadReconciliationV1,
  readWatcherRollbackDurableAuthorityV1,
  type WatcherRollbackDurableAuthorityReadV1,
  type WatcherRollbackDurableAuthorityV1,
  type WatcherRollbackDurableCanonicalProgressResultV1,
  type WatcherRollbackDurableEvaluationResultV1,
  type WatcherRollbackDurableObservationResultV1,
  type WatcherRollbackDurableRecoveryResultV1,
  type WatcherRollbackDurableTrustedHeadV1,
} from "./rollback-engine.js";
import type { WatcherTrustedHeadAuthorityClientV1 } from "./trusted-head-authority-v1.js";

export const WATCHER_PRODUCTION_DURABLE_RUNTIME_V1_SCHEMA_VERSION =
  "midgard-watcher-production-durable-runtime-v1" as const;

export type WatcherProductionDurableRuntimeV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_DURABLE_RUNTIME_V1_SCHEMA_VERSION;
  read(): WatcherRollbackDurableAuthorityReadV1;
  persistObservation(input: {
    readonly block: WatcherNormalizedL1BlockV1;
    readonly observations: readonly WatcherNormalizedL1BlockV1[];
    readonly consistency: WatcherMultiProviderConsistencyV1;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
  }): Promise<WatcherRollbackDurableObservationResultV1>;
  persistCanonicalProgress(input: {
    readonly block: WatcherNormalizedL1BlockV1;
    readonly observations: readonly WatcherNormalizedL1BlockV1[];
    readonly consistency: WatcherMultiProviderConsistencyV1;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
  }): Promise<WatcherRollbackDurableCanonicalProgressResultV1>;
  persistRollback(input: {
    readonly previousFinalityState: unknown;
    readonly consistency: unknown;
    readonly finalityResult: unknown;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
  }): Promise<WatcherRollbackDurableEvaluationResultV1>;
  persistPostFinalityRecovery(input: {
    readonly previousCanonicalPath: unknown;
    readonly replacementCanonicalPath: unknown;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
  }): Promise<WatcherRollbackDurableRecoveryResultV1>;
}>;

const sameHead = (
  left: WatcherRollbackDurableTrustedHeadV1 | null,
  right: WatcherRollbackDurableTrustedHeadV1 | null,
): boolean => JSON.stringify(left) === JSON.stringify(right);

const loadPublishedAuthority = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: WatcherFinalityPolicyV1;
  readonly authenticationKey: Uint8Array;
  readonly client: WatcherTrustedHeadAuthorityClientV1;
  readonly expectedHead: WatcherRollbackDurableTrustedHeadV1;
}): Promise<
  Readonly<{
    authority: WatcherRollbackDurableAuthorityV1;
    trustedHead: WatcherRollbackDurableTrustedHeadV1;
  }>
> => {
  const readBack = await input.client.readCurrent();
  if (!sameHead(readBack, input.expectedHead) || readBack === null) {
    throw new Error(
      "watcher trusted-head read-back differs from the durable snapshot",
    );
  }
  const authority = await loadWatcherRollbackDurableAuthorityV1({
    backend: input.backend,
    policy: input.policy,
    authenticationKey: input.authenticationKey,
    trustedHead: readBack,
  });
  return Object.freeze({ authority, trustedHead: readBack });
};

const publishDirectSuccessor = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: WatcherFinalityPolicyV1;
  readonly authenticationKey: Uint8Array;
  readonly client: WatcherTrustedHeadAuthorityClientV1;
  readonly expectedHead: WatcherRollbackDurableTrustedHeadV1 | null;
  readonly nextHead: WatcherRollbackDurableTrustedHeadV1;
}): Promise<
  Readonly<{
    authority: WatcherRollbackDurableAuthorityV1;
    trustedHead: WatcherRollbackDurableTrustedHeadV1;
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
export const createWatcherProductionDurableRuntimeV1 = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: WatcherFinalityPolicyV1;
  readonly authenticationKey: Uint8Array;
  readonly client: WatcherTrustedHeadAuthorityClientV1;
}): Promise<WatcherProductionDurableRuntimeV1> => {
  let externallyProtectedHead = await input.client.readCurrent();
  let authority: WatcherRollbackDurableAuthorityV1;
  const stored = await input.backend.read();
  if (stored === null) {
    if (externallyProtectedHead !== null) {
      throw new Error(
        "watcher SQLite snapshot is absent while trusted authority is nonempty",
      );
    }
    const bootstrapFinalityState = makeWatcherFinalityBootstrapStateV1(
      input.policy,
    );
    if (bootstrapFinalityState === null) {
      throw new Error("watcher production finality bootstrap is invalid");
    }
    const initialized = await initializeWatcherRollbackDurableAuthorityV1({
      backend: input.backend,
      policy: input.policy,
      bootstrapStore: makeEmptyWatcherDurableStoreV1(
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
      await prepareWatcherRollbackDurableTrustedHeadReconciliationV1({
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
  let publishedHead: WatcherRollbackDurableTrustedHeadV1 =
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
      | WatcherRollbackDurableCanonicalProgressResultV1
      | WatcherRollbackDurableObservationResultV1
      | WatcherRollbackDurableEvaluationResultV1
      | WatcherRollbackDurableRecoveryResultV1,
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
    schemaVersion: WATCHER_PRODUCTION_DURABLE_RUNTIME_V1_SCHEMA_VERSION,
    read: () => readWatcherRollbackDurableAuthorityV1(authority),
    persistObservation: async (operationInput) =>
      await serialized(
        async () =>
          await admitResult(
            await persistWatcherRollbackDurableObservationV1({
              authority,
              ...operationInput,
            }),
          ),
      ),
    persistCanonicalProgress: async (operationInput) =>
      await serialized(
        async () =>
          await admitResult(
            await persistWatcherRollbackDurableCanonicalProgressV1({
              authority,
              ...operationInput,
            }),
          ),
      ),
    persistRollback: async (operationInput) =>
      await serialized(
        async () =>
          await admitResult(
            await evaluateAndPersistWatcherRollbackV1({
              authority,
              ...operationInput,
            }),
          ),
      ),
    persistPostFinalityRecovery: async (operationInput) =>
      await serialized(
        async () =>
          await admitResult(
            await evaluateAndPersistWatcherPostFinalityRecoveryV1({
              authority,
              ...operationInput,
            }),
          ),
      ),
  });
};
