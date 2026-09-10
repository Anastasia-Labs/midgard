import {
  makeWatcherFinalityBootstrapState,
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
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
  persistWatcherRollbackDurableUserEventCheckpoint,
  prepareWatcherRollbackDurableTrustedHeadReconciliation,
  readWatcherRollbackDurableAuthority,
  readWatcherRollbackDurableFinalityState,
  readWatcherRollbackDurableUserEventCheckpoint,
  readWatcherRollbackDurableUserEventValidation,
  revalidateWatcherRollbackDurableAuthority,
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
import {
  parseWatcherUserEventCheckpoint,
  readWatcherUserEventCheckpointPayload,
  type WatcherUserEventArchive,
  type WatcherUserEventCheckpoint,
  type WatcherUserEventCheckpointExpectation,
  type WatcherUserEventValidation,
} from "./user-event-checkpoint.js";

export const WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION =
  "midgard-watcher-production-durable-runtime-v1" as const;

export type WatcherDurableRuntime = Readonly<{
  schemaVersion: typeof WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION;
  read(): WatcherRollbackDurableAuthorityRead;
  readFinality(): WatcherFinalityState;
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

/** Process-local proof of structural publication only; never W15 authority. */
export type WatcherProtectedUserEventCheckpoint = Readonly<{
  schemaVersion: "midgard-watcher-protected-user-event-checkpoint-v1";
}>;

export type WatcherProtectedUserEventCheckpointRead = Readonly<{
  checkpoint: WatcherUserEventCheckpoint | null;
  payload: Uint8Array | null;
  validation: WatcherUserEventValidation | null;
  trustedHead: WatcherRollbackDurableTrustedHead;
}>;

type CheckpointPersistenceInput = WatcherUserEventCheckpointExpectation &
  Readonly<{ nextCheckpoint: unknown; validationCandidate?: unknown }>;
type CheckpointPersistenceResult = Readonly<{
  persistence: "committed" | "unchanged";
  protectedCheckpoint: WatcherProtectedUserEventCheckpoint;
}>;
const checkpointOperations = new WeakMap<
  WatcherDurableRuntime,
  Readonly<{
    read(): Promise<WatcherProtectedUserEventCheckpoint>;
    persist(
      input: CheckpointPersistenceInput,
    ): Promise<CheckpointPersistenceResult>;
  }>
>();
const protectedCheckpoints = new WeakMap<
  WatcherProtectedUserEventCheckpoint,
  Readonly<{
    value: WatcherProtectedUserEventCheckpointRead;
    assertCurrent(): void;
  }>
>();

/** Package-internal helper: shares the global runtime's serializer. */
export const readWatcherProtectedUserEventCheckpoint = async (
  runtime: WatcherDurableRuntime,
): Promise<WatcherProtectedUserEventCheckpoint> => {
  const operations = checkpointOperations.get(runtime);
  if (operations === undefined) {
    throw new Error("watcher checkpoint runtime was not admitted");
  }
  return await operations.read();
};

/** Package-internal structural CAS; the upper owner supplies semantic admission. */
export const persistWatcherUserEventCheckpoint = async (
  runtime: WatcherDurableRuntime,
  input: CheckpointPersistenceInput,
): Promise<CheckpointPersistenceResult> => {
  const operations = checkpointOperations.get(runtime);
  if (operations === undefined) {
    throw new Error("watcher checkpoint runtime was not admitted");
  }
  return await operations.persist(input);
};

export const readWatcherProtectedUserEventCheckpointReceipt = (
  receipt: WatcherProtectedUserEventCheckpoint,
): WatcherProtectedUserEventCheckpointRead => {
  const state = protectedCheckpoints.get(receipt);
  if (state === undefined) {
    throw new Error("watcher protected checkpoint receipt was not admitted");
  }
  state.assertCurrent();
  return Object.freeze({
    ...state.value,
    payload:
      state.value.payload === null
        ? null
        : Uint8Array.from(state.value.payload),
  });
};

const readCheckpointArchive = async (
  authority: WatcherRollbackDurableAuthority,
  archive: WatcherUserEventArchive | undefined,
): Promise<
  Readonly<{
    checkpoint: WatcherUserEventCheckpoint | null;
    payload: Uint8Array | null;
  }>
> => {
  const checkpoint = readWatcherRollbackDurableUserEventCheckpoint(authority);
  if (checkpoint === null) return Object.freeze({ checkpoint, payload: null });
  if (archive === undefined) {
    throw new Error(
      "watcher persisted user-event checkpoint requires its archive",
    );
  }
  const payload = await readWatcherUserEventCheckpointPayload(
    checkpoint,
    archive,
  );
  return Object.freeze({ checkpoint, payload });
};

const sameHead = (
  left: WatcherRollbackDurableTrustedHead | null,
  right: WatcherRollbackDurableTrustedHead | null,
): boolean => JSON.stringify(left) === JSON.stringify(right);

type PublishedAuthority = Readonly<{
  authority: WatcherRollbackDurableAuthority;
  trustedHead: WatcherRollbackDurableTrustedHead;
  checkpoint: WatcherUserEventCheckpoint | null;
  payload: Uint8Array | null;
}>;

const loadPublishedAuthority = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: WatcherFinalityPolicy;
  readonly authenticationKey: Uint8Array;
  readonly client: WatcherTrustedHeadAuthorityClient;
  readonly expectedHead: WatcherRollbackDurableTrustedHead;
  readonly admittedAuthority?: WatcherRollbackDurableAuthority;
  readonly userEventArchive?: WatcherUserEventArchive;
}): Promise<PublishedAuthority> => {
  const readBack = await input.client.readCurrent();
  if (!sameHead(readBack, input.expectedHead) || readBack === null) {
    throw new Error(
      "watcher trusted-head read-back differs from the durable snapshot",
    );
  }
  const authority =
    input.admittedAuthority === undefined
      ? await loadWatcherRollbackDurableAuthority({
          backend: input.backend,
          policy: input.policy,
          authenticationKey: input.authenticationKey,
          trustedHead: readBack,
        })
      : await revalidateWatcherRollbackDurableAuthority({
          authority: input.admittedAuthority,
          trustedHead: readBack,
        });
  const checkpoint = await readCheckpointArchive(
    authority,
    input.userEventArchive,
  );
  // Archive reads yield outside this process. Revalidate both durable owners
  // after those reads before returning a protected checkpoint projection.
  const currentAuthority = await revalidateWatcherRollbackDurableAuthority({
    authority,
    trustedHead: readBack,
  });
  if (!sameHead(await input.client.readCurrent(), readBack)) {
    throw new Error(
      "watcher trusted-head changed during checkpoint archive validation",
    );
  }
  return Object.freeze({
    authority: currentAuthority,
    trustedHead: readBack,
    ...checkpoint,
  });
};

const publishDirectSuccessor = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: WatcherFinalityPolicy;
  readonly authenticationKey: Uint8Array;
  readonly client: WatcherTrustedHeadAuthorityClient;
  readonly expectedHead: WatcherRollbackDurableTrustedHead | null;
  readonly nextHead: WatcherRollbackDurableTrustedHead;
  readonly admittedAuthority?: WatcherRollbackDurableAuthority;
  readonly userEventArchive?: WatcherUserEventArchive;
}): Promise<PublishedAuthority> => {
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
    admittedAuthority: input.admittedAuthority,
    expectedHead: input.nextHead,
    ...(input.userEventArchive === undefined
      ? {}
      : { userEventArchive: input.userEventArchive }),
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
  readonly userEventArchive?: WatcherUserEventArchive;
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
  let checkpointFailureGeneration = 0;
  const serialized = async <Result>(operation: () => Promise<Result>) => {
    const previous = serial;
    let release!: () => void;
    serial = new Promise<void>((resolve) => {
      release = resolve;
    });
    await previous;
    try {
      return await operation();
    } catch (error) {
      checkpointFailureGeneration += 1;
      throw error;
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
  ): Promise<Readonly<{ result: Result; published: PublishedAuthority }>> => {
    if (result.persistence === "conflict") {
      throw new Error("watcher durable snapshot CAS conflicted");
    }
    const published =
      result.persistence === "committed"
        ? await publishDirectSuccessor({
            ...input,
            expectedHead: publishedHead,
            nextHead: result.trustedHead,
            admittedAuthority: result.authority,
          })
        : await loadPublishedAuthority({
            ...input,
            expectedHead: publishedHead,
            admittedAuthority: authority,
          });
    authority = published.authority;
    publishedHead = published.trustedHead;
    return Object.freeze({ result, published });
  };

  const runtime: WatcherDurableRuntime = Object.freeze({
    schemaVersion: WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION,
    read: () => readWatcherRollbackDurableAuthority(authority),
    readFinality: () => readWatcherRollbackDurableFinalityState(authority),
    persistObservation: async (operationInput) =>
      await serialized(
        async () =>
          (
            await admitResult(
              await persistWatcherRollbackDurableObservation({
                authority,
                ...operationInput,
              }),
            )
          ).result,
      ),
    persistCanonicalProgress: async (operationInput) =>
      await serialized(
        async () =>
          (
            await admitResult(
              await persistWatcherRollbackDurableCanonicalProgress({
                authority,
                ...operationInput,
              }),
            )
          ).result,
      ),
    persistRollback: async (operationInput) =>
      await serialized(
        async () =>
          (
            await admitResult(
              await evaluateAndPersistWatcherRollback({
                authority,
                ...operationInput,
              }),
            )
          ).result,
      ),
    persistPostFinalityRecovery: async (operationInput) =>
      await serialized(
        async () =>
          (
            await admitResult(
              await evaluateAndPersistWatcherPostFinalityRecovery({
                authority,
                ...operationInput,
              }),
            )
          ).result,
      ),
  });

  const protectPublication = (
    loaded: PublishedAuthority,
  ): WatcherProtectedUserEventCheckpoint => {
    const value = Object.freeze({
      checkpoint: loaded.checkpoint,
      payload: loaded.payload,
      validation: readWatcherRollbackDurableUserEventValidation(
        loaded.authority,
      ),
    });
    const capturedFailureGeneration = checkpointFailureGeneration;
    const capturedDigest = value.checkpoint?.checkpointDigest ?? null;
    const receipt = Object.freeze({
      schemaVersion:
        "midgard-watcher-protected-user-event-checkpoint-v1" as const,
    });
    const trustedHead = Object.freeze({
      ...loaded.trustedHead,
      deploymentMarker: Object.freeze({
        ...loaded.trustedHead.deploymentMarker,
      }),
    });
    protectedCheckpoints.set(
      receipt,
      Object.freeze({
        value: Object.freeze({ ...value, trustedHead }),
        assertCurrent: () => {
          if (
            capturedFailureGeneration !== checkpointFailureGeneration ||
            capturedDigest !==
              (readWatcherRollbackDurableUserEventCheckpoint(authority)
                ?.checkpointDigest ?? null)
          ) {
            throw new Error("watcher protected checkpoint receipt is stale");
          }
        },
      }),
    );
    return receipt;
  };
  const protectedRead =
    async (): Promise<WatcherProtectedUserEventCheckpoint> => {
      const loaded = await loadPublishedAuthority({
        ...input,
        expectedHead: publishedHead,
        admittedAuthority: authority,
      });
      authority = loaded.authority;
      return protectPublication(loaded);
    };
  checkpointOperations.set(
    runtime,
    Object.freeze({
      read: async () => await serialized(protectedRead),
      persist: async (operationInput) => {
        const validationCandidate = operationInput.validationCandidate;
        // Capture caller-owned input before waiting for the shared serializer.
        let nextCheckpoint: WatcherUserEventCheckpoint;
        try {
          nextCheckpoint = parseWatcherUserEventCheckpoint(
            operationInput.nextCheckpoint,
            {
              deploymentMarker: input.policy.deploymentMarker,
              network: input.policy.network,
              blueprintHash: input.policy.blueprintHash,
              finalityPolicyDigest: input.policy.policyDigest,
            },
          );
        } catch (error) {
          checkpointFailureGeneration += 1;
          throw error;
        }
        const expectation = Object.freeze({
          expectedCheckpointDigest: operationInput.expectedCheckpointDigest,
          expectedCheckpointSequence: operationInput.expectedCheckpointSequence,
        });
        return await serialized(async () => {
          if (input.userEventArchive === undefined) {
            throw new Error(
              "watcher user-event checkpoint persistence requires its archive",
            );
          }
          const { result, published } = await admitResult(
            await persistWatcherRollbackDurableUserEventCheckpoint({
              authority,
              archive: input.userEventArchive,
              ...expectation,
              nextCheckpoint,
              validationCandidate,
            }),
          );
          if (result.persistence === "conflict") {
            throw new Error("watcher user-event checkpoint CAS conflicted");
          }
          return Object.freeze({
            persistence: result.persistence,
            // Publication already validated the archive and both durable owners.
            // Mint synchronously while this operation still holds the serializer.
            protectedCheckpoint: protectPublication(published),
          });
        });
      },
    }),
  );
  return runtime;
};
