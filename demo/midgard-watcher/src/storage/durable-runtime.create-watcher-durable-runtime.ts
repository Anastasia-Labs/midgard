import { readWatcherLocalUserEventValidation } from "../indexers/user-event-indexer.js";
import {
  makeWatcherFinalityBootstrapState,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import {
  evaluateAndPersistWatcherPostFinalityRecovery,
  evaluateAndPersistWatcherRollback,
  initializeWatcherRollbackDurableAuthority,
  persistWatcherRollbackDurableCanonicalProgress,
  persistWatcherRollbackDurableObservation,
  persistWatcherRollbackDurableObservations,
  persistWatcherRollbackDurableUserEventCheckpoint,
  prepareWatcherRollbackDurableTrustedHeadReconciliation,
  readWatcherRollbackDurableAuthority,
  readWatcherRollbackDurableFinalityState,
  readWatcherRollbackDurableUserEventCheckpoint,
  readWatcherRollbackDurableUserEventValidation,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableCanonicalProgressResult,
  type WatcherRollbackDurableEvaluationResult,
  type WatcherRollbackDurableObservationResult,
  type WatcherRollbackDurableRecoveryResult,
  type WatcherRollbackDurableTrustedHead,
} from "../l1/rollback-engine.js";
import type { WatcherTrustedHeadAuthorityClient } from "../runtime/trusted-head-authority.js";
import {
  checkpointOperations,
  loadPublishedAuthority,
  protectedCheckpoints,
  publishDirectSuccessor,
  type PublishedAuthority,
  WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION,
  WatcherDurableAuthorityConflict,
  type WatcherDurableRuntime,
  type WatcherProtectedUserEventCheckpoint,
} from "./durable-runtime.load-published-authority.js";
import {
  makeEmptyWatcherDurableStore,
  type WatcherDurableAtomicBackend,
} from "./durable-store.js";
import {
  parseWatcherUserEventCheckpoint,
  type WatcherUserEventArchive,
  type WatcherUserEventCheckpoint,
} from "./user-event-checkpoint.js";

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
  let writeGuard: (() => void) | undefined;
  let writeFailure: Error | undefined;
  const readWriteFailure = (): Error | undefined => writeFailure;
  let publishedHead: WatcherRollbackDurableTrustedHead | undefined;
  let authority: WatcherRollbackDurableAuthority;
  const backend: WatcherDurableAtomicBackend = Object.freeze({
    ...input.backend,
    read: () => input.backend.read(),
    compareAndSwap: async (expectedSha256, next, canonicalValue) => {
      try {
        // The authority may have changed while observations or archive reads awaited.
        if (publishedHead !== undefined) {
          try {
            await loadPublishedAuthority({
              ...input,
              expectedHead: publishedHead,
              admittedAuthority: authority,
            });
          } catch (error) {
            throw new WatcherDurableAuthorityConflict(
              "watcher published authority changed at CAS",
              { cause: error },
            );
          }
        }
        writeGuard?.();
        return await input.backend.compareAndSwap(
          expectedSha256,
          next,
          canonicalValue,
        );
      } catch (error) {
        writeFailure =
          error instanceof Error
            ? error
            : new Error("watcher durable backend failed", { cause: error });
        throw writeFailure;
      }
    },
  });
  const runtimeInput = { ...input, backend };
  let externallyProtectedHead = await input.client.readCurrent();
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
      backend,
      policy: input.policy,
      bootstrapStore: makeEmptyWatcherDurableStore(
        input.policy.deploymentMarker,
      ),
      bootstrapFinalityState,
      authenticationKey: input.authenticationKey,
      trustedHead: null,
    });
    const published = await publishDirectSuccessor({
      ...runtimeInput,
      expectedHead: null,
      nextHead: initialized.trustedHead,
    });
    authority = published.authority;
    externallyProtectedHead = published.trustedHead;
  } else {
    const reconciliation =
      await prepareWatcherRollbackDurableTrustedHeadReconciliation({
        backend,
        policy: input.policy,
        authenticationKey: input.authenticationKey,
        trustedHead: externallyProtectedHead,
      });
    if (reconciliation.action === "publish_direct_successor") {
      const published = await publishDirectSuccessor({
        ...runtimeInput,
        expectedHead: reconciliation.expectedTrustedHead,
        nextHead: reconciliation.nextTrustedHead,
      });
      authority = published.authority;
      externallyProtectedHead = published.trustedHead;
    } else {
      const loaded = await loadPublishedAuthority({
        ...runtimeInput,
        expectedHead: reconciliation.trustedHead,
      });
      authority = loaded.authority;
      externallyProtectedHead = loaded.trustedHead;
    }
  }

  if (externallyProtectedHead === null) {
    throw new Error("watcher trusted-head authority remained empty");
  }
  publishedHead = externallyProtectedHead;

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
      throw new WatcherDurableAuthorityConflict(
        "watcher durable snapshot CAS conflicted",
      );
    }
    if (publishedHead === undefined)
      throw new Error("watcher authority is unpublished");
    let published: PublishedAuthority;
    try {
      published =
        result.persistence === "committed"
          ? await publishDirectSuccessor({
              ...runtimeInput,
              expectedHead: publishedHead,
              nextHead: result.trustedHead,
              admittedAuthority: result.authority,
            })
          : await loadPublishedAuthority({
              ...runtimeInput,
              expectedHead: publishedHead,
              admittedAuthority: authority,
            });
    } catch (error) {
      throw new WatcherDurableAuthorityConflict(
        `watcher durable publication could not be authenticated: ${error instanceof Error ? error.message : "publication failed"}`,
        { cause: error },
      );
    }
    authority = published.authority;
    publishedHead = published.trustedHead;
    return Object.freeze({ result, published });
  };

  const guarded = async <Result>(
    guard: (() => void) | undefined,
    work: () => Promise<Result>,
  ): Promise<Result> => {
    if (publishedHead === undefined)
      throw new Error("watcher authority is unpublished");
    try {
      await loadPublishedAuthority({
        ...runtimeInput,
        expectedHead: publishedHead,
        admittedAuthority: authority,
      });
    } catch (error) {
      throw new WatcherDurableAuthorityConflict(
        "watcher published authority changed before write",
        { cause: error },
      );
    }
    guard?.();
    writeGuard = guard;
    writeFailure = undefined;
    try {
      const result = await work();
      guard?.();
      return result;
    } catch (error) {
      const backendFailure = readWriteFailure();
      if (backendFailure !== undefined) throw backendFailure;
      throw error;
    } finally {
      writeGuard = undefined;
      writeFailure = undefined;
    }
  };
  const runtime: WatcherDurableRuntime = Object.freeze({
    reconcile: () =>
      serialized(async () => {
        const head = await input.client.readCurrent();
        const plan =
          await prepareWatcherRollbackDurableTrustedHeadReconciliation({
            ...runtimeInput,
            trustedHead: head,
          });
        const loaded =
          plan.action === "publish_direct_successor"
            ? await publishDirectSuccessor({
                ...runtimeInput,
                expectedHead: plan.expectedTrustedHead,
                nextHead: plan.nextTrustedHead,
              })
            : await loadPublishedAuthority({
                ...runtimeInput,
                expectedHead: plan.trustedHead,
              });
        authority = loaded.authority;
        publishedHead = loaded.trustedHead;
        checkpointFailureGeneration += 1;
      }),
    schemaVersion: WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION,
    read: () => readWatcherRollbackDurableAuthority(authority),
    readFinality: () => readWatcherRollbackDurableFinalityState(authority),
    persistObservation: async (operationInput) =>
      await serialized(
        async () =>
          await guarded(
            operationInput.assertCurrent,
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
      ),
    persistObservations: async ({ assertCurrent, entries }) =>
      await serialized(
        async () =>
          await guarded(
            assertCurrent,
            async () =>
              (
                await admitResult(
                  await persistWatcherRollbackDurableObservations({
                    authority,
                    entries,
                  }),
                )
              ).result,
          ),
      ),
    persistCanonicalProgress: async (operationInput) =>
      await serialized(
        async () =>
          await guarded(
            operationInput.assertCurrent,
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
      ),
    persistRollback: async (operationInput) =>
      await serialized(
        async () =>
          await guarded(
            operationInput.assertCurrent,
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
      ),
    persistPostFinalityRecovery: async (operationInput) =>
      await serialized(
        async () =>
          await guarded(
            operationInput.assertCurrent,
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
      if (publishedHead === undefined)
        throw new Error("watcher authority is unpublished");
      const loaded = await loadPublishedAuthority({
        ...runtimeInput,
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
        return await serialized(
          async () =>
            await guarded(
              validationCandidate === undefined
                ? undefined
                : () => {
                    readWatcherLocalUserEventValidation(
                      validationCandidate,
                      nextCheckpoint,
                    );
                  },
              async () => {
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
                  throw new Error(
                    "watcher user-event checkpoint CAS conflicted",
                  );
                }
                return Object.freeze({
                  persistence: result.persistence,
                  // Publication already validated the archive and both durable owners.
                  // Mint synchronously while this operation still holds the serializer.
                  protectedCheckpoint: protectPublication(published),
                });
              },
            ),
        );
      },
    }),
  );
  return runtime;
};
