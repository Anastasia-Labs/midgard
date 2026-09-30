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
