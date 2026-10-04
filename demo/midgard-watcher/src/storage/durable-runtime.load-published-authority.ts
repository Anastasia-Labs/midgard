import {
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from "../l1/finality-engine.js";
import type {
  WatcherL1TransportAttestationContext,
  WatcherNormalizedL1Block,
} from "../l1/l1-adapter.js";
import type { WatcherMultiProviderConsistency } from "../l1/multi-provider-consistency.js";
import {
  loadWatcherRollbackDurableAuthority,
  readWatcherRollbackDurableUserEventCheckpoint,
  revalidateWatcherRollbackDurableAuthority,
  type WatcherRollbackCanonicalAncestryLink,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableAuthorityRead,
  type WatcherRollbackDurableCanonicalProgressResult,
  type WatcherRollbackDurableEvaluationResult,
  type WatcherRollbackDurableObservationResult,
  type WatcherRollbackDurableRecoveryResult,
  type WatcherRollbackDurableTrustedHead,
} from "../l1/rollback-engine.js";
import type { WatcherTrustedHeadAuthorityClient } from "../runtime/trusted-head-authority.js";
import { type WatcherDurableAtomicBackend } from "./durable-store.js";
import {
  readWatcherUserEventCheckpointPayload,
  type WatcherUserEventArchive,
  type WatcherUserEventCheckpoint,
  type WatcherUserEventCheckpointExpectation,
  type WatcherUserEventValidation,
} from "./user-event-checkpoint.js";

export const WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION =
  "midgard-watcher-production-durable-runtime-v1" as const;

/** A live coordinator must reauthenticate both owners before retrying. */
export class WatcherDurableAuthorityConflict extends Error {}

export type WatcherDurableRuntime = Readonly<{
  schemaVersion: typeof WATCHER_DURABLE_RUNTIME_SCHEMA_VERSION;
  read(): WatcherRollbackDurableAuthorityRead;
  readFinality(): WatcherFinalityState;
  /** Reauthenticate both durable owners; admits only an exact direct successor. */
  reconcile?(): Promise<void>;
  persistObservation(input: {
    readonly assertCurrent?: () => void;
    readonly block: WatcherNormalizedL1Block;
    readonly observations: readonly WatcherNormalizedL1Block[];
    readonly consistency: WatcherMultiProviderConsistency;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  }): Promise<WatcherRollbackDurableObservationResult>;
  persistCanonicalProgress(input: {
    readonly assertCurrent?: () => void;
    readonly block: WatcherNormalizedL1Block;
    readonly observations: readonly WatcherNormalizedL1Block[];
    readonly consistency: WatcherMultiProviderConsistency;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
    readonly ancestry?: readonly WatcherRollbackCanonicalAncestryLink[];
  }): Promise<WatcherRollbackDurableCanonicalProgressResult>;
  persistRollback(input: {
    readonly assertCurrent?: () => void;
    readonly previousFinalityState: unknown;
    readonly consistency: unknown;
    readonly finalityResult: unknown;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  }): Promise<WatcherRollbackDurableEvaluationResult>;
  persistPostFinalityRecovery(input: {
    readonly assertCurrent?: () => void;
    readonly previousCanonicalPath: unknown;
    readonly replacementCanonicalPath: unknown;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  }): Promise<WatcherRollbackDurableRecoveryResult>;
}>;

/** Process-local proof of structural publication only; never user-event admission authority. */
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

export const checkpointOperations = new WeakMap<
  WatcherDurableRuntime,
  Readonly<{
    read(): Promise<WatcherProtectedUserEventCheckpoint>;
    persist(
      input: CheckpointPersistenceInput,
    ): Promise<CheckpointPersistenceResult>;
  }>
>();

export const protectedCheckpoints = new WeakMap<
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

export type PublishedAuthority = Readonly<{
  authority: WatcherRollbackDurableAuthority;
  trustedHead: WatcherRollbackDurableTrustedHead;
  checkpoint: WatcherUserEventCheckpoint | null;
  payload: Uint8Array | null;
}>;

export const loadPublishedAuthority = async (input: {
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

export const publishDirectSuccessor = async (input: {
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
    throw new WatcherDurableAuthorityConflict(
      "watcher trusted-head direct-successor CAS conflicted",
    );
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
