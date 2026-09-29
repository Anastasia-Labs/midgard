import { createHash, createHmac, timingSafeEqual } from "node:crypto";

import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { readWatcherLocalUserEventValidation } from "../../indexers/user-event-indexer.js";
import {
  compareAndSwapWatcherDurableAtomicSnapshot,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  parseWatcherDurableStore,
  readWatcherDurableAtomicSnapshot,
  watcherCanonicalJson,
  type WatcherDurableAtomicBackend,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import {
  assertWatcherUserEventCheckpointSuccessor,
  parseWatcherUserEventCheckpoint,
  readWatcherUserEventCheckpointPayload,
  type WatcherUserEventArchive,
  type WatcherUserEventCheckpoint,
  type WatcherUserEventCheckpointExpectation,
  watcherUserEventCheckpointExpectationMatches,
  type WatcherUserEventValidation,
} from "../../storage/user-event-checkpoint.js";
import {
  evaluateWatcherFinality,
  makeWatcherFinalityBootstrapState,
  parseWatcherFinalityPolicy,
  parseWatcherFinalityState,
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import {
  encodeWatcherNormalizedL1Block,
  isWatcherL1BlockAttestedBy,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from ".././l1-adapter.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import {
  exactArray,
  exactPlainRecord,
  sameMarker,
  sameStrings,
} from "./records.js";
import {
  evaluateWatcherPostFinalityRecovery,
  parseWatcherPostFinalityRecoveryResult,
} from "./recovery.js";
import {
  AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE,
  decodeWatcherRollbackStateStructural,
  evaluateWatcherRollbackStep,
  indexPersistedObservations,
  makeEpochBootstrapState,
  makeWatcherRollbackBootstrapState,
  parseRollbackBootstrapStateWithTrustedDigest,
  type PersistedObservationIndexEntry,
  replayWatcherRollbackState,
  sorted,
  storeDigest,
  verifyPersistedConsistencyEvidence,
} from "./state.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  ROLLBACK_AUTHORITY_KEY_BYTES,
  ROLLBACK_VALIDATION_SCHEMA_VERSION,
  rollbackDurableAuthorityRuntime,
  sha256Canonical,
  WATCHER_ROLLBACK_DURABLE_AUTHORITY_HANDLE_SCHEMA_VERSION,
  WATCHER_ROLLBACK_DURABLE_AUTHORITY_SCHEMA_VERSION,
  WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
  type WatcherPostFinalityRecoveryInput,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableAuthorityOpenResult,
  type WatcherRollbackDurableAuthorityRead,
  type WatcherRollbackDurableAuthorityRuntime,
  type WatcherRollbackDurableAuthoritySnapshot,
  type WatcherRollbackDurableAuthorityStatus,
  type WatcherRollbackDurableCanonicalProgressResult,
  type WatcherRollbackDurableEvaluationResult,
  type WatcherRollbackDurableObservationResult,
  type WatcherRollbackDurableRecoveryResult,
  type WatcherRollbackDurableTrustedHead,
  type WatcherRollbackDurableTrustedHeadReconciliation,
  type WatcherRollbackState,
} from "./types.js";

const rollbackAuthorityEncoder = new TextEncoder();
const rollbackAuthorityDecoder = new TextDecoder("utf-8", { fatal: true });
const ownedRollbackSnapshotJson = new WeakSet<object>();

// Only used on process-owned JSON (decoded bytes or freshly parsed records).
// Freezing before repeated digest checks lets the canonical encoder reuse
// validated subtrees without trusting caller-owned objects or self-hashes.
const freezeRollbackSnapshotJson = (value: unknown): void => {
  if (typeof value !== "object" || value === null) return;
  if (ownedRollbackSnapshotJson.has(value)) return;
  for (const child of Object.values(value)) freezeRollbackSnapshotJson(child);
  Object.freeze(value);
  ownedRollbackSnapshotJson.add(value);
};

// Call only after canonical JSON validation. Preserve privately owned immutable
// subtrees, and detach every new caller-owned value before the persistence await.
const ownRollbackSnapshotJson = <T>(value: T): T => {
  if (typeof value !== "object" || value === null) return value;
  if (ownedRollbackSnapshotJson.has(value)) return value;
  const copy = Array.isArray(value)
    ? value.map((child: unknown) => ownRollbackSnapshotJson(child))
    : Object.fromEntries(
        Object.entries(value).map(([key, child]) => [
          key,
          ownRollbackSnapshotJson(child),
        ]),
      );
  Object.freeze(copy);
  ownedRollbackSnapshotJson.add(copy);
  return copy as T;
};

type WatcherRollbackDurableAuthorityContent = Omit<
  WatcherRollbackDurableAuthoritySnapshot,
  "authorityDigest" | "authorityMac"
>;

const rollbackAuthorityCanonical = (
  value: WatcherRollbackDurableAuthorityContent,
): WatcherRollbackDurableAuthorityContent => ({
  validationSchemaVersion: value.validationSchemaVersion,
  schemaVersion: value.schemaVersion,
  revision: value.revision,
  priorSnapshotSha256: value.priorSnapshotSha256,
  policyDigest: value.policyDigest,
  deploymentMarker: value.deploymentMarker,
  currentStore: value.currentStore,
  consistencyHistory: value.consistencyHistory,
  rollbackState: value.rollbackState,
  rollbackBootstrapState: value.rollbackBootstrapState,
  trustedCheckpointStateDigest: value.trustedCheckpointStateDigest,
  userEventCheckpoint: value.userEventCheckpoint,
  userEventValidation: value.userEventValidation,
  authenticationKeyId: value.authenticationKeyId,
});

const parseRollbackAuthorityAuthenticationKey = (
  value: unknown,
): Uint8Array => {
  if (
    !(value instanceof Uint8Array) ||
    value.byteLength !== ROLLBACK_AUTHORITY_KEY_BYTES
  ) {
    throw new Error("invalid watcher rollback authority authentication key");
  }
  return Uint8Array.from(value);
};

const rollbackAuthorityKeyId = (key: Uint8Array): string =>
  createHash("sha256").update(key).digest("hex");

const rollbackAuthorityMac = (
  key: Uint8Array,
  canonical: Readonly<
    WatcherRollbackDurableAuthorityContent & {
      authorityDigest: string;
    }
  >,
): string =>
  createHmac("sha256", key)
    .update(watcherCanonicalJson(canonical), "utf8")
    .digest("hex");

const encodeRollbackDurableAuthoritySnapshot = (
  value: WatcherRollbackDurableAuthoritySnapshot,
): Uint8Array => rollbackAuthorityEncoder.encode(watcherCanonicalJson(value));

const decodeRollbackDurableAuthoritySnapshot = (
  bytes: Uint8Array,
  policy: WatcherFinalityPolicy,
  authenticationKeyInput: unknown,
  validateContents = false,
): WatcherRollbackDurableAuthoritySnapshot | null => {
  try {
    const authenticationKey = parseRollbackAuthorityAuthenticationKey(
      authenticationKeyInput,
    );
    const text = rollbackAuthorityDecoder.decode(bytes);
    const decoded = JSON.parse(text) as unknown;
    freezeRollbackSnapshotJson(decoded);
    const record = exactPlainRecord(decoded, [
      "schemaVersion",
      "validationSchemaVersion",
      "revision",
      "priorSnapshotSha256",
      "policyDigest",
      "deploymentMarker",
      "currentStore",
      "consistencyHistory",
      "rollbackState",
      "rollbackBootstrapState",
      "trustedCheckpointStateDigest",
      "userEventCheckpoint",
      "userEventValidation",
      "authenticationKeyId",
      "authorityDigest",
      "authorityMac",
    ]);
    if (
      record === null ||
      record.validationSchemaVersion !== ROLLBACK_VALIDATION_SCHEMA_VERSION ||
      record.schemaVersion !==
        WATCHER_ROLLBACK_DURABLE_AUTHORITY_SCHEMA_VERSION ||
      typeof record.revision !== "string" ||
      !CANONICAL_NATURAL.test(record.revision) ||
      (record.priorSnapshotSha256 !== null &&
        (typeof record.priorSnapshotSha256 !== "string" ||
          !HEX_32.test(record.priorSnapshotSha256))) ||
      (record.revision === "0") !== (record.priorSnapshotSha256 === null) ||
      record.policyDigest !== policy.policyDigest ||
      typeof record.trustedCheckpointStateDigest !== "string" ||
      !HEX_32.test(record.trustedCheckpointStateDigest) ||
      record.authenticationKeyId !==
        rollbackAuthorityKeyId(authenticationKey) ||
      typeof record.authorityDigest !== "string" ||
      !HEX_32.test(record.authorityDigest) ||
      typeof record.authorityMac !== "string" ||
      !HEX_32.test(record.authorityMac)
    ) {
      return null;
    }
    const untrustedCanonical = rollbackAuthorityCanonical({
      validationSchemaVersion: ROLLBACK_VALIDATION_SCHEMA_VERSION,
      schemaVersion: WATCHER_ROLLBACK_DURABLE_AUTHORITY_SCHEMA_VERSION,
      revision: record.revision,
      priorSnapshotSha256: record.priorSnapshotSha256 as string | null,
      policyDigest: record.policyDigest as string,
      deploymentMarker: record.deploymentMarker as DeploymentMarker,
      currentStore: record.currentStore as WatcherDurableStore,
      consistencyHistory:
        record.consistencyHistory as readonly WatcherMultiProviderConsistency[],
      rollbackState: record.rollbackState as WatcherRollbackState,
      rollbackBootstrapState:
        record.rollbackBootstrapState as WatcherRollbackState,
      trustedCheckpointStateDigest:
        record.trustedCheckpointStateDigest as string,
      userEventCheckpoint:
        record.userEventCheckpoint as WatcherUserEventCheckpoint | null,
      userEventValidation:
        record.userEventValidation as WatcherUserEventValidation | null,
      authenticationKeyId: record.authenticationKeyId as string,
    });
    const expectedMac = rollbackAuthorityMac(authenticationKey, {
      ...untrustedCanonical,
      authorityDigest: record.authorityDigest,
    });
    if (
      !timingSafeEqual(
        Buffer.from(expectedMac, "hex"),
        Buffer.from(record.authorityMac, "hex"),
      )
    ) {
      return null;
    }
    const deploymentMarker = parseDeploymentMarker(record.deploymentMarker);
    if (!sameMarker(deploymentMarker, policy.deploymentMarker)) {
      return null;
    }
    const authenticated = Object.freeze({
      ...untrustedCanonical,
      authorityDigest: record.authorityDigest,
      authorityMac: record.authorityMac,
    });
    if (
      sha256Canonical(untrustedCanonical) !== record.authorityDigest ||
      text !== watcherCanonicalJson(authenticated)
    )
      return null;
    // The MAC binds the completed validation and all its inputs to these exact
    // bytes. Every writer validates before CAS. Restart authenticates that
    // durable result; it must not replay previously validated history.
    if (!validateContents) return authenticated;
    // Canonical progress embeds the same store as the current store and both
    // epoch bootstraps. Validate identical bytes once within this load. The
    // cache is local, keyed by complete content rather than a claimed digest,
    // and contains only parsed, immutable stores.
    const parsedStores = new Map<string, WatcherDurableStore>();
    const parseStore = (value: unknown): WatcherDurableStore => {
      const content = watcherCanonicalJson(value);
      const cached = parsedStores.get(content);
      if (cached !== undefined) return cached;
      const parsed = parseWatcherDurableStore(value);
      freezeRollbackSnapshotJson(parsed);
      parsedStores.set(content, parsed);
      return parsed;
    };
    const currentStore = parseStore(record.currentStore);
    if (!sameMarker(currentStore.deploymentMarker, policy.deploymentMarker)) {
      return null;
    }
    const userEventCheckpoint =
      record.userEventCheckpoint === null
        ? null
        : parseWatcherUserEventCheckpoint(record.userEventCheckpoint, {
            deploymentMarker: policy.deploymentMarker,
            network: policy.network,
            blueprintHash: policy.blueprintHash,
            finalityPolicyDigest: policy.policyDigest,
          });
    const rollbackState = decodeWatcherRollbackStateStructural(
      record.rollbackState,
      parseStore,
    );
    const consistencyInputs = exactArray(record.consistencyHistory);
    if (consistencyInputs === null || consistencyInputs.length > 6_483) {
      return null;
    }
    const consistencyHistory =
      consistencyInputs as readonly WatcherMultiProviderConsistency[];
    const persistedIndex = indexPersistedObservations(currentStore);
    if (
      consistencyHistory.some(
        (consistency) =>
          verifyPersistedConsistencyEvidence(
            policy,
            currentStore,
            consistency,
            [],
            persistedIndex,
            AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE,
          ) === null,
      ) ||
      new Set(
        consistencyHistory.map(({ consistencyDigest }) => consistencyDigest),
      ).size !== consistencyHistory.length
    ) {
      return null;
    }
    const rollbackBootstrapState = parseRollbackBootstrapStateWithTrustedDigest(
      policy,
      record.rollbackBootstrapState,
      record.trustedCheckpointStateDigest,
      parseStore,
    );
    if (
      rollbackState === null ||
      rollbackBootstrapState === null ||
      rollbackBootstrapState.stateDigest !==
        record.trustedCheckpointStateDigest ||
      rollbackState.storeDigest !== storeDigest(currentStore) ||
      replayWatcherRollbackState(
        policy,
        rollbackBootstrapState,
        rollbackState,
        currentStore,
        [],
        AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE,
      ) === null
    ) {
      return null;
    }
    const canonicalWithoutDigest = rollbackAuthorityCanonical({
      validationSchemaVersion: ROLLBACK_VALIDATION_SCHEMA_VERSION,
      schemaVersion: WATCHER_ROLLBACK_DURABLE_AUTHORITY_SCHEMA_VERSION,
      revision: record.revision,
      priorSnapshotSha256: record.priorSnapshotSha256 as string | null,
      policyDigest: policy.policyDigest,
      deploymentMarker,
      currentStore,
      consistencyHistory,
      rollbackState,
      rollbackBootstrapState,
      trustedCheckpointStateDigest:
        record.trustedCheckpointStateDigest as string,
      userEventCheckpoint,
      userEventValidation:
        record.userEventValidation as WatcherUserEventValidation | null,
      authenticationKeyId: record.authenticationKeyId as string,
    });
    const snapshot = Object.freeze({
      ...canonicalWithoutDigest,
      authorityDigest: record.authorityDigest as string,
      authorityMac: record.authorityMac as string,
    });
    return sha256Canonical(canonicalWithoutDigest) ===
      snapshot.authorityDigest && text === watcherCanonicalJson(snapshot)
      ? snapshot
      : null;
  } catch {
    return null;
  }
};

const makeRollbackDurableAuthoritySnapshot = (
  policy: WatcherFinalityPolicy,
  revision: string,
  priorSnapshotSha256: string | null,
  currentStore: WatcherDurableStore,
  consistencyHistory: readonly WatcherMultiProviderConsistency[],
  rollbackState: WatcherRollbackState,
  rollbackBootstrapState: WatcherRollbackState,
  trustedCheckpointStateDigest: string,
  userEventCheckpoint: WatcherUserEventCheckpoint | null,
  authenticationKeyInput: unknown,
  userEventValidation: WatcherUserEventValidation | null = null,
): Readonly<{
  snapshot: WatcherRollbackDurableAuthoritySnapshot;
  encoded: Uint8Array;
}> => {
  const authenticationKey = parseRollbackAuthorityAuthenticationKey(
    authenticationKeyInput,
  );
  const canonicalInput = rollbackAuthorityCanonical({
    validationSchemaVersion: ROLLBACK_VALIDATION_SCHEMA_VERSION,
    schemaVersion: WATCHER_ROLLBACK_DURABLE_AUTHORITY_SCHEMA_VERSION,
    revision,
    priorSnapshotSha256,
    policyDigest: policy.policyDigest,
    deploymentMarker: policy.deploymentMarker,
    currentStore,
    consistencyHistory,
    rollbackState,
    rollbackBootstrapState,
    trustedCheckpointStateDigest,
    userEventCheckpoint,
    userEventValidation,
    authenticationKeyId: rollbackAuthorityKeyId(authenticationKey),
  });
  const authorityDigest = sha256Canonical(canonicalInput);
  const canonical = ownRollbackSnapshotJson(canonicalInput);
  const candidate = Object.freeze({
    ...canonical,
    authorityDigest,
    authorityMac: rollbackAuthorityMac(authenticationKey, {
      ...canonical,
      authorityDigest,
    }),
  });
  const encoded = encodeRollbackDurableAuthoritySnapshot(candidate);
  return Object.freeze({ snapshot: candidate, encoded });
};

const makeRollbackDurableAuthorityHandle = (
  runtime: WatcherRollbackDurableAuthorityRuntime,
): WatcherRollbackDurableAuthority => {
  // Snapshot construction owns all values before CAS; decoding owns them on
  // restart. Retain that graph so unchanged history keeps its cached encoding.
  const snapshot = runtime.snapshot;
  const authority = Object.freeze({
    schemaVersion: WATCHER_ROLLBACK_DURABLE_AUTHORITY_HANDLE_SCHEMA_VERSION,
    revision: snapshot.revision,
    snapshotSha256: runtime.snapshotSha256,
    authorityDigest: snapshot.authorityDigest,
  });
  rollbackDurableAuthorityRuntime.set(
    authority,
    Object.freeze({ ...runtime, snapshot }),
  );
  return authority;
};

type WatcherRollbackDurableTrustedHeadContent = Omit<
  WatcherRollbackDurableTrustedHead,
  "headMac"
>;

const rollbackDurableTrustedHeadCanonical = (
  value: WatcherRollbackDurableTrustedHeadContent,
): WatcherRollbackDurableTrustedHeadContent => ({
  schemaVersion: value.schemaVersion,
  policyDigest: value.policyDigest,
  deploymentMarker: value.deploymentMarker,
  authenticationKeyId: value.authenticationKeyId,
  revision: value.revision,
  snapshotSha256: value.snapshotSha256,
  authorityDigest: value.authorityDigest,
});

const rollbackDurableTrustedHeadMac = (
  authenticationKey: Uint8Array,
  value: WatcherRollbackDurableTrustedHeadContent,
): string =>
  createHmac("sha256", authenticationKey)
    .update(
      `${WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION}:${watcherCanonicalJson(value)}`,
      "utf8",
    )
    .digest("hex");

const makeRollbackDurableTrustedHead = (
  policy: WatcherFinalityPolicy,
  snapshot: WatcherRollbackDurableAuthoritySnapshot,
  snapshotSha256: string,
  authenticationKey: Uint8Array,
): WatcherRollbackDurableTrustedHead => {
  const canonical = rollbackDurableTrustedHeadCanonical({
    schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    deploymentMarker: policy.deploymentMarker,
    authenticationKeyId: snapshot.authenticationKeyId,
    revision: snapshot.revision,
    snapshotSha256,
    authorityDigest: snapshot.authorityDigest,
  });
  return Object.freeze({
    ...canonical,
    headMac: rollbackDurableTrustedHeadMac(authenticationKey, canonical),
  });
};

const parseRollbackDurableTrustedHead = (
  value: unknown,
  policy: WatcherFinalityPolicy,
  authenticationKey: Uint8Array,
): WatcherRollbackDurableTrustedHead => {
  const record = exactPlainRecord(value, [
    "schemaVersion",
    "policyDigest",
    "deploymentMarker",
    "authenticationKeyId",
    "revision",
    "snapshotSha256",
    "authorityDigest",
    "headMac",
  ]);
  if (
    record === null ||
    record.schemaVersion !==
      WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION ||
    record.policyDigest !== policy.policyDigest ||
    record.authenticationKeyId !== rollbackAuthorityKeyId(authenticationKey) ||
    typeof record.revision !== "string" ||
    !CANONICAL_NATURAL.test(record.revision) ||
    typeof record.snapshotSha256 !== "string" ||
    !HEX_32.test(record.snapshotSha256) ||
    typeof record.authorityDigest !== "string" ||
    !HEX_32.test(record.authorityDigest) ||
    typeof record.headMac !== "string" ||
    !HEX_32.test(record.headMac)
  ) {
    throw new Error("invalid watcher rollback durable trusted head");
  }
  let deploymentMarker: DeploymentMarker;
  try {
    deploymentMarker = parseDeploymentMarker(record.deploymentMarker);
  } catch {
    throw new Error("invalid watcher rollback durable trusted head");
  }
  if (!sameMarker(deploymentMarker, policy.deploymentMarker)) {
    throw new Error("invalid watcher rollback durable trusted head");
  }
  const canonical = rollbackDurableTrustedHeadCanonical({
    schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    deploymentMarker,
    authenticationKeyId: record.authenticationKeyId,
    revision: record.revision,
    snapshotSha256: record.snapshotSha256,
    authorityDigest: record.authorityDigest,
  }) as WatcherRollbackDurableTrustedHeadContent;
  const expectedMac = rollbackDurableTrustedHeadMac(
    authenticationKey,
    canonical,
  );
  if (
    !timingSafeEqual(
      Buffer.from(expectedMac, "hex"),
      Buffer.from(record.headMac, "hex"),
    )
  ) {
    throw new Error("invalid watcher rollback durable trusted head");
  }
  return Object.freeze({
    ...canonical,
    headMac: record.headMac,
  }) as WatcherRollbackDurableTrustedHead;
};

/**
 * Strict public admission used by the independently persisted trusted-head
 * authority. It authenticates the exact release policy, deployment marker and
 * rollback-authority key; a structurally plausible caller-authored head is not
 * authority.
 */
export const admitWatcherRollbackDurableTrustedHead = (input: {
  readonly head: unknown;
  readonly policy: unknown;
  readonly authenticationKey: Uint8Array;
}): WatcherRollbackDurableTrustedHead | null => {
  try {
    const policy = parseWatcherFinalityPolicy(input.policy);
    if (policy === null) return null;
    const authenticationKey = parseRollbackAuthorityAuthenticationKey(
      input.authenticationKey,
    );
    return parseRollbackDurableTrustedHead(
      input.head,
      policy,
      authenticationKey,
    );
  } catch {
    return null;
  }
};

const assertRollbackDurableTrustedHeadMatches = (
  trustedHead: WatcherRollbackDurableTrustedHead,
  snapshot: WatcherRollbackDurableAuthoritySnapshot,
  snapshotSha256: string,
): void => {
  if (
    trustedHead.revision !== snapshot.revision ||
    trustedHead.snapshotSha256 !== snapshotSha256 ||
    trustedHead.authorityDigest !== snapshot.authorityDigest ||
    trustedHead.authenticationKeyId !== snapshot.authenticationKeyId
  ) {
    throw new Error("watcher rollback durable trusted head mismatch");
  }
};

const runtimeForRollbackDurableAuthority = (
  authority: WatcherRollbackDurableAuthority,
): WatcherRollbackDurableAuthorityRuntime => {
  const runtime = rollbackDurableAuthorityRuntime.get(authority);
  if (runtime === undefined) {
    throw new Error("unknown watcher rollback durable authority");
  }
  return runtime;
};

const authorityFromEncodedSnapshot = (
  backend: WatcherDurableAtomicBackend,
  policy: WatcherFinalityPolicy,
  encoded: Uint8Array,
  snapshotSha256: string,
  authenticationKeyInput: unknown,
  trustedHeadInput: unknown,
): WatcherRollbackDurableAuthority => {
  const authenticationKey = parseRollbackAuthorityAuthenticationKey(
    authenticationKeyInput,
  );
  const trustedHead = parseRollbackDurableTrustedHead(
    trustedHeadInput,
    policy,
    authenticationKey,
  );
  const copy = Uint8Array.from(encoded);
  if (watcherDurableStoreBytesSha256(copy) !== snapshotSha256) {
    throw new Error("watcher rollback durable authority digest mismatch");
  }
  const snapshot = decodeRollbackDurableAuthoritySnapshot(
    copy,
    policy,
    authenticationKey,
  );
  if (snapshot === null) {
    throw new Error("invalid watcher rollback durable authority");
  }
  assertRollbackDurableTrustedHeadMatches(
    trustedHead,
    snapshot,
    snapshotSha256,
  );
  const authority = makeRollbackDurableAuthorityHandle(
    Object.freeze({
      backend,
      policy,
      snapshot,
      encoded: copy,
      snapshotSha256,
      authenticationKey,
    }),
  );
  return authority;
};

/**
 * Loads the rollback journal, its active epoch bootstrap, and its trust anchor
 * from one backend snapshot. `trustedHead` must come from independently
 * protected monotonic storage: the candidate snapshot can authenticate its
 * bytes but can never testify that it is the newest committed snapshot. An
 * ordinary row in the same rollbackable database is insufficient. The
 * returned handle is an in-process capability; a copied plain object cannot
 * authorize an epoch restart.
 */
export const loadWatcherRollbackDurableAuthority = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: unknown;
  readonly authenticationKey: Uint8Array;
  readonly trustedHead: unknown;
}): Promise<WatcherRollbackDurableAuthority> => {
  const policy = parseWatcherFinalityPolicy(input.policy);
  if (policy === null) {
    throw new Error("invalid watcher rollback durable authority policy");
  }
  const stored = await readWatcherDurableAtomicSnapshot(input.backend);
  if (stored === null) {
    throw new Error("watcher rollback durable authority missing");
  }
  return authorityFromEncodedSnapshot(
    input.backend,
    policy,
    stored.bytes,
    stored.sha256,
    input.authenticationKey,
    input.trustedHead,
  );
};

/** Checks that an admitted, durably validated result still matches both
 * durable owners. Fresh reads enforce exact bytes and monotonic publication;
 * the validation itself survives restart through its authenticated binding. */
export const revalidateWatcherRollbackDurableAuthority = async (input: {
  readonly authority: WatcherRollbackDurableAuthority;
  readonly trustedHead: unknown;
}): Promise<WatcherRollbackDurableAuthority> => {
  const runtime = runtimeForRollbackDurableAuthority(input.authority);
  const trustedHead = parseRollbackDurableTrustedHead(
    input.trustedHead,
    runtime.policy,
    runtime.authenticationKey,
  );
  const stored = await readWatcherDurableAtomicSnapshot(runtime.backend);
  if (stored === null) {
    throw new Error("watcher rollback durable authority missing");
  }
  if (
    stored.sha256 !== runtime.snapshotSha256 ||
    Buffer.compare(stored.bytes, runtime.encoded) !== 0
  ) {
    throw new Error("watcher rollback durable authority bytes changed");
  }
  assertRollbackDurableTrustedHeadMatches(
    trustedHead,
    runtime.snapshot,
    stored.sha256,
  );
  return input.authority;
};

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

/**
 * Publishes structural checkpoint bytes without evaluating a user-event
 * transition. The upper owner remains responsible for semantic admission and
 * its declared evidence closure. A committed head still needs independent
 * publication/read-back before any protected receipt is issued.
 */
export const persistWatcherRollbackDurableUserEventCheckpoint = async (
  input: WatcherUserEventCheckpointExpectation &
    Readonly<{
      authority: WatcherRollbackDurableAuthority;
      archive: WatcherUserEventArchive;
      nextCheckpoint: unknown;
      validationCandidate?: unknown;
    }>,
): Promise<WatcherRollbackDurableObservationResult> => {
  const runtime = runtimeForRollbackDurableAuthority(input.authority);
  const current = runtime.snapshot.userEventCheckpoint;
  const next = parseWatcherUserEventCheckpoint(input.nextCheckpoint, {
    deploymentMarker: runtime.policy.deploymentMarker,
    network: runtime.policy.network,
    blueprintHash: runtime.policy.blueprintHash,
    finalityPolicyDigest: runtime.policy.policyDigest,
  });
  if (!watcherUserEventCheckpointExpectationMatches(current, input)) {
    return Object.freeze({ persistence: "conflict" });
  }
  if (current?.checkpointDigest !== next.checkpointDigest) {
    assertWatcherUserEventCheckpointSuccessor(current, next);
  }
  await readWatcherUserEventCheckpointPayload(next, input.archive);
  const validation =
    input.validationCandidate === undefined
      ? current?.checkpointDigest === next.checkpointDigest
        ? runtime.snapshot.userEventValidation
        : null
      : readWatcherLocalUserEventValidation(input.validationCandidate, next);
  if (
    current?.checkpointDigest === next.checkpointDigest &&
    watcherSameCanonicalJson(validation, runtime.snapshot.userEventValidation)
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
    });
  }
  const committed = await commitRollbackDurableAuthority(
    input.authority,
    runtime.snapshot.currentStore,
    runtime.snapshot.rollbackState,
    runtime.snapshot.rollbackBootstrapState,
    runtime.snapshot.trustedCheckpointStateDigest,
    runtime.snapshot.consistencyHistory,
    next,
    validation,
  );
  return committed === null
    ? Object.freeze({ persistence: "conflict" })
    : Object.freeze({ persistence: "committed", ...committed });
};

// This private commit accepts only results from the validated transition
// builders below (or the parsed checkpoint publisher above). Their source
// capability already carries durable validation. Validate new dependencies
// before calling here, then atomically persist that result and its binding;
// replaying the unchanged prefix here would discard the benefit of validation.
const commitRollbackDurableAuthority = async (
  authority: WatcherRollbackDurableAuthority,
  currentStore: WatcherDurableStore,
  rollbackState: WatcherRollbackState,
  rollbackBootstrapState: WatcherRollbackState,
  trustedCheckpointStateDigest: string,
  consistencyHistory?: readonly WatcherMultiProviderConsistency[],
  userEventCheckpoint?: WatcherUserEventCheckpoint,
  userEventValidation?: WatcherUserEventValidation | null,
): Promise<Readonly<{
  authority: WatcherRollbackDurableAuthority;
  trustedHead: WatcherRollbackDurableTrustedHead;
}> | null> => {
  const runtime = runtimeForRollbackDurableAuthority(authority);
  const observationIds = new Set(
    currentStore.l1Observations.map(({ observationId }) => observationId),
  );
  const retainedConsistencyHistory = Object.freeze(
    (consistencyHistory ?? runtime.snapshot.consistencyHistory).filter(
      ({ observationEvidenceDigests }) =>
        observationEvidenceDigests.every((digest) =>
          observationIds.has(digest),
        ),
    ),
  );
  const { snapshot, encoded } = makeRollbackDurableAuthoritySnapshot(
    runtime.policy,
    (BigInt(runtime.snapshot.revision) + 1n).toString(),
    runtime.snapshotSha256,
    currentStore,
    retainedConsistencyHistory,
    rollbackState,
    rollbackBootstrapState,
    trustedCheckpointStateDigest,
    userEventCheckpoint ?? runtime.snapshot.userEventCheckpoint,
    runtime.authenticationKey,
    userEventValidation === undefined
      ? runtime.snapshot.userEventValidation
      : userEventValidation,
  );
  const commit = await compareAndSwapWatcherDurableAtomicSnapshot({
    backend: runtime.backend,
    expectedSha256: runtime.snapshotSha256,
    next: encoded,
    canonicalValue: snapshot,
  });
  if (!commit.committed) {
    return null;
  }
  const nextAuthority = makeRollbackDurableAuthorityHandle(
    Object.freeze({
      backend: runtime.backend,
      policy: runtime.policy,
      snapshot,
      encoded,
      snapshotSha256: commit.sha256,
      authenticationKey: runtime.authenticationKey,
    }),
  );
  return Object.freeze({
    authority: nextAuthority,
    trustedHead: makeRollbackDurableTrustedHead(
      runtime.policy,
      snapshot,
      commit.sha256,
      runtime.authenticationKey,
    ),
  });
};

const currentRollbackFinalityState = (
  runtime: WatcherRollbackDurableAuthorityRuntime,
): WatcherFinalityState => {
  const input =
    runtime.snapshot.rollbackState.transitions.at(-1)?.finalityResult.state ??
    runtime.snapshot.rollbackState.bootstrapFinalityState;
  const parsed = parseWatcherFinalityState(input, runtime.policy);
  if (parsed === null) {
    throw new Error("watcher rollback durable current finality state invalid");
  }
  return parsed;
};

const storeWithAuthenticatedObservations = (
  source: WatcherDurableStore,
  blocks: readonly WatcherNormalizedL1Block[],
): WatcherDurableStore => {
  const chainPoints = blocks.map((block) =>
    Object.freeze({
      chainPointId: block.chainPoint.chainPointId,
      providerId: block.provider.providerId,
      blockHash: block.chainPoint.blockHash,
      slot: block.chainPoint.slot,
      blockNo: block.chainPoint.blockNo,
      depth: block.chainPoint.depth,
    }),
  );
  const observations = blocks.map((block) =>
    Object.freeze({
      observationId: block.observationDigest,
      providerId: block.provider.providerId,
      chainPointId: block.chainPoint.chainPointId,
      payload: makeWatcherDurablePayload(
        encodeWatcherNormalizedL1Block(block).toString("hex"),
      ),
    }),
  );
  const observationIds = new Set(
    observations.map(({ observationId }) => observationId),
  );
  const chainPointIds = new Set(
    chainPoints.map(({ chainPointId }) => chainPointId),
  );
  return makeWatcherDurableStore({
    deploymentMarker: source.deploymentMarker,
    revision: (BigInt(source.revision) + 1n).toString(),
    records: {
      l1Observations: [
        ...source.l1Observations.filter(
          ({ observationId }) => !observationIds.has(observationId),
        ),
        ...observations,
      ],
      chainPoints: [
        ...source.chainPoints.filter(
          ({ chainPointId }) => !chainPointIds.has(chainPointId),
        ),
        ...chainPoints,
      ],
      protocolUtxos: source.protocolUtxos,
      spentProtocolUtxos: source.spentProtocolUtxos,
      daProofInputs: source.daProofInputs,
      reconstructedStates: source.reconstructedStates,
      decisions: source.decisions,
      faults: source.faults,
      submissions: source.submissions,
      confirmations: source.confirmations,
      retries: source.retries,
      deadlines: source.deadlines,
      correctionResults: source.correctionResults,
    },
  });
};

const authenticatesCanonicalBlock = (input: {
  readonly block: WatcherNormalizedL1Block;
  readonly observations: readonly WatcherNormalizedL1Block[];
  readonly consistency: WatcherMultiProviderConsistency;
  readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
}): boolean =>
  input.observations.length === input.consistency.observationCount &&
  input.observations.some(
    ({ observationDigest }) =>
      observationDigest === input.block.observationDigest,
  ) &&
  input.observations.every((observation) =>
    input.transportAttestations.some((context) =>
      isWatcherL1BlockAttestedBy(observation, context),
    ),
  ) &&
  sameStrings(
    sorted(
      input.observations.map(({ observationDigest }) => observationDigest),
    ),
    input.consistency.observationEvidenceDigests,
  ) &&
  input.consistency.status === "agreed" &&
  input.consistency.protocolDecision === "allowed" &&
  input.consistency.chainAuthorityObservationDigest ===
    input.block.observationDigest &&
  input.consistency.agreement?.pointDigest ===
    input.block.chainPoint.pointDigest &&
  input.consistency.agreement.blockContentDigest ===
    input.block.blockContentDigest;

/** Verify the newly appended evidence without decoding unchanged history.
 * The source is privately owned; the store builder only changes these two
 * record collections. An existing identity must retain its exact content. */
const assertCanonicalProgressEvidence = (
  policy: WatcherFinalityPolicy,
  source: WatcherDurableStore,
  next: WatcherDurableStore,
  input: {
    readonly observations: readonly WatcherNormalizedL1Block[];
    readonly consistency: WatcherMultiProviderConsistency;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  },
): void => {
  const index = new Map<string, PersistedObservationIndexEntry>();
  for (const observation of input.observations) {
    const durable = next.l1Observations.find(
      ({ observationId }) => observationId === observation.observationDigest,
    );
    const point = next.chainPoints.find(
      ({ chainPointId }) =>
        chainPointId === observation.chainPoint.chainPointId,
    );
    const priorObservation = source.l1Observations.find(
      ({ observationId }) => observationId === observation.observationDigest,
    );
    const priorPoint = source.chainPoints.find(
      ({ chainPointId }) =>
        chainPointId === observation.chainPoint.chainPointId,
    );
    if (
      durable === undefined ||
      point === undefined ||
      index.has(observation.observationDigest) ||
      (priorObservation !== undefined &&
        !watcherSameCanonicalJson(priorObservation, durable)) ||
      (priorPoint !== undefined && !watcherSameCanonicalJson(priorPoint, point))
    ) {
      throw new Error("watcher canonical progress changed retained evidence");
    }
    index.set(observation.observationDigest, { durable, point, observation });
  }
  if (
    verifyPersistedConsistencyEvidence(
      policy,
      next,
      input.consistency,
      input.transportAttestations,
      index,
    ) === null
  ) {
    throw new Error(
      "watcher canonical progress evidence failed live verification",
    );
  }
};

/**
 * Journals authenticated replacement evidence before a rewind/incident is
 * evaluated. This operation changes no finality or rollback decision and its
 * emitted head still requires external CAS publication before use.
 */
export const persistWatcherRollbackDurableObservation = async (input: {
  readonly authority: WatcherRollbackDurableAuthority;
  readonly block: WatcherNormalizedL1Block;
  readonly observations: readonly WatcherNormalizedL1Block[];
  readonly consistency: WatcherMultiProviderConsistency;
  readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
}): Promise<WatcherRollbackDurableObservationResult> => {
  const runtime = runtimeForRollbackDurableAuthority(input.authority);
  if (!authenticatesCanonicalBlock(input)) {
    throw new Error(
      "watcher durable observation lacks authenticated local-node agreement",
    );
  }
  const existingById = new Map(
    runtime.snapshot.currentStore.l1Observations.map((observation) => [
      observation.observationId,
      observation,
    ]),
  );
  const alreadyStored = input.observations.every((observation) => {
    const existing = existingById.get(observation.observationDigest);
    return (
      existing !== undefined &&
      existing.providerId === observation.provider.providerId &&
      existing.chainPointId === observation.chainPoint.chainPointId &&
      existing.payload.cborHex ===
        encodeWatcherNormalizedL1Block(observation).toString("hex")
    );
  });
  if (
    alreadyStored &&
    runtime.snapshot.consistencyHistory.some(
      ({ consistencyDigest }) =>
        consistencyDigest === input.consistency.consistencyDigest,
    )
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
    });
  }
  if (
    input.observations.some((observation) => {
      const existing = existingById.get(observation.observationDigest);
      return (
        existing !== undefined &&
        (existing.providerId !== observation.provider.providerId ||
          existing.chainPointId !== observation.chainPoint.chainPointId ||
          existing.payload.cborHex !==
            encodeWatcherNormalizedL1Block(observation).toString("hex"))
      );
    })
  ) {
    throw new Error("watcher durable observation identity was substituted");
  }
  if (
    runtime.snapshot.rollbackState.incident !== null ||
    currentRollbackFinalityState(runtime).phase === "quarantined"
  ) {
    throw new Error(
      "watcher quarantined observation requires post-finality recovery",
    );
  }
  const nextStore = storeWithAuthenticatedObservations(
    runtime.snapshot.currentStore,
    input.observations,
  );
  const nextHistory = Object.freeze([
    ...runtime.snapshot.consistencyHistory.filter(
      ({ consistencyDigest }) =>
        consistencyDigest !== input.consistency.consistencyDigest,
    ),
    input.consistency,
  ]);
  if (nextHistory.length > 6_483) {
    throw new Error(
      "watcher authenticated consistency history exceeds its bound",
    );
  }
  assertCanonicalProgressEvidence(
    runtime.policy,
    runtime.snapshot.currentStore,
    nextStore,
    input,
  );
  freezeRollbackSnapshotJson(nextStore);
  // Persist the new evidence and its store binding in the same revision.
  // Finality and the authenticated prior transition lineage stay unchanged.
  const checkpoint = makeEpochBootstrapState(
    runtime.policy,
    runtime.snapshot.rollbackState,
    nextStore,
    currentRollbackFinalityState(runtime),
    null,
    "observation",
  );
  const committed = await commitRollbackDurableAuthority(
    input.authority,
    nextStore,
    checkpoint,
    checkpoint,
    checkpoint.stateDigest,
    nextHistory,
  );
  return committed === null
    ? Object.freeze({ persistence: "conflict" })
    : Object.freeze({
        persistence: "committed",
        authority: committed.authority,
        trustedHead: committed.trustedHead,
      });
};

/**
 * Atomically journals the exact native chain-authority observation before any
 * finality/rollback decision is acted upon. The observation must be the one
 * admitted by the live chain-sync transport and selected by the independently
 * reconciled local Kupo/Ogmios consistency result.
 */
/**
 * A processed-but-unrecorded stretch of blocks between the finalized
 * authority block and a new canonical block. Quiet blocks are not persisted
 * into the authority; the caller attests them from its block-progress store.
 */
export type WatcherRollbackCanonicalAncestryLink = Readonly<{
  blockHash: string;
  parentBlockHash: string;
  blockNo: string;
  slot: string;
}>;

const ancestryLinksFinalizedToBlock = (
  finalized: Readonly<{ blockHash: string; blockNo: string; slot: string }>,
  block: WatcherNormalizedL1Block,
  ancestry: readonly WatcherRollbackCanonicalAncestryLink[],
): boolean => {
  let previous = finalized;
  for (const link of ancestry) {
    if (
      link.parentBlockHash !== previous.blockHash ||
      BigInt(link.blockNo) !== BigInt(previous.blockNo) + 1n ||
      BigInt(link.slot) <= BigInt(previous.slot)
    )
      return false;
    previous = link;
  }
  return (
    block.chainPoint.parentBlockHash === previous.blockHash &&
    BigInt(block.chainPoint.blockNo) === BigInt(previous.blockNo) + 1n &&
    BigInt(block.chainPoint.slot) > BigInt(previous.slot)
  );
};

/** Pure ancestry-link test seam; it grants no durable authority. */
export const unsafeWatcherCanonicalAncestryLinksForTest =
  ancestryLinksFinalizedToBlock;

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
  const finalityResult = evaluateWatcherFinality(
    runtime.policy,
    evaluationState,
    input.consistency,
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
  const nextStore = storeWithAuthenticatedObservations(
    runtime.snapshot.currentStore,
    input.observations,
  );
  const nextHistory = Object.freeze([
    ...runtime.snapshot.consistencyHistory.filter(
      ({ consistencyDigest }) =>
        consistencyDigest !== input.consistency.consistencyDigest,
    ),
    input.consistency,
  ]);
  if (nextHistory.length > 6_483) {
    throw new Error(
      "watcher authenticated consistency history exceeds its bound",
    );
  }
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
  const verified = evaluateWatcherRollbackStep(
    runtime.policy,
    runtime.snapshot.currentStore,
    runtime.snapshot.rollbackState,
    runtime.snapshot.rollbackBootstrapState,
    input.previousFinalityState,
    input.consistency,
    input.finalityResult,
    input.transportAttestations,
  );
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
