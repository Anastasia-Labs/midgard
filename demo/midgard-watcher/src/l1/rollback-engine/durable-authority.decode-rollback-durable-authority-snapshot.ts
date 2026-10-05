import { timingSafeEqual } from "node:crypto";

import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  parseWatcherDurableStore,
  watcherCanonicalJson,
  type WatcherDurableStore,
} from "../../storage/durable-store.js";
import {
  parseWatcherUserEventCheckpoint,
  type WatcherUserEventCheckpoint,
  type WatcherUserEventValidation,
} from "../../storage/user-event-checkpoint.js";
import { type WatcherFinalityPolicy } from ".././finality-engine.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import {
  encodeRollbackDurableAuthoritySnapshot,
  freezeRollbackSnapshotJson,
  ownRollbackSnapshotJson,
  parseRollbackAuthorityAuthenticationKey,
  rollbackAuthorityCanonical,
  rollbackAuthorityDecoder,
  rollbackAuthorityKeyId,
  rollbackAuthorityMac,
} from "./durable-authority.rollback-authority-canonical.js";
import { exactArray, exactPlainRecord, sameMarker } from "./records.js";
import {
  AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE,
  decodeWatcherRollbackStateStructural,
  indexPersistedObservations,
  parseRollbackBootstrapStateWithTrustedDigest,
  replayWatcherRollbackState,
  storeDigest,
  verifyPersistedConsistencyEvidence,
} from "./state.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  ROLLBACK_VALIDATION_SCHEMA_VERSION,
  rollbackDurableAuthorityRuntime,
  sha256Canonical,
  WATCHER_ROLLBACK_DURABLE_AUTHORITY_HANDLE_SCHEMA_VERSION,
  WATCHER_ROLLBACK_DURABLE_AUTHORITY_SCHEMA_VERSION,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableAuthorityRuntime,
  type WatcherRollbackDurableAuthoritySnapshot,
  type WatcherRollbackDurableTrustedHead,
  type WatcherRollbackState,
} from "./types.js";

/** Authenticated consistency history admitted by one durable snapshot. */
export const WATCHER_ROLLBACK_CONSISTENCY_HISTORY_BOUND = 6_483;

export const decodeRollbackDurableAuthoritySnapshot = (
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
    if (
      consistencyInputs === null ||
      consistencyInputs.length > WATCHER_ROLLBACK_CONSISTENCY_HISTORY_BOUND
    ) {
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

export const makeRollbackDurableAuthoritySnapshot = (
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

export const makeRollbackDurableAuthorityHandle = (
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

export type WatcherRollbackDurableTrustedHeadContent = Omit<
  WatcherRollbackDurableTrustedHead,
  "headMac"
>;
