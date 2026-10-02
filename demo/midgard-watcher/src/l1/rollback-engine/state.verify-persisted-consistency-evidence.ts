import {
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import {
  evaluateWatcherFinality,
  type WatcherFinalityBoundObservation,
  watcherFinalityConfiguredSource,
  type WatcherFinalityPolicy,
} from ".././finality-engine.js";
import {
  encodeWatcherNormalizedL1Block,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from ".././l1-adapter.js";
import {
  evaluateWatcherMultiProviderConsistency,
  type WatcherMultiProviderConsistency,
} from ".././multi-provider-consistency.js";
import {
  freezeRollbackSnapshotJson,
  ownedRollbackSnapshotJson,
} from "./durable-authority.rollback-authority-canonical.js";
import { exactPlainRecord, sameStrings } from "./records.js";
import {
  type PersistedConsistencyEvidence,
  type PersistedObservationIndex,
  type PersistedObservationIndexEntry,
  sorted,
} from "./state.decode-watcher-rollback-state-structural.js";
import {
  HEX_32,
  type ParsedFinalityTransition,
  sha256Canonical,
} from "./types.js";

const decodePersistedObservation = (
  cborHex: string,
): WatcherNormalizedL1Block | null => {
  try {
    const bytes = Buffer.from(cborHex, "hex");
    const text = new TextDecoder("utf-8", { fatal: true }).decode(bytes);
    const decoded = JSON.parse(text) as unknown;
    const root = exactPlainRecord(decoded, [
      "schemaVersion",
      "network",
      "provider",
      "chainPoint",
      "transactions",
      "blockContentDigest",
      "observationDigest",
    ]);
    if (
      root === null ||
      typeof root.observationDigest !== "string" ||
      !HEX_32.test(root.observationDigest)
    ) {
      return null;
    }
    const observation = decoded as WatcherNormalizedL1Block;
    if (!bytes.equals(encodeWatcherNormalizedL1Block(observation))) {
      return null;
    }
    return observation;
  } catch {
    return null;
  }
};

// A process-owned store is deeply frozen, so an index derived from it cannot
// go stale. Rewind evaluation and journal replay look the same store up once
// per transition; decoding every observation each time made each durable
// operation cost the whole retained store again. Caller-owned stores are
// re-indexed on every call.
const ownedStoreIndexes = new WeakMap<object, PersistedObservationIndex>();

export const indexPersistedObservations = (
  store: WatcherDurableStore,
): PersistedObservationIndex => {
  const owned = ownedRollbackSnapshotJson.has(store);
  const cached = owned ? ownedStoreIndexes.get(store) : undefined;
  if (cached !== undefined) return cached;
  const points = new Map(
    store.chainPoints.map((point) => [point.chainPointId, point] as const),
  );
  const index = new Map<string, PersistedObservationIndexEntry | null>();
  for (const durable of store.l1Observations) {
    const observation = decodePersistedObservation(durable.payload.cborHex);
    if (observation === null) {
      continue;
    }
    // Decoded from validated bytes, so process-owned; shared through the
    // cache, so immutable.
    freezeRollbackSnapshotJson(observation);
    const digest = observation.observationDigest;
    if (index.has(digest)) {
      index.set(digest, null);
      continue;
    }
    index.set(
      digest,
      Object.freeze({
        durable,
        point: points.get(observation.chainPoint.chainPointId) ?? null,
        observation,
      }),
    );
  }
  if (owned) ownedStoreIndexes.set(store, index);
  return index;
};

export const AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE = Symbol(
  "authenticated-rollback-snapshot-evidence",
);

export const verifyPersistedConsistencyEvidence = (
  policy: WatcherFinalityPolicy,
  store: WatcherDurableStore,
  consistency: WatcherMultiProviderConsistency,
  transportAttestationsInput: unknown,
  persistedIndex: PersistedObservationIndex = indexPersistedObservations(store),
  authenticatedSnapshotEvidence:
    | typeof AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
    | null = null,
): PersistedConsistencyEvidence | null => {
  const structuralVerification = evaluateWatcherFinality(
    policy,
    null,
    consistency,
  );
  if (
    structuralVerification.action !== "observe_pending" ||
    structuralVerification.state?.pending === null ||
    structuralVerification.state?.pending?.lastSeenConsistencyDigest !==
      consistency.consistencyDigest
  ) {
    return null;
  }
  const evidenceDigests = consistency.observationEvidenceDigests;
  if (
    consistency.sourceMode !== policy.sourceMode ||
    consistency.configuredNetwork !== policy.network ||
    consistency.authorityNodeId !== policy.authorityNodeId ||
    consistency.authorityGenesisIdentitySha256 !==
      policy.authorityGenesisIdentitySha256 ||
    consistency.configuredSourceDigest !==
      sha256Canonical(watcherFinalityConfiguredSource(policy)) ||
    consistency.rejectedObservationCount !== 0 ||
    evidenceDigests.length === 0 ||
    consistency.observationCount !== evidenceDigests.length
  ) {
    return null;
  }
  const decoded: WatcherNormalizedL1Block[] = [];
  const observationIds = new Set<string>();
  const chainPointIds = new Set<string>();
  for (const evidenceDigest of evidenceDigests) {
    const indexed = persistedIndex.get(evidenceDigest);
    if (indexed === undefined || indexed === null) {
      return null;
    }
    const { durable, observation, point } = indexed;
    if (
      durable.providerId !== observation.provider.providerId ||
      durable.chainPointId !== observation.chainPoint.chainPointId ||
      point === null ||
      point.providerId !== observation.provider.providerId ||
      point.blockHash !== observation.chainPoint.blockHash ||
      point.slot !== observation.chainPoint.slot ||
      point.blockNo !== observation.chainPoint.blockNo ||
      point.depth !== observation.chainPoint.depth
    ) {
      return null;
    }
    observationIds.add(durable.observationId);
    chainPointIds.add(durable.chainPointId);
    decoded.push(observation);
  }
  if (
    decoded.length !== evidenceDigests.length ||
    !sameStrings(
      sorted(decoded.map(({ observationDigest }) => observationDigest)),
      evidenceDigests,
    )
  ) {
    return null;
  }
  const recomputed =
    authenticatedSnapshotEvidence === AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
      ? consistency
      : evaluateWatcherMultiProviderConsistency(
          watcherFinalityConfiguredSource(policy),
          decoded,
          transportAttestationsInput as readonly WatcherL1TransportAttestationContext[],
        );
  return watcherSameCanonicalJson(recomputed, consistency)
    ? Object.freeze({
        observationIds,
        chainPointIds,
        observations: Object.freeze(decoded),
        consistency: recomputed,
      })
    : null;
};

export const verifyPersistedReplacementEvidence = (
  policy: WatcherFinalityPolicy,
  store: WatcherDurableStore,
  transition: Extract<ParsedFinalityTransition, { readonly kind: "rewind" }>,
  transportAttestationsInput: unknown,
  authenticatedSnapshotEvidence:
    | typeof AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
    | null = null,
): PersistedConsistencyEvidence | null => {
  const evidence = verifyPersistedConsistencyEvidence(
    policy,
    store,
    transition.consistency,
    transportAttestationsInput,
    undefined,
    authenticatedSnapshotEvidence,
  );
  const agreement = transition.consistency.agreement;
  const replacement = transition.next
    .pending as WatcherFinalityBoundObservation;
  return evidence !== null &&
    transition.consistency.status === "agreed" &&
    transition.consistency.protocolDecision === "allowed" &&
    agreement !== null &&
    agreement.pointDigest === replacement.pointDigest &&
    agreement.blockHash === replacement.blockHash &&
    agreement.slot === replacement.slot &&
    agreement.blockNo === replacement.blockNo &&
    agreement.minimumDepth === replacement.currentDepth &&
    agreement.blockContentDigest === replacement.blockContentDigest
    ? evidence
    : null;
};
