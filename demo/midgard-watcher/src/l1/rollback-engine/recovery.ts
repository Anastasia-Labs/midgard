import {
  makeWatcherDurableStore,
  parseWatcherDurableStore,
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import {
  parseWatcherFinalityPolicy,
  parseWatcherFinalityState,
  type WatcherFinalityIncident,
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import {
  emptyRemovedRecords,
  exactArray,
  exactPlainRecord,
  exactUnrestrictedStringArray,
  marker,
  sameMarker,
} from "./records.js";
import {
  AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE,
  indexPersistedObservations,
  makeEpochBootstrapState,
  parseRemovedRecords,
  parseWatcherRollbackState,
  type PersistedConsistencyEvidence,
  type PersistedObservationIndex,
  planRewind,
  removedRecordCount,
  storeDigest,
  verifyPersistedConsistencyEvidence,
} from "./state.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  isNetwork,
  type ParsedFinalityTransition,
  rollbackDurableAuthorityRuntime,
  sha256Canonical,
  WATCHER_POST_FINALITY_RECOVERY_RESULT_SCHEMA_VERSION,
  WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION,
  WATCHER_ROLLBACK_BOUNDS,
  type WatcherPostFinalityRecoveryInput,
  type WatcherPostFinalityRecoveryPath,
  type WatcherPostFinalityRecoveryReasonCode,
  type WatcherPostFinalityRecoveryResult,
  type WatcherPostFinalityRecoveryState,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackRemovedRecords,
  type WatcherRollbackState,
} from "./types.js";

type VerifiedPostFinalityPath = Readonly<{
  agreements: readonly NonNullable<
    WatcherMultiProviderConsistency["agreement"]
  >[];
  evidence: readonly PersistedConsistencyEvidence[];
  consistencyDigests: readonly string[];
  observationIds: ReadonlySet<string>;
  chainPointIds: ReadonlySet<string>;
}>;

const makePostFinalityRecoveryResult = (
  value: Omit<
    WatcherPostFinalityRecoveryResult,
    "schemaVersion" | "resultDigest"
  >,
): WatcherPostFinalityRecoveryResult => {
  const canonical = {
    schemaVersion: WATCHER_POST_FINALITY_RECOVERY_RESULT_SCHEMA_VERSION,
    action: value.action,
    protocolDecision: value.protocolDecision,
    reasonCodes: Object.freeze([...value.reasonCodes]),
    sourceRevision: value.sourceRevision,
    nextRevision: value.nextRevision,
    sourceStoreDigest: value.sourceStoreDigest,
    nextStoreDigest: value.nextStoreDigest,
    removedRecords: value.removedRecords,
    nextStore: value.nextStore,
    resumableFinalityState: value.resumableFinalityState,
    resumableRollbackState: value.resumableRollbackState,
    resumableRollbackBootstrapState: value.resumableRollbackBootstrapState,
    resumableTrustedCheckpointStateDigest:
      value.resumableTrustedCheckpointStateDigest,
    recoveryState: value.recoveryState,
  };
  return Object.freeze({
    ...canonical,
    resultDigest: sha256Canonical(canonical),
  });
};

const rejectPostFinalityRecovery = (
  reason: WatcherPostFinalityRecoveryReasonCode,
): WatcherPostFinalityRecoveryResult =>
  makePostFinalityRecoveryResult({
    action: "reject",
    protocolDecision: "quarantined",
    reasonCodes: [reason],
    sourceRevision: null,
    nextRevision: null,
    sourceStoreDigest: null,
    nextStoreDigest: null,
    removedRecords: emptyRemovedRecords(),
    nextStore: null,
    resumableFinalityState: null,
    resumableRollbackState: null,
    resumableRollbackBootstrapState: null,
    resumableTrustedCheckpointStateDigest: null,
    recoveryState: null,
  });

const verifyPostFinalityPath = (
  policy: WatcherFinalityPolicy,
  store: WatcherDurableStore,
  value: unknown,
  persistedIndex: PersistedObservationIndex,
  transportAttestationsInput: unknown,
  authenticatedSnapshotEvidence:
    | typeof AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
    | null = null,
): VerifiedPostFinalityPath | WatcherPostFinalityRecoveryReasonCode => {
  const inputs = exactArray(value);
  if (inputs === null || inputs.length < 2) {
    return "recovery_path_malformed";
  }
  if (
    inputs.length >
    Number(WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth) + 1
  ) {
    return "recovery_depth_exceeded";
  }
  const evidence: PersistedConsistencyEvidence[] = [];
  const agreements: NonNullable<
    WatcherMultiProviderConsistency["agreement"]
  >[] = [];
  for (const input of inputs) {
    if (
      exactPlainRecord(input, [
        "schemaVersion",
        "status",
        "protocolDecision",
        "sourceMode",
        "configuredNetwork",
        "configuredSourceDigest",
        "authorityNodeId",
        "authorityGenesisIdentitySha256",
        "authorityChainSyncSocketPath",
        "chainAuthorityObservationDigest",
        "queryObservationCount",
        "observationCount",
        "independentProviderCount",
        "externalProviderBindings",
        "localQueryServiceBindings",
        "reasonCodes",
        "alertCodes",
        "observationEvidenceDigests",
        "rejectedObservationCount",
        "agreement",
        "consistencyDigest",
      ]) === null
    ) {
      return "recovery_path_malformed";
    }
    const persisted = verifyPersistedConsistencyEvidence(
      policy,
      store,
      input as WatcherMultiProviderConsistency,
      transportAttestationsInput,
      persistedIndex,
      authenticatedSnapshotEvidence,
    );
    const consistency = persisted?.consistency;
    if (
      persisted === null ||
      consistency === undefined ||
      consistency.status !== "agreed" ||
      consistency.protocolDecision !== "allowed" ||
      consistency.agreement === null ||
      (policy.sourceMode === "local_node"
        ? consistency.independentProviderCount !== 1 ||
          consistency.authorityNodeId !== policy.authorityNodeId ||
          consistency.chainAuthorityObservationDigest === null ||
          consistency.localQueryServiceBindings.length !==
            policy.localQueryServices.length ||
          consistency.localQueryServiceBindings.some(
            ({ observationStatus }) => observationStatus !== "aligned",
          )
        : consistency.independentProviderCount < 2 ||
          consistency.externalProviderBindings.length < 2 ||
          new Set(
            consistency.externalProviderBindings.map(
              ({ operatorIdentitySha256 }) => operatorIdentitySha256,
            ),
          ).size !== consistency.externalProviderBindings.length)
    ) {
      return "canonical_agreement_required";
    }
    evidence.push(persisted);
    agreements.push(consistency.agreement);
  }
  for (let index = 1; index < evidence.length; index += 1) {
    const previous = agreements[index - 1]!;
    const current = agreements[index]!;
    if (
      BigInt(current.blockNo) !== BigInt(previous.blockNo) + 1n ||
      BigInt(current.slot) <= BigInt(previous.slot) ||
      evidence[index]!.observations.some(
        ({ chainPoint }) =>
          chainPoint.parentBlockHash !== previous.blockHash ||
          chainPoint.blockHash !== current.blockHash ||
          chainPoint.blockNo !== current.blockNo ||
          chainPoint.slot !== current.slot,
      )
    ) {
      return "recovery_path_gap";
    }
  }
  return Object.freeze({
    agreements: Object.freeze(agreements),
    evidence: Object.freeze(evidence),
    consistencyDigests: Object.freeze(
      evidence.map(({ consistency }) => consistency.consistencyDigest),
    ),
    observationIds: new Set(
      evidence.flatMap(({ observationIds }) => [...observationIds]),
    ),
    chainPointIds: new Set(
      evidence.flatMap(({ chainPointIds }) => [...chainPointIds]),
    ),
  });
};

const sameAgreementPoint = (
  left: NonNullable<WatcherMultiProviderConsistency["agreement"]>,
  right: NonNullable<WatcherMultiProviderConsistency["agreement"]>,
): boolean =>
  left.pointDigest === right.pointDigest &&
  left.blockHash === right.blockHash &&
  left.slot === right.slot &&
  left.blockNo === right.blockNo &&
  left.blockContentDigest === right.blockContentDigest;

const makeRecoveryPath = (
  previous: VerifiedPostFinalityPath,
  replacement: VerifiedPostFinalityPath,
): WatcherPostFinalityRecoveryPath => {
  const commonAncestor = previous.agreements[0]!;
  const orphanedFinalized = previous.agreements.at(-1)!;
  const replacementTip = replacement.agreements.at(-1)!;
  const canonical = {
    commonAncestorPointDigest: commonAncestor.pointDigest,
    commonAncestorBlockHash: commonAncestor.blockHash,
    commonAncestorBlockNo: commonAncestor.blockNo,
    orphanedFinalizedPointDigest: orphanedFinalized.pointDigest,
    orphanedFinalizedBlockHash: orphanedFinalized.blockHash,
    replacementTipPointDigest: replacementTip.pointDigest,
    replacementTipBlockHash: replacementTip.blockHash,
    replacementTipBlockNo: replacementTip.blockNo,
    rollbackDepth: (
      BigInt(orphanedFinalized.blockNo) - BigInt(commonAncestor.blockNo)
    ).toString(),
    previousConsistencyDigests: previous.consistencyDigests,
    replacementConsistencyDigests: replacement.consistencyDigests,
  };
  return Object.freeze({
    ...canonical,
    pathDigest: sha256Canonical(canonical),
  });
};

const makeResumableFinalityState = (
  policy: WatcherFinalityPolicy,
): WatcherFinalityState => {
  const canonical = {
    schemaVersion: "midgard-watcher-finality-state-v1" as const,
    policyDigest: policy.policyDigest,
    network: policy.network,
    blueprintHash: policy.blueprintHash,
    deploymentMarker: policy.deploymentMarker,
    phase: "unobserved" as const,
    pending: null,
    finalized: null,
    incident: null,
  };
  return Object.freeze({
    ...canonical,
    stateDigest: sha256Canonical(canonical),
  });
};

const makePostFinalityRecoveryState = (
  policy: WatcherFinalityPolicy,
  sourceRollbackState: WatcherRollbackState,
  sourceStoreDigest: string,
  nextStoreDigest: string,
  path: WatcherPostFinalityRecoveryPath,
  removedRecords: WatcherRollbackRemovedRecords,
  resumableFinalityState: WatcherFinalityState,
): WatcherPostFinalityRecoveryState => {
  const detectedIncident = sourceRollbackState.incident!;
  const lifecycleCanonical = {
    detectedIncidentDigest: detectedIncident.incidentDigest,
    detectedReasonCode: detectedIncident.reasonCode,
    detectedTriggerConsistencyDigest: detectedIncident.triggerConsistencyDigest,
    detectedFinalityStateDigest: detectedIncident.finalityStateDigest,
    detectedStoreDigest: sourceStoreDigest,
    status: "recovered" as const,
    recoveryPathDigest: path.pathDigest,
    recoveredStoreDigest: nextStoreDigest,
    resumableFinalityStateDigest: resumableFinalityState.stateDigest,
  };
  const incidentLifecycle = Object.freeze({
    ...lifecycleCanonical,
    lifecycleDigest: sha256Canonical(lifecycleCanonical),
  });
  const canonical = {
    schemaVersion: WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    blueprintHash: policy.blueprintHash,
    deploymentMarker: policy.deploymentMarker,
    sourceRollbackStateDigest: sourceRollbackState.stateDigest,
    sourceStoreDigest,
    nextStoreDigest,
    path,
    removedRecords,
    resumableFinalityState,
    incidentLifecycle,
  };
  return Object.freeze({
    ...canonical,
    stateDigest: sha256Canonical(canonical),
  });
};

const parseRecoveryPath = (
  value: unknown,
): WatcherPostFinalityRecoveryPath | null => {
  const path = exactPlainRecord(value, [
    "commonAncestorPointDigest",
    "commonAncestorBlockHash",
    "commonAncestorBlockNo",
    "orphanedFinalizedPointDigest",
    "orphanedFinalizedBlockHash",
    "replacementTipPointDigest",
    "replacementTipBlockHash",
    "replacementTipBlockNo",
    "rollbackDepth",
    "previousConsistencyDigests",
    "replacementConsistencyDigests",
    "pathDigest",
  ]);
  if (
    path === null ||
    ![
      path.commonAncestorPointDigest,
      path.commonAncestorBlockHash,
      path.orphanedFinalizedPointDigest,
      path.orphanedFinalizedBlockHash,
      path.replacementTipPointDigest,
      path.replacementTipBlockHash,
      path.pathDigest,
    ].every((digest) => typeof digest === "string" && HEX_32.test(digest)) ||
    ![
      path.commonAncestorBlockNo,
      path.replacementTipBlockNo,
      path.rollbackDepth,
    ].every(
      (natural) =>
        typeof natural === "string" && CANONICAL_NATURAL.test(natural),
    )
  ) {
    return null;
  }
  const previousConsistencyDigests = exactUnrestrictedStringArray(
    path.previousConsistencyDigests,
  );
  const replacementConsistencyDigests = exactUnrestrictedStringArray(
    path.replacementConsistencyDigests,
  );
  if (
    previousConsistencyDigests === null ||
    replacementConsistencyDigests === null ||
    previousConsistencyDigests.length < 2 ||
    replacementConsistencyDigests.length < 2 ||
    [...previousConsistencyDigests, ...replacementConsistencyDigests].some(
      (digest) => !HEX_32.test(digest),
    )
  ) {
    return null;
  }
  const canonical = {
    commonAncestorPointDigest: path.commonAncestorPointDigest as string,
    commonAncestorBlockHash: path.commonAncestorBlockHash as string,
    commonAncestorBlockNo: path.commonAncestorBlockNo as string,
    orphanedFinalizedPointDigest: path.orphanedFinalizedPointDigest as string,
    orphanedFinalizedBlockHash: path.orphanedFinalizedBlockHash as string,
    replacementTipPointDigest: path.replacementTipPointDigest as string,
    replacementTipBlockHash: path.replacementTipBlockHash as string,
    replacementTipBlockNo: path.replacementTipBlockNo as string,
    rollbackDepth: path.rollbackDepth as string,
    previousConsistencyDigests: Object.freeze([...previousConsistencyDigests]),
    replacementConsistencyDigests: Object.freeze([
      ...replacementConsistencyDigests,
    ]),
  };
  return sha256Canonical(canonical) === path.pathDigest
    ? Object.freeze({ ...canonical, pathDigest: path.pathDigest as string })
    : null;
};

const decodePostFinalityRecoveryState = (
  value: unknown,
): WatcherPostFinalityRecoveryState | null => {
  try {
    const state = exactPlainRecord(value, [
      "schemaVersion",
      "policyDigest",
      "network",
      "blueprintHash",
      "deploymentMarker",
      "sourceRollbackStateDigest",
      "sourceStoreDigest",
      "nextStoreDigest",
      "path",
      "removedRecords",
      "resumableFinalityState",
      "incidentLifecycle",
      "stateDigest",
    ]);
    if (
      state === null ||
      state.schemaVersion !==
        WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION ||
      !isNetwork(state.network) ||
      ![
        state.policyDigest,
        state.blueprintHash,
        state.sourceRollbackStateDigest,
        state.sourceStoreDigest,
        state.nextStoreDigest,
        state.stateDigest,
      ].every((digest) => typeof digest === "string" && HEX_32.test(digest))
    ) {
      return null;
    }
    const deploymentMarker = marker(state.deploymentMarker);
    const path = parseRecoveryPath(state.path);
    const removedRecords = parseRemovedRecords(state.removedRecords);
    const resumableFinalityState = parseWatcherFinalityState(
      state.resumableFinalityState,
    );
    const lifecycle = exactPlainRecord(state.incidentLifecycle, [
      "detectedIncidentDigest",
      "detectedReasonCode",
      "detectedTriggerConsistencyDigest",
      "detectedFinalityStateDigest",
      "detectedStoreDigest",
      "status",
      "recoveryPathDigest",
      "recoveredStoreDigest",
      "resumableFinalityStateDigest",
      "lifecycleDigest",
    ]);
    if (
      deploymentMarker === null ||
      path === null ||
      removedRecords === null ||
      resumableFinalityState === null ||
      lifecycle === null ||
      lifecycle.status !== "recovered" ||
      lifecycle.detectedReasonCode !== "post_finality_point_changed" ||
      !(
        lifecycle.detectedTriggerConsistencyDigest === null ||
        (typeof lifecycle.detectedTriggerConsistencyDigest === "string" &&
          HEX_32.test(lifecycle.detectedTriggerConsistencyDigest))
      ) ||
      ![
        lifecycle.detectedIncidentDigest,
        lifecycle.detectedFinalityStateDigest,
        lifecycle.detectedStoreDigest,
        lifecycle.recoveryPathDigest,
        lifecycle.recoveredStoreDigest,
        lifecycle.resumableFinalityStateDigest,
        lifecycle.lifecycleDigest,
      ].every((digest) => typeof digest === "string" && HEX_32.test(digest))
    ) {
      return null;
    }
    const lifecycleCanonical = {
      detectedIncidentDigest: lifecycle.detectedIncidentDigest as string,
      detectedReasonCode:
        lifecycle.detectedReasonCode as WatcherFinalityIncident["reasonCode"],
      detectedTriggerConsistencyDigest:
        lifecycle.detectedTriggerConsistencyDigest as string | null,
      detectedFinalityStateDigest:
        lifecycle.detectedFinalityStateDigest as string,
      detectedStoreDigest: lifecycle.detectedStoreDigest as string,
      status: "recovered" as const,
      recoveryPathDigest: lifecycle.recoveryPathDigest as string,
      recoveredStoreDigest: lifecycle.recoveredStoreDigest as string,
      resumableFinalityStateDigest:
        lifecycle.resumableFinalityStateDigest as string,
    };
    if (
      sha256Canonical(lifecycleCanonical) !== lifecycle.lifecycleDigest ||
      lifecycleCanonical.detectedStoreDigest !== state.sourceStoreDigest ||
      lifecycleCanonical.recoveryPathDigest !== path.pathDigest ||
      lifecycleCanonical.recoveredStoreDigest !== state.nextStoreDigest ||
      lifecycleCanonical.resumableFinalityStateDigest !==
        resumableFinalityState.stateDigest
    ) {
      return null;
    }
    const canonical = {
      schemaVersion: WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION,
      policyDigest: state.policyDigest as string,
      network: state.network as WatcherPostFinalityRecoveryState["network"],
      blueprintHash: state.blueprintHash as string,
      deploymentMarker,
      sourceRollbackStateDigest: state.sourceRollbackStateDigest as string,
      sourceStoreDigest: state.sourceStoreDigest as string,
      nextStoreDigest: state.nextStoreDigest as string,
      path,
      removedRecords,
      resumableFinalityState,
      incidentLifecycle: Object.freeze({
        ...lifecycleCanonical,
        lifecycleDigest: lifecycle.lifecycleDigest as string,
      }),
    };
    return sha256Canonical(canonical) === state.stateDigest
      ? Object.freeze({
          ...canonical,
          stateDigest: state.stateDigest as string,
        })
      : null;
  } catch {
    return null;
  }
};

/**
 * Resolves a durable post-finality incident only after W10 bytes and W11
 * decisions prove both sides of the exact common ancestor. The recovery is
 * one W03 revision: every dependent orphan record is removed together while
 * canonical replacement evidence remains available for deterministic replay.
 */
const evaluateWatcherPostFinalityRecoveryInternal = (
  input: WatcherPostFinalityRecoveryInput,
): WatcherPostFinalityRecoveryResult => {
  const recoveryInputKeys = [
    "policy",
    "sourceStore",
    "currentStore",
    "quarantinedRollbackState",
    "rollbackBootstrapState",
    "previousCanonicalPath",
    "replacementCanonicalPath",
    "previousRecoveryState",
  ];
  const hasTrustedCheckpointAuthority =
    typeof input === "object" &&
    input !== null &&
    !Array.isArray(input) &&
    Reflect.ownKeys(input).includes("trustedCheckpointAuthority");
  const hasTransportAttestations =
    typeof input === "object" &&
    input !== null &&
    !Array.isArray(input) &&
    Reflect.ownKeys(input).includes("transportAttestations");
  if (
    exactPlainRecord(input, [
      ...recoveryInputKeys,
      ...(hasTrustedCheckpointAuthority ? ["trustedCheckpointAuthority"] : []),
      ...(hasTransportAttestations ? ["transportAttestations"] : []),
    ]) === null
  ) {
    return rejectPostFinalityRecovery("recovery_path_malformed");
  }
  const policy = parseWatcherFinalityPolicy(input.policy);
  if (policy === null) {
    return rejectPostFinalityRecovery("malformed_policy");
  }
  if (
    BigInt(policy.maximumPostFinalityRecoveryDepth) >
    WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth
  ) {
    return rejectPostFinalityRecovery("malformed_policy");
  }
  let sourceStore: WatcherDurableStore;
  let currentStore: WatcherDurableStore;
  try {
    sourceStore = parseWatcherDurableStore(input.sourceStore);
    currentStore = parseWatcherDurableStore(input.currentStore);
  } catch {
    return rejectPostFinalityRecovery("malformed_store");
  }
  if (
    !sameMarker(sourceStore.deploymentMarker, policy.deploymentMarker) ||
    !sameMarker(currentStore.deploymentMarker, policy.deploymentMarker)
  ) {
    return rejectPostFinalityRecovery("malformed_store");
  }
  const sourceStoreDigest = storeDigest(sourceStore);
  const rollbackState = parseWatcherRollbackState(
    input.quarantinedRollbackState,
    {
      policy,
      rollbackBootstrapState: input.rollbackBootstrapState,
      trustedCheckpointAuthority: input.trustedCheckpointAuthority,
      currentStore: sourceStore,
      transportAttestations: input.transportAttestations,
    },
  );
  if (rollbackState === null) {
    return rejectPostFinalityRecovery("malformed_rollback_state");
  }
  if (
    rollbackState.storeDigest !== sourceStoreDigest ||
    rollbackState.incident === null
  ) {
    return rejectPostFinalityRecovery(
      rollbackState.incident === null
        ? "incident_required"
        : "rollback_state_store_mismatch",
    );
  }
  const persistedIndex = indexPersistedObservations(sourceStore);
  const authenticatedSnapshotEvidence = (() => {
    if (
      typeof input.trustedCheckpointAuthority !== "object" ||
      input.trustedCheckpointAuthority === null
    ) {
      return null;
    }
    const runtime = rollbackDurableAuthorityRuntime.get(
      input.trustedCheckpointAuthority as WatcherRollbackDurableAuthority,
    );
    return runtime !== undefined &&
      watcherSameCanonicalJson(runtime.snapshot.currentStore, sourceStore)
      ? AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
      : null;
  })();
  const previousPath = verifyPostFinalityPath(
    policy,
    sourceStore,
    input.previousCanonicalPath,
    persistedIndex,
    input.transportAttestations,
    authenticatedSnapshotEvidence,
  );
  if (typeof previousPath === "string") {
    return rejectPostFinalityRecovery(previousPath);
  }
  const replacementPath = verifyPostFinalityPath(
    policy,
    sourceStore,
    input.replacementCanonicalPath,
    persistedIndex,
    input.transportAttestations,
    authenticatedSnapshotEvidence,
  );
  if (typeof replacementPath === "string") {
    return rejectPostFinalityRecovery(replacementPath);
  }
  const commonAncestor = previousPath.agreements[0]!;
  const replacementAncestor = replacementPath.agreements[0]!;
  const orphanedFinalized = previousPath.agreements.at(-1)!;
  const finalizedBinding = rollbackState.incident.finalizedBinding;
  if (
    !sameAgreementPoint(commonAncestor, replacementAncestor) ||
    previousPath.agreements
      .slice(1)
      .some((oldPoint) =>
        replacementPath.agreements
          .slice(1)
          .some((newPoint) => oldPoint.blockHash === newPoint.blockHash),
      )
  ) {
    return rejectPostFinalityRecovery("common_ancestor_mismatch");
  }
  if (
    orphanedFinalized.pointDigest !== finalizedBinding.pointDigest ||
    orphanedFinalized.blockHash !== finalizedBinding.blockHash ||
    orphanedFinalized.slot !== finalizedBinding.slot ||
    orphanedFinalized.blockNo !== finalizedBinding.blockNo ||
    orphanedFinalized.blockContentDigest !==
      finalizedBinding.blockContentDigest ||
    previousPath.consistencyDigests.at(-1) !==
      finalizedBinding.lastSeenConsistencyDigest
  ) {
    return rejectPostFinalityRecovery("finalized_binding_mismatch");
  }
  if (
    rollbackState.incident.triggerConsistencyDigest === null ||
    replacementPath.consistencyDigests.at(-1) !==
      rollbackState.incident.triggerConsistencyDigest
  ) {
    return rejectPostFinalityRecovery("incident_provenance_mismatch");
  }
  const rollbackDepth =
    BigInt(orphanedFinalized.blockNo) - BigInt(commonAncestor.blockNo);
  if (
    rollbackDepth <= 0n ||
    rollbackDepth > BigInt(policy.maximumPostFinalityRecoveryDepth) ||
    BigInt(replacementPath.agreements.at(-1)!.blockNo) -
      BigInt(commonAncestor.blockNo) >
      BigInt(policy.maximumPostFinalityRecoveryDepth)
  ) {
    return rejectPostFinalityRecovery("recovery_depth_exceeded");
  }
  const path = makeRecoveryPath(previousPath, replacementPath);
  const previousRecoveryState =
    input.previousRecoveryState === null
      ? null
      : decodePostFinalityRecoveryState(input.previousRecoveryState);
  if (input.previousRecoveryState !== null && previousRecoveryState === null) {
    return rejectPostFinalityRecovery("malformed_recovery_state");
  }
  if (
    previousRecoveryState === null &&
    !watcherSameCanonicalJson(currentStore, sourceStore)
  ) {
    return rejectPostFinalityRecovery("recovery_state_mismatch");
  }
  const replacementEvidence: PersistedConsistencyEvidence = Object.freeze({
    consistency: replacementPath.evidence.at(-1)!.consistency,
    observations: Object.freeze(
      replacementPath.evidence.flatMap(({ observations }) => observations),
    ),
    observationIds: replacementPath.observationIds,
    chainPointIds: replacementPath.chainPointIds,
  });
  const firstReplacement = replacementPath.agreements[1]!;
  const syntheticTransition = {
    previous: {
      pending: {
        ...finalizedBinding,
        blockNo: (BigInt(commonAncestor.blockNo) + 1n).toString(),
      },
    },
    next: {
      pending: {
        pointDigest: firstReplacement.pointDigest,
        blockHash: firstReplacement.blockHash,
        slot: firstReplacement.slot,
        blockNo: firstReplacement.blockNo,
        blockContentDigest: firstReplacement.blockContentDigest,
        firstSeenConsistencyDigest: replacementPath.consistencyDigests[1]!,
        lastSeenConsistencyDigest: replacementPath.consistencyDigests[1]!,
        firstSeenDepth: firstReplacement.minimumDepth,
        currentDepth: firstReplacement.minimumDepth,
        visibilityCount: "1",
      },
    },
    instruction: { kind: "pending_point_changed" },
  } as unknown as Extract<
    ParsedFinalityTransition,
    { readonly kind: "rewind" }
  >;
  const plan = planRewind(
    sourceStore,
    syntheticTransition,
    replacementEvidence,
  );
  if (
    plan.removed.l1ObservationIds.some((id) =>
      replacementPath.observationIds.has(id),
    ) ||
    plan.removed.chainPointIds.some((id) =>
      replacementPath.chainPointIds.has(id),
    )
  ) {
    return rejectPostFinalityRecovery("canonical_agreement_required");
  }
  if (removedRecordCount(plan.removed) === 0) {
    return rejectPostFinalityRecovery("unknown_recovery_target");
  }
  const nextStore = makeWatcherDurableStore({
    deploymentMarker: sourceStore.deploymentMarker,
    revision: (BigInt(sourceStore.revision) + 1n).toString(),
    records: plan.records,
  });
  const nextStoreDigest = storeDigest(nextStore);
  const resumableFinalityState = makeResumableFinalityState(policy);
  if (parseWatcherFinalityState(resumableFinalityState, policy) === null) {
    return rejectPostFinalityRecovery("malformed_policy");
  }
  const recoveryState = makePostFinalityRecoveryState(
    policy,
    rollbackState,
    sourceStoreDigest,
    nextStoreDigest,
    path,
    plan.removed,
    resumableFinalityState,
  );
  const resumableRollbackState = makeEpochBootstrapState(
    policy,
    rollbackState,
    nextStore,
    resumableFinalityState,
    {
      stateDigest: recoveryState.stateDigest,
      lifecycleDigest: recoveryState.incidentLifecycle.lifecycleDigest,
    },
  );
  if (previousRecoveryState !== null) {
    const currentStoreDigest = storeDigest(currentStore);
    if (
      currentStoreDigest !== nextStoreDigest ||
      !watcherSameCanonicalJson(currentStore, nextStore) ||
      !watcherSameCanonicalJson(previousRecoveryState, recoveryState)
    ) {
      return rejectPostFinalityRecovery("recovery_state_mismatch");
    }
    return makePostFinalityRecoveryResult({
      action: "duplicate_recovery",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_recovery"],
      sourceRevision: currentStore.revision,
      nextRevision: currentStore.revision,
      sourceStoreDigest: currentStoreDigest,
      nextStoreDigest: currentStoreDigest,
      removedRecords: emptyRemovedRecords(),
      nextStore: currentStore,
      resumableFinalityState,
      resumableRollbackState,
      resumableRollbackBootstrapState: resumableRollbackState,
      resumableTrustedCheckpointStateDigest: resumableRollbackState.stateDigest,
      recoveryState,
    });
  }
  return makePostFinalityRecoveryResult({
    action: "rewind_and_replay",
    protocolDecision: "resume_replay",
    reasonCodes: ["recovery_applied"],
    sourceRevision: sourceStore.revision,
    nextRevision: nextStore.revision,
    sourceStoreDigest,
    nextStoreDigest,
    removedRecords: plan.removed,
    nextStore,
    resumableFinalityState,
    resumableRollbackState,
    resumableRollbackBootstrapState: resumableRollbackState,
    resumableTrustedCheckpointStateDigest: resumableRollbackState.stateDigest,
    recoveryState,
  });
};

export const evaluateWatcherPostFinalityRecovery = (
  input: WatcherPostFinalityRecoveryInput,
): WatcherPostFinalityRecoveryResult => {
  try {
    return evaluateWatcherPostFinalityRecoveryInternal(input);
  } catch {
    return rejectPostFinalityRecovery("recovery_path_malformed");
  }
};

const sameCanonicalStructure = (
  value: unknown,
  canonical: unknown,
  visiting: WeakSet<object> = new WeakSet<object>(),
  depth = 0,
): boolean => {
  if (
    value === null ||
    canonical === null ||
    typeof value !== "object" ||
    typeof canonical !== "object"
  ) {
    return value === canonical;
  }
  if (depth > 256 || visiting.has(value)) {
    return false;
  }
  visiting.add(value);
  try {
    if (Array.isArray(canonical)) {
      const members = exactArray(value);
      return (
        members !== null &&
        members.length === canonical.length &&
        canonical.every((expected, index) =>
          sameCanonicalStructure(members[index], expected, visiting, depth + 1),
        )
      );
    }
    if (Array.isArray(value)) {
      return false;
    }
    const canonicalRecord = canonical as Record<string, unknown>;
    const record = exactPlainRecord(value, Object.keys(canonicalRecord));
    return (
      record !== null &&
      Object.keys(canonicalRecord).every((key) =>
        sameCanonicalStructure(
          record[key],
          canonicalRecord[key],
          visiting,
          depth + 1,
        ),
      )
    );
  } finally {
    visiting.delete(value);
  }
};

/**
 * Shared W13 trust boundary. Candidate self-hashes are not
 * authority: the exact recovery input is replayed and only a safe,
 * byte-equivalent canonical result shape is accepted.
 */
export const parseWatcherPostFinalityRecoveryResult = (
  value: unknown,
  input: WatcherPostFinalityRecoveryInput,
): WatcherPostFinalityRecoveryResult | null => {
  try {
    const expected = evaluateWatcherPostFinalityRecovery(input);
    return sameCanonicalStructure(value, expected) ? expected : null;
  } catch {
    return null;
  }
};
