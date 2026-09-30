import { type WatcherDurableStore } from "../../storage/durable-store.js";
import {
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import {
  emptyRemovedRecords,
  exactArray,
  exactPlainRecord,
} from "./records.js";
import {
  AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE,
  type PersistedConsistencyEvidence,
  type PersistedObservationIndex,
  verifyPersistedConsistencyEvidence,
} from "./state.js";
import {
  sha256Canonical,
  WATCHER_POST_FINALITY_RECOVERY_RESULT_SCHEMA_VERSION,
  WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION,
  WATCHER_ROLLBACK_BOUNDS,
  type WatcherPostFinalityRecoveryPath,
  type WatcherPostFinalityRecoveryReasonCode,
  type WatcherPostFinalityRecoveryResult,
  type WatcherPostFinalityRecoveryState,
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

export const makePostFinalityRecoveryResult = (
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

export const rejectPostFinalityRecovery = (
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

export const verifyPostFinalityPath = (
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

export const sameAgreementPoint = (
  left: NonNullable<WatcherMultiProviderConsistency["agreement"]>,
  right: NonNullable<WatcherMultiProviderConsistency["agreement"]>,
): boolean =>
  left.pointDigest === right.pointDigest &&
  left.blockHash === right.blockHash &&
  left.slot === right.slot &&
  left.blockNo === right.blockNo &&
  left.blockContentDigest === right.blockContentDigest;

export const makeRecoveryPath = (
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

export const makeResumableFinalityState = (
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

export const makePostFinalityRecoveryState = (
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
