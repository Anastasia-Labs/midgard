import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  type CanonicalJson,
  canonicalJson,
  fail,
  sha256Utf8,
  WATCHER_DURABLE_CACHE_SCHEMA_VERSION,
  watcherSha256CanonicalJson,
} from "./durable-store.canonical-json.js";
import {
  type CacheNamespace,
  compareChainPointOrder,
  type WatcherBlockDecision,
  type WatcherConfirmation,
  type WatcherCorrectionResult,
  type WatcherDaProofInput,
  type WatcherDeadline,
  type WatcherDurableCaches,
  type WatcherDurableRecords,
  type WatcherFault,
  type WatcherL1ChainPoint,
  type WatcherL1Observation,
  type WatcherProtocolUtxo,
  type WatcherReconstructedState,
  type WatcherRetry,
  type WatcherSpentProtocolUtxo,
  type WatcherSubmission,
} from "./durable-store.parse-l1-observation.js";

export const assertReferences = (records: WatcherDurableRecords): void => {
  const chainPoints = new Map(
    records.chainPoints.map((entry) => [entry.chainPointId, entry]),
  );
  const inputs = new Set(records.daProofInputs.map((entry) => entry.inputId));
  const reconstructed = new Set(
    records.reconstructedStates.map((entry) => entry.blockHash),
  );
  const decisions = new Map(
    records.decisions.map((entry) => [entry.blockHash, entry]),
  );
  const faults = new Set(records.faults.map((entry) => entry.faultId));
  const submissions = new Map(
    records.submissions.map((entry) => [entry.submissionId, entry]),
  );
  const confirmations = new Map(
    records.confirmations.map((entry) => [entry.confirmationId, entry]),
  );
  const assertUniqueRelation = <T>(
    entries: readonly T[],
    relationKey: (entry: T) => string,
    path: string,
  ): void => {
    const seen = new Set<string>();
    for (const entry of entries) {
      const key = relationKey(entry);
      if (seen.has(key)) {
        fail("duplicate_key", path);
      }
      seen.add(key);
    }
  };

  assertUniqueRelation(
    records.submissions,
    (entry) => entry.txBodyHash,
    "$.submissions.txBodyHash",
  );
  assertUniqueRelation(
    records.retries,
    (entry) => `${entry.submissionId}\u0000${entry.attempt}`,
    "$.retries.(submissionId,attempt)",
  );
  assertUniqueRelation(
    records.deadlines,
    (entry) =>
      `${entry.subjectKind}\u0000${entry.subjectId}\u0000${entry.kind}`,
    "$.deadlines.(subjectKind,subjectId,kind)",
  );
  assertUniqueRelation(
    records.correctionResults,
    (entry) => entry.faultId,
    "$.correctionResults.faultId",
  );

  for (const observation of records.l1Observations) {
    const point = chainPoints.get(observation.chainPointId);
    if (point === undefined || point.providerId !== observation.providerId) {
      fail(
        "broken_reference",
        `$.l1Observations.${observation.observationId}.chainPointId`,
      );
    }
  }
  for (const utxo of records.protocolUtxos) {
    if (!chainPoints.has(utxo.chainPointId)) {
      fail("broken_reference", `$.protocolUtxos.${utxo.outRef}.chainPointId`);
    }
  }
  const activeOutRefs = new Set(
    records.protocolUtxos.map(({ outRef }) => outRef),
  );
  for (const utxo of records.spentProtocolUtxos) {
    const creationPoint = chainPoints.get(utxo.chainPointId);
    const spentAtPoint = chainPoints.get(utxo.spentAtChainPointId);
    if (
      activeOutRefs.has(utxo.outRef) ||
      creationPoint === undefined ||
      spentAtPoint === undefined ||
      compareChainPointOrder(spentAtPoint, creationPoint) < 0
    ) {
      fail(
        activeOutRefs.has(utxo.outRef) ? "duplicate_key" : "broken_reference",
        `$.spentProtocolUtxos.${utxo.outRef}`,
      );
    }
  }
  for (const state of records.reconstructedStates) {
    if (!chainPoints.has(state.chainPointId)) {
      fail(
        "broken_reference",
        `$.reconstructedStates.${state.blockHash}.chainPointId`,
      );
    }
    for (const inputId of state.inputIds) {
      if (!inputs.has(inputId)) {
        fail(
          "broken_reference",
          `$.reconstructedStates.${state.blockHash}.inputIds`,
        );
      }
    }
  }
  for (const decision of records.decisions) {
    if (!reconstructed.has(decision.blockHash)) {
      fail("broken_reference", `$.decisions.${decision.blockHash}.blockHash`);
    }
  }
  for (const fault of records.faults) {
    const decision = decisions.get(fault.blockHash);
    if (
      decision === undefined ||
      (decision.decision !== "fault_detected" &&
        decision.decision !== "fault_proven" &&
        decision.decision !== "removed_or_resolved")
    ) {
      fail("broken_reference", `$.faults.${fault.faultId}.blockHash`);
    }
  }
  for (const submission of records.submissions) {
    if (!faults.has(submission.faultId)) {
      fail(
        "broken_reference",
        `$.submissions.${submission.submissionId}.faultId`,
      );
    }
  }
  for (const confirmation of records.confirmations) {
    if (
      !submissions.has(confirmation.submissionId) ||
      !chainPoints.has(confirmation.chainPointId)
    ) {
      fail(
        "broken_reference",
        `$.confirmations.${confirmation.confirmationId}`,
      );
    }
  }
  for (const retry of records.retries) {
    if (!submissions.has(retry.submissionId)) {
      fail("broken_reference", `$.retries.${retry.retryId}.submissionId`);
    }
  }
  for (const deadline of records.deadlines) {
    const targetExists =
      deadline.subjectKind === "fault"
        ? faults.has(deadline.subjectId)
        : submissions.has(deadline.subjectId);
    if (!targetExists) {
      fail("broken_reference", `$.deadlines.${deadline.deadlineId}.subjectId`);
    }
  }
  for (const result of records.correctionResults) {
    const confirmation = confirmations.get(result.confirmationId);
    const submission =
      confirmation === undefined
        ? undefined
        : submissions.get(confirmation.submissionId);
    if (
      !faults.has(result.faultId) ||
      confirmation?.status !== "confirmed" ||
      submission?.faultId !== result.faultId
    ) {
      fail("broken_reference", `$.correctionResults.${result.correctionId}`);
    }
  }
};

type CacheSource = WatcherDurableRecords &
  Readonly<{ deploymentMarker: DeploymentMarker }>;

const cacheCollections = (
  source: CacheSource,
): readonly Readonly<{
  namespace: CacheNamespace;
  records: readonly unknown[];
  keyOf: (record: never) => string;
}>[] => [
  {
    namespace: "l1_observations",
    records: source.l1Observations,
    keyOf: (record: WatcherL1Observation) => record.observationId,
  },
  {
    namespace: "chain_points",
    records: source.chainPoints,
    keyOf: (record: WatcherL1ChainPoint) => record.chainPointId,
  },
  {
    namespace: "protocol_utxos",
    records: source.protocolUtxos,
    keyOf: (record: WatcherProtocolUtxo) => record.outRef,
  },
  {
    namespace: "spent_protocol_utxos",
    records: source.spentProtocolUtxos,
    keyOf: (record: WatcherSpentProtocolUtxo) => record.outRef,
  },
  {
    namespace: "da_proof_inputs",
    records: source.daProofInputs,
    keyOf: (record: WatcherDaProofInput) => record.inputId,
  },
  {
    namespace: "reconstructed_states",
    records: source.reconstructedStates,
    keyOf: (record: WatcherReconstructedState) => record.blockHash,
  },
  {
    namespace: "decisions",
    records: source.decisions,
    keyOf: (record: WatcherBlockDecision) => record.blockHash,
  },
  {
    namespace: "faults",
    records: source.faults,
    keyOf: (record: WatcherFault) => record.faultId,
  },
  {
    namespace: "submissions",
    records: source.submissions,
    keyOf: (record: WatcherSubmission) => record.submissionId,
  },
  {
    namespace: "confirmations",
    records: source.confirmations,
    keyOf: (record: WatcherConfirmation) => record.confirmationId,
  },
  {
    namespace: "retries",
    records: source.retries,
    keyOf: (record: WatcherRetry) => record.retryId,
  },
  {
    namespace: "deadlines",
    records: source.deadlines,
    keyOf: (record: WatcherDeadline) => record.deadlineId,
  },
  {
    namespace: "correction_results",
    records: source.correctionResults,
    keyOf: (record: WatcherCorrectionResult) => record.correctionId,
  },
];

export const rebuildWatcherDurableCaches = (
  source: CacheSource,
): WatcherDurableCaches => {
  const sourceSha256 = sha256Utf8(canonicalJson(source as CanonicalJson));
  const entries = cacheCollections(source)
    .flatMap(({ namespace, records, keyOf }) =>
      records.map((record, index) => ({
        namespace,
        key: keyOf(record as never),
        index: index.toString(),
        // Same digest; a frozen, validated record reuses its cached digest.
        recordSha256: watcherSha256CanonicalJson(record),
      })),
    )
    .sort((left, right) => {
      const namespaceOrder = left.namespace.localeCompare(right.namespace);
      return namespaceOrder === 0
        ? left.key.localeCompare(right.key)
        : namespaceOrder;
    });
  return {
    schemaVersion: WATCHER_DURABLE_CACHE_SCHEMA_VERSION,
    sourceSha256,
    entries,
  };
};
