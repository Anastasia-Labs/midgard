import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  encodeWatcherNormalizedL1Block,
  type WatcherNormalizedL1Block,
} from "../../src/l1/l1-adapter.js";
import { type WatcherRollbackRemovedRecords } from "../../src/l1/rollback-engine.js";
import {
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  type WatcherDurableRecords,
  type WatcherDurableStore,
} from "../../src/storage/durable-store.js";
import { sha256Canonical } from "../support/canonical-json.js";
import {
  type Point,
  type WatcherEvidenceSet,
} from "./crash-rollback-matrix.test-tls-identities.js";

export const evidenceOf = (store: WatcherDurableStore): WatcherEvidenceSet => ({
  faultIds: store.faults.map(({ faultId }) => faultId).sort(),
  proofInputIds: store.daProofInputs.map(({ inputId }) => inputId).sort(),
  reconstructedBlockHashes: store.reconstructedStates
    .map(({ blockHash }) => blockHash)
    .sort(),
  observationIds: store.l1Observations
    .map(({ observationId }) => observationId)
    .sort(),
  correctionIds: store.correctionResults
    .map(({ correctionId }) => correctionId)
    .sort(),
});

/**
 * The evidence a rewind or recovery is *allowed* to drop is exactly the set it
 * reports as removed. Subtracting the reported set from the baseline turns
 * "no lost evidence" into an exact accounting rather than a weaker subset
 * claim: anything the engine drops silently is still counted as lost.
 */
export const retainedBaseline = (
  baseline: WatcherEvidenceSet,
  removed: WatcherRollbackRemovedRecords,
): WatcherEvidenceSet => {
  const without = (
    values: readonly string[],
    dropped: readonly string[],
  ): readonly string[] => {
    const droppedSet = new Set(dropped);
    return values.filter((value) => !droppedSet.has(value));
  };
  return {
    faultIds: without(baseline.faultIds, removed.faultIds),
    proofInputIds: without(baseline.proofInputIds, removed.daProofInputIds),
    reconstructedBlockHashes: without(
      baseline.reconstructedBlockHashes,
      removed.reconstructedBlockHashes,
    ),
    observationIds: without(baseline.observationIds, removed.l1ObservationIds),
    correctionIds: without(baseline.correctionIds, removed.correctionResultIds),
  };
};

export const countDuplicates = (values: readonly string[]): number =>
  values.length - new Set(values).size;

export type WatcherWorkflowMeasurement = Readonly<{
  doubleSubmits: number;
  duplicateRewards: number;
  lostEvidence: number;
  falseVerifiedStates: number;
  unrecoverableWorkflows: number;
  publicDataViolations: number;
  sourceConsistencyViolations: number;
  maturityViolations: number;
  disabledFamilyFaults: number;
  ready: boolean;
}>;

/* ------------------------------------------------------------------------ */
/* Rollback fixtures                                                         */
/* ------------------------------------------------------------------------ */

export type Graph = Readonly<{ records: WatcherDurableRecords }>;

export const combine = (
  deploymentMarker: ReturnType<typeof makeDeploymentMarker>,
  revision: string,
  graphs: readonly Graph[],
  persistedObservations: readonly WatcherNormalizedL1Block[] = [],
): WatcherDurableStore => {
  const persistedChainPoints = [
    ...new Map(
      persistedObservations.map((value) => [
        value.chainPoint.chainPointId,
        {
          chainPointId: value.chainPoint.chainPointId,
          providerId: value.provider.providerId,
          blockHash: value.chainPoint.blockHash,
          slot: value.chainPoint.slot,
          blockNo: value.chainPoint.blockNo,
          depth: value.chainPoint.depth,
        },
      ]),
    ).values(),
  ];
  return makeWatcherDurableStore({
    deploymentMarker,
    revision,
    records: {
      l1Observations: graphs
        .flatMap(({ records }) => records.l1Observations)
        .concat(
          persistedObservations.map((value) => ({
            observationId: value.observationDigest,
            providerId: value.provider.providerId,
            chainPointId: value.chainPoint.chainPointId,
            payload: makeWatcherDurablePayload(
              encodeWatcherNormalizedL1Block(value).toString("hex"),
            ),
          })),
        ),
      chainPoints: graphs
        .flatMap(({ records }) => records.chainPoints)
        .concat(persistedChainPoints),
      protocolUtxos: graphs.flatMap(({ records }) => records.protocolUtxos),
      spentProtocolUtxos: graphs.flatMap(
        ({ records }) => records.spentProtocolUtxos,
      ),
      daProofInputs: graphs.flatMap(({ records }) => records.daProofInputs),
      reconstructedStates: graphs.flatMap(
        ({ records }) => records.reconstructedStates,
      ),
      decisions: graphs.flatMap(({ records }) => records.decisions),
      faults: graphs.flatMap(({ records }) => records.faults),
      submissions: graphs.flatMap(({ records }) => records.submissions),
      confirmations: graphs.flatMap(({ records }) => records.confirmations),
      retries: graphs.flatMap(({ records }) => records.retries),
      deadlines: graphs.flatMap(({ records }) => records.deadlines),
      correctionResults: graphs.flatMap(
        ({ records }) => records.correctionResults,
      ),
    },
  });
};

export const recoveryPoints = (
  branch: "old" | "replacement",
  common: Point,
  length: number,
  finalDepth: string,
): readonly Point[] => {
  const points: Point[] = [common];
  for (let index = 1; index <= length; index += 1) {
    const previous = points.at(-1)!;
    points.push({
      blockHash: sha256Canonical({ branch, index }),
      parentBlockHash: previous.blockHash,
      blockNo: (BigInt(common.blockNo) + BigInt(index)).toString(),
      slot: (BigInt(common.slot) + BigInt(index)).toString(),
      depth: index === length ? finalDepth : "0",
    });
  }
  return points;
};
