import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  encodeWatcherNormalizedL1Block,
  type WatcherNormalizedL1Block,
} from "../../src/l1/l1-adapter.js";
import {
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  type WatcherDurableRecords,
  type WatcherDurableStore,
} from "../../src/storage/durable-store.js";
import { sha256Canonical } from "../support/canonical-json.js";
import {
  type Graph,
  hex32,
  payload,
  type Point,
} from "./rollback-engine.test-tls-identities.js";

export const graph = (
  idByte: string,
  point: Point,
  sharedInputId?: string,
): Graph => {
  const ids = {
    observation: hex32(`${idByte[0]}1`),
    chainPoint: hex32(`${idByte[0]}2`),
    outRef: `${hex32(`${idByte[0]}3`)}#0`,
    input: hex32(`${idByte[0]}4`),
    blockHash: hex32(`${idByte[0]}0`),
    fault: hex32(`${idByte[0]}5`),
    submission: hex32(`${idByte[0]}6`),
    confirmation: hex32(`${idByte[0]}7`),
    retry: hex32(`${idByte[0]}8`),
    deadline: hex32(`${idByte[0]}9`),
    correction: hex32(`${idByte[0]}a`),
  };
  const inputIds = [
    ids.input,
    ...(sharedInputId === undefined ? [] : [sharedInputId]),
  ].sort();
  return {
    ids,
    records: {
      l1Observations: [
        {
          observationId: ids.observation,
          providerId: "provider-a",
          chainPointId: ids.chainPoint,
          payload: payload("8100"),
        },
      ],
      chainPoints: [
        {
          chainPointId: ids.chainPoint,
          providerId: "provider-a",
          blockHash: point.blockHash,
          slot: point.slot,
          blockNo: point.blockNo,
          depth: point.depth,
        },
      ],
      protocolUtxos: [
        {
          outRef: ids.outRef,
          role: "state_queue",
          chainPointId: ids.chainPoint,
          output: payload("d87980"),
        },
      ],
      spentProtocolUtxos: [],
      daProofInputs: [
        {
          inputId: ids.input,
          kind: "da_payload",
          payload: payload("4401020304"),
        },
      ],
      reconstructedStates: [
        {
          blockHash: ids.blockHash,
          chainPointId: ids.chainPoint,
          priorStateRoot: hex32(`${idByte[0]}b`),
          postStateRoot: hex32(`${idByte[0]}c`),
          inputIds,
          state: payload("82190100190101"),
        },
      ],
      decisions: [
        {
          blockHash: ids.blockHash,
          decision: "fault_detected",
          reconstructionDigest: hex32(`${idByte[0]}d`),
          evidenceDigest: hex32(`${idByte[0]}e`),
        },
      ],
      faults: [
        {
          faultId: ids.fault,
          blockHash: ids.blockHash,
          familyId: "transition-trace",
          evidence: payload("a10001"),
        },
      ],
      submissions: [
        {
          submissionId: ids.submission,
          faultId: ids.fault,
          txBodyHash: hex32(`${idByte[0]}f`),
          status: "submitted",
        },
      ],
      confirmations: [
        {
          confirmationId: ids.confirmation,
          submissionId: ids.submission,
          txHash: hex32(`${idByte[1]}0`),
          chainPointId: ids.chainPoint,
          depth: point.depth,
          status: "confirmed",
        },
      ],
      retries: [
        {
          retryId: ids.retry,
          submissionId: ids.submission,
          attempt: "1",
          nextEligibleSlot: (BigInt(point.slot) + 1n).toString(),
          reason: "rollback",
        },
      ],
      deadlines: [
        {
          deadlineId: ids.deadline,
          subjectKind: "submission",
          subjectId: ids.submission,
          kind: "rollback",
          expiresAtSlot: (BigInt(point.slot) + 10n).toString(),
        },
      ],
      correctionResults: [
        {
          correctionId: ids.correction,
          faultId: ids.fault,
          confirmationId: ids.confirmation,
          outcome: "removed",
          finalStateRoot: hex32(`${idByte[1]}1`),
          slashLovelace: "0",
          rewardLovelace: "0",
        },
      ],
    },
  };
};

export const combine = (
  deploymentMarker: ReturnType<typeof makeDeploymentMarker>,
  revision: string,
  graphs: readonly Graph[],
  sharedInputId?: string,
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
  const records: WatcherDurableRecords = {
    l1Observations: graphs
      .flatMap(({ records: value }) => value.l1Observations)
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
      .flatMap(({ records: value }) => value.chainPoints)
      .concat(persistedChainPoints),
    protocolUtxos: graphs.flatMap(({ records: value }) => value.protocolUtxos),
    spentProtocolUtxos: graphs.flatMap(
      ({ records: value }) => value.spentProtocolUtxos,
    ),
    daProofInputs: [
      ...graphs.flatMap(({ records: value }) => value.daProofInputs),
      ...(sharedInputId === undefined
        ? []
        : [
            {
              inputId: sharedInputId,
              kind: "proof_input" as const,
              payload: payload("820102"),
            },
          ]),
    ],
    reconstructedStates: graphs.flatMap(
      ({ records: value }) => value.reconstructedStates,
    ),
    decisions: graphs.flatMap(({ records: value }) => value.decisions),
    faults: graphs.flatMap(({ records: value }) => value.faults),
    submissions: graphs.flatMap(({ records: value }) => value.submissions),
    confirmations: graphs.flatMap(({ records: value }) => value.confirmations),
    retries: graphs.flatMap(({ records: value }) => value.retries),
    deadlines: graphs.flatMap(({ records: value }) => value.deadlines),
    correctionResults: graphs.flatMap(
      ({ records: value }) => value.correctionResults,
    ),
  };
  return makeWatcherDurableStore({
    deploymentMarker,
    revision,
    records,
  });
};

export type RecoverySourceMode = "external_providers";

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
