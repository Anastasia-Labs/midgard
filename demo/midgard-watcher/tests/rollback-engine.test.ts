import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { computeHash32 } from "../../midgard-core/src/codec/hash.js";
import { makeDeploymentMarkerV1 } from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../src/config.js";
import {
  encodeWatcherDurableStoreV1,
  journalWatcherProtocolUtxoTransitionV1,
  makeWatcherDurablePayloadV1,
  makeWatcherDurableStoreV1,
  type WatcherDurableRecordsV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
} from "../src/durable-store.js";
import {
  evaluateWatcherFinalityV1,
  makeWatcherFinalityPolicyV1,
  type WatcherFinalityPolicyV1,
  type WatcherFinalityResultV1,
  type WatcherFinalityStateV1,
} from "../src/finality-engine.js";
import {
  encodeWatcherNormalizedL1BlockV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1,
  WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherNormalizedL1BlockV1,
} from "../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 } from "../src/multi-provider-consistency.js";
import {
  evaluateWatcherPostFinalityRecoveryV1,
  evaluateWatcherRollbackV1,
  makeWatcherRollbackBootstrapStateV1,
  parseWatcherPostFinalityRecoveryResultV1,
  parseWatcherRollbackResultV1,
  parseWatcherRollbackStateV1,
  WATCHER_POST_FINALITY_RECOVERY_RESULT_V1_SCHEMA_VERSION,
  WATCHER_ROLLBACK_INCIDENT_V1_SCHEMA_VERSION,
  WATCHER_ROLLBACK_RESULT_V1_SCHEMA_VERSION,
  WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION,
} from "../src/rollback-engine.js";

const hex32 = (byte: string): string => byte.repeat(32);
const externalSource = {
  sourceMode: "external_providers",
  network: "Preprod",
  providers: [
    { providerId: "provider-a", operatorIdentitySha256: hex32("a1") },
    { providerId: "provider-b", operatorIdentitySha256: hex32("b2") },
  ],
} as const;
const payload = (cborHex = "80") => makeWatcherDurablePayloadV1(cborHex);
const sha256Canonical = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value), "utf8").digest("hex");

const bootstrap = (
  finalityPolicy: WatcherFinalityPolicyV1,
  store: WatcherDurableStoreV1,
  initialFinalityState: WatcherFinalityStateV1,
) => {
  const state = makeWatcherRollbackBootstrapStateV1(
    finalityPolicy,
    store,
    initialFinalityState,
  );
  expect(state).not.toBeNull();
  return state!;
};

const config = (depth = 5) => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "external_providers",
      providers: [
        {
          identity: "provider-a",
          operatorIdentitySha256: hex32("a1"),
          endpoint: "https://cardano-a.example",
        },
        {
          identity: "provider-b",
          operatorIdentitySha256: hex32("b2"),
          endpoint: "https://cardano-b.example",
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
    finality: {
      depth,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: depth,
      },
    },
  },
  da: {
    peers: [
      {
        identity: "da-peer-a",
        multiaddr:
          "/dns4/da-a.example/tcp/443/tls/ws/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
      },
    ],
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
  },
  storage: {
    driver: "sqlite",
    path: "/var/lib/midgard-watcher/watcher.sqlite",
  },
  proverWallet: {
    keySource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_PROVER_KEY",
    },
  },
  deadlines: {
    daFetchMs: 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
});

const localConfig = (depth = 5) => {
  const base = config(depth);
  return {
    ...base,
    l1: {
      ...base.l1,
      source: {
        sourceMode: "local_node" as const,
        authorityNodeId: "cardano-node-a",
        chainSync: {
          kind: "cardano_node_socket" as const,
          socketPath: "/var/lib/cardano/node.socket",
          genesisIdentitySha256: hex32("a1"),
        },
        queryServices: [],
      },
    },
  };
};

const deploymentIdentity = (manifestByte = "11", releaseByte = "22") => ({
  manifestId: hex32(manifestByte),
  network: "Preprod" as const,
  trustRootId: hex32("33"),
  releaseEvidenceDigest: hex32(releaseByte),
  ruleBundleCommitment: hex32("44"),
  programCommitments: { validation: hex32("55") },
  durableMarker: makeDeploymentMarkerV1(hex32(manifestByte)),
});

const policy = (
  manifestByte = "11",
  releaseByte = "22",
): WatcherFinalityPolicyV1 => {
  const value = makeWatcherFinalityPolicyV1(
    config(),
    deploymentIdentity(manifestByte, releaseByte),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicyV1;
};

const localPolicy = (): WatcherFinalityPolicyV1 => {
  const value = makeWatcherFinalityPolicyV1(
    localConfig(),
    deploymentIdentity(),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicyV1;
};

const recoveryLocalPolicy = (): WatcherFinalityPolicyV1 => {
  const base = localConfig();
  const value = makeWatcherFinalityPolicyV1(
    {
      ...base,
      l1: {
        ...base.l1,
        source: {
          ...base.l1.source,
          queryServices: [
            {
              kind: "ogmios",
              identity: "cardano-node-a-ogmios",
              endpoint: "ws://127.0.0.1:1337",
            },
          ],
        },
      },
    },
    deploymentIdentity(),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicyV1;
};

const provider = (providerId: string, identityByte: string) => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod" as const,
  providerId,
  source: {
    sourceMode: "external_providers" as const,
    operatorIdentitySha256: hex32(identityByte),
  },
  authentication: {
    kind: "https_tls_identity_v1" as const,
    publicIdentitySha256: hex32(identityByte),
  },
});

const localNodeProvider = (
  surface: "chain_sync" | "ogmios" = "chain_sync",
) => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod" as const,
  providerId:
    surface === "chain_sync" ? "cardano-node-a" : "cardano-node-a-ogmios",
  source: {
    sourceMode: "local_node" as const,
    authorityNodeId: "cardano-node-a",
    surface,
  },
  authentication: {
    kind:
      surface === "chain_sync"
        ? ("cardano_node_genesis_v1" as const)
        : ("https_tls_identity_v1" as const),
    publicIdentitySha256: surface === "chain_sync" ? hex32("a1") : hex32("b1"),
  },
});

type Point = Readonly<{
  blockHash: string;
  parentBlockHash?: string | null;
  slot: string;
  blockNo: string;
  depth: string;
  bodyHex?: string;
}>;

const transaction = (seedHex: string) => {
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    BigInt(`0x${seedHex}`),
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  const fullTransaction = CML.Transaction.new(
    body,
    witnessSet,
    true,
    undefined,
  );
  const bodyBytes = body.to_canonical_cbor_hex();
  return {
    txHash: computeHash32(Buffer.from(bodyBytes, "hex")).toString("hex"),
    fullTransaction: makeWatcherL1PublicBytesV1(
      fullTransaction.to_canonical_cbor_hex(),
    ),
    body: makeWatcherL1PublicBytesV1(bodyBytes),
    witnessSet: makeWatcherL1PublicBytesV1(witnessSet.to_canonical_cbor_hex()),
    utxos: [],
    scripts: [],
    datums: [],
    redeemers: [],
  };
};

const observation = (
  providerId: string,
  identityByte: string,
  point: Point,
): WatcherNormalizedL1BlockV1 =>
  normalizeWatcherL1BlockV1(provider(providerId, identityByte), {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId,
    chainPoint: {
      blockHash: point.blockHash,
      parentBlockHash: point.parentBlockHash ?? null,
      slot: point.slot,
      blockNo: point.blockNo,
      depth: point.depth,
    },
    transactions:
      point.bodyHex === undefined ? [] : [transaction(point.bodyHex)],
  });

const localObservation = (
  point: Point,
  surface: "chain_sync" | "ogmios" = "chain_sync",
): WatcherNormalizedL1BlockV1 =>
  normalizeWatcherL1BlockV1(localNodeProvider(surface), {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId:
      surface === "chain_sync" ? "cardano-node-a" : "cardano-node-a-ogmios",
    chainPoint: {
      blockHash: point.blockHash,
      parentBlockHash: point.parentBlockHash ?? null,
      slot: point.slot,
      blockNo: point.blockNo,
      depth: point.depth,
    },
    transactions:
      point.bodyHex === undefined ? [] : [transaction(point.bodyHex)],
  });

const localAgreement = (point: Point) =>
  evaluateWatcherMultiProviderConsistencyV1(
    {
      sourceMode: "local_node",
      network: "Preprod",
      authorityNodeId: "cardano-node-a",
      genesisIdentitySha256: hex32("a1"),
      queryServices: [],
    },
    [localObservation(point)],
  );

const agreement = (point: Point) =>
  evaluateWatcherMultiProviderConsistencyV1(
    externalSource,
    agreementObservations(point),
  );

const agreementObservations = (
  point: Point,
): readonly WatcherNormalizedL1BlockV1[] => [
  observation("provider-a", "a1", point),
  observation("provider-b", "b2", point),
];

const pending = (
  finalityPolicy: WatcherFinalityPolicyV1,
  point: Point,
): WatcherFinalityStateV1 => {
  const result = evaluateWatcherFinalityV1(
    finalityPolicy,
    null,
    agreement(point),
  );
  expect(result.action).toBe("observe_pending");
  return result.state as WatcherFinalityStateV1;
};

const transition = (
  finalityPolicy: WatcherFinalityPolicyV1,
  prior: WatcherFinalityStateV1,
  point: Point,
): Readonly<{
  consistency: ReturnType<typeof agreement>;
  observations: readonly WatcherNormalizedL1BlockV1[];
  result: WatcherFinalityResultV1;
}> => {
  const observations = agreementObservations(point);
  const consistency = evaluateWatcherMultiProviderConsistencyV1(
    externalSource,
    observations,
  );
  const result = evaluateWatcherFinalityV1(finalityPolicy, prior, consistency);
  expect(result.action).toBe("rewind_pending");
  return { consistency, observations, result };
};

type Graph = Readonly<{
  records: WatcherDurableRecordsV1;
  ids: Readonly<{
    observation: string;
    chainPoint: string;
    outRef: string;
    input: string;
    blockHash: string;
    fault: string;
    submission: string;
    confirmation: string;
    retry: string;
    deadline: string;
    correction: string;
  }>;
}>;

const graph = (idByte: string, point: Point, sharedInputId?: string): Graph => {
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

const combine = (
  deploymentMarker: ReturnType<typeof makeDeploymentMarkerV1>,
  revision: string,
  graphs: readonly Graph[],
  sharedInputId?: string,
  persistedObservations: readonly WatcherNormalizedL1BlockV1[] = [],
): WatcherDurableStoreV1 => {
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
  const records: WatcherDurableRecordsV1 = {
    l1Observations: graphs
      .flatMap(({ records: value }) => value.l1Observations)
      .concat(
        persistedObservations.map((value) => ({
          observationId: value.observationDigest,
          providerId: value.provider.providerId,
          chainPointId: value.chainPoint.chainPointId,
          payload: makeWatcherDurablePayloadV1(
            encodeWatcherNormalizedL1BlockV1(value).toString("hex"),
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
  return makeWatcherDurableStoreV1({
    deploymentMarker,
    revision,
    records,
  });
};

const oldPoint = {
  blockHash: hex32("aa"),
  slot: "1000",
  blockNo: "100",
  depth: "1",
} as const;
const replacementPoint = {
  blockHash: hex32("bb"),
  slot: "1001",
  blockNo: "101",
  depth: "2",
} as const;
const descendantPoint = {
  blockHash: hex32("cc"),
  slot: "1002",
  blockNo: "102",
  depth: "0",
} as const;
const finalizedPoint = {
  blockHash: hex32("dd"),
  slot: "900",
  blockNo: "90",
  depth: "5",
} as const;

type RecoverySourceMode = "local_node" | "external_providers";

const recoveryAgreement = (sourceMode: RecoverySourceMode, point: Point) => {
  const observations =
    sourceMode === "local_node"
      ? [localObservation(point), localObservation(point, "ogmios")]
      : agreementObservations(point);
  return {
    observations,
    consistency: evaluateWatcherMultiProviderConsistencyV1(
      sourceMode === "local_node"
        ? {
            sourceMode: "local_node",
            network: "Preprod",
            authorityNodeId: "cardano-node-a",
            genesisIdentitySha256: hex32("a1"),
            queryServices: [
              {
                kind: "ogmios" as const,
                providerId: "cardano-node-a-ogmios",
              },
            ],
          }
        : externalSource,
      observations,
    ),
  };
};

const recoveryPoints = (
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

const postFinalityRecoveryFixture = (
  rollbackDepth: number,
  sourceMode: RecoverySourceMode,
  options: Readonly<{
    malformedPreviousEvidenceAt?: number;
    omitPreviousEvidenceAt?: number;
  }> = {},
) => {
  const finalityPolicy =
    sourceMode === "local_node" ? recoveryLocalPolicy() : policy();
  const common: Point = {
    blockHash: hex32("01"),
    parentBlockHash: hex32("00"),
    blockNo: "1000",
    slot: "1000",
    depth: "0",
  };
  const previousBundles = recoveryPoints("old", common, rollbackDepth, "5").map(
    (point) => recoveryAgreement(sourceMode, point),
  );
  const replacementBundles = recoveryPoints("replacement", common, 2, "0").map(
    (point) => recoveryAgreement(sourceMode, point),
  );
  const orphanedTip = previousBundles.at(-1)!;
  const replacementTip = replacementBundles.at(-1)!;
  const alternatePreviousTip = recoveryAgreement(sourceMode, {
    ...orphanedTip.observations[0]!.chainPoint,
    depth: (
      BigInt(orphanedTip.observations[0]!.chainPoint.depth) + 1n
    ).toString(),
  });
  const alternateReplacementTip = recoveryAgreement(sourceMode, {
    ...replacementTip.observations[0]!.chainPoint,
    depth: (
      BigInt(replacementTip.observations[0]!.chainPoint.depth) + 1n
    ).toString(),
  });
  const pendingTip = recoveryAgreement(sourceMode, {
    ...orphanedTip.observations[0]!.chainPoint,
    depth: "2",
  });
  const pendingState = evaluateWatcherFinalityV1(
    finalityPolicy,
    null,
    pendingTip.consistency,
  ).state as WatcherFinalityStateV1;
  const finalizedState = evaluateWatcherFinalityV1(
    finalityPolicy,
    pendingState,
    orphanedTip.consistency,
  ).state as WatcherFinalityStateV1;
  expect(finalizedState.phase).toBe("finalized");
  const contradiction = evaluateWatcherFinalityV1(
    finalityPolicy,
    finalizedState,
    replacementTip.consistency,
  );
  expect(contradiction.action).toBe("quarantine_incident");
  const persistedObservations = [
    ...new Map(
      [
        ...previousBundles,
        ...replacementBundles,
        alternatePreviousTip,
        alternateReplacementTip,
      ]
        .flatMap(({ observations }) => observations)
        .map((observation) => [observation.observationDigest, observation]),
    ).values(),
  ];
  const orphanedGraph = graph("10", orphanedTip.observations[0]!.chainPoint);
  const commonGraph = graph("40", common);
  let store = combine(
    finalityPolicy.deploymentMarker,
    "11",
    [orphanedGraph, commonGraph],
    undefined,
    persistedObservations,
  );
  const malformedDigest =
    options.malformedPreviousEvidenceAt === undefined
      ? null
      : previousBundles[options.malformedPreviousEvidenceAt]?.observations[0]
          ?.observationDigest;
  const omittedDigest =
    options.omitPreviousEvidenceAt === undefined
      ? null
      : previousBundles[options.omitPreviousEvidenceAt]?.observations[0]
          ?.observationDigest;
  if (malformedDigest !== null || omittedDigest !== null) {
    store = makeWatcherDurableStoreV1({
      deploymentMarker: store.deploymentMarker,
      revision: store.revision,
      records: {
        ...store,
        l1Observations: store.l1Observations
          .filter(({ observationId }) => observationId !== omittedDigest)
          .map((entry) =>
            entry.observationId === malformedDigest
              ? { ...entry, payload: payload("ff") }
              : entry,
          ),
      },
    });
  }
  const rollbackBootstrapState = bootstrap(
    finalityPolicy,
    store,
    finalizedState,
  );
  const incident = evaluateWatcherRollbackV1(
    finalityPolicy,
    store,
    finalizedState,
    replacementTip.consistency,
    contradiction,
    rollbackBootstrapState,
    rollbackBootstrapState,
  );
  expect(incident.action).toBe("quarantine_incident");
  return {
    finalityPolicy,
    sourceStore: incident.nextStore!,
    rollbackState: incident.rollbackState!,
    rollbackBootstrapState,
    previousPath: previousBundles.map(({ consistency }) => consistency),
    replacementPath: replacementBundles.map(({ consistency }) => consistency),
    alternatePreviousTip: alternatePreviousTip.consistency,
    alternateReplacementTip: alternateReplacementTip.consistency,
    orphanedGraph,
    commonGraph,
  };
};

describe("canonical watcher rollback engine", () => {
  it("applies and replays an exact one-authority local-node rollback", () => {
    const finalityPolicy = localPolicy();
    const prior = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      localAgreement(oldPoint),
    ).state as WatcherFinalityStateV1;
    const consistency = localAgreement(replacementPoint);
    const finalityResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      prior,
      consistency,
    );
    expect(finalityResult.action).toBe("rewind_pending");
    const replacementObservation = localObservation(replacementPoint);
    const store = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      undefined,
      [replacementObservation],
    );
    const bootstrapState = bootstrap(finalityPolicy, store, prior);
    const applied = evaluateWatcherRollbackV1(
      finalityPolicy,
      store,
      prior,
      consistency,
      finalityResult,
      bootstrapState,
      bootstrapState,
    );

    expect(consistency).toMatchObject({
      status: "agreed",
      sourceMode: "local_node",
      observationCount: 1,
      independentProviderCount: 1,
      chainAuthorityObservationDigest: replacementObservation.observationDigest,
    });
    expect(applied).toMatchObject({
      action: "apply_rewind",
      protocolDecision: "resume_pending",
      rollbackState: { transitionCount: "1" },
    });
    expect(applied.removedRecords.reconstructedBlockHashes).toContain(
      graph("10", oldPoint).ids.blockHash,
    );
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        applied.nextStore,
        prior,
        consistency,
        finalityResult,
        JSON.parse(JSON.stringify(applied.rollbackState)),
        bootstrapState,
      ),
    ).toMatchObject({
      action: "duplicate_rewind",
      rollbackState: applied.rollbackState,
    });

    const substitutedConsistency = agreement(replacementPoint);
    const substitutedFinality = evaluateWatcherFinalityV1(
      finalityPolicy,
      prior,
      substitutedConsistency,
    );
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        store,
        prior,
        substitutedConsistency,
        substitutedFinality,
        bootstrapState,
        bootstrapState,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_finality_result"],
      nextStore: null,
      rollbackState: null,
    });
  });

  it("deterministically rewinds every dependent W03 record class and preserves finalized/unrelated records", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const rewound = transition(finalityPolicy, prior, replacementPoint);
    const sharedInput = hex32("ee");
    const oldGraph = graph("10", oldPoint, sharedInput);
    const replacementGraph = graph("20", replacementPoint);
    const descendantGraph = graph("30", descendantPoint);
    const finalizedGraph = graph("40", finalizedPoint, sharedInput);
    const store = combine(
      finalityPolicy.deploymentMarker,
      "7",
      [oldGraph, replacementGraph, descendantGraph, finalizedGraph],
      sharedInput,
      rewound.observations,
    );
    const bootstrapState = bootstrap(finalityPolicy, store, prior);

    const result = evaluateWatcherRollbackV1(
      finalityPolicy,
      store,
      prior,
      rewound.consistency,
      rewound.result,
      bootstrapState,
      bootstrapState,
    );

    expect(result).toMatchObject({
      schemaVersion: WATCHER_ROLLBACK_RESULT_V1_SCHEMA_VERSION,
      action: "apply_rewind",
      protocolDecision: "resume_pending",
      reasonCodes: ["rewind_applied"],
      sourceRevision: "7",
      nextRevision: "8",
      rollbackState: {
        schemaVersion: WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION,
        incident: null,
      },
    });
    expect(
      parseWatcherRollbackResultV1(JSON.parse(JSON.stringify(result)), {
        policy: finalityPolicy,
        sourceStore: store,
        previousFinalityState: prior,
        consistency: rewound.consistency,
        finalityResult: rewound.result,
        previousRollbackState: bootstrapState,
        rollbackBootstrapState: bootstrapState,
      }),
    ).toEqual(result);
    for (const removed of [oldGraph, descendantGraph]) {
      expect(result.removedRecords.l1ObservationIds).toContain(
        removed.ids.observation,
      );
      expect(result.removedRecords.chainPointIds).toContain(
        removed.ids.chainPoint,
      );
      expect(result.removedRecords.protocolUtxoOutRefs).toContain(
        removed.ids.outRef,
      );
      expect(result.removedRecords.daProofInputIds).toContain(
        removed.ids.input,
      );
      expect(result.removedRecords.reconstructedBlockHashes).toContain(
        removed.ids.blockHash,
      );
      expect(result.removedRecords.decisionBlockHashes).toContain(
        removed.ids.blockHash,
      );
      expect(result.removedRecords.faultIds).toContain(removed.ids.fault);
      expect(result.removedRecords.submissionIds).toContain(
        removed.ids.submission,
      );
      expect(result.removedRecords.confirmationIds).toContain(
        removed.ids.confirmation,
      );
      expect(result.removedRecords.retryIds).toContain(removed.ids.retry);
      expect(result.removedRecords.deadlineIds).toContain(removed.ids.deadline);
      expect(result.removedRecords.correctionResultIds).toContain(
        removed.ids.correction,
      );
    }
    expect(result.removedRecords.daProofInputIds).not.toContain(sharedInput);
    expect(
      result.nextStore?.daProofInputs.map(({ inputId }) => inputId),
    ).toContain(sharedInput);
    for (const retained of [replacementGraph, finalizedGraph]) {
      expect(
        result.nextStore?.chainPoints.map(({ chainPointId }) => chainPointId),
      ).toContain(retained.ids.chainPoint);
      expect(
        result.nextStore?.submissions.map(({ submissionId }) => submissionId),
      ).toContain(retained.ids.submission);
    }
  });

  it("restores an older protocol UTxO consumed only by the orphaned point", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const rewound = transition(finalityPolicy, prior, replacementPoint);
    const oldGraph = graph("10", oldPoint);
    const replacementGraph = graph("20", replacementPoint);
    const retainedGraph = graph("40", finalizedPoint);
    const beforeSpend = combine(
      finalityPolicy.deploymentMarker,
      "6",
      [oldGraph, replacementGraph, retainedGraph],
      undefined,
      rewound.observations,
    );
    const consumed = retainedGraph.records.protocolUtxos[0]!;
    const journal = journalWatcherProtocolUtxoTransitionV1({
      sourceStore: beforeSpend,
      nextChainPoints: beforeSpend.chainPoints,
      nextProtocolUtxos: beforeSpend.protocolUtxos.filter(
        ({ outRef }) => outRef !== consumed.outRef,
      ),
      spentAtChainPointId: oldGraph.ids.chainPoint,
    });
    const store = makeWatcherDurableStoreV1({
      deploymentMarker: beforeSpend.deploymentMarker,
      revision: "7",
      records: {
        ...beforeSpend,
        ...journal,
      },
    });
    const bootstrapState = bootstrap(finalityPolicy, store, prior);
    const result = evaluateWatcherRollbackV1(
      finalityPolicy,
      store,
      prior,
      rewound.consistency,
      rewound.result,
      bootstrapState,
      bootstrapState,
    );

    expect(result.action).toBe("apply_rewind");
    expect(result.nextStore?.protocolUtxos).toContainEqual(consumed);
    expect(
      result.nextStore?.spentProtocolUtxos.some(
        ({ outRef }) => outRef === consumed.outRef,
      ),
    ).toBe(false);
    expect(
      parseWatcherRollbackStateV1(
        JSON.parse(JSON.stringify(result.rollbackState)),
        {
          policy: finalityPolicy,
          rollbackBootstrapState: bootstrapState,
          currentStore: JSON.parse(JSON.stringify(result.nextStore)),
        },
      ),
    ).toEqual(result.rollbackState);
  });

  it("replays an applied instruction idempotently after a serialized restart", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const rewound = transition(finalityPolicy, prior, replacementPoint);
    const store = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      undefined,
      rewound.observations,
    );
    const bootstrapState = bootstrap(finalityPolicy, store, prior);
    const applied = evaluateWatcherRollbackV1(
      finalityPolicy,
      store,
      prior,
      rewound.consistency,
      rewound.result,
      bootstrapState,
      bootstrapState,
    );
    const restartedState = JSON.parse(
      JSON.stringify(applied.rollbackState),
    ) as unknown;
    const restartedStore = JSON.parse(
      JSON.stringify(applied.nextStore),
    ) as unknown;

    expect(
      parseWatcherRollbackStateV1(restartedState, {
        policy: finalityPolicy,
        rollbackBootstrapState: bootstrapState,
        currentStore: restartedStore,
      }),
    ).toEqual(applied.rollbackState);
    const duplicate = evaluateWatcherRollbackV1(
      finalityPolicy,
      restartedStore,
      prior,
      rewound.consistency,
      rewound.result,
      restartedState,
      bootstrapState,
    );
    expect(duplicate).toMatchObject({
      action: "duplicate_rewind",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_instruction"],
      sourceRevision: "1",
      nextRevision: "1",
      nextStoreDigest: applied.nextStoreDigest,
    });
    expect(duplicate.nextStore).toEqual(applied.nextStore);
  });

  it("rewinds content-derived observations and proof work while retaining the canonical chain-point row", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const contentChanged = transition(finalityPolicy, prior, {
      ...oldPoint,
      depth: "2",
      bodyHex: "a100",
    });
    const oldGraph = graph("10", oldPoint);
    const store = combine(
      finalityPolicy.deploymentMarker,
      "2",
      [oldGraph],
      undefined,
      contentChanged.observations,
    );
    const bootstrapState = bootstrap(finalityPolicy, store, prior);
    const result = evaluateWatcherRollbackV1(
      finalityPolicy,
      store,
      prior,
      contentChanged.consistency,
      contentChanged.result,
      bootstrapState,
      bootstrapState,
    );

    expect(result.action).toBe("apply_rewind");
    expect(result.removedRecords.chainPointIds).toEqual([]);
    expect(result.removedRecords.l1ObservationIds).toEqual([
      oldGraph.ids.observation,
    ]);
    expect(result.removedRecords.reconstructedBlockHashes).toEqual([
      oldGraph.ids.blockHash,
    ]);
    expect(result.nextStore?.chainPoints).toEqual(store.chainPoints);
  });

  it("rewinds only regressed-depth observations and descendants beyond the replacement tip", () => {
    const finalityPolicy = policy();
    const priorPoint = { ...oldPoint, depth: "3" };
    const replacement = { ...oldPoint, depth: "2" };
    const prior = pending(finalityPolicy, priorPoint);
    const regressed = transition(finalityPolicy, prior, replacement);
    const anchor = graph("10", replacement);
    const beyond = graph("30", {
      blockHash: descendantPoint.blockHash,
      slot: "1003",
      blockNo: "103",
      depth: "0",
    });
    const highDepthPointId = hex32("99");
    const base = combine(
      finalityPolicy.deploymentMarker,
      "3",
      [anchor, beyond],
      undefined,
      regressed.observations,
    );
    const store = makeWatcherDurableStoreV1({
      deploymentMarker: base.deploymentMarker,
      revision: base.revision,
      records: {
        ...base,
        chainPoints: [
          ...base.chainPoints,
          {
            chainPointId: highDepthPointId,
            providerId: "provider-b",
            blockHash: oldPoint.blockHash,
            slot: oldPoint.slot,
            blockNo: oldPoint.blockNo,
            depth: "3",
          },
        ],
        l1Observations: [
          ...base.l1Observations,
          {
            observationId: hex32("98"),
            providerId: "provider-b",
            chainPointId: highDepthPointId,
            payload: payload("8101"),
          },
        ],
      },
    });
    const bootstrapState = bootstrap(finalityPolicy, store, prior);
    const result = evaluateWatcherRollbackV1(
      finalityPolicy,
      store,
      prior,
      regressed.consistency,
      regressed.result,
      bootstrapState,
      bootstrapState,
    );

    expect(result.action).toBe("apply_rewind");
    expect(result.removedRecords.chainPointIds).toContain(highDepthPointId);
    expect(result.removedRecords.chainPointIds).toContain(
      beyond.ids.chainPoint,
    );
    expect(result.removedRecords.reconstructedBlockHashes).toEqual([
      beyond.ids.blockHash,
    ]);
    expect(result.nextStore?.reconstructedStates).toContainEqual(
      anchor.records.reconstructedStates[0],
    );
  });

  it("persists a post-finality contradiction as quarantine while preserving store and finalized history", () => {
    const finalityPolicy = policy();
    const first = pending(finalityPolicy, {
      ...oldPoint,
      depth: "2",
    });
    const finalizedResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      first,
      agreement({ ...oldPoint, depth: "5" }),
    );
    expect(finalizedResult.action).toBe("finalize");
    const finalizedState = finalizedResult.state as WatcherFinalityStateV1;
    const contradictionPoint = { ...replacementPoint, depth: "0" };
    const contradictionConsistency = agreement(contradictionPoint);
    const contradiction = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalizedState,
      contradictionConsistency,
    );
    const store = combine(
      finalityPolicy.deploymentMarker,
      "9",
      [graph("10", oldPoint)],
      undefined,
      agreementObservations(contradictionPoint),
    );
    const bootstrapState = bootstrap(finalityPolicy, store, finalizedState);
    const quarantined = evaluateWatcherRollbackV1(
      finalityPolicy,
      store,
      finalizedState,
      contradictionConsistency,
      contradiction,
      bootstrapState,
      bootstrapState,
    );

    expect(quarantined).toMatchObject({
      action: "quarantine_incident",
      protocolDecision: "quarantined",
      reasonCodes: ["post_finality_incident"],
      sourceRevision: "9",
      nextRevision: "10",
      rollbackState: {
        incident: {
          schemaVersion: WATCHER_ROLLBACK_INCIDENT_V1_SCHEMA_VERSION,
          reasonCode: "post_finality_point_changed",
          finalizedBinding: finalizedState.finalized,
        },
      },
    });
    expect(quarantined.nextStore).toMatchObject({
      revision: "10",
      l1Observations: store.l1Observations,
      chainPoints: store.chainPoints,
      protocolUtxos: store.protocolUtxos,
      daProofInputs: store.daProofInputs,
      reconstructedStates: store.reconstructedStates,
      decisions: store.decisions,
      faults: store.faults,
      submissions: store.submissions,
      confirmations: store.confirmations,
      retries: store.retries,
      deadlines: store.deadlines,
      correctionResults: store.correctionResults,
    });
    expect(quarantined.removedRecords).toEqual({
      l1ObservationIds: [],
      chainPointIds: [],
      protocolUtxoOutRefs: [],
      daProofInputIds: [],
      reconstructedBlockHashes: [],
      decisionBlockHashes: [],
      faultIds: [],
      submissionIds: [],
      confirmationIds: [],
      retryIds: [],
      deadlineIds: [],
      correctionResultIds: [],
    });
    expect(
      parseWatcherRollbackResultV1(JSON.parse(JSON.stringify(quarantined)), {
        policy: finalityPolicy,
        sourceStore: store,
        previousFinalityState: finalizedState,
        consistency: contradictionConsistency,
        finalityResult: contradiction,
        previousRollbackState: bootstrapState,
        rollbackBootstrapState: bootstrapState,
      }),
    ).toEqual(quarantined);
    expect(
      parseWatcherRollbackStateV1(
        JSON.parse(JSON.stringify(quarantined.rollbackState)),
        {
          policy: finalityPolicy,
          rollbackBootstrapState: bootstrapState,
          currentStore: quarantined.nextStore,
        },
      ),
    ).toEqual(quarantined.rollbackState);

    const held = evaluateWatcherRollbackV1(
      finalityPolicy,
      quarantined.nextStore,
      finalizedState,
      contradictionConsistency,
      contradiction,
      JSON.parse(JSON.stringify(quarantined.rollbackState)),
      bootstrapState,
    );
    expect(held).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["state_quarantined"],
      rollbackState: quarantined.rollbackState,
    });
    expect(held.nextStore).toEqual(quarantined.nextStore);
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        quarantined.nextStore,
        finalizedState,
        contradictionConsistency,
        contradiction,
        bootstrapState,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["rollback_state_store_mismatch"]);
  });

  it.each<RecoverySourceMode>(["local_node", "external_providers"])(
    "automatically recovers an agreed %s replacement, sweeps every dependent record, and is restart-idempotent",
    (sourceMode) => {
      const fixture = postFinalityRecoveryFixture(6, sourceMode);
      const recoveryInput = {
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: fixture.replacementPath,
        previousRecoveryState: null,
      };
      const applied = evaluateWatcherPostFinalityRecoveryV1(recoveryInput);

      expect(applied).toMatchObject({
        schemaVersion: WATCHER_POST_FINALITY_RECOVERY_RESULT_V1_SCHEMA_VERSION,
        action: "rewind_and_replay",
        protocolDecision: "resume_replay",
        reasonCodes: ["recovery_applied"],
        sourceRevision: "12",
        nextRevision: "13",
        resumableFinalityState: {
          phase: "unobserved",
          pending: null,
          finalized: null,
          incident: null,
        },
        recoveryState: {
          path: { rollbackDepth: "6" },
          incidentLifecycle: { status: "recovered" },
        },
      });
      if (sourceMode === "local_node") {
        expect(fixture.replacementPath.at(-1)).toMatchObject({
          status: "agreed",
          sourceMode: "local_node",
          independentProviderCount: 1,
          queryObservationCount: 1,
          localQueryServiceBindings: [
            {
              providerId: "cardano-node-a-ogmios",
              observationStatus: "aligned",
            },
          ],
        });
      } else {
        expect(fixture.replacementPath.at(-1)).toMatchObject({
          status: "agreed",
          sourceMode: "external_providers",
          independentProviderCount: 2,
        });
      }
      expect(applied.removedRecords.reconstructedBlockHashes).toContain(
        fixture.orphanedGraph.ids.blockHash,
      );
      expect(fixture.orphanedGraph.ids.blockHash).not.toBe(
        fixture.rollbackState.incident?.finalizedBinding.blockHash,
      );
      expect(applied.removedRecords.decisionBlockHashes).toContain(
        fixture.orphanedGraph.ids.blockHash,
      );
      expect(applied.removedRecords.protocolUtxoOutRefs).toContain(
        fixture.orphanedGraph.ids.outRef,
      );
      expect(applied.removedRecords.faultIds).toContain(
        fixture.orphanedGraph.ids.fault,
      );
      expect(applied.removedRecords.submissionIds).toContain(
        fixture.orphanedGraph.ids.submission,
      );
      expect(applied.removedRecords.confirmationIds).toContain(
        fixture.orphanedGraph.ids.confirmation,
      );
      expect(applied.removedRecords.retryIds).toContain(
        fixture.orphanedGraph.ids.retry,
      );
      expect(applied.removedRecords.deadlineIds).toContain(
        fixture.orphanedGraph.ids.deadline,
      );
      expect(applied.removedRecords.correctionResultIds).toContain(
        fixture.orphanedGraph.ids.correction,
      );
      expect(applied.nextStore?.reconstructedStates).toContainEqual(
        fixture.commonGraph.records.reconstructedStates[0],
      );
      expect(
        parseWatcherPostFinalityRecoveryResultV1(
          JSON.parse(JSON.stringify(applied)),
          recoveryInput,
        ),
      ).toEqual(applied);
      expect(applied.resumableRollbackState).toMatchObject({
        epoch: "1",
        transitions: [],
        epochCheckpoint: {
          priorTerminalStateDigest: fixture.rollbackState.stateDigest,
          priorTerminalStoreDigest: fixture.rollbackState.storeDigest,
          priorTerminalFinalityStateDigest:
            fixture.rollbackState.currentFinalityStateDigest,
          priorTerminalIncidentDigest:
            fixture.rollbackState.incident?.incidentDigest,
          recoveryStateDigest: applied.recoveryState?.stateDigest,
          recoveryLifecycleDigest:
            applied.recoveryState?.incidentLifecycle.lifecycleDigest,
          checkpointStoreDigest: applied.nextStoreDigest,
          checkpointFinalityStateDigest:
            applied.resumableFinalityState?.stateDigest,
        },
      });
      expect(applied.resumableRollbackBootstrapState).toEqual(
        applied.resumableRollbackState,
      );
      expect(applied.resumableTrustedCheckpointStateDigest).toBe(
        applied.resumableRollbackState?.stateDigest,
      );
      expect(
        parseWatcherRollbackStateV1(
          JSON.parse(JSON.stringify(applied.resumableRollbackState)),
          {
            policy: fixture.finalityPolicy,
            rollbackBootstrapState: JSON.parse(
              JSON.stringify(applied.resumableRollbackBootstrapState),
            ),
            trustedCheckpointStateDigest:
              applied.resumableTrustedCheckpointStateDigest,
            currentStore: JSON.parse(JSON.stringify(applied.nextStore)),
          },
        ),
      ).toEqual(applied.resumableRollbackState);

      const forgedRecoveryCheckpoint = JSON.parse(
        JSON.stringify(applied),
      ) as Record<string, any>;
      for (const key of [
        "resumableRollbackState",
        "resumableRollbackBootstrapState",
      ]) {
        forgedRecoveryCheckpoint[key].epochCheckpoint.recoveryLifecycleDigest =
          hex32("f3");
        const {
          checkpointDigest: _discardedCheckpointDigest,
          ...checkpointCanonical
        } = forgedRecoveryCheckpoint[key].epochCheckpoint;
        forgedRecoveryCheckpoint[key].epochCheckpoint.checkpointDigest =
          sha256Canonical(checkpointCanonical);
        const {
          stateDigest: _discardedRollbackStateDigest,
          ...rollbackStateCanonical
        } = forgedRecoveryCheckpoint[key];
        forgedRecoveryCheckpoint[key].stateDigest = sha256Canonical(
          rollbackStateCanonical,
        );
      }
      const {
        resultDigest: _discardedRecoveryResultDigest,
        ...recoveryResultCanonical
      } = forgedRecoveryCheckpoint;
      forgedRecoveryCheckpoint.resultDigest = sha256Canonical(
        recoveryResultCanonical,
      );
      expect(
        parseWatcherPostFinalityRecoveryResultV1(
          forgedRecoveryCheckpoint,
          recoveryInput,
        ),
      ).toBeNull();

      const restarted = evaluateWatcherPostFinalityRecoveryV1({
        policy: JSON.parse(JSON.stringify(fixture.finalityPolicy)),
        sourceStore: JSON.parse(JSON.stringify(fixture.sourceStore)),
        currentStore: JSON.parse(JSON.stringify(applied.nextStore)),
        quarantinedRollbackState: JSON.parse(
          JSON.stringify(fixture.rollbackState),
        ),
        rollbackBootstrapState: JSON.parse(
          JSON.stringify(fixture.rollbackBootstrapState),
        ),
        previousCanonicalPath: JSON.parse(JSON.stringify(fixture.previousPath)),
        replacementCanonicalPath: JSON.parse(
          JSON.stringify(fixture.replacementPath),
        ),
        previousRecoveryState: JSON.parse(
          JSON.stringify(applied.recoveryState),
        ),
      });
      expect(restarted).toMatchObject({
        action: "duplicate_recovery",
        protocolDecision: "hold",
        reasonCodes: ["duplicate_recovery"],
        sourceRevision: "13",
        nextRevision: "13",
        recoveryState: applied.recoveryState,
      });
      expect(restarted.nextStore).toEqual(applied.nextStore);
    },
    30_000,
  );

  it("accepts the exact k=2160 recovery boundary and rejects 2161 without mutation", () => {
    const atBoundary = postFinalityRecoveryFixture(2_160, "local_node");
    const accepted = evaluateWatcherPostFinalityRecoveryV1({
      policy: atBoundary.finalityPolicy,
      sourceStore: atBoundary.sourceStore,
      currentStore: atBoundary.sourceStore,
      quarantinedRollbackState: atBoundary.rollbackState,
      rollbackBootstrapState: atBoundary.rollbackBootstrapState,
      previousCanonicalPath: atBoundary.previousPath,
      replacementCanonicalPath: atBoundary.replacementPath,
      previousRecoveryState: null,
    });
    expect(accepted).toMatchObject({
      action: "rewind_and_replay",
      recoveryState: { path: { rollbackDepth: "2160" } },
    });

    const beyondBoundaryPath = [
      ...atBoundary.previousPath,
      atBoundary.previousPath.at(-1)!,
    ];
    const rejected = evaluateWatcherPostFinalityRecoveryV1({
      policy: atBoundary.finalityPolicy,
      sourceStore: atBoundary.sourceStore,
      currentStore: atBoundary.sourceStore,
      quarantinedRollbackState: atBoundary.rollbackState,
      rollbackBootstrapState: atBoundary.rollbackBootstrapState,
      previousCanonicalPath: beyondBoundaryPath,
      replacementCanonicalPath: atBoundary.replacementPath,
      previousRecoveryState: null,
    });
    expect(rejected).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["recovery_depth_exceeded"],
      nextStore: null,
      recoveryState: null,
    });
    expect(
      Object.values(rejected.removedRecords).every(
        (records) => records.length === 0,
      ),
    ).toBe(true);
  }, 120_000);

  it("fails closed on a non-agreed replacement, an ancestry gap, and forged recovery state", () => {
    const fixture = postFinalityRecoveryFixture(6, "external_providers");
    const wrongPreviousEndpoint = [...fixture.previousPath];
    wrongPreviousEndpoint[wrongPreviousEndpoint.length - 1] =
      fixture.alternatePreviousTip;
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: wrongPreviousEndpoint,
        replacementCanonicalPath: fixture.replacementPath,
        previousRecoveryState: null,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["finalized_binding_mismatch"],
      nextStore: null,
    });

    const wrongReplacementEndpoint = [...fixture.replacementPath];
    wrongReplacementEndpoint[wrongReplacementEndpoint.length - 1] =
      fixture.alternateReplacementTip;
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: wrongReplacementEndpoint,
        previousRecoveryState: null,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["incident_provenance_mismatch"],
      nextStore: null,
    });

    const malformedMemberPath = [fixture.previousPath[0], null];
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: malformedMemberPath,
        replacementCanonicalPath: fixture.replacementPath,
        previousRecoveryState: null,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["recovery_path_malformed"],
      nextStore: null,
    });

    const accessorMember = {
      ...fixture.replacementPath[1],
    } as Record<string, unknown>;
    Object.defineProperty(accessorMember, "status", {
      enumerable: true,
      get: () => {
        throw new Error("must not execute hostile recovery-path accessors");
      },
    });
    const accessorPath = [...fixture.replacementPath];
    accessorPath[1] = accessorMember as (typeof accessorPath)[number];
    expect(() =>
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: accessorPath,
        previousRecoveryState: null,
      }),
    ).not.toThrow();
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: accessorPath,
        previousRecoveryState: null,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["recovery_path_malformed"],
      nextStore: null,
    });

    const cyclicMember = JSON.parse(
      JSON.stringify(fixture.replacementPath[1]),
    ) as Record<string, unknown>;
    cyclicMember.agreement = cyclicMember;
    const cyclicPath = [...fixture.replacementPath];
    cyclicPath[1] = cyclicMember as (typeof cyclicPath)[number];
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: cyclicPath,
        previousRecoveryState: null,
      }),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      nextStore: null,
    });

    const pendingReplacement = JSON.parse(
      JSON.stringify(fixture.replacementPath),
    ) as Array<Record<string, unknown>>;
    pendingReplacement[1] = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      [],
    ) as unknown as Record<string, unknown>;
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: pendingReplacement,
        previousRecoveryState: null,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["canonical_agreement_required"],
      nextStore: null,
    });

    const gap = JSON.parse(JSON.stringify(fixture.replacementPath)) as Array<
      Record<string, unknown>
    >;
    gap.splice(1, 1);
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: gap,
        previousRecoveryState: null,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["recovery_path_gap"],
      nextStore: null,
    });

    const applied = evaluateWatcherPostFinalityRecoveryV1({
      policy: fixture.finalityPolicy,
      sourceStore: fixture.sourceStore,
      currentStore: fixture.sourceStore,
      quarantinedRollbackState: fixture.rollbackState,
      rollbackBootstrapState: fixture.rollbackBootstrapState,
      previousCanonicalPath: fixture.previousPath,
      replacementCanonicalPath: fixture.replacementPath,
      previousRecoveryState: null,
    });
    const forged = JSON.parse(JSON.stringify(applied.recoveryState)) as Record<
      string,
      unknown
    >;
    forged.nextStoreDigest = hex32("ff");
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: applied.nextStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: fixture.replacementPath,
        previousRecoveryState: forged,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_recovery_state"],
      nextStore: null,
    });

    const selfRehashed = JSON.parse(
      JSON.stringify(applied.recoveryState),
    ) as Record<string, unknown>;
    const removedRecords = selfRehashed.removedRecords as Record<
      string,
      unknown
    >;
    removedRecords.faultIds = [];
    const { stateDigest: _stateDigest, ...canonicalRecoveryState } =
      selfRehashed;
    selfRehashed.stateDigest = sha256Canonical(canonicalRecoveryState);
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: applied.nextStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: fixture.replacementPath,
        previousRecoveryState: selfRehashed,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["recovery_state_mismatch"],
      nextStore: null,
    });

    const recoveryInput = {
      policy: fixture.finalityPolicy,
      sourceStore: fixture.sourceStore,
      currentStore: fixture.sourceStore,
      quarantinedRollbackState: fixture.rollbackState,
      rollbackBootstrapState: fixture.rollbackBootstrapState,
      previousCanonicalPath: fixture.previousPath,
      replacementCanonicalPath: fixture.replacementPath,
      previousRecoveryState: null,
    };
    const selfRehashedResult = JSON.parse(JSON.stringify(applied)) as Record<
      string,
      unknown
    >;
    selfRehashedResult.reasonCodes = ["duplicate_recovery"];
    const { resultDigest: _resultDigest, ...canonicalResult } =
      selfRehashedResult;
    selfRehashedResult.resultDigest = sha256Canonical(canonicalResult);
    expect(
      parseWatcherPostFinalityRecoveryResultV1(
        selfRehashedResult,
        recoveryInput,
      ),
    ).toBeNull();

    const unknownResult = {
      ...JSON.parse(JSON.stringify(applied)),
      attacker: true,
    };
    expect(
      parseWatcherPostFinalityRecoveryResultV1(unknownResult, recoveryInput),
    ).toBeNull();
    const accessorResult = JSON.parse(JSON.stringify(applied)) as Record<
      string,
      unknown
    >;
    Object.defineProperty(accessorResult, "action", {
      enumerable: true,
      get: () => "rewind_and_replay",
    });
    expect(
      parseWatcherPostFinalityRecoveryResultV1(accessorResult, recoveryInput),
    ).toBeNull();
  });

  it("fails closed without mutation on wrong bindings and missing or malformed W10 bytes", () => {
    const external = postFinalityRecoveryFixture(6, "external_providers");
    const previewConfig = {
      ...config(),
      targetNetwork: "Preview" as const,
    };
    const previewPolicy = makeWatcherFinalityPolicyV1(previewConfig, {
      ...deploymentIdentity(),
      network: "Preview" as const,
    });
    expect(previewPolicy).not.toBeNull();
    const wrongPolicies = [
      recoveryLocalPolicy(),
      previewPolicy,
      {
        ...external.finalityPolicy,
        policyDigest: hex32("ff"),
      },
    ];
    for (const wrongPolicy of wrongPolicies) {
      const result = evaluateWatcherPostFinalityRecoveryV1({
        policy: wrongPolicy,
        sourceStore: external.sourceStore,
        currentStore: external.sourceStore,
        quarantinedRollbackState: external.rollbackState,
        rollbackBootstrapState: external.rollbackBootstrapState,
        previousCanonicalPath: external.previousPath,
        replacementCanonicalPath: external.replacementPath,
        previousRecoveryState: null,
      });
      expect(result.action).toBe("reject");
      expect(result.nextStore).toBeNull();
      expect(result.recoveryState).toBeNull();
    }

    const local = postFinalityRecoveryFixture(6, "local_node");
    const foreignLocalConfig = localConfig();
    const foreignAuthorityPolicy = makeWatcherFinalityPolicyV1(
      {
        ...foreignLocalConfig,
        l1: {
          ...foreignLocalConfig.l1,
          source: {
            ...foreignLocalConfig.l1.source,
            authorityNodeId: "cardano-node-b",
            chainSync: {
              ...foreignLocalConfig.l1.source.chainSync,
              genesisIdentitySha256: hex32("b2"),
            },
          },
        },
      },
      deploymentIdentity(),
    );
    expect(foreignAuthorityPolicy).not.toBeNull();
    expect(
      evaluateWatcherPostFinalityRecoveryV1({
        policy: foreignAuthorityPolicy,
        sourceStore: local.sourceStore,
        currentStore: local.sourceStore,
        quarantinedRollbackState: local.rollbackState,
        rollbackBootstrapState: local.rollbackBootstrapState,
        previousCanonicalPath: local.previousPath,
        replacementCanonicalPath: local.replacementPath,
        previousRecoveryState: null,
      }),
    ).toMatchObject({
      action: "reject",
      nextStore: null,
      recoveryState: null,
    });

    for (const fixture of [
      postFinalityRecoveryFixture(6, "local_node", {
        omitPreviousEvidenceAt: 2,
      }),
      postFinalityRecoveryFixture(6, "local_node", {
        malformedPreviousEvidenceAt: 2,
      }),
    ]) {
      const result = evaluateWatcherPostFinalityRecoveryV1({
        policy: fixture.finalityPolicy,
        sourceStore: fixture.sourceStore,
        currentStore: fixture.sourceStore,
        quarantinedRollbackState: fixture.rollbackState,
        rollbackBootstrapState: fixture.rollbackBootstrapState,
        previousCanonicalPath: fixture.previousPath,
        replacementCanonicalPath: fixture.replacementPath,
        previousRecoveryState: null,
      });
      expect(result).toMatchObject({
        action: "reject",
        protocolDecision: "quarantined",
        reasonCodes: ["canonical_agreement_required"],
        nextStore: null,
        recoveryState: null,
      });
      expect(
        Object.values(result.removedRecords).every(
          (records) => records.length === 0,
        ),
      ).toBe(true);
    }
  });

  it("opens W13 only for an agreed contradiction and never journals transient W11 disagreement", () => {
    const finalityPolicy = policy();
    const first = pending(finalityPolicy, { ...oldPoint, depth: "2" });
    const finalizedState = evaluateWatcherFinalityV1(
      finalityPolicy,
      first,
      agreement({ ...oldPoint, depth: "5" }),
    ).state as WatcherFinalityStateV1;
    const agreedObservations = agreementObservations({
      ...replacementPoint,
      depth: "0",
    });
    const agreedConsistency = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      agreedObservations,
    );
    const agreedFinalityResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalizedState,
      agreedConsistency,
    );
    expect(agreedFinalityResult.action).toBe("quarantine_incident");
    const agreedStore = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint)],
      undefined,
      agreedObservations,
    );
    const agreedBootstrap = bootstrap(
      finalityPolicy,
      agreedStore,
      finalizedState,
    );
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        agreedStore,
        finalizedState,
        agreedConsistency,
        agreedFinalityResult,
        agreedBootstrap,
        agreedBootstrap,
      ),
    ).toMatchObject({
      action: "quarantine_incident",
      reasonCodes: ["post_finality_incident"],
    });

    const transientCases = [
      {
        status: "pending",
        observations: [
          observation("provider-a", "a1", {
            ...oldPoint,
            depth: "5",
          }),
          observation("provider-b", "b2", {
            ...replacementPoint,
            depth: "0",
          }),
        ],
      },
      {
        status: "quarantined",
        observations: [
          observation("provider-a", "a1", {
            ...oldPoint,
            depth: "5",
          }),
        ],
      },
    ] as const;

    for (const value of transientCases) {
      const consistency = evaluateWatcherMultiProviderConsistencyV1(
        externalSource,
        value.observations,
      );
      expect(consistency.status).toBe(value.status);
      const finalityResult = evaluateWatcherFinalityV1(
        finalityPolicy,
        finalizedState,
        consistency,
      );
      expect(finalityResult).toMatchObject({
        action: "reject",
        protocolDecision: "quarantined",
        state: finalizedState,
      });
      expect(finalityResult.state?.incident).toBeNull();
      const store = combine(
        finalityPolicy.deploymentMarker,
        "0",
        [graph("10", oldPoint)],
        undefined,
        value.observations,
      );
      const bootstrapState = bootstrap(finalityPolicy, store, finalizedState);
      const result = evaluateWatcherRollbackV1(
        finalityPolicy,
        store,
        finalizedState,
        consistency,
        finalityResult,
        bootstrapState,
        bootstrapState,
      );
      expect(result).toMatchObject({
        action: "reject",
        nextStore: null,
        rollbackState: null,
      });
      expect(
        evaluateWatcherFinalityV1(
          finalityPolicy,
          finalityResult.state,
          agreement({ ...oldPoint, depth: "6" }),
        ),
      ).toMatchObject({
        action: "duplicate",
        protocolDecision: "hold",
        state: finalizedState,
      });
    }
  });

  it("fails closed on a post-finality incident without durable W10 evidence", () => {
    const finalityPolicy = policy();
    const first = pending(finalityPolicy, { ...oldPoint, depth: "2" });
    const finalizedState = evaluateWatcherFinalityV1(
      finalityPolicy,
      first,
      agreement({ ...oldPoint, depth: "5" }),
    ).state as WatcherFinalityStateV1;
    const consistency = agreement({ ...replacementPoint, depth: "0" });
    const finalityResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalizedState,
      consistency,
    );
    expect(finalityResult.action).toBe("quarantine_incident");
    const store = combine(finalityPolicy.deploymentMarker, "0", [
      graph("10", oldPoint),
    ]);
    const bootstrapState = bootstrap(finalityPolicy, store, finalizedState);

    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        store,
        finalizedState,
        consistency,
        finalityResult,
        bootstrapState,
        bootstrapState,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["consistency_evidence_missing"],
      nextStore: null,
      rollbackState: null,
    });
  });

  it("rejects foreign deployment/release bindings and malformed inputs with value-free diagnostics", () => {
    const originalPolicy = policy();
    const prior = pending(originalPolicy, oldPoint);
    const rewound = transition(originalPolicy, prior, replacementPoint);
    const store = combine(
      originalPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      undefined,
      rewound.observations,
    );
    const bootstrapState = bootstrap(originalPolicy, store, prior);
    const foreignDeployment = evaluateWatcherRollbackV1(
      policy("99"),
      store,
      prior,
      rewound.consistency,
      rewound.result,
      bootstrapState,
      bootstrapState,
    );
    const foreignRelease = evaluateWatcherRollbackV1(
      policy("11", "99"),
      store,
      prior,
      rewound.consistency,
      rewound.result,
      bootstrapState,
      bootstrapState,
    );
    const malformed = evaluateWatcherRollbackV1(
      { ...originalPolicy, credential: "operator-secret" },
      store,
      prior,
      rewound.consistency,
      rewound.result,
      bootstrapState,
      bootstrapState,
    );

    expect(foreignDeployment.reasonCodes).toEqual(["deployment_mismatch"]);
    expect(foreignRelease.reasonCodes).toEqual(["release_evidence_mismatch"]);
    expect(malformed.reasonCodes).toEqual(["malformed_policy"]);
    for (const diagnostic of [foreignDeployment, foreignRelease, malformed]) {
      expect(JSON.stringify(diagnostic)).not.toContain("operator-secret");
      expect(diagnostic.nextStore).toBeNull();
      expect(diagnostic.rollbackState).toBeNull();
    }
  });

  it("rejects wrong-order, duplicate, unknown-field, and unknown-target mutations", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const rewound = transition(finalityPolicy, prior, replacementPoint);
    const oldGraph = graph("10", oldPoint);
    const replacementGraph = graph("20", replacementPoint);
    const store = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [oldGraph, replacementGraph],
      undefined,
      rewound.observations,
    );
    const wrongOrder = JSON.parse(JSON.stringify(store)) as Record<string, any>;
    wrongOrder.chainPoints.reverse();
    const duplicate = JSON.parse(JSON.stringify(store)) as Record<string, any>;
    duplicate.chainPoints.push(duplicate.chainPoints[0]);
    const unknownResult = JSON.parse(JSON.stringify(rewound.result)) as Record<
      string,
      any
    >;
    unknownResult.operatorOverride = true;
    const replacementOnly = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [replacementGraph],
      undefined,
      rewound.observations,
    );
    const bootstrapState = bootstrap(finalityPolicy, store, prior);
    const replacementOnlyBootstrap = bootstrap(
      finalityPolicy,
      replacementOnly,
      prior,
    );

    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        wrongOrder,
        prior,
        rewound.consistency,
        rewound.result,
        bootstrapState,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["malformed_store"]);
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        duplicate,
        prior,
        rewound.consistency,
        rewound.result,
        bootstrapState,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["malformed_store"]);
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        store,
        prior,
        rewound.consistency,
        unknownResult,
        bootstrapState,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["finality_provenance_mismatch"]);
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        replacementOnly,
        prior,
        rewound.consistency,
        rewound.result,
        replacementOnlyBootstrap,
        replacementOnlyBootstrap,
      ).reasonCodes,
    ).toEqual(["unknown_rewind_target"]);
  });

  it("rejects missing replacement observations, stale journals, and digest/schema mutation", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const rewound = transition(finalityPolicy, prior, replacementPoint);
    const store = combine(finalityPolicy.deploymentMarker, "0", [
      graph("10", oldPoint),
    ]);
    const missingBootstrap = bootstrap(finalityPolicy, store, prior);
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        store,
        prior,
        rewound.consistency,
        rewound.result,
        missingBootstrap,
        missingBootstrap,
      ).reasonCodes,
    ).toEqual(["replacement_evidence_missing"]);

    const completeStore = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      undefined,
      rewound.observations,
    );
    const bootstrapState = bootstrap(finalityPolicy, completeStore, prior);
    const applied = evaluateWatcherRollbackV1(
      finalityPolicy,
      completeStore,
      prior,
      rewound.consistency,
      rewound.result,
      bootstrapState,
      bootstrapState,
    );
    const staleStore = makeWatcherDurableStoreV1({
      deploymentMarker: applied.nextStore!.deploymentMarker,
      revision: "2",
      records: applied.nextStore!,
    });
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        staleStore,
        prior,
        rewound.consistency,
        rewound.result,
        applied.rollbackState,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["rollback_state_store_mismatch"]);

    const tampered = JSON.parse(
      JSON.stringify(applied.rollbackState),
    ) as Record<string, any>;
    tampered.storeDigest = hex32("ff");
    expect(
      parseWatcherRollbackStateV1(tampered, {
        policy: finalityPolicy,
        rollbackBootstrapState: bootstrapState,
        currentStore: applied.nextStore,
      }),
    ).toBeNull();
    const unknown = JSON.parse(JSON.stringify(applied.rollbackState)) as Record<
      string,
      any
    >;
    unknown.operatorTarget = "private";
    expect(
      parseWatcherRollbackStateV1(unknown, {
        policy: finalityPolicy,
        rollbackBootstrapState: bootstrapState,
        currentStore: applied.nextStore,
      }),
    ).toBeNull();
    const tamperedResult = JSON.parse(JSON.stringify(applied)) as Record<
      string,
      any
    >;
    tamperedResult.nextRevision = "9";
    expect(
      parseWatcherRollbackResultV1(tamperedResult, {
        policy: finalityPolicy,
        sourceStore: completeStore,
        previousFinalityState: prior,
        consistency: rewound.consistency,
        finalityResult: rewound.result,
        previousRollbackState: bootstrapState,
        rollbackBootstrapState: bootstrapState,
      }),
    ).toBeNull();
    const unknownResult = JSON.parse(JSON.stringify(applied)) as Record<
      string,
      any
    >;
    unknownResult.privateTarget = oldPoint.blockHash;
    expect(
      parseWatcherRollbackResultV1(unknownResult, {
        policy: finalityPolicy,
        sourceStore: completeStore,
        previousFinalityState: prior,
        consistency: rewound.consistency,
        finalityResult: rewound.result,
        previousRollbackState: bootstrapState,
        rollbackBootstrapState: bootstrapState,
      }),
    ).toBeNull();
  });

  it("recomputes W12 from W11 and rejects self-hashed point/content/depth relabeling", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const rewound = transition(finalityPolicy, prior, replacementPoint);
    const store = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      undefined,
      rewound.observations,
    );
    const bootstrapState = bootstrap(finalityPolicy, store, prior);
    const mutations: readonly ((value: Record<string, any>) => void)[] = [
      (value) => {
        value.rewindInstruction.kind = "pending_content_changed";
        value.reasonCodes = ["pending_content_changed"];
      },
      (value) => {
        value.rewindInstruction.replacementPointDigest = hex32("91");
      },
      (value) => {
        value.rewindInstruction.replacementContentDigest = hex32("92");
      },
      (value) => {
        value.rewindInstruction.replacementDepth = "4";
      },
    ];

    for (const mutate of mutations) {
      const forged = JSON.parse(JSON.stringify(rewound.result)) as Record<
        string,
        any
      >;
      mutate(forged);
      const {
        instructionDigest: _discardedInstructionDigest,
        ...instructionCanonical
      } = forged.rewindInstruction;
      forged.rewindInstruction.instructionDigest =
        sha256Canonical(instructionCanonical);
      const { resultDigest: _discardedResultDigest, ...resultCanonical } =
        forged;
      forged.resultDigest = sha256Canonical(resultCanonical);

      expect(
        evaluateWatcherRollbackV1(
          finalityPolicy,
          store,
          prior,
          rewound.consistency,
          forged,
          bootstrapState,
          bootstrapState,
        ),
      ).toMatchObject({
        action: "reject",
        reasonCodes: ["finality_provenance_mismatch"],
        nextStore: null,
      });
    }
  });

  it("authoritatively rejects an otherwise self-consistent forged restart nextStore", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const rewound = transition(finalityPolicy, prior, replacementPoint);
    const sourceStore = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      undefined,
      rewound.observations,
    );
    const bootstrapState = bootstrap(finalityPolicy, sourceStore, prior);
    const applied = evaluateWatcherRollbackV1(
      finalityPolicy,
      sourceStore,
      prior,
      rewound.consistency,
      rewound.result,
      bootstrapState,
      bootstrapState,
    );
    const attackerStore = combine(
      finalityPolicy.deploymentMarker,
      applied.nextRevision as string,
      [
        graph("50", {
          blockHash: hex32("ef"),
          slot: "700",
          blockNo: "70",
          depth: "8",
        }),
      ],
    );
    const attackerStoreDigest = watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(attackerStore),
    );
    const forged = JSON.parse(JSON.stringify(applied)) as Record<string, any>;
    forged.nextStore = attackerStore;
    forged.nextStoreDigest = attackerStoreDigest;
    forged.rollbackState.storeDigest = attackerStoreDigest;
    forged.rollbackState.transitionLineageDigest = hex32("f1");
    const { stateDigest: _discardedStateDigest, ...rollbackStateCanonical } =
      forged.rollbackState;
    forged.rollbackState.stateDigest = sha256Canonical(rollbackStateCanonical);
    const { resultDigest: _discardedResultDigest, ...resultCanonical } = forged;
    forged.resultDigest = sha256Canonical(resultCanonical);

    expect(
      parseWatcherRollbackResultV1(forged, {
        policy: finalityPolicy,
        sourceStore,
        previousFinalityState: prior,
        consistency: rewound.consistency,
        finalityResult: rewound.result,
        previousRollbackState: bootstrapState,
        rollbackBootstrapState: bootstrapState,
      }),
    ).toBeNull();
  });

  it("replays the bounded transition history and rejects self-hashed state forgery or journal reset", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const rewound = transition(finalityPolicy, prior, replacementPoint);
    const sourceStore = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      undefined,
      rewound.observations,
    );
    const bootstrapState = bootstrap(finalityPolicy, sourceStore, prior);

    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        sourceStore,
        prior,
        rewound.consistency,
        rewound.result,
        null,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["malformed_rollback_state"]);

    const applied = evaluateWatcherRollbackV1(
      finalityPolicy,
      sourceStore,
      prior,
      rewound.consistency,
      rewound.result,
      bootstrapState,
      bootstrapState,
    );
    const forged = JSON.parse(JSON.stringify(applied.rollbackState)) as Record<
      string,
      any
    >;
    forged.currentFinalityStateDigest = hex32("f2");
    const { stateDigest: _discardedStateDigest, ...canonical } = forged;
    forged.stateDigest = sha256Canonical(canonical);
    expect(
      parseWatcherRollbackStateV1(forged, {
        policy: finalityPolicy,
        rollbackBootstrapState: bootstrapState,
        currentStore: applied.nextStore,
      }),
    ).toBeNull();
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        applied.nextStore,
        prior,
        rewound.consistency,
        rewound.result,
        forged,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["malformed_rollback_state"]);

    const forgedTransition = JSON.parse(
      JSON.stringify(applied.rollbackState),
    ) as Record<string, any>;
    forgedTransition.transitions[0].consistency.agreement.minimumDepth = "3";
    const {
      consistencyDigest: _discardedConsistencyDigest,
      ...consistencyCanonical
    } = forgedTransition.transitions[0].consistency;
    forgedTransition.transitions[0].consistency.consistencyDigest =
      sha256Canonical(consistencyCanonical);
    const {
      transitionDigest: _discardedTransitionDigest,
      ...transitionCanonical
    } = forgedTransition.transitions[0];
    forgedTransition.transitions[0].transitionDigest =
      sha256Canonical(transitionCanonical);
    const {
      stateDigest: _discardedTransitionStateDigest,
      ...transitionStateCanonical
    } = forgedTransition;
    forgedTransition.stateDigest = sha256Canonical(transitionStateCanonical);
    expect(
      parseWatcherRollbackStateV1(forgedTransition, {
        policy: finalityPolicy,
        rollbackBootstrapState: bootstrapState,
        currentStore: applied.nextStore,
      }),
    ).toBeNull();

    const duplicatedTransition = JSON.parse(
      JSON.stringify(applied.rollbackState),
    ) as Record<string, any>;
    duplicatedTransition.transitions.push(
      JSON.parse(JSON.stringify(duplicatedTransition.transitions[0])),
    );
    duplicatedTransition.transitionCount = "2";
    const {
      stateDigest: _discardedDuplicatedStateDigest,
      ...duplicatedStateCanonical
    } = duplicatedTransition;
    duplicatedTransition.stateDigest = sha256Canonical(
      duplicatedStateCanonical,
    );
    expect(
      parseWatcherRollbackStateV1(duplicatedTransition, {
        policy: finalityPolicy,
        rollbackBootstrapState: bootstrapState,
        currentStore: applied.nextStore,
      }),
    ).toBeNull();

    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        applied.nextStore,
        prior,
        rewound.consistency,
        rewound.result,
        bootstrapState,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["rollback_state_store_mismatch"]);

    const resetState = bootstrap(
      finalityPolicy,
      applied.nextStore as WatcherDurableStoreV1,
      rewound.result.state as WatcherFinalityStateV1,
    );
    expect(
      parseWatcherRollbackStateV1(resetState, {
        policy: finalityPolicy,
        rollbackBootstrapState: bootstrapState,
        currentStore: applied.nextStore,
      }),
    ).toBeNull();
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        applied.nextStore,
        prior,
        rewound.consistency,
        rewound.result,
        resetState,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["malformed_rollback_state"]);
  });

  it("rotates an authenticated checkpoint at transition 129 and rejects linked-state reset or forgery", () => {
    const value = makeWatcherFinalityPolicyV1(
      localConfig(256),
      deploymentIdentity(),
    );
    expect(value).not.toBeNull();
    const finalityPolicy = value as WatcherFinalityPolicyV1;
    const points = Array.from({ length: 130 }, (_, depth) => ({
      ...oldPoint,
      depth: depth.toString(),
    }));
    const observations = points.map((point) => localObservation(point));
    let currentStore = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [],
      undefined,
      observations,
    );
    let previousFinalityState = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      localAgreement(points[129]!),
    ).state as WatcherFinalityStateV1;
    const rootBootstrapState = bootstrap(
      finalityPolicy,
      currentStore,
      previousFinalityState,
    );
    let currentRollbackState = rootBootstrapState;
    let currentRollbackBootstrapState = rootBootstrapState;

    for (let depth = 128; depth >= 1; depth -= 1) {
      const consistency = localAgreement(points[depth]!);
      const finalityResult = evaluateWatcherFinalityV1(
        finalityPolicy,
        previousFinalityState,
        consistency,
      );
      expect(finalityResult.action).toBe("rewind_pending");
      const applied = evaluateWatcherRollbackV1(
        finalityPolicy,
        currentStore,
        previousFinalityState,
        consistency,
        finalityResult,
        currentRollbackState,
        currentRollbackBootstrapState,
      );
      expect(applied.action).toBe("apply_rewind");
      currentStore = applied.nextStore!;
      currentRollbackState = applied.rollbackState!;
      currentRollbackBootstrapState = applied.rollbackBootstrapState!;
      previousFinalityState = finalityResult.state!;
    }

    expect(currentRollbackState).toMatchObject({
      epoch: "0",
      transitionCount: "128",
    });
    expect(currentRollbackState.transitions).toHaveLength(128);
    expect(currentRollbackBootstrapState).toEqual(rootBootstrapState);

    const sourceStore = currentStore;
    const priorRollbackState = currentRollbackState;
    const priorRollbackBootstrapState = currentRollbackBootstrapState;
    const priorFinalityState = previousFinalityState;
    const consistency = localAgreement(points[0]!);
    const finalityResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      priorFinalityState,
      consistency,
    );
    const rotated = evaluateWatcherRollbackV1(
      finalityPolicy,
      sourceStore,
      priorFinalityState,
      consistency,
      finalityResult,
      priorRollbackState,
      priorRollbackBootstrapState,
    );

    expect(rotated).toMatchObject({
      action: "apply_rewind",
      rollbackState: {
        epoch: "1",
        transitionCount: "129",
        epochCheckpoint: {
          epoch: "1",
          rootBootstrapStateDigest: rootBootstrapState.stateDigest,
          priorCheckpointDigest: null,
          priorTerminalStateDigest: priorRollbackState.stateDigest,
          priorTerminalTransitionCount: "128",
          priorTerminalTransitionLineageDigest:
            priorRollbackState.transitionLineageDigest,
          priorTerminalStoreDigest: priorRollbackState.storeDigest,
          priorTerminalFinalityStateDigest: priorFinalityState.stateDigest,
          priorTerminalIncidentDigest: null,
          recoveryStateDigest: null,
          recoveryLifecycleDigest: null,
        },
      },
    });
    expect(rotated.rollbackState?.transitions).toHaveLength(1);
    expect(rotated.rollbackBootstrapState).toMatchObject({
      epoch: "1",
      transitionCount: "128",
      transitions: [],
      epochCheckpoint: rotated.rollbackState?.epochCheckpoint,
    });
    expect(rotated.trustedCheckpointStateDigest).toBe(
      rotated.rollbackBootstrapState?.stateDigest,
    );
    expect(
      parseWatcherRollbackResultV1(JSON.parse(JSON.stringify(rotated)), {
        policy: finalityPolicy,
        sourceStore,
        previousFinalityState: priorFinalityState,
        consistency,
        finalityResult,
        previousRollbackState: priorRollbackState,
        rollbackBootstrapState: priorRollbackBootstrapState,
      }),
    ).toEqual(rotated);
    expect(
      parseWatcherRollbackStateV1(
        JSON.parse(JSON.stringify(rotated.rollbackState)),
        {
          policy: finalityPolicy,
          rollbackBootstrapState: JSON.parse(
            JSON.stringify(rotated.rollbackBootstrapState),
          ),
          trustedCheckpointStateDigest: rotated.trustedCheckpointStateDigest,
          currentStore: JSON.parse(JSON.stringify(rotated.nextStore)),
        },
      ),
    ).toEqual(rotated.rollbackState);

    const duplicate = evaluateWatcherRollbackV1(
      finalityPolicy,
      rotated.nextStore,
      priorFinalityState,
      consistency,
      finalityResult,
      rotated.rollbackState,
      rotated.rollbackBootstrapState,
      rotated.trustedCheckpointStateDigest,
    );
    expect(duplicate).toMatchObject({
      action: "duplicate_rewind",
      rollbackBootstrapState: rotated.rollbackBootstrapState,
    });

    const forged = JSON.parse(JSON.stringify(rotated.rollbackState)) as Record<
      string,
      any
    >;
    forged.epochCheckpoint.priorTerminalTransitionLineageDigest = hex32("f1");
    const {
      checkpointDigest: _discardedCheckpointDigest,
      ...checkpointCanonical
    } = forged.epochCheckpoint;
    forged.epochCheckpoint.checkpointDigest =
      sha256Canonical(checkpointCanonical);
    const { stateDigest: _discardedStateDigest, ...stateCanonical } = forged;
    forged.stateDigest = sha256Canonical(stateCanonical);
    expect(
      parseWatcherRollbackStateV1(forged, {
        policy: finalityPolicy,
        rollbackBootstrapState: rotated.rollbackBootstrapState,
        trustedCheckpointStateDigest: rotated.trustedCheckpointStateDigest,
        currentStore: rotated.nextStore,
      }),
    ).toBeNull();

    const resetState = bootstrap(
      finalityPolicy,
      rotated.nextStore!,
      finalityResult.state!,
    );
    expect(
      parseWatcherRollbackStateV1(resetState, {
        policy: finalityPolicy,
        rollbackBootstrapState: rotated.rollbackBootstrapState,
        trustedCheckpointStateDigest: rotated.trustedCheckpointStateDigest,
        currentStore: rotated.nextStore,
      }),
    ).toBeNull();

    const forgedBootstrap = JSON.parse(
      JSON.stringify(rotated.rollbackBootstrapState),
    ) as Record<string, any>;
    forgedBootstrap.epochCheckpoint.priorTerminalTransitionLineageDigest =
      hex32("f2");
    const {
      checkpointDigest: _discardedBootstrapCheckpointDigest,
      ...bootstrapCheckpointCanonical
    } = forgedBootstrap.epochCheckpoint;
    forgedBootstrap.epochCheckpoint.checkpointDigest = sha256Canonical(
      bootstrapCheckpointCanonical,
    );
    forgedBootstrap.transitionLineageDigest = hex32("f2");
    const {
      stateDigest: _discardedBootstrapStateDigest,
      ...bootstrapStateCanonical
    } = forgedBootstrap;
    forgedBootstrap.stateDigest = sha256Canonical(bootstrapStateCanonical);
    expect(
      parseWatcherRollbackStateV1(forgedBootstrap, {
        policy: finalityPolicy,
        rollbackBootstrapState: forgedBootstrap,
        currentStore: sourceStore,
      }),
    ).toBeNull();
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        sourceStore,
        priorFinalityState,
        consistency,
        finalityResult,
        forgedBootstrap,
        forgedBootstrap,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_rollback_state"],
    });
  }, 180_000);

  it("rejects a convergent alternate first-transition origin against the persisted W12 bootstrap", () => {
    const finalityPolicy = policy();
    const genuinePrior = pending(finalityPolicy, oldPoint);
    const advancedPriorResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      genuinePrior,
      agreement({ ...oldPoint, depth: "2" }),
    );
    expect(advancedPriorResult.action).toBe("advance_pending");
    const alternatePrior = advancedPriorResult.state as WatcherFinalityStateV1;
    expect(alternatePrior.stateDigest).not.toBe(genuinePrior.stateDigest);

    const genuineTransition = transition(
      finalityPolicy,
      genuinePrior,
      replacementPoint,
    );
    const alternateTransition = transition(
      finalityPolicy,
      alternatePrior,
      replacementPoint,
    );
    expect(alternateTransition.result.state).toEqual(
      genuineTransition.result.state,
    );
    const sourceStore = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      undefined,
      genuineTransition.observations,
    );
    const genuineBootstrap = bootstrap(
      finalityPolicy,
      sourceStore,
      genuinePrior,
    );
    const alternateBootstrap = bootstrap(
      finalityPolicy,
      sourceStore,
      alternatePrior,
    );
    const genuine = evaluateWatcherRollbackV1(
      finalityPolicy,
      sourceStore,
      genuinePrior,
      genuineTransition.consistency,
      genuineTransition.result,
      genuineBootstrap,
      genuineBootstrap,
    );
    const alternate = evaluateWatcherRollbackV1(
      finalityPolicy,
      sourceStore,
      alternatePrior,
      alternateTransition.consistency,
      alternateTransition.result,
      alternateBootstrap,
      alternateBootstrap,
    );
    expect(alternate.nextStore).toEqual(genuine.nextStore);
    expect(alternate.rollbackState?.currentFinalityStateDigest).toBe(
      genuine.rollbackState?.currentFinalityStateDigest,
    );
    expect(alternate.rollbackState?.transitions).not.toEqual(
      genuine.rollbackState?.transitions,
    );
    expect(
      parseWatcherRollbackStateV1(alternate.rollbackState, {
        policy: finalityPolicy,
        rollbackBootstrapState: genuineBootstrap,
        currentStore: genuine.nextStore,
      }),
    ).toBeNull();
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        sourceStore,
        alternatePrior,
        alternateTransition.consistency,
        alternateTransition.result,
        genuineBootstrap,
        genuineBootstrap,
      ).reasonCodes,
    ).toEqual(["stale_finality_state"]);
  });

  it("rejects self-hashed nested incident policy, release, deployment, store, and finality-lineage divergence", () => {
    const finalityPolicy = policy();
    const first = pending(finalityPolicy, { ...oldPoint, depth: "2" });
    const finalized = evaluateWatcherFinalityV1(
      finalityPolicy,
      first,
      agreement({ ...oldPoint, depth: "5" }),
    ).state as WatcherFinalityStateV1;
    const contradictionPoint = { ...replacementPoint, depth: "0" };
    const contradictionConsistency = agreement(contradictionPoint);
    const contradiction = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      contradictionConsistency,
    );
    const store = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint)],
      undefined,
      agreementObservations(contradictionPoint),
    );
    const bootstrapState = bootstrap(finalityPolicy, store, finalized);
    const quarantined = evaluateWatcherRollbackV1(
      finalityPolicy,
      store,
      finalized,
      contradictionConsistency,
      contradiction,
      bootstrapState,
      bootstrapState,
    );
    const mutations: readonly ((incident: Record<string, any>) => void)[] = [
      (incident) => {
        incident.policyDigest = hex32("81");
      },
      (incident) => {
        incident.releaseEvidenceDigest = hex32("82");
      },
      (incident) => {
        incident.deploymentMarker = makeDeploymentMarkerV1(hex32("83"));
      },
      (incident) => {
        incident.sourceStoreDigest = hex32("84");
      },
      (incident) => {
        incident.nextStoreDigest = hex32("89");
      },
      (incident) => {
        incident.finalityStateDigest = hex32("85");
      },
      (incident) => {
        incident.previousFinalityStateDigest = hex32("86");
      },
      (incident) => {
        incident.consistencyDigest = hex32("87");
      },
      (incident) => {
        incident.finalityResultDigest = hex32("88");
      },
    ];

    for (const mutate of mutations) {
      const state = JSON.parse(
        JSON.stringify(quarantined.rollbackState),
      ) as Record<string, any>;
      mutate(state.incident);
      const { incidentDigest: _discardedIncidentDigest, ...incidentCanonical } =
        state.incident;
      state.incident.incidentDigest = sha256Canonical(incidentCanonical);
      const { stateDigest: _discardedStateDigest, ...stateCanonical } = state;
      state.stateDigest = sha256Canonical(stateCanonical);
      expect(
        parseWatcherRollbackStateV1(state, {
          policy: finalityPolicy,
          rollbackBootstrapState: bootstrapState,
          currentStore: quarantined.nextStore,
        }),
      ).toBeNull();
    }

    const malformedConsistency = { status: "quarantined" };
    const malformedIncident = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      malformedConsistency,
    );
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        store,
        finalized,
        malformedConsistency,
        malformedIncident,
        bootstrapState,
        bootstrapState,
      ).reasonCodes,
    ).toEqual(["finality_provenance_mismatch"]);
  });

  it("accepts exact adjacent idempotence but rejects an older transition after a later lineage step", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const firstTransition = transition(finalityPolicy, prior, replacementPoint);
    const secondPrior = firstTransition.result.state as WatcherFinalityStateV1;
    const secondTransition = transition(finalityPolicy, secondPrior, {
      ...replacementPoint,
      bodyHex: "a100",
    });
    const sourceStore = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      undefined,
      [...firstTransition.observations, ...secondTransition.observations],
    );
    const bootstrapState = bootstrap(finalityPolicy, sourceStore, prior);
    const firstApplied = evaluateWatcherRollbackV1(
      finalityPolicy,
      sourceStore,
      prior,
      firstTransition.consistency,
      firstTransition.result,
      bootstrapState,
      bootstrapState,
    );
    const adjacent = evaluateWatcherRollbackV1(
      finalityPolicy,
      firstApplied.nextStore,
      prior,
      firstTransition.consistency,
      firstTransition.result,
      firstApplied.rollbackState,
      bootstrapState,
    );
    expect(adjacent.action).toBe("duplicate_rewind");

    const secondApplied = evaluateWatcherRollbackV1(
      finalityPolicy,
      firstApplied.nextStore,
      secondPrior,
      secondTransition.consistency,
      secondTransition.result,
      firstApplied.rollbackState,
      bootstrapState,
    );
    expect(secondApplied).toMatchObject({
      action: "apply_rewind",
      rollbackState: { transitionCount: "2" },
    });
    const olderReplay = evaluateWatcherRollbackV1(
      finalityPolicy,
      secondApplied.nextStore,
      prior,
      firstTransition.consistency,
      firstTransition.result,
      secondApplied.rollbackState,
      bootstrapState,
    );
    expect(olderReplay).toMatchObject({
      action: "reject",
      reasonCodes: ["stale_finality_state"],
      nextStore: null,
    });
  });

  it("fails closed unless both exact independent W11 observations and chain-point associations are durable", () => {
    const finalityPolicy = policy();
    const prior = pending(finalityPolicy, oldPoint);
    const rewound = transition(finalityPolicy, prior, replacementPoint);
    const oldGraph = graph("10", oldPoint);
    const replacementGraph = graph("20", replacementPoint);
    const oneProviderStore = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [oldGraph, replacementGraph],
      undefined,
      rewound.observations.slice(0, 1),
    );
    const oneProviderBootstrap = bootstrap(
      finalityPolicy,
      oneProviderStore,
      prior,
    );
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        oneProviderStore,
        prior,
        rewound.consistency,
        rewound.result,
        oneProviderBootstrap,
        oneProviderBootstrap,
      ).reasonCodes,
    ).toEqual(["replacement_evidence_missing"]);

    const completeStore = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [oldGraph, replacementGraph],
      undefined,
      rewound.observations,
    );
    const providerA = rewound.observations[0] as WatcherNormalizedL1BlockV1;
    const misassociatedStore = makeWatcherDurableStoreV1({
      deploymentMarker: completeStore.deploymentMarker,
      revision: completeStore.revision,
      records: {
        ...completeStore,
        l1Observations: completeStore.l1Observations.map((value) =>
          value.observationId === providerA.observationDigest
            ? {
                ...value,
                chainPointId: oldGraph.ids.chainPoint,
              }
            : value,
        ),
      },
    });
    const misassociatedBootstrap = bootstrap(
      finalityPolicy,
      misassociatedStore,
      prior,
    );
    expect(
      evaluateWatcherRollbackV1(
        finalityPolicy,
        misassociatedStore,
        prior,
        rewound.consistency,
        rewound.result,
        misassociatedBootstrap,
        misassociatedBootstrap,
      ).reasonCodes,
    ).toEqual(["replacement_evidence_missing"]);
  });
});
