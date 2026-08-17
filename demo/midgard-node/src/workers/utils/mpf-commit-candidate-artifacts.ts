import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import {
  boundedNonEmptyString,
  canonicalAbsolutePath,
  canonicalUtcTimestamp,
  exactKeysRecord,
  nonNegativeFiniteNumber,
  nonNegativeSafeInteger,
  positiveFiniteNumber,
  positiveSafeInteger,
  sha256Digest,
} from "@/artifact-schema.js";
import {
  ogmiosEndpointIdentitySha256,
  type ShelleyGenesisSlotEvidence,
} from "@/local-ledger-slot.js";

type JsonRecord = Record<string, unknown>;

export type ArchitectureGPhase1FormalBindingIdentityV1 = {
  readonly schemaVersion: "midgard-architecture-g-phase1-formal-binding-identity-v1";
  readonly path: string;
  readonly sha256: string;
  readonly deploymentManifestId: string;
  readonly nodeImageId: string;
  readonly nodeContainerId: string;
  readonly walletSetSha256: string;
  readonly fundingSetSha256: string;
  readonly corpus: {
    readonly path: string;
    readonly indexPath: string;
    readonly manifestPath: string;
    readonly sliceId: string;
    readonly corpusSha256: string;
    readonly indexSha256: string;
    readonly manifestSha256: string;
  };
  readonly generationResult: {
    readonly path: string;
    readonly sha256: string;
    readonly schemaVersion: "midgard-stress-corpus-generation-v1";
  };
  readonly harness: {
    readonly scenarioId: string;
    readonly engineId: string;
  };
};

export type ArchitectureGRuntimeIdentityV1 = {
  readonly schemaVersion: "midgard-architecture-g-runtime-identity-v1";
  readonly version: string;
  readonly execPath: string;
  readonly executableSha256: string;
};

export type ArchitectureGCommitCandidateSeedInputV1 = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-seed-v1";
  readonly phase1FormalBinding: ArchitectureGPhase1FormalBindingIdentityV1;
  readonly runtimeIdentity: ArchitectureGRuntimeIdentityV1;
  readonly corpusSlicePath: string;
  readonly corpusSliceSha256: string;
  readonly fundingMapPath: string;
  readonly fundingMapSha256: string;
  readonly expectedTransactionCount: number;
  readonly firstTimestampIso: string;
};

export type ArchitectureGCorpusFundingV1 = {
  readonly schemaVersion: "midgard-architecture-g-corpus-funding-v1";
  readonly corpusSha256: string;
  readonly sliceSha256: string;
  readonly entries: readonly {
    readonly walletId: string;
    readonly outref: string;
    readonly outputCbor: string;
  }[];
};

export type ArchitectureGCommitCandidateSeedResultV1 = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-seed-result-v1";
  readonly databaseName: string;
  readonly corpusSliceSha256: string;
  readonly mempoolTxCount: number;
  readonly fundingCount: number;
  readonly terminalLedgerCount: number;
  readonly deltaCount: number;
};

export type ArchitectureGCommitCandidateInputV1 = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-input-v1";
  readonly phase1FormalBinding: ArchitectureGPhase1FormalBindingIdentityV1;
  readonly runtimeIdentity: ArchitectureGRuntimeIdentityV1;
  readonly levelPath: string;
  readonly binaryPath: string;
  readonly binarySha256: string;
  readonly sidecarPath: string;
  readonly expectedTransactionCount: number;
  readonly corpusSha256: string;
  readonly corpusSliceSha256: string;
  readonly fundingMapSha256: string;
  readonly fixtureCreationPath: string;
  readonly fixtureCreationSha256: string;
  readonly fixtureInitialUtxoCount: number;
  readonly baseUtxoPayloadAggregate: {
    readonly entryCount: number;
    readonly encodedTupleBytes: number;
  };
  readonly forcedValidationSlotConfigArtifact: {
    readonly path: string;
    readonly sha256: string;
    readonly document: {
      readonly schemaVersion: "midgard-node-slot-config-evidence-v1";
      readonly capturedAtIso: string;
      readonly network: "Mainnet" | "Preview" | "Preprod" | "Custom";
      readonly source:
        | {
            readonly kind: "lucid_network_table";
            readonly lucidVersion: "0.6.0";
          }
        | {
            readonly kind: "local_ogmios_genesis";
            readonly endpointIdentitySha256: string;
            readonly configurationSha256: string;
          };
      readonly slotConfig: {
        readonly zeroTime: number;
        readonly zeroSlot: number;
        readonly slotLength: number;
      };
    };
  };
  readonly workerInput: {
    readonly data: {
      readonly availableConfirmedBlock: "";
      readonly availableLocalFinalizationBlock: "";
      readonly currentBlockStartTimeMs: number;
      readonly forcedValidationSlotConfig: {
        readonly zeroTime: number;
        readonly zeroSlot: number;
        readonly slotLength: number;
      };
      readonly localFinalizationPending: false;
      readonly ledgerStoreLeaseOwner: string;
      readonly mempoolTxsCountSoFar: 0;
      readonly sizeOfProcessedTxsSoFar: 0;
      readonly baseSnapshotId: string;
      readonly stateQueueHasUnmergedTail: true;
      readonly speculativeBuild: {
        readonly base: {
          readonly headerHash: string;
          readonly utxosRoot: string;
          readonly blockEndTimeMs: number;
          readonly submittedTxHash: string;
        };
        readonly watermarks: {
          readonly depositMs: number;
          readonly withdrawalMs: number;
          readonly txOrderMs: number;
          readonly refreshedAtMs: number;
        };
        readonly excludedMempoolTxIds: readonly string[];
        readonly excludedDepositEventIds: readonly string[];
        readonly excludedForcedTransactionEventIds: readonly string[];
        readonly excludedWithdrawalEventIds: readonly string[];
      };
    };
  };
};

export type ArchitectureGFixtureCreationV1 = {
  readonly fixtureCreated: true;
  readonly fixturePath: string;
  readonly initialUtxoCount: number;
  readonly marker: string;
  readonly durationMs: number;
  readonly diagnostics: Readonly<Record<string, number>>;
  readonly utxoPayloadAggregate: {
    readonly entryCount: number;
    readonly encodedTupleBytes: number;
  };
  readonly canonicalFunding: null | {
    readonly path: string;
    readonly sha256: string;
    readonly entryCount: number;
  };
};

const architectureGFixtureDiagnosticKeys = [
  "entries",
  "storePuts",
  "storeDels",
  "serialiseCalls",
  "serialiseMs",
  "deferredMaterializedEstimatedBytes",
  "deferredMaterializedActualBytes",
  "deferredLazyReads",
  "deferredLazySerialiseMs",
  "deferredLazySerialisedBytes",
  "arenaCheckpointCalls",
  "arenaCheckpointMs",
  "arenaCheckpointNodes",
  "arenaCheckpointBytes",
  "pathCacheEntries",
  "pathCacheBytes",
  "pathCacheHits",
  "liveArenaPrunedNodes",
  "liveArenaPromotedNodes",
  "liveArenaPromotedBytes",
  "retainedSnapshotAuthentications",
  "retainedSnapshotAuthenticationMs",
  "transientLiveNodes",
  "transientLiveBytes",
  "transientDirtyNodes",
  "transientSnapshotsCaptured",
  "eventAtomicFinalizations",
  "eventAtomicDirtyNodes",
  "eventAtomicMaxDirtyNodes",
  "levelGets",
  "levelGetManyCalls",
  "levelGetManyMaxKeys",
  "levelGetMs",
  "jsonCodecMs",
  "overlayHits",
  "readCacheHits",
  "levelBatchWrites",
  "bytesFlushed",
  "overlayEntries",
  "overlayBytes",
  "overlaySpills",
  "overlaySpillMs",
  "flushMs",
] as const;

const architectureGOwnerDiagnosticKeys = [
  "ownerEpoch",
  "durableRoot",
  "residentNodes",
  "residentEdges",
  "residentBytes",
  "activeGenerations",
  "generatedNodes",
  "generatedBytes",
  "rssBytes",
  "peakRssBytes",
  "childRestarts",
] as const;

/** Converts a database count for a JSON artifact without truncation. */
export const toJsonSafeCount = (count: bigint, label: string): number => {
  const maximum = BigInt(Number.MAX_SAFE_INTEGER);
  if (count < 0n || count > maximum) {
    throw new Error(
      `${label} must be a safe integer between 0 and ${maximum.toString()}: ${count.toString()}`,
    );
  }
  return Number(count);
};

const sameJson = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

const decodeArchitectureGOwnerDiagnostics = (
  value: unknown,
  label: string,
): JsonRecord => {
  const owner = exactKeysRecord(value, label, architectureGOwnerDiagnosticKeys);
  const ownerEpoch = exactKeysRecord(owner.ownerEpoch, `${label}.ownerEpoch`, [
    "type",
    "data",
  ]);
  if (
    ownerEpoch.type !== "Buffer" ||
    !Array.isArray(ownerEpoch.data) ||
    ownerEpoch.data.length !== 16 ||
    !ownerEpoch.data.every(
      (byte) => Number.isInteger(byte) && byte >= 0 && byte <= 255,
    )
  ) {
    throw new Error(`${label}.ownerEpoch is invalid`);
  }
  sha256Digest(owner.durableRoot, `${label}.durableRoot`);
  for (const field of architectureGOwnerDiagnosticKeys.slice(2)) {
    nonNegativeSafeInteger(owner[field], `${label}.${field}`);
  }
  return owner;
};

const decodePhase1FormalBindingIdentity = (
  value: unknown,
): ArchitectureGPhase1FormalBindingIdentityV1 => {
  const identity = exactKeysRecord(
    value,
    "Architecture G Phase 1 formal-binding identity",
    [
      "schemaVersion",
      "path",
      "sha256",
      "deploymentManifestId",
      "nodeImageId",
      "nodeContainerId",
      "walletSetSha256",
      "fundingSetSha256",
      "corpus",
      "generationResult",
      "harness",
    ],
  );
  const corpus = exactKeysRecord(
    identity.corpus,
    "Architecture G Phase 1 corpus identity",
    [
      "path",
      "indexPath",
      "manifestPath",
      "sliceId",
      "corpusSha256",
      "indexSha256",
      "manifestSha256",
    ],
  );
  const generationResult = exactKeysRecord(
    identity.generationResult,
    "Architecture G Phase 1 generation-result identity",
    ["path", "sha256", "schemaVersion"],
  );
  const harness = exactKeysRecord(
    identity.harness,
    "Architecture G Phase 1 harness identity",
    ["scenarioId", "engineId"],
  );
  if (
    identity.schemaVersion !==
      "midgard-architecture-g-phase1-formal-binding-identity-v1" ||
    generationResult.schemaVersion !== "midgard-stress-corpus-generation-v1"
  ) {
    throw new Error("Unsupported Architecture G Phase 1 identity");
  }
  canonicalAbsolutePath(identity.path, "formalBinding.path");
  sha256Digest(identity.sha256, "formalBinding.sha256");
  boundedNonEmptyString(
    identity.deploymentManifestId,
    "formalBinding.deploymentManifestId",
  );
  boundedNonEmptyString(identity.nodeImageId, "formalBinding.nodeImageId");
  boundedNonEmptyString(
    identity.nodeContainerId,
    "formalBinding.nodeContainerId",
  );
  sha256Digest(identity.walletSetSha256, "formalBinding.walletSetSha256");
  sha256Digest(identity.fundingSetSha256, "formalBinding.fundingSetSha256");
  canonicalAbsolutePath(corpus.path, "formalBinding.corpus.path");
  canonicalAbsolutePath(corpus.indexPath, "formalBinding.corpus.indexPath");
  canonicalAbsolutePath(
    corpus.manifestPath,
    "formalBinding.corpus.manifestPath",
  );
  boundedNonEmptyString(corpus.sliceId, "formalBinding.corpus.sliceId");
  sha256Digest(corpus.corpusSha256, "formalBinding.corpus.corpusSha256");
  sha256Digest(corpus.indexSha256, "formalBinding.corpus.indexSha256");
  sha256Digest(corpus.manifestSha256, "formalBinding.corpus.manifestSha256");
  canonicalAbsolutePath(
    generationResult.path,
    "formalBinding.generationResult.path",
  );
  sha256Digest(
    generationResult.sha256,
    "formalBinding.generationResult.sha256",
  );
  sha256Digest(harness.scenarioId, "formalBinding.harness.scenarioId");
  sha256Digest(harness.engineId, "formalBinding.harness.engineId");
  return identity as ArchitectureGPhase1FormalBindingIdentityV1;
};

const decodeRuntimeIdentity = (
  value: unknown,
): ArchitectureGRuntimeIdentityV1 => {
  const identity = exactKeysRecord(value, "Architecture G runtime identity", [
    "schemaVersion",
    "version",
    "execPath",
    "executableSha256",
  ]);
  if (identity.schemaVersion !== "midgard-architecture-g-runtime-identity-v1") {
    throw new Error("Unsupported Architecture G runtime identity");
  }
  boundedNonEmptyString(identity.version, "runtimeIdentity.version");
  canonicalAbsolutePath(identity.execPath, "runtimeIdentity.execPath");
  sha256Digest(identity.executableSha256, "runtimeIdentity.executableSha256");
  return identity as ArchitectureGRuntimeIdentityV1;
};

export const decodeArchitectureGCommitCandidateSeedInputV1 = (
  value: unknown,
): ArchitectureGCommitCandidateSeedInputV1 => {
  const input = exactKeysRecord(
    value,
    "Architecture G commit-candidate seed input",
    [
      "schemaVersion",
      "phase1FormalBinding",
      "runtimeIdentity",
      "corpusSlicePath",
      "corpusSliceSha256",
      "fundingMapPath",
      "fundingMapSha256",
      "expectedTransactionCount",
      "firstTimestampIso",
    ],
  );
  if (
    input.schemaVersion !== "midgard-architecture-g-commit-candidate-seed-v1"
  ) {
    throw new Error("Unsupported Architecture G candidate seed input");
  }
  decodePhase1FormalBindingIdentity(input.phase1FormalBinding);
  decodeRuntimeIdentity(input.runtimeIdentity);
  canonicalAbsolutePath(input.corpusSlicePath, "seedInput.corpusSlicePath");
  sha256Digest(input.corpusSliceSha256, "seedInput.corpusSliceSha256");
  canonicalAbsolutePath(input.fundingMapPath, "seedInput.fundingMapPath");
  sha256Digest(input.fundingMapSha256, "seedInput.fundingMapSha256");
  positiveSafeInteger(
    input.expectedTransactionCount,
    "seedInput.expectedTransactionCount",
  );
  canonicalUtcTimestamp(input.firstTimestampIso, "seedInput.firstTimestampIso");
  return input as ArchitectureGCommitCandidateSeedInputV1;
};

export const decodeArchitectureGCommitCandidateInputV1 = (
  value: unknown,
): ArchitectureGCommitCandidateInputV1 => {
  const input = exactKeysRecord(
    value,
    "Architecture G commit-candidate input",
    [
      "schemaVersion",
      "phase1FormalBinding",
      "runtimeIdentity",
      "levelPath",
      "binaryPath",
      "binarySha256",
      "sidecarPath",
      "expectedTransactionCount",
      "corpusSha256",
      "corpusSliceSha256",
      "fundingMapSha256",
      "fixtureCreationPath",
      "fixtureCreationSha256",
      "fixtureInitialUtxoCount",
      "baseUtxoPayloadAggregate",
      "forcedValidationSlotConfigArtifact",
      "workerInput",
    ],
  );
  const phase1FormalBinding = decodePhase1FormalBindingIdentity(
    input.phase1FormalBinding,
  );
  decodeRuntimeIdentity(input.runtimeIdentity);
  const aggregate = exactKeysRecord(
    input.baseUtxoPayloadAggregate,
    "Architecture G candidate base UTxO aggregate",
    ["entryCount", "encodedTupleBytes"],
  );
  const slotConfigArtifact = exactKeysRecord(
    input.forcedValidationSlotConfigArtifact,
    "Architecture G candidate slot-config artifact binding",
    ["path", "sha256", "document"],
  );
  const slotConfigArtifactPath = canonicalAbsolutePath(
    slotConfigArtifact.path,
    "candidateInput.forcedValidationSlotConfigArtifact.path",
  );
  const slotConfigArtifactSha256 = sha256Digest(
    slotConfigArtifact.sha256,
    "candidateInput.forcedValidationSlotConfigArtifact.sha256",
  );
  const slotConfigArtifactBytes = readFileSync(slotConfigArtifactPath);
  if (
    createHash("sha256").update(slotConfigArtifactBytes).digest("hex") !==
    slotConfigArtifactSha256
  ) {
    throw new Error("Node slot-config evidence SHA-256 mismatch");
  }
  const slotConfigDocument = exactKeysRecord(
    slotConfigArtifact.document,
    "Architecture G candidate slot-config artifact document",
    ["schemaVersion", "capturedAtIso", "network", "source", "slotConfig"],
  );
  if (
    slotConfigDocument.schemaVersion !== "midgard-node-slot-config-evidence-v1"
  ) {
    throw new Error("Unsupported node slot-config evidence schema");
  }
  canonicalUtcTimestamp(
    slotConfigDocument.capturedAtIso,
    "candidateInput.forcedValidationSlotConfigArtifact.capturedAtIso",
  );
  const slotConfigSource =
    slotConfigDocument.network === "Custom"
      ? exactKeysRecord(
          slotConfigDocument.source,
          "Custom slot-config source",
          ["kind", "endpointIdentitySha256", "configurationSha256"],
        )
      : exactKeysRecord(
          slotConfigDocument.source,
          "Static slot-config source",
          ["kind", "lucidVersion"],
        );
  if (slotConfigDocument.network === "Custom") {
    if (slotConfigSource.kind !== "local_ogmios_genesis") {
      throw new Error("Custom slot-config source is invalid");
    }
    sha256Digest(
      slotConfigSource.endpointIdentitySha256,
      "candidateInput.forcedValidationSlotConfigArtifact.source.endpointIdentitySha256",
    );
    sha256Digest(
      slotConfigSource.configurationSha256,
      "candidateInput.forcedValidationSlotConfigArtifact.source.configurationSha256",
    );
  } else if (
    !["Mainnet", "Preview", "Preprod"].includes(
      slotConfigDocument.network as string,
    ) ||
    slotConfigSource.kind !== "lucid_network_table" ||
    slotConfigSource.lucidVersion !== "0.6.0"
  ) {
    throw new Error("Static slot-config source is invalid");
  }
  const artifactSlotConfig = exactKeysRecord(
    slotConfigDocument.slotConfig,
    "Architecture G candidate artifact slot configuration",
    ["zeroTime", "zeroSlot", "slotLength"],
  );
  if (
    JSON.stringify(
      JSON.parse(slotConfigArtifactBytes.toString("utf8")) as unknown,
    ) !== JSON.stringify(slotConfigArtifact.document)
  ) {
    throw new Error(
      "Architecture G candidate slot-config document does not match its bound artifact",
    );
  }
  const staticSlotConfigs: Readonly<
    Record<string, Readonly<Record<string, number>>>
  > = {
    Mainnet: {
      zeroTime: 1_596_059_091_000,
      zeroSlot: 4_492_800,
      slotLength: 1_000,
    },
    Preview: {
      zeroTime: 1_666_656_000_000,
      zeroSlot: 0,
      slotLength: 1_000,
    },
    Preprod: {
      zeroTime: 1_655_769_600_000,
      zeroSlot: 86_400,
      slotLength: 1_000,
    },
  };
  if (
    slotConfigDocument.network !== "Custom" &&
    JSON.stringify(artifactSlotConfig) !==
      JSON.stringify(staticSlotConfigs[slotConfigDocument.network as string])
  ) {
    throw new Error(
      "Static slot configuration does not match the pinned Lucid network table",
    );
  }
  const workerInput = exactKeysRecord(
    input.workerInput,
    "Architecture G candidate worker input",
    ["data"],
  );
  const data = exactKeysRecord(
    workerInput.data,
    "Architecture G candidate worker data",
    [
      "availableConfirmedBlock",
      "availableLocalFinalizationBlock",
      "currentBlockStartTimeMs",
      "forcedValidationSlotConfig",
      "localFinalizationPending",
      "ledgerStoreLeaseOwner",
      "mempoolTxsCountSoFar",
      "sizeOfProcessedTxsSoFar",
      "baseSnapshotId",
      "stateQueueHasUnmergedTail",
      "speculativeBuild",
    ],
  );
  const speculativeBuild = exactKeysRecord(
    data.speculativeBuild,
    "Architecture G candidate speculative build",
    [
      "base",
      "watermarks",
      "excludedMempoolTxIds",
      "excludedDepositEventIds",
      "excludedForcedTransactionEventIds",
      "excludedWithdrawalEventIds",
    ],
  );
  const base = exactKeysRecord(
    speculativeBuild.base,
    "Architecture G candidate speculative base",
    ["headerHash", "utxosRoot", "blockEndTimeMs", "submittedTxHash"],
  );
  const watermarks = exactKeysRecord(
    speculativeBuild.watermarks,
    "Architecture G candidate barrier watermarks",
    ["depositMs", "withdrawalMs", "txOrderMs", "refreshedAtMs"],
  );
  const forcedValidationSlotConfig = exactKeysRecord(
    data.forcedValidationSlotConfig,
    "Architecture G candidate forced-validation slot configuration",
    ["zeroTime", "zeroSlot", "slotLength"],
  );
  if (
    input.schemaVersion !== "midgard-architecture-g-commit-candidate-input-v1"
  ) {
    throw new Error("Unsupported Architecture G commit-candidate input");
  }
  for (const [pathValue, label] of [
    [input.levelPath, "candidateInput.levelPath"],
    [input.binaryPath, "candidateInput.binaryPath"],
    [input.sidecarPath, "candidateInput.sidecarPath"],
    [input.fixtureCreationPath, "candidateInput.fixtureCreationPath"],
  ] as const) {
    canonicalAbsolutePath(pathValue, label);
  }
  for (const [hashValue, label] of [
    [input.binarySha256, "candidateInput.binarySha256"],
    [input.corpusSha256, "candidateInput.corpusSha256"],
    [input.corpusSliceSha256, "candidateInput.corpusSliceSha256"],
    [input.fundingMapSha256, "candidateInput.fundingMapSha256"],
    [input.fixtureCreationSha256, "candidateInput.fixtureCreationSha256"],
  ] as const) {
    sha256Digest(hashValue, label);
  }
  positiveSafeInteger(
    input.expectedTransactionCount,
    "candidateInput.expectedTransactionCount",
  );
  const fixtureInitialUtxoCount = positiveSafeInteger(
    input.fixtureInitialUtxoCount,
    "candidateInput.fixtureInitialUtxoCount",
  );
  if (
    input.corpusSha256 !== phase1FormalBinding.corpus.corpusSha256 ||
    aggregate.entryCount !== fixtureInitialUtxoCount ||
    data.availableConfirmedBlock !== "" ||
    data.availableLocalFinalizationBlock !== "" ||
    data.localFinalizationPending !== false ||
    !/^commit:[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u.test(
      data.ledgerStoreLeaseOwner as string,
    ) ||
    data.mempoolTxsCountSoFar !== 0 ||
    data.sizeOfProcessedTxsSoFar !== 0 ||
    data.stateQueueHasUnmergedTail !== true
  ) {
    throw new Error("Architecture G candidate input identity is invalid");
  }
  positiveSafeInteger(
    aggregate.encodedTupleBytes,
    "candidateInput.baseUtxoPayloadAggregate.encodedTupleBytes",
  );
  const currentBlockStartTimeMs = positiveSafeInteger(
    data.currentBlockStartTimeMs,
    "candidateInput.currentBlockStartTimeMs",
  );
  const slotZeroTime = nonNegativeSafeInteger(
    forcedValidationSlotConfig.zeroTime,
    "candidateInput.forcedValidationSlotConfig.zeroTime",
  );
  const slotZeroSlot = nonNegativeSafeInteger(
    forcedValidationSlotConfig.zeroSlot,
    "candidateInput.forcedValidationSlotConfig.zeroSlot",
  );
  const slotLength = positiveSafeInteger(
    forcedValidationSlotConfig.slotLength,
    "candidateInput.forcedValidationSlotConfig.slotLength",
  );
  if (
    JSON.stringify(forcedValidationSlotConfig) !==
    JSON.stringify(artifactSlotConfig)
  ) {
    throw new Error(
      "Architecture G candidate worker slot configuration does not match its bound artifact",
    );
  }
  const currentBlockSlot =
    Math.floor((currentBlockStartTimeMs - slotZeroTime) / slotLength) +
    slotZeroSlot;
  if (!Number.isSafeInteger(currentBlockSlot) || currentBlockSlot < 0) {
    throw new Error(
      "Architecture G candidate block time is outside its forced-validation slot configuration",
    );
  }
  const submittedTxHash = sha256Digest(
    base.submittedTxHash,
    "candidateInput.speculativeBuild.base.submittedTxHash",
  );
  sha256Digest(
    base.utxosRoot,
    "candidateInput.speculativeBuild.base.utxosRoot",
  );
  if (
    typeof base.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(base.headerHash) ||
    base.headerHash !== submittedTxHash.slice(0, 56) ||
    base.blockEndTimeMs !== currentBlockStartTimeMs ||
    data.baseSnapshotId !== `architecture-g-candidate:${submittedTxHash}`
  ) {
    throw new Error("Architecture G candidate speculative base is invalid");
  }
  const watermarkValues = [
    positiveSafeInteger(watermarks.depositMs, "watermarks.depositMs"),
    positiveSafeInteger(watermarks.withdrawalMs, "watermarks.withdrawalMs"),
    positiveSafeInteger(watermarks.txOrderMs, "watermarks.txOrderMs"),
  ];
  const refreshedAtMs = positiveSafeInteger(
    watermarks.refreshedAtMs,
    "watermarks.refreshedAtMs",
  );
  if (
    Math.max(...watermarkValues) > refreshedAtMs ||
    currentBlockStartTimeMs >= Math.min(...watermarkValues)
  ) {
    throw new Error("Architecture G candidate barrier watermarks are invalid");
  }
  for (const field of [
    "excludedMempoolTxIds",
    "excludedDepositEventIds",
    "excludedForcedTransactionEventIds",
    "excludedWithdrawalEventIds",
  ] as const) {
    if (
      !Array.isArray(speculativeBuild[field]) ||
      speculativeBuild[field].length !== 0
    ) {
      throw new Error(
        `Architecture G candidate ${field} must be an exact empty array`,
      );
    }
  }
  return input as ArchitectureGCommitCandidateInputV1;
};

export const assertArchitectureGCandidateSlotRuntimeIdentityV1 = ({
  input,
  runtimeNetwork,
  ogmiosUrl,
  customGenesis,
}: {
  readonly input: ArchitectureGCommitCandidateInputV1;
  readonly runtimeNetwork: "Mainnet" | "Preview" | "Preprod" | "Custom";
  readonly ogmiosUrl?: string;
  readonly customGenesis?: ShelleyGenesisSlotEvidence;
}): void => {
  const document = input.forcedValidationSlotConfigArtifact.document;
  if (document.network !== runtimeNetwork) {
    throw new Error(
      "Architecture G candidate slot-config network does not match NodeConfig.NETWORK",
    );
  }
  if (runtimeNetwork !== "Custom") return;
  if (
    document.source.kind !== "local_ogmios_genesis" ||
    ogmiosUrl === undefined ||
    customGenesis === undefined ||
    document.source.endpointIdentitySha256 !==
      ogmiosEndpointIdentitySha256(ogmiosUrl) ||
    document.source.configurationSha256 !== customGenesis.configurationSha256 ||
    JSON.stringify(document.slotConfig) !==
      JSON.stringify({
        zeroTime: customGenesis.startTimeMs,
        zeroSlot: 0,
        slotLength: customGenesis.slotLengthMs,
      })
  ) {
    throw new Error(
      "Architecture G Custom slot configuration does not match the live configured Ogmios genesis",
    );
  }
};

export const decodeArchitectureGFixtureCreationV1 = ({
  value,
  expectedFixturePath,
  expectedMarker,
  expectedUtxos,
  expectedAggregate,
  expectedFundingMapSha256,
}: {
  readonly value: unknown;
  readonly expectedFixturePath: string;
  readonly expectedMarker: string;
  readonly expectedUtxos: number;
  readonly expectedAggregate: {
    readonly entryCount: number;
    readonly encodedTupleBytes: number;
  };
  readonly expectedFundingMapSha256: string | null;
}): ArchitectureGFixtureCreationV1 => {
  const artifact = exactKeysRecord(
    value,
    "Architecture G fixture-creation artifact",
    [
      "fixtureCreated",
      "fixturePath",
      "initialUtxoCount",
      "marker",
      "durationMs",
      "diagnostics",
      "utxoPayloadAggregate",
      "canonicalFunding",
    ],
  );
  const aggregate = exactKeysRecord(
    artifact.utxoPayloadAggregate,
    "Architecture G fixture payload aggregate",
    ["entryCount", "encodedTupleBytes"],
  );
  const diagnostics = exactKeysRecord(
    artifact.diagnostics,
    "Architecture G fixture diagnostics",
    architectureGFixtureDiagnosticKeys,
  );
  for (const field of architectureGFixtureDiagnosticKeys) {
    if (field.endsWith("Ms")) {
      nonNegativeFiniteNumber(
        diagnostics[field],
        `fixtureCreation.diagnostics.${field}`,
      );
    } else {
      nonNegativeSafeInteger(
        diagnostics[field],
        `fixtureCreation.diagnostics.${field}`,
      );
    }
  }
  const fixturePath = canonicalAbsolutePath(
    artifact.fixturePath,
    "fixtureCreation.fixturePath",
  );
  const expectedPath = canonicalAbsolutePath(
    expectedFixturePath,
    "expectedFixturePath",
  );
  const marker = sha256Digest(artifact.marker, "fixtureCreation.marker");
  const expectedRoot = sha256Digest(expectedMarker, "expectedMarker");
  const initialUtxoCount = positiveSafeInteger(
    artifact.initialUtxoCount,
    "fixtureCreation.initialUtxoCount",
  );
  const expectedCount = positiveSafeInteger(expectedUtxos, "expectedUtxos");
  const aggregateEntryCount = positiveSafeInteger(
    aggregate.entryCount,
    "fixtureCreation.utxoPayloadAggregate.entryCount",
  );
  const aggregateEncodedTupleBytes = positiveSafeInteger(
    aggregate.encodedTupleBytes,
    "fixtureCreation.utxoPayloadAggregate.encodedTupleBytes",
  );
  const expectedAggregateEntryCount = positiveSafeInteger(
    expectedAggregate.entryCount,
    "expectedAggregate.entryCount",
  );
  const expectedAggregateEncodedTupleBytes = positiveSafeInteger(
    expectedAggregate.encodedTupleBytes,
    "expectedAggregate.encodedTupleBytes",
  );
  positiveFiniteNumber(artifact.durationMs, "fixtureCreation.durationMs");
  let canonicalFundingSha256: string | null = null;
  if (expectedFundingMapSha256 === null) {
    if (artifact.canonicalFunding !== null) {
      throw new Error(
        "Architecture G fixture creation unexpectedly claims canonical funding",
      );
    }
  } else {
    const fundingMapSha256 = sha256Digest(
      expectedFundingMapSha256,
      "expectedFundingMapSha256",
    );
    const canonicalFunding = exactKeysRecord(
      artifact.canonicalFunding,
      "Architecture G fixture canonical-funding identity",
      ["path", "sha256", "entryCount"],
    );
    canonicalAbsolutePath(
      canonicalFunding.path,
      "fixtureCreation.canonicalFunding.path",
    );
    canonicalFundingSha256 = sha256Digest(
      canonicalFunding.sha256,
      "fixtureCreation.canonicalFunding.sha256",
    );
    positiveSafeInteger(
      canonicalFunding.entryCount,
      "fixtureCreation.canonicalFunding.entryCount",
    );
    if (canonicalFundingSha256 !== fundingMapSha256) {
      throw new Error(
        "Architecture G fixture creation canonical funding SHA-256 drifted",
      );
    }
  }
  if (
    artifact.fixtureCreated !== true ||
    fixturePath !== expectedPath ||
    marker !== expectedRoot ||
    initialUtxoCount !== expectedCount ||
    aggregateEntryCount !== expectedCount ||
    aggregateEntryCount !== expectedAggregateEntryCount ||
    aggregateEncodedTupleBytes !== expectedAggregateEncodedTupleBytes
  ) {
    throw new Error(
      "Architecture G fixture creation does not bind the candidate path, root, cardinality, payload aggregate, and canonical funding",
    );
  }
  return artifact as ArchitectureGFixtureCreationV1;
};

export const decodeArchitectureGCorpusFundingV1 = ({
  value,
  expectedCorpusSha256,
  expectedSliceSha256,
  expectedFundingRoots,
}: {
  readonly value: unknown;
  readonly expectedCorpusSha256: string;
  readonly expectedSliceSha256: string;
  readonly expectedFundingRoots?: readonly {
    readonly walletId: string;
    readonly outref: string;
  }[];
}): ArchitectureGCorpusFundingV1 => {
  sha256Digest(expectedCorpusSha256, "expectedCorpusSha256");
  sha256Digest(expectedSliceSha256, "expectedSliceSha256");
  const funding = exactKeysRecord(value, "Architecture G corpus funding", [
    "schemaVersion",
    "corpusSha256",
    "sliceSha256",
    "entries",
  ]);
  if (
    funding.schemaVersion !== "midgard-architecture-g-corpus-funding-v1" ||
    funding.corpusSha256 !== expectedCorpusSha256 ||
    funding.sliceSha256 !== expectedSliceSha256 ||
    !Array.isArray(funding.entries) ||
    funding.entries.length === 0
  ) {
    throw new Error("Architecture G corpus funding identity is invalid");
  }
  const walletIds = new Set<string>();
  const outrefs = new Set<string>();
  const identities: { readonly walletId: string; readonly outref: string }[] =
    [];
  for (const [index, value] of funding.entries.entries()) {
    const entry = exactKeysRecord(
      value,
      `Architecture G funding entry ${index.toString()}`,
      ["walletId", "outref", "outputCbor"],
    );
    const walletId = boundedNonEmptyString(
      entry.walletId,
      `funding.entries[${index.toString()}].walletId`,
    );
    const outref = boundedNonEmptyString(
      entry.outref,
      `funding.entries[${index.toString()}].outref`,
    );
    const outputCbor = boundedNonEmptyString(
      entry.outputCbor,
      `funding.entries[${index.toString()}].outputCbor`,
      1_048_576,
    );
    if (
      walletIds.has(walletId) ||
      outrefs.has(outref) ||
      outref !== outref.toLowerCase() ||
      outputCbor !== outputCbor.toLowerCase() ||
      !/^[0-9a-f]{64}#(?:0|[1-9]\d*)$/u.test(outref) ||
      outputCbor.length % 2 !== 0 ||
      outputCbor.length > 1_048_576 ||
      Buffer.from(outputCbor, "hex").toString("hex") !== outputCbor
    ) {
      throw new Error(
        `Architecture G funding entry ${index.toString()} is invalid or duplicated`,
      );
    }
    walletIds.add(walletId);
    outrefs.add(outref);
    identities.push({ walletId, outref });
  }
  if (expectedFundingRoots !== undefined) {
    if (
      !Array.isArray(expectedFundingRoots) ||
      expectedFundingRoots.length === 0 ||
      !sameJson(identities, expectedFundingRoots)
    ) {
      throw new Error(
        "Architecture G corpus funding entries do not match the selected corpus roots",
      );
    }
    for (const [index, value] of expectedFundingRoots.entries()) {
      const expected = exactKeysRecord(
        value,
        `Expected Architecture G funding root ${index.toString()}`,
        ["walletId", "outref"],
      );
      boundedNonEmptyString(
        expected.walletId,
        `expectedFundingRoots[${index.toString()}].walletId`,
      );
      if (
        typeof expected.outref !== "string" ||
        !/^[0-9a-f]{64}#(?:0|[1-9]\d*)$/u.test(expected.outref)
      ) {
        throw new Error(
          `expectedFundingRoots[${index.toString()}].outref is invalid`,
        );
      }
    }
  }
  return funding as ArchitectureGCorpusFundingV1;
};

export const validateArchitectureGCommitCandidateProbeResultV1 = ({
  value,
  expectedInput,
  expectedInputPath,
  expectedInputSha256,
  expectedProbePath,
  expectedProbeSha256,
  expectedCpuAffinity,
}: {
  readonly value: unknown;
  readonly expectedInput: ArchitectureGCommitCandidateInputV1;
  readonly expectedInputPath: string;
  readonly expectedInputSha256: string;
  readonly expectedProbePath: string;
  readonly expectedProbeSha256: string;
  readonly expectedCpuAffinity: string;
}): JsonRecord => {
  const input = decodeArchitectureGCommitCandidateInputV1(expectedInput);
  const result = exactKeysRecord(
    JSON.parse(JSON.stringify(value)) as unknown,
    "Architecture G commit-candidate probe result",
    [
      "schemaVersion",
      "probePath",
      "probeSha256",
      "inputPath",
      "inputSha256",
      "expectedTransactionCount",
      "corpusSha256",
      "corpusSliceSha256",
      "fundingMapSha256",
      "fixtureCreationSha256",
      "fixtureInitialUtxoCount",
      "baseUtxoPayloadAggregate",
      "binarySha256",
      "cpuAffinity",
      "durationMs",
      "confirmedLedgerFullScans",
      "journalRowsBefore",
      "journalRowsAfter",
      "candidateConfig",
      "providerBoundaryAttempts",
      "submissionAttempts",
      "candidate",
      "ownerBefore",
      "ownerAfter",
    ],
  );
  const aggregate = exactKeysRecord(
    result.baseUtxoPayloadAggregate,
    "Commit-candidate base UTxO payload aggregate",
    ["entryCount", "encodedTupleBytes"],
  );
  const config = exactKeysRecord(
    result.candidateConfig,
    "Commit-candidate configuration evidence",
    [
      "mpfEngine",
      "scratchBuild",
      "payloadRootCheck",
      "parallelRoots",
      "costModel",
      "mempoolRetrievePageSize",
      "maxL2TxCount",
      "maxLedgerOpCount",
      "maxTransitionStepCount",
    ],
  );
  const candidate = exactKeysRecord(
    result.candidate,
    "Commit-candidate summary",
    [
      "candidateId",
      "baseHeaderHash",
      "endTimeMs",
      "builtAtMs",
      "buildDurationMs",
      "invalidationKey",
      "watermarks",
      "expectedUserEventCounts",
      "expectedL2TransactionCount",
      "roots",
    ],
  );
  const watermarks = exactKeysRecord(
    candidate.watermarks,
    "Commit-candidate barrier watermarks",
    ["depositMs", "withdrawalMs", "txOrderMs", "refreshedAtMs"],
  );
  const expectedUserEventCounts = exactKeysRecord(
    candidate.expectedUserEventCounts,
    "Commit-candidate expected user-event counts",
    ["deposits", "forcedTransactions", "withdrawals"],
  );
  const rootKeys = [
    "utxos",
    "rawTransactions",
    "transactions",
    "deposits",
    "forcedTransactions",
    "withdrawals",
    "transitionTrace",
    "eventToStep",
  ] as const;
  const roots = exactKeysRecord(
    candidate.roots,
    "Commit-candidate roots",
    rootKeys,
  );
  const ownerBefore = decodeArchitectureGOwnerDiagnostics(
    result.ownerBefore,
    "Commit-candidate owner-before diagnostics",
  );
  const ownerAfter = decodeArchitectureGOwnerDiagnostics(
    result.ownerAfter,
    "Commit-candidate owner-after diagnostics",
  );
  const inputPath = canonicalAbsolutePath(
    expectedInputPath,
    "expectedInputPath",
  );
  const probePath = canonicalAbsolutePath(
    expectedProbePath,
    "expectedProbePath",
  );
  const inputSha256 = sha256Digest(expectedInputSha256, "expectedInputSha256");
  const probeSha256 = sha256Digest(expectedProbeSha256, "expectedProbeSha256");
  const cpuAffinity = boundedNonEmptyString(
    expectedCpuAffinity,
    "expectedCpuAffinity",
  );
  const transactionCount = input.expectedTransactionCount;
  if (
    result.schemaVersion !==
      "midgard-architecture-g-commit-candidate-probe-v1" ||
    result.probePath !== probePath ||
    result.probeSha256 !== probeSha256 ||
    result.inputPath !== inputPath ||
    result.inputSha256 !== inputSha256 ||
    result.expectedTransactionCount !== transactionCount ||
    result.corpusSha256 !== input.corpusSha256 ||
    result.corpusSliceSha256 !== input.corpusSliceSha256 ||
    result.fundingMapSha256 !== input.fundingMapSha256 ||
    result.fixtureCreationSha256 !== input.fixtureCreationSha256 ||
    result.fixtureInitialUtxoCount !== input.fixtureInitialUtxoCount ||
    !sameJson(aggregate, input.baseUtxoPayloadAggregate) ||
    result.binarySha256 !== input.binarySha256 ||
    result.cpuAffinity !== cpuAffinity ||
    result.confirmedLedgerFullScans !== 0 ||
    result.journalRowsBefore !== 0 ||
    result.journalRowsAfter !== 0 ||
    result.providerBoundaryAttempts !== 0 ||
    result.submissionAttempts !== result.providerBoundaryAttempts
  ) {
    throw new Error(
      "Architecture G commit-candidate probe boundary identity is invalid",
    );
  }
  positiveFiniteNumber(result.durationMs, "candidateProbe.durationMs");
  if (
    config.mpfEngine !== "architecture_g" ||
    config.scratchBuild !== "fromlist" ||
    config.payloadRootCheck !== "off" ||
    config.parallelRoots !== true ||
    config.costModel !== "ewma" ||
    positiveSafeInteger(
      config.mempoolRetrievePageSize,
      "candidateConfig.mempoolRetrievePageSize",
    ) < transactionCount ||
    positiveSafeInteger(config.maxL2TxCount, "candidateConfig.maxL2TxCount") <
      transactionCount ||
    positiveSafeInteger(
      config.maxLedgerOpCount,
      "candidateConfig.maxLedgerOpCount",
    ) <
      transactionCount * 3 ||
    positiveSafeInteger(
      config.maxTransitionStepCount,
      "candidateConfig.maxTransitionStepCount",
    ) < transactionCount
  ) {
    throw new Error("Commit-candidate configuration evidence is invalid");
  }
  const watermarkValues = Object.entries(watermarks).map(([field, value]) =>
    nonNegativeSafeInteger(value, `candidate.watermarks.${field}`),
  );
  for (const [field, count] of Object.entries(expectedUserEventCounts)) {
    nonNegativeSafeInteger(count, `candidate.expectedUserEventCounts.${field}`);
  }
  const endTimeMs = positiveSafeInteger(
    candidate.endTimeMs,
    "candidate.endTimeMs",
  );
  positiveSafeInteger(candidate.builtAtMs, "candidate.builtAtMs");
  positiveFiniteNumber(candidate.buildDurationMs, "candidate.buildDurationMs");
  const minimumWatermarkMs = Math.min(...watermarkValues);
  if (
    typeof candidate.candidateId !== "string" ||
    !/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u.test(
      candidate.candidateId,
    ) ||
    candidate.baseHeaderHash !==
      input.workerInput.data.speculativeBuild.base.headerHash ||
    !sameJson(watermarks, input.workerInput.data.speculativeBuild.watermarks) ||
    candidate.expectedL2TransactionCount !== transactionCount ||
    candidate.invalidationKey !==
      `${candidate.baseHeaderHash as string}:${endTimeMs.toString()}:${minimumWatermarkMs.toString()}`
  ) {
    throw new Error("Commit-candidate identity or barrier evidence is invalid");
  }
  for (const field of rootKeys) {
    sha256Digest(roots[field], `candidate.roots.${field}`);
  }
  if (
    ownerBefore.durableRoot !==
      input.workerInput.data.speculativeBuild.base.utxosRoot ||
    ownerBefore.durableRoot !== ownerAfter.durableRoot ||
    !sameJson(ownerBefore.ownerEpoch, ownerAfter.ownerEpoch) ||
    ownerBefore.childRestarts !== ownerAfter.childRestarts
  ) {
    throw new Error("Commit-candidate native owner identity drifted");
  }
  return result;
};

export const validateArchitectureGRootProbeResultV1 = ({
  value,
  expectedTransactionCount,
  expectedInitialUtxoCount,
  expectedProbePath,
  expectedProbeSha256,
}: {
  readonly value: unknown;
  readonly expectedTransactionCount: number;
  readonly expectedInitialUtxoCount: number;
  readonly expectedProbePath: string;
  readonly expectedProbeSha256: string;
}): JsonRecord => {
  const result = exactKeysRecord(
    JSON.parse(JSON.stringify(value)) as unknown,
    "Architecture G root-probe result",
    [
      "engine",
      "transactionCount",
      "initialUtxoCount",
      "workloadSha256",
      "canonicalCorpusSlice",
      "canonicalFunding",
      "levelBackedInitialView",
      "reusedLevelFixture",
      "ledgerOpCount",
      "startupMs",
      "durationMs",
      "buildPlusCaptureMs",
      "phaseMs",
      "utxoRoot",
      "rawTxRoot",
      "txRoot",
      "transitionTraceRoot",
      "eventToStepRoot",
      "depositsRoot",
      "withdrawalsRoot",
      "forcedTransactionsRoot",
      "transitionRoots",
      "nativePhaseMs",
      "pathHydration",
      "confirmedLedgerFullScans",
      "binarySha256",
      "cpuAffinity",
      "ownerBefore",
      "ownerAfter",
      "probePath",
      "probeSha256",
    ],
  );
  const phaseMs = exactKeysRecord(
    result.phaseMs,
    "Architecture G root-probe phase timings",
    [
      "transactionSourceRoot",
      "transitionTraceBuild",
      "transactionMpfApply",
      "auxiliaryRoots",
    ],
  );
  const nativePhaseMs = exactKeysRecord(
    result.nativePhaseMs,
    "Architecture G root-probe native phase timings",
    [
      "validation",
      "eventLogEncode",
      "ownerApply",
      "ownerProofArena",
      "ownerMutation",
      "memberAssembly",
      "retainedRoots",
    ],
  );
  const hydrationKeys = [
    "prefetchMs",
    "uniquePaths",
    "nodesRequested",
    "hydrationHits",
    "hydrationMisses",
    "loadedNodes",
    "maxInFlight",
    "maxBatchKeys",
    "maxFrontierPaths",
    "retainedBytesEstimate",
    "chunkCount",
    "checkpointMs",
    "authenticationMs",
    "materializeMs",
    "collapseMs",
    "checkpointSerializedNodes",
    "checkpointSerializedBytes",
    "verifiedUpperNodes",
    "retainedUpperNodes",
    "collapsedNodes",
    "peakDecodedNodes",
  ] as const;
  const pathHydration = exactKeysRecord(
    result.pathHydration,
    "Architecture G root-probe path-hydration diagnostics",
    hydrationKeys,
  );
  const transactionCount = positiveSafeInteger(
    expectedTransactionCount,
    "expectedTransactionCount",
  );
  const initialUtxoCount = positiveSafeInteger(
    expectedInitialUtxoCount,
    "expectedInitialUtxoCount",
  );
  const probePath = canonicalAbsolutePath(
    expectedProbePath,
    "expectedProbePath",
  );
  const probeSha256 = sha256Digest(expectedProbeSha256, "expectedProbeSha256");
  if (
    result.engine !== "architecture_g" ||
    result.transactionCount !== transactionCount ||
    result.initialUtxoCount !== initialUtxoCount ||
    result.levelBackedInitialView !== true ||
    result.reusedLevelFixture !== true ||
    result.confirmedLedgerFullScans !== 0 ||
    result.probePath !== probePath ||
    result.probeSha256 !== probeSha256
  ) {
    throw new Error("Architecture G root-probe boundary identity is invalid");
  }
  sha256Digest(result.workloadSha256, "rootProbe.workloadSha256");
  sha256Digest(result.binarySha256, "rootProbe.binarySha256");
  boundedNonEmptyString(result.cpuAffinity, "rootProbe.cpuAffinity");
  positiveSafeInteger(result.ledgerOpCount, "rootProbe.ledgerOpCount");
  nonNegativeFiniteNumber(result.startupMs, "rootProbe.startupMs");
  const durationMs = positiveFiniteNumber(
    result.durationMs,
    "rootProbe.durationMs",
  );
  if (result.buildPlusCaptureMs !== durationMs) {
    throw new Error("Architecture G root-probe duration identity drifted");
  }
  for (const [field, timing] of Object.entries(phaseMs)) {
    nonNegativeFiniteNumber(timing, `rootProbe.phaseMs.${field}`);
  }
  for (const [field, timing] of Object.entries(nativePhaseMs)) {
    nonNegativeFiniteNumber(timing, `rootProbe.nativePhaseMs.${field}`);
  }
  const hydrationTimingFields = new Set([
    "prefetchMs",
    "checkpointMs",
    "authenticationMs",
    "materializeMs",
    "collapseMs",
  ]);
  for (const [field, metric] of Object.entries(pathHydration)) {
    if (hydrationTimingFields.has(field)) {
      nonNegativeFiniteNumber(metric, `rootProbe.pathHydration.${field}`);
    } else {
      nonNegativeSafeInteger(metric, `rootProbe.pathHydration.${field}`);
    }
  }
  for (const field of [
    "utxoRoot",
    "rawTxRoot",
    "txRoot",
    "transitionTraceRoot",
    "eventToStepRoot",
    "depositsRoot",
    "withdrawalsRoot",
    "forcedTransactionsRoot",
  ] as const) {
    sha256Digest(result[field], `rootProbe.${field}`);
  }
  if (
    result.canonicalCorpusSlice === null ||
    result.canonicalFunding === null
  ) {
    if (
      result.canonicalCorpusSlice !== null ||
      result.canonicalFunding !== null
    ) {
      throw new Error(
        "Architecture G root-probe corpus and funding identities must be present together",
      );
    }
  } else {
    const canonicalSlice = exactKeysRecord(
      result.canonicalCorpusSlice,
      "Architecture G root-probe corpus slice",
      ["path", "sha256", "rowCount"],
    );
    const canonicalFunding = exactKeysRecord(
      result.canonicalFunding,
      "Architecture G root-probe canonical funding",
      ["path", "sha256", "entryCount"],
    );
    canonicalAbsolutePath(
      canonicalSlice.path,
      "rootProbe.canonicalCorpusSlice.path",
    );
    sha256Digest(
      canonicalSlice.sha256,
      "rootProbe.canonicalCorpusSlice.sha256",
    );
    if (canonicalSlice.rowCount !== transactionCount) {
      throw new Error(
        "Architecture G root-probe corpus row count does not match the workload",
      );
    }
    canonicalAbsolutePath(
      canonicalFunding.path,
      "rootProbe.canonicalFunding.path",
    );
    sha256Digest(canonicalFunding.sha256, "rootProbe.canonicalFunding.sha256");
    positiveSafeInteger(
      canonicalFunding.entryCount,
      "rootProbe.canonicalFunding.entryCount",
    );
  }
  if (
    !Array.isArray(result.transitionRoots) ||
    result.transitionRoots.length !== transactionCount
  ) {
    throw new Error(
      "Architecture G root-probe transition-root count is invalid",
    );
  }
  const transitions = result.transitionRoots.map((value, index) => {
    const transition = exactKeysRecord(
      value,
      `Architecture G transition-root pair ${index.toString()}`,
      ["pre", "post"],
    );
    sha256Digest(transition.pre, `transitionRoots[${index.toString()}].pre`);
    sha256Digest(transition.post, `transitionRoots[${index.toString()}].post`);
    return transition;
  });
  const ownerBefore = decodeArchitectureGOwnerDiagnostics(
    result.ownerBefore,
    "Architecture G root-probe owner-before diagnostics",
  );
  const ownerAfter = decodeArchitectureGOwnerDiagnostics(
    result.ownerAfter,
    "Architecture G root-probe owner-after diagnostics",
  );
  if (
    transitions[0]?.pre !== ownerBefore.durableRoot ||
    transitions.at(-1)?.post !== result.utxoRoot ||
    ownerBefore.durableRoot !== ownerAfter.durableRoot ||
    !sameJson(ownerBefore.ownerEpoch, ownerAfter.ownerEpoch) ||
    ownerBefore.childRestarts !== ownerAfter.childRestarts
  ) {
    throw new Error("Architecture G root-probe owner/root identity drifted");
  }
  for (let index = 1; index < transitions.length; index += 1) {
    if (transitions[index]?.pre !== transitions[index - 1]?.post) {
      throw new Error(
        `Architecture G root-probe transition chain broke at ${index.toString()}`,
      );
    }
  }
  return result;
};

export const validateArchitectureGCommitCandidateSeedResultV1 = ({
  value,
  expectedDatabaseName,
  expectedCorpusSliceSha256,
  expectedTransactionCount,
}: {
  readonly value: unknown;
  readonly expectedDatabaseName: string;
  readonly expectedCorpusSliceSha256: string;
  readonly expectedTransactionCount: number;
}): ArchitectureGCommitCandidateSeedResultV1 => {
  const result = exactKeysRecord(
    value,
    "Architecture G commit-candidate seed result",
    [
      "schemaVersion",
      "databaseName",
      "corpusSliceSha256",
      "mempoolTxCount",
      "fundingCount",
      "terminalLedgerCount",
      "deltaCount",
    ],
  );
  const expectedCount = positiveSafeInteger(
    expectedTransactionCount,
    "expectedTransactionCount",
  );
  const fundingCount = positiveSafeInteger(
    result.fundingCount,
    "seedResult.fundingCount",
  );
  const terminalLedgerCount = positiveSafeInteger(
    result.terminalLedgerCount,
    "seedResult.terminalLedgerCount",
  );
  if (
    result.schemaVersion !==
      "midgard-architecture-g-commit-candidate-seed-result-v1" ||
    typeof expectedDatabaseName !== "string" ||
    !/^midgard_phase3_arch_g_[a-z0-9_]+$/u.test(expectedDatabaseName) ||
    result.databaseName !== expectedDatabaseName ||
    result.corpusSliceSha256 !==
      sha256Digest(expectedCorpusSliceSha256, "expectedCorpusSliceSha256") ||
    result.mempoolTxCount !== expectedCount ||
    result.deltaCount !== expectedCount ||
    result.fundingCount !== fundingCount ||
    result.terminalLedgerCount !== terminalLedgerCount
  ) {
    throw new Error("Architecture G commit-candidate seed result is invalid");
  }
  return result as ArchitectureGCommitCandidateSeedResultV1;
};
