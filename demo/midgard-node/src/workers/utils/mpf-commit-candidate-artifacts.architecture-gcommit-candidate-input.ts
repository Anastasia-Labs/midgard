import {
  exactKeysRecord,
  nonNegativeSafeInteger,
  sha256Digest,
} from "../../artifact-schema.js";

export type JsonRecord = Record<string, unknown>;

export type ArchitectureGPhase1FormalBindingIdentity = {
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

export type ArchitectureGRuntimeIdentity = {
  readonly schemaVersion: "midgard-architecture-g-runtime-identity-v1";
  readonly version: string;
  readonly execPath: string;
  readonly executableSha256: string;
};

export type ArchitectureGCommitCandidateSeedInput = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-seed-v1";
  readonly phase1FormalBinding: ArchitectureGPhase1FormalBindingIdentity;
  readonly runtimeIdentity: ArchitectureGRuntimeIdentity;
  readonly corpusSlicePath: string;
  readonly corpusSliceSha256: string;
  readonly fundingMapPath: string;
  readonly fundingMapSha256: string;
  readonly expectedTransactionCount: number;
  readonly fixtureInitialUtxoCount: number;
  readonly firstTimestampIso: string;
};

export type ArchitectureGCorpusFunding = {
  readonly schemaVersion: "midgard-architecture-g-corpus-funding-v1";
  readonly corpusSha256: string;
  readonly sliceSha256: string;
  readonly entries: readonly {
    readonly walletId: string;
    readonly outref: string;
    readonly outputCbor: string;
  }[];
};

export type ArchitectureGCommitCandidateSeedResult = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-seed-result-v1";
  readonly databaseName: string;
  readonly corpusSliceSha256: string;
  readonly mempoolTxCount: number;
  readonly fundingCount: number;
  readonly terminalLedgerCount: number;
  readonly deltaCount: number;
  readonly confirmedLedgerCount: number;
};

export type ArchitectureGCommitCandidateInput = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-input-v1";
  readonly phase1FormalBinding: ArchitectureGPhase1FormalBindingIdentity;
  readonly runtimeIdentity: ArchitectureGRuntimeIdentity;
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
  /** The Level fixture's root marker: the base every candidate builds on. */
  readonly baseUtxosRoot: string;
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
    };
  };
};

export type ArchitectureGFixtureCreation = {
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

export const architectureGFixtureDiagnosticKeys = [
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

export const sameJson = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

export const decodeArchitectureGOwnerDiagnostics = (
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
