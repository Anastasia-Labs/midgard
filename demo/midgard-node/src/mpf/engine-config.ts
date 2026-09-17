/**
 * Process-wide MPF engine configuration: engine selection, scratch-build mode,
 * path-hydration mode, arena limits, and the diagnostics they report.
 */

import { Effect } from "effect";

import { positiveSafeInteger } from "../artifact-schema.js";
import { type NodeConfigDep } from "../services/index.js";
import {
  configureMpfRootWorkers,
  prewarmMpfRootWorkers,
} from "../workers/utils/mpf-root-pool.js";
import { MpfError } from "./errors.js";

export type MpfEngine = "legacy" | "overlay" | "event_flat";

export type MpfScratchBuild = "insert" | "fromlist";

export type MpfPathHydrationMode = "whole_block" | "chunked" | "chunked_arena";

export type MpfArenaLimits = {
  readonly pathCacheMaxNodes: number;
  readonly pathCacheMaxBytes: number;
  readonly liveArenaMaxNodes: number;
  readonly liveArenaMaxBytes: number;
};

export type ParkedMpfOverlay = {
  readonly schemaVersion: 1;
  readonly trieName: string;
  readonly baseRoot: ArrayBuffer;
  readonly candidateRoot: ArrayBuffer;
  readonly closureDigest: ArrayBuffer;
  readonly nodeCount: number;
  readonly nodeHashes: ArrayBuffer;
  readonly nodeValues: ArrayBuffer;
  /** Uint32 pairs of JSON byte offset/length, one pair per node hash. */
  readonly nodeValueOffsets: ArrayBuffer;
  readonly encodedBytes: number;
};

export const DEFAULT_MPF_ARENA_LIMITS: MpfArenaLimits = {
  pathCacheMaxNodes: 1_000_000,
  pathCacheMaxBytes: 1024 * 1024 * 1024,
  liveArenaMaxNodes: 1_000_000,
  liveArenaMaxBytes: 1024 * 1024 * 1024,
};

export type MpfPathHydrationConfig = {
  readonly mode: MpfPathHydrationMode;
  readonly chunkOps: number;
  readonly retainDepth: number;
};

let configuredMpfScratchBuild: MpfScratchBuild = "insert";

let configuredMpfPathHydration: MpfPathHydrationConfig = {
  mode: "whole_block",
  chunkOps: 512,
  retainDepth: 2,
};

let configuredMpfArenaLimits: MpfArenaLimits = {
  ...DEFAULT_MPF_ARENA_LIMITS,
};

export const setMpfScratchBuild = (mode: MpfScratchBuild): void => {
  configuredMpfScratchBuild = mode;
};

export const getMpfScratchBuild = (): MpfScratchBuild =>
  configuredMpfScratchBuild;

export const configureMpfPathHydration = ({
  mode,
  chunkOps,
  retainDepth,
}: MpfPathHydrationConfig): void => {
  if (!Number.isSafeInteger(chunkOps) || chunkOps <= 0) {
    throw new Error("MPF hydration chunk ops must be a positive safe integer");
  }
  if (
    !Number.isSafeInteger(retainDepth) ||
    retainDepth < 0 ||
    retainDepth > 8
  ) {
    throw new Error("MPF retained hydration depth must be between 0 and 8");
  }
  configuredMpfPathHydration = { mode, chunkOps, retainDepth };
};

export const getMpfPathHydrationConfig = (): MpfPathHydrationConfig => ({
  ...configuredMpfPathHydration,
});

export const configureMpfArenaLimits = (limits: MpfArenaLimits): void => {
  for (const [name, value] of Object.entries(limits)) {
    positiveSafeInteger(value, name);
  }
  configuredMpfArenaLimits = { ...limits };
};

export const resetMpfArenaLimits = (): void => {
  configuredMpfArenaLimits = { ...DEFAULT_MPF_ARENA_LIMITS };
};

export const getMpfArenaLimits = (): MpfArenaLimits => ({
  ...configuredMpfArenaLimits,
});

export type MpfStoreDiagnostics = {
  readonly entries: number;
  readonly storePuts: number;
  readonly storeDels: number;
  readonly serialiseCalls: number;
  readonly serialiseMs: number;
  readonly deferredMaterializedEstimatedBytes: number;
  readonly deferredMaterializedActualBytes: number;
  readonly deferredLazyReads: number;
  readonly deferredLazySerialiseMs: number;
  readonly deferredLazySerialisedBytes: number;
  readonly arenaCheckpointCalls: number;
  readonly arenaCheckpointMs: number;
  readonly arenaCheckpointNodes: number;
  readonly arenaCheckpointBytes: number;
  readonly pathCacheEntries: number;
  readonly pathCacheBytes: number;
  readonly pathCacheHits: number;
  readonly liveArenaPrunedNodes: number;
  readonly liveArenaPromotedNodes: number;
  readonly liveArenaPromotedBytes: number;
  readonly retainedSnapshotAuthentications: number;
  readonly retainedSnapshotAuthenticationMs: number;
  readonly transientLiveNodes: number;
  readonly transientLiveBytes: number;
  readonly transientDirtyNodes: number;
  readonly transientSnapshotsCaptured: number;
  readonly eventAtomicFinalizations: number;
  readonly eventAtomicDirtyNodes: number;
  readonly eventAtomicMaxDirtyNodes: number;
  readonly levelGets: number;
  readonly levelGetManyCalls: number;
  readonly levelGetManyMaxKeys: number;
  readonly levelGetMs: number;
  readonly jsonCodecMs: number;
  readonly overlayHits: number;
  readonly readCacheHits: number;
  readonly levelBatchWrites: number;
  readonly bytesFlushed: number;
  readonly overlayEntries: number;
  readonly overlayBytes: number;
  readonly overlaySpills: number;
  readonly overlaySpillMs: number;
  readonly flushMs: number;
};

export type MpfPathHydrationDiagnostics = {
  readonly prefetchMs: number;
  readonly uniquePaths: number;
  readonly nodesRequested: number;
  readonly hydrationHits: number;
  readonly hydrationMisses: number;
  readonly loadedNodes: number;
  readonly maxInFlight: number;
  readonly maxBatchKeys: number;
  readonly maxFrontierPaths: number;
  readonly retainedBytesEstimate: number;
  readonly chunkCount: number;
  readonly checkpointMs: number;
  readonly authenticationMs: number;
  readonly materializeMs: number;
  readonly collapseMs: number;
  readonly checkpointSerializedNodes: number;
  readonly checkpointSerializedBytes: number;
  readonly verifiedUpperNodes: number;
  readonly retainedUpperNodes: number;
  readonly collapsedNodes: number;
  readonly peakDecodedNodes: number;
};

export type MpfArenaCheckpointDiagnostics = {
  readonly checkpointMs: number;
  readonly authenticationMs: number;
  readonly materializeMs: number;
  readonly collapseMs: number;
  readonly serializedNodes: number;
  readonly serializedBytes: number;
  readonly verifiedUpperNodes: number;
  readonly retainedUpperNodes: number;
  readonly collapsedNodes: number;
};

export const configureCommitMpfRuntime = (
  nodeConfig: Pick<
    NodeConfigDep,
    | "MPF_SCRATCH_BUILD"
    | "MPF_PATH_HYDRATION_MODE"
    | "MPF_HYDRATION_CHUNK_OPS"
    | "MPF_RETAIN_HYDRATED_DEPTH"
    | "MPF_PARALLEL_ROOTS"
    | "MPF_ROOT_WORKERS"
    | "MPF_PARALLEL_ROOT_MIN_ENTRIES"
  >,
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    setMpfScratchBuild(nodeConfig.MPF_SCRATCH_BUILD);
    configureMpfPathHydration({
      mode: nodeConfig.MPF_PATH_HYDRATION_MODE,
      chunkOps: nodeConfig.MPF_HYDRATION_CHUNK_OPS,
      retainDepth: nodeConfig.MPF_RETAIN_HYDRATED_DEPTH,
    });
    configureMpfRootWorkers({
      enabled: nodeConfig.MPF_PARALLEL_ROOTS,
      workers: nodeConfig.MPF_ROOT_WORKERS,
      minEntries: nodeConfig.MPF_PARALLEL_ROOT_MIN_ENTRIES,
    });
    if (nodeConfig.MPF_PARALLEL_ROOTS) {
      yield* Effect.tryPromise({
        try: () => prewarmMpfRootWorkers(),
        catch: (cause) => MpfError.rootBuild("MPF root worker warmup", cause),
      });
    }
  });
