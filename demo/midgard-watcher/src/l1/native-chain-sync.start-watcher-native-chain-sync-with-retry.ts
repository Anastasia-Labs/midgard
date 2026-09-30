import { type WatcherConfig } from "../runtime/config.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  type ReadIdentityFile,
  type SpawnProcess,
} from "./native-chain-sync.derive-watcher-native-genesis-identity.js";
import {
  MAX_INTERSECTIONS,
  NativeChainSyncStartupFailure,
  parsePoint,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncPoint,
  type WatcherNativeChainSyncRuntime,
} from "./native-chain-sync.exact-record.js";
import { startWatcherNativeChainSync } from "./native-chain-sync.open-watcher-native-exact-point-query.js";

export const startWatcherNativeChainSyncWithRetry = async (input: {
  readonly binaryPath: string;
  readonly watcherConfig: WatcherConfig;
  readonly intersectionCandidates: readonly WatcherNativeChainSyncPoint[];
  readonly startupTimeoutMs: number;
  readonly onEvent: (event: WatcherNativeChainSyncEvent) => Promise<void>;
  readonly unsafeSpawnForTest?: SpawnProcess;
  readonly unsafeReadIdentityFileForTest?: ReadIdentityFile;
}): Promise<WatcherNativeChainSyncRuntime> => {
  if (
    input.intersectionCandidates.length === 0 ||
    input.intersectionCandidates.length > MAX_INTERSECTIONS
  ) {
    throw new Error(
      "native chain-sync intersection candidate bounds are invalid",
    );
  }
  const seen = new Set<string>();
  const candidates = input.intersectionCandidates.map((candidate, index) => {
    const parsed = parsePoint(candidate, "native intersection candidate");
    const key = watcherCanonicalJson(parsed);
    if (seen.has(key))
      throw new Error("native intersection candidate is duplicated");
    if (
      parsed.kind === "origin" &&
      index !== input.intersectionCandidates.length - 1
    ) {
      throw new Error("native Origin candidate must be the final fallback");
    }
    seen.add(key);
    return parsed;
  });
  let lastIntersectionFailure: NativeChainSyncStartupFailure | undefined;
  for (const intersection of candidates) {
    try {
      return await startWatcherNativeChainSync({
        binaryPath: input.binaryPath,
        watcherConfig: input.watcherConfig,
        intersection,
        startupTimeoutMs: input.startupTimeoutMs,
        onEvent: input.onEvent,
        ...(input.unsafeSpawnForTest === undefined
          ? {}
          : { unsafeSpawnForTest: input.unsafeSpawnForTest }),
        ...(input.unsafeReadIdentityFileForTest === undefined
          ? {}
          : {
              unsafeReadIdentityFileForTest:
                input.unsafeReadIdentityFileForTest,
            }),
      });
    } catch (error) {
      if (
        !(error instanceof NativeChainSyncStartupFailure) ||
        error.code !== "intersection_failed"
      ) {
        throw error;
      }
      lastIntersectionFailure = error;
    }
  }
  throw (
    lastIntersectionFailure ??
    new Error("native chain-sync did not admit an intersection")
  );
};
