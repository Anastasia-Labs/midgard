import { type WatcherConfig } from "../runtime/config.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
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
import { isWatcherNativeNodeUnavailable } from "./transient-failure.js";
import { retryWatcherL1Transient } from "./transient-retry.js";

export type WatcherNativeNodeWait = Readonly<{
  event: "native_node_unavailable";
  code: string;
  retryAfterMs: number;
}>;

const writeNodeWait = (warning: WatcherNativeNodeWait): void => {
  process.stderr.write(
    `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "warn", ...warning })}\n`,
  );
};

/**
 * Bound on one native process start: spawn, node handshake, intersection and
 * tip. It is a process-start bound, not a per-request one, so a node that is
 * slow to answer after its own restart is not cut off at the request timeout.
 * A start that still times out is retried like any node-unavailable start.
 */
export const watcherNativeChainSyncStartupTimeoutMs = (
  watcherConfig: WatcherConfig,
): number => Math.max(120_000, watcherConfig.l1.requestTimeoutMs);

export const startWatcherNativeChainSyncWithRetry = async (input: {
  readonly binaryPath: string;
  readonly signal?: AbortSignal;
  readonly watcherConfig: WatcherConfig;
  readonly intersectionCandidates: readonly WatcherNativeChainSyncPoint[];
  readonly startupTimeoutMs: number;
  readonly onEvent: (event: WatcherNativeChainSyncEvent) => Promise<void>;
  readonly onAuthorityRevoked?: () => void;
  readonly unsafeSpawnForTest?: SpawnProcess;
  readonly unsafeReadIdentityFileForTest?: ReadIdentityFile;
  /** Defaults to one JSON line on stderr when the node first does not answer. */
  readonly warn?: (warning: WatcherNativeNodeWait) => void;
  readonly retryDelayMs?: (retry: number) => number;
}): Promise<WatcherNativeChainSyncRuntime> => {
  const signal = input.signal;
  signal?.throwIfAborted();
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
  // An unanswering node restarts the whole walk, newest candidate first, after
  // a capped backoff; it never ends startup.
  let waiting: { readonly error: unknown } | undefined;
  try {
    return await retryWatcherL1Transient(
      () => {
        waiting = undefined;
        signal?.throwIfAborted();
        return walk(input, candidates, signal);
      },
      {
        signal,
        transient: isWatcherNativeNodeUnavailable,
        onRetry: (error, retry, retryAfterMs) => {
          if (retry === 1)
            (input.warn ?? writeNodeWait)({
              event: "native_node_unavailable",
              code: (error as NativeChainSyncStartupFailure).code,
              retryAfterMs,
            });
          // The generic retry preserves its prior transient on an aborted wait.
          // Bind only that wait, after its warning callback actually succeeded.
          waiting = { error };
        },
        ...(input.retryDelayMs === undefined
          ? {}
          : { delayMs: input.retryDelayMs }),
      },
    );
  } catch (error) {
    if (waiting?.error === error && signal?.aborted === true)
      signal.throwIfAborted();
    throw error;
  }
};

const walk = async (
  input: Parameters<typeof startWatcherNativeChainSyncWithRetry>[0],
  candidates: readonly WatcherNativeChainSyncPoint[],
  signal?: AbortSignal,
): Promise<WatcherNativeChainSyncRuntime> => {
  let lastIntersectionFailure: NativeChainSyncStartupFailure | undefined;
  for (const intersection of candidates) {
    signal?.throwIfAborted();
    try {
      const runtime = await startWatcherNativeChainSync({
        binaryPath: input.binaryPath,
        signal,
        watcherConfig: input.watcherConfig,
        intersection,
        startupTimeoutMs: input.startupTimeoutMs,
        onEvent: input.onEvent,
        onAuthorityRevoked: input.onAuthorityRevoked,
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
      if (signal?.aborted === true) {
        await runtime.close();
        signal.throwIfAborted();
      }
      return runtime;
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
