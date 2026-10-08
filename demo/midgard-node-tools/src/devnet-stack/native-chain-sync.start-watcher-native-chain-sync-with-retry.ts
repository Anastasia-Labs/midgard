import {
  isWatcherNativeNodeUnavailable,
  retryWatcherL1Transient,
  WATCHER_PACKAGE_NAME,
  type WatcherConfig,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncPoint,
  type watcherNativeChainSyncRecord,
  type WatcherNativeChainSyncRuntime,
  type watcherNativeGenesisIdentity,
} from "midgard-watcher";

import {
  parseIntersectionCandidates,
  startNativeSupervisor,
} from "./native-chain-sync.start-native-supervisor.js";

type NativeChainSyncStartupFailure =
  watcherNativeChainSyncRecord.NativeChainSyncStartupFailure;
type ReadIdentityFile = watcherNativeGenesisIdentity.ReadIdentityFile;

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
 * Bound on one native read start: a ready node transport, the intersection
 * and the tip. It is a start bound, not a per-request one, so a node that is
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
  readonly unsafeReadIdentityFileForTest?: ReadIdentityFile;
  /** Defaults to one JSON line on stderr when the node first does not answer. */
  readonly warn?: (warning: WatcherNativeNodeWait) => void;
  readonly retryDelayMs?: (retry: number) => number;
}): Promise<WatcherNativeChainSyncRuntime> => {
  const signal = input.signal;
  signal?.throwIfAborted();
  const candidates = parseIntersectionCandidates(input.intersectionCandidates);
  // One intersection over every candidate: the node takes the first one it
  // has, newest first. An unanswering node restarts the start after a capped
  // backoff; it never ends startup.
  let waiting: { readonly error: unknown } | undefined;
  let runtime: WatcherNativeChainSyncRuntime;
  try {
    runtime = await retryWatcherL1Transient(
      () => {
        waiting = undefined;
        signal?.throwIfAborted();
        return startNativeSupervisor({
          binaryPath: input.binaryPath,
          ...(signal === undefined ? {} : { signal }),
          watcherConfig: input.watcherConfig,
          intersections: candidates,
          startupTimeoutMs: input.startupTimeoutMs,
          onEvent: input.onEvent,
          ...(input.onAuthorityRevoked === undefined
            ? {}
            : { onAuthorityRevoked: input.onAuthorityRevoked }),
          operation: Object.freeze({ kind: "stream" }),
          ...(input.unsafeReadIdentityFileForTest === undefined
            ? {}
            : {
                unsafeReadIdentityFileForTest:
                  input.unsafeReadIdentityFileForTest,
              }),
        });
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
    if (
      waiting !== undefined &&
      waiting.error === error &&
      signal?.aborted === true
    )
      signal.throwIfAborted();
    throw error;
  }
  if (signal?.aborted === true) {
    await runtime.close();
    signal.throwIfAborted();
  }
  return runtime;
};
