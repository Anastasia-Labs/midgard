import type { WatcherAvailabilityStatusTransition } from "../availability/runtime.js";
import { WatcherL1UnavailableError } from "../l1/transient-retry.js";
import { startWatcherOperationsHttpServer } from "./operations-http.js";
import { hasSystemErrorCode } from "./permanent-refusal.js";
import type { WatcherProcessConfig } from "./process-config.js";
import {
  createWatcherStartupOperations,
  WatcherStartupHeldError,
} from "./startup-operations.js";
import {
  createWatcherStartupProgress,
  type WatcherStartupProgress,
} from "./startup-progress.js";
import { startWatcherRuntime } from "./watcher-runtime.create-watcher-runtime.js";
import type { WatcherRuntime } from "./watcher-runtime.launch-checks.js";

export { WatcherStartupHeldError };

/**
 * Whether a startup failure exits rather than holds (owner ruling
 * 2026-10-09): only one a restart may clear. That is L1 transients that
 * outlasted the startup budget (`WatcherL1UnavailableError`) and a system
 * error (a file not written yet, a port in use), each anywhere on the cause
 * chain. Anything else, deterministic or unknown (a configuration or
 * deployment-identity refusal, a malformed read), holds.
 */
export const watcherStartupFailureExits = (error: unknown): boolean => {
  if (hasSystemErrorCode(error)) return true;
  const seen = new Set<unknown>();
  const pending = [error];
  while (pending.length > 0 && seen.size < 16) {
    const current = pending.shift();
    if (!(current instanceof Error) || seen.has(current)) continue;
    seen.add(current);
    if (current instanceof WatcherL1UnavailableError) return true;
    if (current instanceof AggregateError) pending.push(...current.errors);
    pending.push(current.cause);
  }
  return false;
};

/**
 * Compose production startup (`startWatcherRuntime`) behind the operations
 * server, bound first, as soon as the process configuration is known:
 * every refusal and wait after it is named on `/readyz`
 * (`startup:<stage>`, then `startup_failed`). A startup failure that a
 * restart may clear (`watcherStartupFailureExits`) closes the server and
 * rethrows, so the process exits non-zero; any other throws
 * `WatcherStartupHeldError` with the server still serving.
 */
export const createWatcherRuntime = async (input: {
  readonly config: WatcherProcessConfig;
  readonly onStartupProgress?: (progress: WatcherStartupProgress) => void;
  readonly onAvailabilityStatusTransition?: (
    event: WatcherAvailabilityStatusTransition,
  ) => void;
}): Promise<WatcherRuntime> => {
  const startupOperations = createWatcherStartupOperations();
  const startup = createWatcherStartupProgress((progress) => {
    startupOperations.report(progress);
    input.onStartupProgress?.(progress);
  });
  const operationsHttp = await startWatcherOperationsHttpServer({
    endpoint: input.config.operationsEndpoint,
    observability: startupOperations,
  });
  try {
    return await startWatcherRuntime(input, {
      startup,
      startupOperations,
      operationsHttp,
    });
  } catch (error) {
    if (watcherStartupFailureExits(error)) {
      await operationsHttp.close().catch(() => undefined);
      throw error;
    }
    startupOperations.fail(error);
    throw new WatcherStartupHeldError(error, () => operationsHttp.close());
  }
};
