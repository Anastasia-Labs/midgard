import type { WatcherDaBondPoolObservation } from "../availability/pool-observation.js";
import {
  HEX_32,
  type WatcherOperationsSink,
} from "./operations-observability.watcher-operations-metrics.js";

export const hash32 = (value: string, label: string): string => {
  if (!HEX_32.test(value)) throw new Error(`${label} is invalid`);
  return value;
};

export const percentile = (
  values: readonly bigint[],
  numerator: number,
  denominator: number,
): string | null => {
  if (values.length === 0) return null;
  const ordered = [...values].sort((left, right) =>
    left < right ? -1 : left > right ? 1 : 0,
  );
  const rank = Math.max(
    0,
    Math.ceil((ordered.length * numerator) / denominator) - 1,
  );
  return ordered[rank]!.toString();
};

/**
 * The availability runtime's `onDaBondPool` hook as the watcher runtime wires
 * it: each reconciliation's pool readout becomes the served `daBondPool` and
 * sets or clears the two pool alerts for this deployment (spec #685 E5).
 */
export const watcherDaBondPoolReporter =
  (
    sink: Pick<WatcherOperationsSink, "recordDaBondPool">,
    deploymentIdentity: string,
    nowMs: () => number = Date.now,
  ) =>
  (pool: WatcherDaBondPoolObservation): void =>
    sink.recordDaBondPool(pool, deploymentIdentity, BigInt(nowMs()).toString());

/**
 * The availability runtime's `onDaBondPoolReadFailure` hook as the watcher
 * runtime wires it: a failed pool read becomes the served
 * `daBondPoolReadFailure`.
 */
export const watcherDaBondPoolReadFailureReporter =
  (
    sink: Pick<WatcherOperationsSink, "recordDaBondPoolReadFailure">,
    nowMs: () => number = Date.now,
  ) =>
  (error: string): void =>
    sink.recordDaBondPoolReadFailure(error, BigInt(nowMs()).toString());
