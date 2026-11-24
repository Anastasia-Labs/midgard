import { createHash } from "node:crypto";

import type { WatcherDaBondPoolObservation } from "../availability/pool-observation.js";
import {
  HEX_32,
  type WatcherOperationsSink,
} from "./operations-observability.watcher-operations-metrics.js";

export const hash32 = (value: string, label: string): string => {
  if (!HEX_32.test(value)) throw new Error(`${label} is invalid`);
  return value;
};

const POLICY_ID = /^[0-9a-f]{56}$/u;

export const policyId = (value: string, label: string): string => {
  if (!POLICY_ID.test(value)) throw new Error(`${label} is invalid`);
  return value;
};

/** The DA bond pool alerts' subject: one per deployment. */
export const watcherDaBondPoolAlertSubject = (
  hubOraclePolicyId: string,
): string =>
  createHash("sha256")
    .update("midgard-watcher-da-bond-pool-alert-v1\u0000")
    .update(hubOraclePolicyId)
    .digest("hex");

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
 * sets or clears the two pool alerts for this deployment (spec #685).
 */
export const watcherDaBondPoolReporter =
  (
    sink: Pick<WatcherOperationsSink, "recordDaBondPool">,
    hubOraclePolicyId: string,
    nowMs: () => number = Date.now,
  ) =>
  (pool: WatcherDaBondPoolObservation): void =>
    sink.recordDaBondPool(
      pool,
      watcherDaBondPoolAlertSubject(hubOraclePolicyId),
      BigInt(nowMs()).toString(),
    );

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
