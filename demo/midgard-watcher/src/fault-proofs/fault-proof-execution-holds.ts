import {
  FraudProofL1RefusedError,
  isValidationTraceReplayHeldError,
  type ValidationTraceReplayHeldError,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";

/** How far down a `cause` chain a wrapped error is still recognised. */
const MAXIMUM_CAUSE_DEPTH = 8;

const causeOf = <T>(
  error: unknown,
  matches: (value: unknown) => value is T,
): T | undefined => {
  let current = error;
  for (let depth = 0; depth <= MAXIMUM_CAUSE_DEPTH; depth++) {
    if (matches(current)) return current;
    if (!(current instanceof Error)) return undefined;
    current = current.cause;
  }
  return undefined;
};

/** The readiness reason of an objective held on a refused L1 read is
 * `<this>:<refusal reason>`. */
export const WATCHER_FAULT_PROOF_L1_REFUSED = "fault_proof_l1_refused";

/** The refused L1 read `error` is or wraps, if any. */
export const l1RefusalOf = (
  error: unknown,
): FraudProofL1RefusedError | undefined =>
  causeOf(
    error,
    (value): value is FraudProofL1RefusedError =>
      value instanceof FraudProofL1RefusedError,
  );

/**
 * Holds an objective whose run met a held script re-execution: not a verdict
 * and not a process failure, so the objective is held under the hold's
 * readiness reason and retried once the hold's retry time has come. Any
 * other error is left to the caller.
 */
export const holdWatcherFaultProofOnReplay = (
  holds: Map<string, Readonly<{ reason: string; detail: string }>>,
  job: WatcherProofObjective,
  error: unknown,
  nowMs: number = Date.now(),
):
  | Readonly<{
      kind: "retryable";
      resume: "backoff";
      reason: string;
      retryAfterMs: number;
    }>
  | undefined => {
  const replay = causeOf<ValidationTraceReplayHeldError>(
    error,
    isValidationTraceReplayHeldError,
  );
  if (replay === undefined) return undefined;
  const objective = `${job.category}/${job.headerHash}`;
  holds.set(objective, {
    reason: replay.readiness,
    detail: `${objective}: ${replay.message}`,
  });
  return {
    kind: "retryable",
    resume: "backoff",
    reason: replay.message,
    retryAfterMs: Math.max(1_000, replay.retryAtMs - nowMs),
  };
};
