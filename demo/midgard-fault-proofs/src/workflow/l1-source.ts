import type {
  FraudProofL1ObservationDepth,
  FraudProofRawL1SnapshotAuthority,
} from "./raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";

export const FRAUD_PROOF_L1_SOURCE =
  "midgard-fraud-proof-l1-source-v1" as const;

/**
 * The fault-proof families' one L1 source. The watcher constructs it over its
 * chain follower; the families never name a provider. Every value it returns
 * is untrusted input to the admission in this package.
 */
export interface FraudProofL1Source {
  readonly sourceVersion: typeof FRAUD_PROOF_L1_SOURCE;
  /** Raw snapshots pinned at `observationDepth` under the release policy. */
  snapshotAuthority(
    input: Readonly<{
      releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
      observationDepth: FraudProofL1ObservationDepth;
    }>,
  ): FraudProofRawL1SnapshotAuthority;
}

/**
 * The chain moved under a capture (a rollback replaced a block it read).
 * Captures retry on it a bounded number of times; nothing read before it is
 * reused.
 */
export class FraudProofL1CheckpointChangedError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "FraudProofL1CheckpointChangedError";
  }
}

/**
 * The L1 source could not answer (the follower is not ready, or its node
 * connection is down). It says nothing about the chain: callers wait and
 * retry, and never read it as an absence.
 */
export class FraudProofL1UnavailableError extends Error {
  readonly cause: unknown;
  constructor(message: string, options?: Readonly<{ cause?: unknown }>) {
    super(message);
    this.cause = options?.cause;
    this.name = "FraudProofL1UnavailableError";
  }
}

/**
 * The L1 source refused a read it cannot answer as asked: the facts it needs
 * were pruned, lie before its origin, or are not tracked. It says nothing
 * about the fault, so the workflow never stalls on it or reads it as an
 * absence; its caller holds the objective, named by `reason`, and runs it
 * again on a later observation.
 */
export class FraudProofL1RefusedError extends Error {
  constructor(
    readonly reason: string,
    readonly detail: string,
  ) {
    super(`the L1 source refused the read (${reason}): ${detail}`);
    this.name = "FraudProofL1RefusedError";
  }
}

/** An L1 failure the workflow hands to its caller to wait on, never stalls on. */
export const isFraudProofL1WaitError = (
  error: unknown,
): error is FraudProofL1UnavailableError | FraudProofL1RefusedError =>
  error instanceof FraudProofL1UnavailableError ||
  error instanceof FraudProofL1RefusedError;
