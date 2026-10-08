import type {
  FraudProofL1ObservationDepth,
  FraudProofRawL1SnapshotAuthority,
} from "./raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import {
  inspectSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "./signed-transaction-reconciliation.js";

export const FRAUD_PROOF_L1_SOURCE =
  "midgard-fraud-proof-l1-source-v1" as const;

/** Signed-intent recovery against the canonical L1 chain. */
export interface FraudProofSignedTransactionRecovery {
  /**
   * Where the exact signed transaction stands on the canonical chain: included,
   * still pending, worth rebroadcasting, or dead (expired or invalidated),
   * each relative to the release-final point of the policy.
   */
  observeSignedTransaction(
    input: SignedWorkflowTransaction,
  ): Promise<SignedTransactionRecoveryObservation>;
  /** Resubmits the recorded bytes unchanged; never rebuilds or re-signs. */
  rebroadcastSignedTransaction(
    input: SignedWorkflowTransaction & {
      readonly authorizeResubmission: (
        input: SignedWorkflowTransaction,
      ) => Promise<void>;
    },
  ): Promise<string>;
}

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
  /** Signed-intent recovery under the release policy. */
  signedTransactions(
    input: Readonly<{
      releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
    }>,
  ): FraudProofSignedTransactionRecovery;
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

export const assertFraudProofL1Source = (
  value: unknown,
): FraudProofL1Source => {
  const source = value as Partial<FraudProofL1Source> | null;
  if (
    source === null ||
    typeof source !== "object" ||
    source.sourceVersion !== FRAUD_PROOF_L1_SOURCE ||
    typeof source.snapshotAuthority !== "function" ||
    typeof source.signedTransactions !== "function"
  ) {
    throw new Error("fault-proof L1 source has an unsupported version");
  }
  return source as FraudProofL1Source;
};

/** Signed recovery bound to one policy, with the signed bytes checked first. */
export const fraudProofSignedTransactionRecovery = (
  l1: FraudProofL1Source,
  releaseFinality: VerifiedFraudProofReleaseFinalityPolicy,
): FraudProofSignedTransactionRecovery => {
  const recovery = assertFraudProofL1Source(l1).signedTransactions({
    releaseFinality,
  });
  return Object.freeze({
    observeSignedTransaction: async (input: SignedWorkflowTransaction) => {
      inspectSignedWorkflowTransaction(input);
      return await recovery.observeSignedTransaction(input);
    },
    rebroadcastSignedTransaction: async (
      input: Parameters<
        FraudProofSignedTransactionRecovery["rebroadcastSignedTransaction"]
      >[0],
    ) => {
      inspectSignedWorkflowTransaction(input);
      return await recovery.rebroadcastSignedTransaction(input);
    },
  });
};

/** A family port with the source's signed recovery on it and on its publications. */
export const withFraudProofL1Recovery = <
  Port extends Readonly<{ publications: object }>,
>(
  port: Port,
  recovery: FraudProofSignedTransactionRecovery,
): Port => {
  const methods = {
    observeSignedTransaction: recovery.observeSignedTransaction,
    rebroadcastSignedTransaction: recovery.rebroadcastSignedTransaction,
  };
  return Object.freeze({
    ...port,
    ...methods,
    publications: Object.freeze({ ...port.publications, ...methods }),
  });
};
