import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Runtime } from "effect";
import type { publishWorkflowDeploymentOnChain } from "midgard-node/tests/helpers/published-workflow-deployment";

import {
  type PublishedDaTargetCorrection,
  type PublishedDaTransactionRecord,
} from "./published-da-target-consumption.js";

export type PublishedWatcherDeployment = Awaited<
  ReturnType<typeof publishWorkflowDeploymentOnChain>
>;

export type PublishedWatcherBlock = {
  header: SDK.Header;
  headerHash: string;
  payloadEnvelopeCbor: Buffer;
};

/**
 * How a DA attestation finished: the apply receipt is authenticated, or the
 * target header's own fraud correction consumed it first and that correction
 * is authenticated instead. A target that vanished for any other reason throws.
 */
export type PublishedDaAttestationOutcome =
  | {
      kind: "attested";
      /** The transaction whose state queue output carries the attested header. */
      txHash: string;
    }
  | PublishedDaTargetCorrection;

export type PublishedDaAttestOptions = {
  /** DA transactions an earlier attempt already submitted for this header. */
  submitted?: readonly PublishedDaTransactionRecord[];
  /** Settles the exact retained bytes before any replacement DA construction. */
  reconcileSubmitted?(
    record: PublishedDaTransactionRecord,
  ): Promise<{ kind: "included" | "retired" }>;
  /** Called with each DA transaction after signing and before submission. */
  onSubmitted?(record: PublishedDaTransactionRecord): Promise<void>;
};

/**
 * The expected output was not observed before the local validity wait elapsed.
 * This is a reconciliation signal, not proof of canonical non-inclusion: the
 * indexer can lag, or another transaction can already have consumed the output.
 */
export class PublishedTransactionExpiredError extends Error {
  constructor(
    readonly label: string,
    readonly txHash: string,
    readonly expiryMs: number,
  ) {
    super(
      `${label} ${txHash} output was not observed after validity bound ${expiryMs}`,
    );
    this.name = "PublishedTransactionExpiredError";
  }
}

/** Submission failed after signing; only canonical recovery can settle the attempt. */
export class PublishedTransactionSubmissionError extends Error {
  constructor(
    readonly txHash: string,
    cause: unknown,
  ) {
    super(`Header submission ${txHash} is unresolved`, { cause });
    this.name = "PublishedTransactionSubmissionError";
  }
}

/**
 * Whether a submission failed without the node's answer: the provider's
 * request timed out or its transport failed, so the transaction may already
 * be in the mempool. A ledger or script refusal is an answer (an Ogmios
 * JSON-RPC error) and never counts.
 */
export const isUnansweredSubmission = (error: unknown): boolean => {
  const seen = new Set<unknown>();
  const visit = (value: unknown): boolean => {
    if (typeof value !== "object" || value === null || seen.has(value))
      return false;
    seen.add(value);
    const link = value as { _tag?: unknown; kind?: unknown; cause?: unknown };
    if (link._tag === "KupmiosError")
      return link.kind === "transport" || link.kind === "timeout";
    if (link._tag === "OgmiosJsonRpcError") return false;
    if (Runtime.isFiberFailure(value)) {
      const cause = value[Runtime.FiberFailureCauseId];
      return [...Cause.failures(cause), ...Cause.defects(cause)].some(visit);
    }
    return visit(link.cause);
  };
  return visit(error);
};

/** Grace after a validity upper bound for the indexer to publish the last eligible block. */
export const EXPIRY_GRACE_MS = 60_000;

/**
 * A backdated validity lower bound (sixty seconds before the wall clock)
 * holds only while the ledger tip is younger than that; wait for this fresh
 * a tip before building one.
 */
export const FRESH_TIP_MS = 30_000;

/** Bound on an unbounded-validity DA transaction reaching the chain. */
export const DA_SUBMISSION_TIMEOUT_MS = 600_000;

/** Bound on the indexer publishing the spend of a target it already reports absent. */
export const CONSUMPTION_INDEX_GRACE_MS = 120_000;

/**
 * How long a missing target may precede our own transaction's outputs before
 * it counts as consumed by someone else. A block-indexed provider flips both
 * at once; the emulator marks spent inputs at submission and publishes the
 * outputs at its next block, at most twenty slots later.
 */
export const OWN_SPEND_VISIBILITY_GRACE_MS = 30_000;
