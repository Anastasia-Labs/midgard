import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
} from "@al-ft/midgard-fault-proofs";
import {
  fromTransportError,
  L1ProviderTransientError,
} from "@al-ft/midgard-l1-follower/provider";

/** Socket and resolver codes Node and undici give a request that never completed. */
const NETWORK_FAILURE_CODES: ReadonlySet<string> = new Set([
  "ECONNREFUSED",
  "ECONNRESET",
  "EPIPE",
  "ETIMEDOUT",
  "ENOTFOUND",
  "EAI_AGAIN",
  "UND_ERR_SOCKET",
  "UND_ERR_CONNECT_TIMEOUT",
  "UND_ERR_HEADERS_TIMEOUT",
  "UND_ERR_BODY_TIMEOUT",
]);

/** How far down a `cause` chain a wrapped transient is still recognised. */
const MAXIMUM_CAUSE_DEPTH = 8;

const transientCode = (error: Error): boolean => {
  const code = (error as { readonly code?: unknown }).code;
  return typeof code === "string" && NETWORK_FAILURE_CODES.has(code);
};

/**
 * Whether a failed L1 read says only that the follower or its node transport
 * did not answer, or that
 * the chain moved while a snapshot was read: nothing about the chain or the
 * deployment. Such a read can be repeated as is. Every other error, including
 * one that merely carries a transient error's name, is not one. A raw node
 * transport failure is classified as the follower's provider classifies a
 * query's (`fromTransportError`): an outage, a timeout, a sidecar exit and
 * the refusals that clear on their own are transient.
 */
export const isWatcherL1TransientFailure = (error: unknown): error is Error => {
  let current: unknown = error;
  for (let depth = 0; depth < MAXIMUM_CAUSE_DEPTH; depth += 1) {
    if (!(current instanceof Error)) return false;
    if (
      current instanceof FraudProofL1UnavailableError ||
      current instanceof FraudProofL1CheckpointChangedError ||
      current instanceof L1ProviderTransientError ||
      fromTransportError(current) instanceof L1ProviderTransientError ||
      transientCode(current)
    )
      return true;
    current = current.cause;
  }
  return false;
};
