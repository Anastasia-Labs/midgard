import {
  TransportTimeoutError,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";
import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
  LocalKupmiosCheckpointChangedError,
  LocalKupmiosTransportUnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";

import { NativeChainSyncStartupFailure } from "./native-chain-sync.exact-record.js";

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

/**
 * Startup failures that say only that the node did not answer: the node
 * transport was not ready (its sidecar was starting or restarting, or the
 * node's socket did not accept, did not complete the handshake or dropped the
 * connection), an auxiliary node connection could not be opened, or the read
 * did not start in time. A node that is restarting or still opening its
 * database gives exactly these. A rejected intersection, an invalid startup
 * and every identity mismatch are not among them.
 */
const NODE_UNAVAILABLE_CODES: ReadonlySet<string> = new Set([
  "sidecar_starting",
  "sidecar_restarting",
  "sidecar_unavailable",
  "sidecar_exited",
  "node_unreachable",
  "node_handshake_failed",
  "node_connection_lost",
  "node_unresponsive",
  "node_unavailable",
  "startup_timed_out",
]);

export const isWatcherNativeNodeUnavailable = (
  error: unknown,
): error is NativeChainSyncStartupFailure =>
  error instanceof NativeChainSyncStartupFailure &&
  NODE_UNAVAILABLE_CODES.has(error.code);

/** How far down a `cause` chain a wrapped transient is still recognised. */
const MAXIMUM_CAUSE_DEPTH = 8;

/**
 * A Lucid Kupmios provider error that the provider itself marks retryable
 * (timeout, transport loss, HTTP 408/425/429/5xx). Matched by shape: the
 * workspace holds more than one copy of the provider, so `instanceof` cannot
 * cross them. A non-retryable one (a decode failure, any other status) is not
 * a transient.
 */
const retryableProviderError = (error: Error): boolean =>
  (error.name === "KupmiosError" || error.name === "OgmiosJsonRpcError") &&
  (error as { readonly _tag?: unknown })._tag === error.name &&
  (error as { readonly provider?: unknown }).provider === "Kupmios" &&
  (error as { readonly retryable?: unknown }).retryable === true;

const transientCode = (error: Error): boolean => {
  const code = (error as { readonly code?: unknown }).code;
  return typeof code === "string" && NETWORK_FAILURE_CODES.has(code);
};

/**
 * Whether a failed L1 read says only that the follower, its node transport
 * (or, for the user-event history, Kupo or Ogmios) did not answer, or that
 * the chain moved while a snapshot was read: nothing about the chain or the
 * deployment. Such a read can be repeated as is. Every other error, including
 * one that merely carries a transient error's name, is not one.
 */
export const isWatcherL1TransientFailure = (error: unknown): error is Error => {
  let current: unknown = error;
  for (let depth = 0; depth < MAXIMUM_CAUSE_DEPTH; depth += 1) {
    if (!(current instanceof Error)) return false;
    if (
      current instanceof FraudProofL1UnavailableError ||
      current instanceof FraudProofL1CheckpointChangedError ||
      current instanceof TransportUnavailableError ||
      current instanceof TransportTimeoutError ||
      current instanceof L1ProviderTransientError ||
      current instanceof LocalKupmiosTransportUnavailableError ||
      current instanceof LocalKupmiosCheckpointChangedError ||
      retryableProviderError(current) ||
      isWatcherNativeNodeUnavailable(current) ||
      transientCode(current)
    )
      return true;
    current = current.cause;
  }
  return false;
};
