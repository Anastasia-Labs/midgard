import {
  LocalKupmiosCheckpointChangedError,
  LocalKupmiosTransportUnavailableError,
} from "@al-ft/midgard-fault-proofs";

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
 * Startup failures that say only that the node did not answer: its socket did
 * not accept, its tip query or the chain-sync session broke before readiness,
 * or it did not become ready in time. A node that is restarting or still
 * opening its database gives exactly these. A rejected intersection, an
 * invalid startup and every identity mismatch are not among them.
 */
const NODE_UNAVAILABLE_CODES: ReadonlySet<string> = new Set([
  "node_handshake_failed",
  "tip_query_failed",
  "chain_sync_failed",
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
 * Whether a failed L1 read says only that Kupo, Ogmios or the node did not
 * answer, or moved while a snapshot was read: nothing about the chain or the
 * deployment. Such a read can be repeated as is. Every other error, including
 * one that merely carries a transient error's name, is not one.
 */
export const isWatcherL1TransientFailure = (error: unknown): error is Error => {
  let current: unknown = error;
  for (let depth = 0; depth < MAXIMUM_CAUSE_DEPTH; depth += 1) {
    if (!(current instanceof Error)) return false;
    if (
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
