import {
  SidecarExitedError,
  STREAM_REOPEN_CODES,
  StreamInterruptedError,
  TransportFailedError,
  TransportRequestError,
  TransportTimeoutError,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";

import { classifyRoleFailure } from "../role-failure.js";

/**
 * How the follow loop treats a failed store write (plan §7.5): `transient`
 * recovers by itself (a dropped or refused connection, a lock, a
 * serialization conflict) and is retried with backoff; `deterministic`
 * fails the same way on every retry (a constraint or data error, a
 * statement the database refuses) and stops the loop at once; `unknown` is
 * not treated as transient: it stops the loop after N consecutive failures
 * on the same point.
 */
export type FailureClass = "transient" | "deterministic" | "unknown";

/**
 * The follow loop's adapter over the shared table (`classifyRoleFailure`):
 * contention is retried like a transient. The loop never exits the process,
 * so a failure a restart may clear is `unknown` here, and its host decides.
 */
export const classifyFailure = (error: unknown): FailureClass => {
  const failure = classifyRoleFailure(error);
  return failure === "contention"
    ? "transient"
    : failure === "restart"
      ? "unknown"
      : failure;
};

/**
 * How the follow loop treats a failed chain-sync stream: the transport
 * unavailable or restarting, its sidecar exiting, a timeout, an interrupted
 * stream, a refusal the stream itself would reopen from
 * (`STREAM_REOPEN_CODES`) and a connection-level error are `transient`, and
 * the loop reopens the stream with backoff. A transport that failed on a
 * fault no restart repairs (`TransportFailedError`: the node refused the
 * handshake) is `deterministic`: the loop stops at once. Anything else (a
 * stream failure code such as a protocol violation or an undecodable block, a
 * protocol error in the sidecar's frames) is `unknown`: the loop reopens it,
 * and stops after N consecutive failures on the same point.
 */
export const classifyStreamFailure = (error: unknown): FailureClass => {
  if (error instanceof TransportFailedError) return "deterministic";
  if (
    error instanceof TransportUnavailableError ||
    error instanceof SidecarExitedError ||
    error instanceof TransportTimeoutError ||
    error instanceof StreamInterruptedError
  )
    return "transient";
  if (error instanceof TransportRequestError)
    return STREAM_REOPEN_CODES.has(error.code) ? "transient" : "unknown";
  return classifyFailure(error) === "transient" ? "transient" : "unknown";
};
