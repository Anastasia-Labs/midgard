import type { TransportFailedReason, TransportReadiness } from "./protocol.js";

/**
 * The transport failed on a fault no restart repairs
 * (`TRANSPORT_FAILED_REASONS`): it no longer restarts its sidecar, and every
 * call and stream fails with this until the process restarts.
 */
export class TransportFailedError extends Error {
  override readonly name = "TransportFailedError";
  constructor(
    readonly reason: TransportFailedReason,
    readonly detail: string,
  ) {
    super(
      `L1 node transport failed: ${reason}: ${detail}; it is not restarted`,
    );
  }
}

/** The failure a failed readiness carries; undefined while it is not one. */
export const transportFailureOf = (
  readiness: TransportReadiness,
): TransportFailedError | undefined =>
  !readiness.ready && "failed" in readiness
    ? new TransportFailedError(readiness.reason, readiness.detail)
    : undefined;
