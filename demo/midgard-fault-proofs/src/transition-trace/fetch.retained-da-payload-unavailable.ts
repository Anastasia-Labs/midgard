import { TransitionTraceChallengerError } from "./errors.js";
import type {
  RetainedDaFetchAttempt,
  RetainedDaFetchAttemptStatus,
} from "./fetch.admit-retained-da-provenance.js";

const UNAVAILABLE_ATTEMPT_STATUSES: ReadonlySet<RetainedDaFetchAttemptStatus> =
  new Set(["not_found", "transport_error", "timeout"]);

/**
 * True when every attempt reports only that its source does not serve the
 * payload yet. A rejected, conflicting or invalid response is evidence about
 * the payload, never ordinary unavailability, and keeps its fail-closed path.
 * Callers that need at least one answered request check that themselves.
 */
export const retainedDaAttemptsOnlyUnavailable = (
  attempts: readonly RetainedDaFetchAttempt[],
): boolean =>
  attempts.every(({ status }) => UNAVAILABLE_ATTEMPT_STATUSES.has(status));

export const RETAINED_DA_PAYLOAD_UNAVAILABLE = "payloadUnavailable" as const;

/**
 * `not_found`: every source answered and none holds the payload.
 * `unreachable`: at least one source did not answer (transport error or
 * timeout), so nothing is known about the payload yet.
 */
export type RetainedDaPayloadAvailability = "not_found" | "unreachable";

export const retainedDaAttemptsAvailability = (
  attempts: readonly RetainedDaFetchAttempt[],
): RetainedDaPayloadAvailability =>
  attempts.every(({ status }) => status === "not_found")
    ? "not_found"
    : "unreachable";

/**
 * No public source served the payload and none returned anything but
 * not_found, transport_error or timeout. The code stays `fetchFailed`;
 * `reason` and `headerHash` let a caller that can wait tell this apart, and
 * `availability` says whether every source answered.
 */
export class RetainedDaPayloadUnavailableError extends TransitionTraceChallengerError {
  readonly reason = RETAINED_DA_PAYLOAD_UNAVAILABLE;
  readonly headerHash: string;
  readonly availability: RetainedDaPayloadAvailability;

  constructor(
    headerHash: string,
    message: string,
    availability: RetainedDaPayloadAvailability = "not_found",
  ) {
    super("fetchFailed", message);
    this.headerHash = headerHash;
    this.availability = availability;
  }
}

/**
 * Matches by value: a consumer that bundles this package separately holds a
 * different class, so `instanceof` cannot cross that boundary.
 */
export const isRetainedDaPayloadUnavailableError = (
  error: unknown,
): error is Error &
  Readonly<{
    code: "fetchFailed";
    reason: typeof RETAINED_DA_PAYLOAD_UNAVAILABLE;
    headerHash: string;
    availability?: RetainedDaPayloadAvailability;
  }> =>
  error instanceof Error &&
  (error as { readonly code?: unknown }).code === "fetchFailed" &&
  (error as { readonly reason?: unknown }).reason ===
    RETAINED_DA_PAYLOAD_UNAVAILABLE &&
  typeof (error as { readonly headerHash?: unknown }).headerHash === "string";
