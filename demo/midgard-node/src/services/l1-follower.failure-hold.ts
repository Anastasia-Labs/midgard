/**
 * How the node holds on a failure of a follower-change run (plan §7.5): the
 * classification that tells an L1 node outage from a database transient and
 * from a failure that is not transient. It names the L1 node transport's
 * error classes and the provider retry policy (which jitters its backoff),
 * so it lives outside the pure event driver (`l1-events/driver.ts`), which
 * the determinism lint keeps free of the network and of randomness; the
 * driver takes `failureHold` as an option.
 */
import {
  SidecarExitedError,
  StreamInterruptedError,
  TransportRequestError,
  TransportTimeoutError,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";
import { classifyFailure } from "@al-ft/midgard-l1-follower";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";

import {
  type DriverHold,
  notRetried,
  transientFailure,
} from "../l1-events/driver.js";
import {
  isConnectionClassError,
  isRetryableProviderError,
} from "../provider-retry.js";

/**
 * Whether a failure says only that a dependency did not answer: a
 * connection-class or retryable provider failure (`isConnectionClassError`,
 * `isRetryableProviderError`) or a store failure the follower classifies as
 * transient (`classifyFailure`). An unrecognised failure is not transient.
 */
export const isTransientDriverFailure = (error: unknown): boolean => {
  if (isConnectionClassError(error) || isRetryableProviderError(error))
    return true;
  let current: unknown = error;
  for (let depth = 0; depth < 8 && current instanceof Error; depth += 1) {
    if (classifyFailure(current) === "transient") return true;
    current = current.cause;
  }
  return false;
};

/**
 * Whether a failure comes from the L1 node: its transport or sidecar, or
 * the follower provider's node or follower path (not its store). Waiting on
 * the node has no bound (plan §7.5).
 */
export const isL1NodeOutage = (error: unknown): boolean => {
  let current: unknown = error;
  for (let depth = 0; depth < 8 && current instanceof Error; depth += 1) {
    if (
      current instanceof TransportUnavailableError ||
      current instanceof SidecarExitedError ||
      current instanceof TransportTimeoutError ||
      current instanceof StreamInterruptedError ||
      current instanceof TransportRequestError ||
      (current instanceof L1ProviderTransientError &&
        current.source !== "store")
    )
      return true;
    current = current.cause;
  }
  return false;
};

/**
 * A failure hold. A transient failure is retried: one from the L1 node
 * (`isL1NodeOutage`) without bound, any other (the database) as a
 * `transientFailure`, for a bounded time. A failure that is not transient
 * is `notRetried`.
 */
export const failureHold = (
  reason: string,
  detail: string,
  error: unknown,
): DriverHold => {
  const hold = { reason, detail };
  if (!isTransientDriverFailure(error)) return notRetried(hold);
  return isL1NodeOutage(error) ? hold : transientFailure(hold);
};
