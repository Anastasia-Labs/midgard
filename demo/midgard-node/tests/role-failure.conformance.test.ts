/**
 * The node's adapters agree with the shared failure classification
 * (`classifyRoleFailure`, `@al-ft/midgard-l1-follower`) on every
 * conformance row:
 * - `isConnectionClassError` (the database's startup wait and its
 *   transient budget) holds exactly the `transient` class;
 * - `isRetryableProviderError` retries the `transient` class, and refuses
 *   every other class except where the node's provider-message fallback
 *   matches the text (`PROVIDER_MESSAGE_ROWS`);
 * - `failureHold` (a follower-change run) bounds a `transient` or
 *   `contention` failure and holds any other, not retried.
 */
import { ROLE_FAILURE_ROWS } from "@al-ft/midgard-l1-follower/testing";
import { Cause, Runtime } from "effect";
import { describe, expect, it } from "vitest";

import {
  isRetriedHold,
  isTransientFailureHold,
} from "../src/l1-events/driver.js";
import {
  isConnectionClassError,
  isRetryableProviderError,
} from "../src/provider-retry.js";
import { failureHold } from "../src/services/l1-follower.failure-hold.js";

/**
 * Rows the provider retry takes by its message fallback (a provider's text
 * naming a timeout), outside the shared table.
 */
const PROVIDER_MESSAGE_ROWS: ReadonlySet<string> = new Set([
  "a message that only mentions a timeout",
  "a non-Error value",
]);

const rows = ROLE_FAILURE_ROWS.map((row) => ({
  ...row,
  transient: row.expected === "transient",
  provider: row.expected === "transient" || PROVIDER_MESSAGE_ROWS.has(row.name),
  bounded:
    row.expected === "transient" ||
    row.expected === "contention" ||
    PROVIDER_MESSAGE_ROWS.has(row.name),
}));

describe("the node agrees with the shared failure classification", () => {
  it.each(rows)(
    "isConnectionClassError on $name ($expected)",
    ({ error, transient }) => {
      expect(isConnectionClassError(error())).toBe(transient);
      // As an Effect failure.
      expect(isConnectionClassError(Cause.fail(error()))).toBe(transient);
    },
  );

  // A FiberFailure takes its message from what it carries, so only the
  // rows whose error is an Error keep their meaning inside one.
  it.each(rows.filter((row) => row.error() instanceof Error))(
    "isConnectionClassError on $name ($expected) as `Effect.runPromise` rejects with it",
    ({ error, transient }) => {
      expect(
        isConnectionClassError(Runtime.makeFiberFailure(Cause.die(error()))),
      ).toBe(transient);
    },
  );

  it.each(rows)(
    "isRetryableProviderError on $name ($expected)",
    ({ error, provider }) => {
      expect(isRetryableProviderError(error())).toBe(provider);
    },
  );

  it.each(rows)("failureHold on $name ($expected)", ({ error, bounded }) => {
    const hold = failureHold("events_ingestion_failed", "x", error());
    expect(isRetriedHold(hold)).toBe(bounded);
    expect(isTransientFailureHold(hold)).toBe(bounded);
  });
});
