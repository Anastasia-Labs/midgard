/**
 * Retrying a due takeover the watchdog could not build or submit.
 *
 * A strike or forced retirement that fails for any reason other than a script
 * refusal (which passes over its citation instead) is retried on a doubling
 * backoff, bounded by `OPERATOR_WATCHDOG_TAKEOVER_RETRY_MAX_MS`, and never
 * given up: the shift it would hand on stays stalled until a takeover lands.
 * An unfunded wallet is retried the same way. After
 * `OPERATOR_WATCHDOG_TAKEOVER_FAILING_AFTER` failures in a row readiness
 * reports `operator_watchdog_takeover_failing` with the last error; a landed
 * takeover, or a tick with no takeover due, clears it.
 *
 * A citation such failures pass over (`recordCitationFailure`) is cited again
 * once no other event is citable, so passing over the only evidence never
 * leaves the watchdog idle in front of a stalled shift.
 */
import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  publishedNeglectedUserEventProgram,
  type PublishedOperatorSet,
} from "../l1-operator-set/index.js";
import type { Globals } from "../services/globals.globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import type { CitationFailures } from "./operator-watchdog-policy.js";

/** Readiness reason: the watchdog failed to build or submit a due takeover
 * `OPERATOR_WATCHDOG_TAKEOVER_FAILING_AFTER` times in a row; it keeps
 * retrying. */
export const OPERATOR_WATCHDOG_TAKEOVER_FAILING =
  "operator_watchdog_takeover_failing";

export const OPERATOR_WATCHDOG_TAKEOVER_FAILING_AFTER = 3;

/** The first retry after a failed takeover; each further failure in a row
 * doubles it up to `OPERATOR_WATCHDOG_TAKEOVER_RETRY_MAX_MS`. */
export const OPERATOR_WATCHDOG_TAKEOVER_RETRY_BASE_MS = 10_000;
export const OPERATOR_WATCHDOG_TAKEOVER_RETRY_MAX_MS = 300_000;

const SOURCE = "operator_watchdog_takeover";

/** Takeovers failed in a row since the last one landed or fell idle. */
let consecutiveFailures = 0;

export const resetTakeoverFailuresForTests = (): void => {
  consecutiveFailures = 0;
};

/**
 * Records one failed takeover at L1 time `nowMs` and answers when to retry
 * it. Raises the readiness reason once the failures in a row reach
 * `OPERATOR_WATCHDOG_TAKEOVER_FAILING_AFTER`.
 */
export const recordTakeoverFailure = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  input: Readonly<{ nowMs: number; detail: string }>,
): Effect.Effect<Readonly<{ consecutive: number; retryAtMs: number }>> =>
  Effect.gen(function* () {
    consecutiveFailures += 1;
    const consecutive = consecutiveFailures;
    const retryAtMs =
      input.nowMs +
      Math.min(
        OPERATOR_WATCHDOG_TAKEOVER_RETRY_MAX_MS,
        OPERATOR_WATCHDOG_TAKEOVER_RETRY_BASE_MS *
          2 ** Math.min(consecutive - 1, 30),
      );
    if (consecutive >= OPERATOR_WATCHDOG_TAKEOVER_FAILING_AFTER)
      yield* raiseLivenessIncident(
        globals,
        SOURCE,
        OPERATOR_WATCHDOG_TAKEOVER_FAILING,
        `the operator watchdog failed a due takeover ${consecutive.toString()} times in a row and retries at ${new Date(retryAtMs).toISOString()}: ${input.detail}`,
      );
    return { consecutive, retryAtMs };
  });

/** A takeover landed or none is due: the failures in a row end. */
export const clearTakeoverFailures = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
): Effect.Effect<void> =>
  Effect.suspend(() => {
    consecutiveFailures = 0;
    return clearLivenessIncident(globals, SOURCE);
  });

/**
 * The event the next strike cites: the earliest citable one no failure
 * passed over, or else the earliest one only failed attempts passed over. A
 * script refusal is deterministic, so a refused citation is never cited
 * again under the same scheduler state.
 */
export const citableNeglectedEventProgram = (
  published: PublishedOperatorSet | undefined,
  tailEndMs: bigint,
  failures: CitationFailures,
): Effect.Effect<SDK.NeglectedUserEventClaim | null, SDK.StateQueueError> =>
  publishedNeglectedUserEventProgram(
    published,
    tailEndMs,
    failures.excluded,
  ).pipe(
    Effect.flatMap((event) =>
      event !== null ||
      [...failures.excluded].every((id) => failures.refused.has(id))
        ? Effect.succeed(event)
        : publishedNeglectedUserEventProgram(
            published,
            tailEndMs,
            failures.refused,
          ),
    ),
  );
