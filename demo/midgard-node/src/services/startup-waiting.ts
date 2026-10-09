/**
 * The named reasons a node startup step waits on, for `/readyz` while the
 * node starts (`listen.startup-http.ts`).
 *
 * A step that retries a condition the node cannot settle itself (a
 * PostgreSQL that does not answer, an L1 provider not reachable yet, a DA
 * committee whose quorum is still forming) runs under `retryStartupStep`:
 * it retries without a deadline on a capped backoff, and while it waits its
 * reason is reported under the step's key; once the step succeeds its key
 * reports none. Steps run in the runtime's layers as well as in `runNode`,
 * so the reporter is a fiber reference the startup HTTP server sets for the
 * whole startup; outside a node's startup (a CLI command, a test) nothing
 * listens and the retry still runs.
 *
 * The reasons the steps wait under are declared here.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Duration, Effect, FiberRef } from "effect";

/** A Lucid client's construction (its protocol-parameter read) failed
 * (`lucid.ts`). */
export const LUCID_INITIALIZATION_PENDING = "lucid_initialization_pending";

/** PostgreSQL cannot be reached or will not yet take a connection: a pool
 * waits to open (`database.ts`), or the schema check to run
 * (`database/init.ts`). */
export const DATABASE_UNREACHABLE = "database_unreachable";

/** A `db:migrate` holds the schema lock; the schema check waits for it. */
export const SCHEMA_MIGRATION_IN_PROGRESS = "schema_migration_in_progress";

/** The protocol deployment status read failed with a retryable provider
 * error (`listen-startup.ensure-protocol-initialized-on-startup.ts`). */
export const PROTOCOL_DEPLOYMENT_STATUS_UNAVAILABLE =
  "protocol_deployment_status_unavailable";

/** A run of the startup protocol check failed; it runs again from the start
 * (`listen-startup.ensure-protocol-initialized-on-startup.ts`). */
export const PROTOCOL_INITIALIZATION_FAILED = "protocol_initialization_failed";

/** The DA provider assertions read failed with a retryable provider error
 * (`listen.run-node.ts`). */
export const DA_PROVIDER_ASSERTIONS_UNAVAILABLE =
  "da_provider_assertions_unavailable";

/** Too few DA committee peers answered capably yet, and the quorum can
 * still form (`DaCapabilityQuorumPendingError`). */
export const DA_CAPABILITY_QUORUM_PENDING = "da_capability_quorum_pending";

/** The startup's first read of the durable-admission backlog failed
 * (`refreshAdmissionBacklogGaugeOnStartup`). */
export const ADMISSION_BACKLOG_UNREAD = "admission_backlog_unread";

/** Reports `reasons` (none: the step is past its wait) under `key`. */
export type StartupWaitingReport = (
  key: string,
  reasons: readonly string[],
) => Effect.Effect<void>;

/** The startup's reporter; nothing listens by default. */
export const StartupWaitingReporter = FiberRef.unsafeMake<StartupWaitingReport>(
  () => Effect.void,
);

/** Reports `reasons` under `key` to the running startup, if any. */
export const reportStartupWaiting = (
  key: string,
  reasons: readonly string[],
): Effect.Effect<void> =>
  Effect.flatMap(FiberRef.get(StartupWaitingReporter), (report) =>
    report(key, reasons),
  );

/** First wait between a startup step's attempts. */
export const STARTUP_STEP_RETRY_INITIAL_MS = 1_000;
/** Ceiling of that wait as it doubles. */
export const STARTUP_STEP_RETRY_MAX_MS = 30_000;

export type StartupStepRetry<E> = Readonly<{
  /** The step's key among the startup's waits, and its log label. */
  key: string;
  /** The `/readyz` reason the step waits under: one name, or one per
   * failure. */
  reason: string | ((error: E) => string);
  /** Whether a failure is waited out; others fail the step. Default: all. */
  retryable?: (error: E) => boolean;
  initialMs?: number;
  maxMs?: number;
}>;

/**
 * Runs `step` until it succeeds: a failure `retryable` admits is logged,
 * reported under the step's key, and retried after a wait that doubles from
 * `initialMs` up to `maxMs`, with no deadline. Any other failure fails the
 * step at once. Once the step succeeds or fails after waiting, its key
 * reports none.
 */
export const retryStartupStep = <A, E, R>(
  step: Effect.Effect<A, E, R>,
  options: StartupStepRetry<E>,
): Effect.Effect<A, E, R> =>
  Effect.gen(function* () {
    const maxMs = options.maxMs ?? STARTUP_STEP_RETRY_MAX_MS;
    let delayMs = options.initialMs ?? STARTUP_STEP_RETRY_INITIAL_MS;
    let waited: string | undefined;
    for (let attempt = 1; ; attempt += 1) {
      const result = yield* Effect.either(step);
      if (result._tag === "Right") {
        if (waited !== undefined) {
          yield* reportStartupWaiting(options.key, []);
          yield* Effect.logInfo(
            `${options.key}: done after ${attempt.toString()} attempts`,
          );
        }
        return result.right;
      }
      const error = result.left;
      if (options.retryable !== undefined && !options.retryable(error)) {
        if (waited !== undefined) yield* reportStartupWaiting(options.key, []);
        return yield* Effect.fail(error);
      }
      const reason =
        typeof options.reason === "string"
          ? options.reason
          : options.reason(error);
      yield* reportStartupWaiting(options.key, [reason]);
      const detail = `${options.key} waits: reason=${reason}; attempt ${attempt.toString()}, retrying in ${delayMs.toString()} ms. cause=${formatUnknownError(error, { includeCause: true })}`;
      yield* reason === waited
        ? Effect.logDebug(detail)
        : Effect.logWarning(detail);
      waited = reason;
      yield* Effect.sleep(Duration.millis(delayMs));
      delayMs = Math.min(delayMs * 2, maxMs);
    }
  });
