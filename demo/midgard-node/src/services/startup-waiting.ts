/**
 * The node's startup steps that wait out a transient failure, the named
 * reasons they wait under, and the named terminal error a step fails with
 * (`StartupStepFailedError`), for `/readyz` while the node starts
 * (`listen.startup-http.ts`).
 *
 * A step that depends on something the node cannot settle itself (a
 * PostgreSQL that does not answer, an L1 node or sidecar still starting, a
 * provider read that timed out) runs under `retryStartupStep`. The step's
 * own `retryable` predicate classifies each failure: one it admits is
 * transient and is retried on a capped backoff under the step's bounded
 * budget, its reason reported under the step's key while it waits. Any
 * other failure is not transient and fails the step at once; a transient
 * one that outlives the budget fails it too. Both fail with a
 * `StartupStepFailedError` naming the step, the reason and the last cause.
 * The node's startup then fails: `/readyz` reports `startup_failed` with the
 * step and its reason. A transient failure that outlived the budget exits the
 * process non-zero, its supervisor's restart being the backoff; any other
 * failure holds the process up, unready, until it is restarted
 * (`withStartupHttpServer`).
 *
 * Steps run in the runtime's layers as well as in `runNode`, so the
 * reporter is a fiber reference the startup HTTP server sets for the whole
 * startup; outside a node's startup (a CLI command, a test) nothing listens
 * and the step still runs.
 *
 * Waiting on another actor's progress (an instance lock another process
 * holds) is not a retry and does not run here.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type * as SDK from "@al-ft/midgard-sdk";
import { Cause, Clock, Data, Duration, Effect, FiberRef } from "effect";

/** A Lucid client's construction (its protocol-parameter read) failed with
 * a transient provider error (`lucid.ts`). */
export const LUCID_INITIALIZATION_PENDING = "lucid_initialization_pending";

/** The local node's configuration files are not there yet, so its network
 * magic cannot be read (`lucid.ts`). */
export const L1_NODE_CONFIG_PENDING = "l1_node_config_pending";

/** The ledger's slot mapping read failed with a transient L1 read error
 * (`custom-slot-mapping.ts`). */
export const L1_SLOT_MAPPING_PENDING = "l1_slot_mapping_pending";

/** PostgreSQL cannot be reached or will not yet take a connection: a pool
 * waits to open (`database.ts`), or the schema check to run
 * (`database/init.ts`). */
export const DATABASE_UNREACHABLE = "database_unreachable";

/** PostgreSQL refused the connection for a reason waiting does not clear
 * (bad credentials, a missing database): `database.ts`. */
export const DATABASE_CONNECTION_FAILED = "database_connection_failed";

/** A `db:migrate` holds the schema lock; the schema check waits for it. */
export const SCHEMA_MIGRATION_IN_PROGRESS = "schema_migration_in_progress";

/** The database's schema is not the one this binary runs on (not migrated,
 * unversioned, a ledger or shape mismatch): `database/init.ts`. */
export const DATABASE_SCHEMA_INCOMPATIBLE = "database_schema_incompatible";

/** The protocol deployment status read failed with a retryable provider
 * error (`listen-startup.ensure-protocol-initialized-on-startup.ts`). */
export const PROTOCOL_DEPLOYMENT_STATUS_UNAVAILABLE =
  "protocol_deployment_status_unavailable";

/** The availability-challenge reward-account reads failed with a retryable
 * provider error (`listen-startup.ensure-protocol-initialized-on-startup.ts`). */
export const REWARD_ACCOUNT_STATUS_UNAVAILABLE =
  "reward_account_status_unavailable";

/** The configured deployment manifest does not match the deployment. */
export const DEPLOYMENT_MANIFEST_MISMATCH = "deployment_manifest_mismatch";

/** The configured deployment manifest could not be read or verified. */
export const DEPLOYMENT_MANIFEST_UNVERIFIABLE =
  "deployment_manifest_unverifiable";

/** The deployment is complete but no deployment manifest is configured. */
export const DEPLOYMENT_MANIFEST_MISSING = "deployment_manifest_missing";

/** Some, not all, of the protocol deployment is on chain. */
export const PROTOCOL_DEPLOYMENT_PARTIAL = "protocol_deployment_partial";

/** An availability-challenge reward account is not registered. */
export const AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED =
  "availability_reward_account_unregistered";

/** The node-runtime reference-script preflight failed. */
export const RUNTIME_REFERENCE_SCRIPTS_FAILED =
  "runtime_reference_scripts_failed";

/** The startup's protocol initialization (its transaction, the reference
 * scripts after it, or the deployment-info write) failed. */
export const PROTOCOL_INITIALIZATION_FAILED = "protocol_initialization_failed";

/** The DA provider assertions read failed with a retryable provider error
 * (`listen.run-node.ts`). */
export const DA_PROVIDER_ASSERTIONS_UNAVAILABLE =
  "da_provider_assertions_unavailable";

/** Too few DA committee peers answered capably yet, and the quorum can
 * still form (`DaCapabilityQuorumPendingError`). */
export const DA_CAPABILITY_QUORUM_PENDING = "da_capability_quorum_pending";

/** The startup's first read of the durable-admission backlog failed with a
 * connection-class error (`refreshAdmissionBacklogGaugeOnStartup`). */
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

/**
 * How long a step waits for PostgreSQL to take connections: the budget the
 * pools had before they waited without one.
 */
export const STARTUP_DATABASE_BUDGET = Duration.minutes(15);

/**
 * How long a step waits for the local cardano-node and its transport
 * sidecar: its configuration files, the slot mapping, a Lucid client's
 * protocol parameters. The same as the default deployment-status budget
 * (`STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS` 120 x 5 s), which waits on
 * the same node.
 */
export const STARTUP_L1_NODE_BUDGET = Duration.minutes(10);

/**
 * A startup step failed for good: on a failure it does not wait out
 * (`exhausted: false`), or on a transient one that outlived its budget
 * (`exhausted: true`). `step` is the step's key, `reason` the named reason,
 * `cause` the last failure.
 */
export class StartupStepFailedError extends Data.TaggedError(
  "StartupStepFailedError",
)<
  SDK.GenericErrorFields & {
    readonly step: string;
    readonly reason: string;
    readonly exhausted: boolean;
    readonly attempts: number;
  }
> {}

/** The step's terminal error; its message names step, reason and cause. */
export const startupStepFailed = (
  fields: Readonly<{
    step: string;
    reason: string;
    cause: unknown;
    exhausted?: boolean;
    attempts?: number;
    /** How long the step waited before it ran out of budget. */
    waitedMs?: number;
  }>,
): StartupStepFailedError => {
  const exhausted = fields.exhausted ?? false;
  const attempts = fields.attempts ?? 1;
  const waited =
    fields.waitedMs === undefined
      ? ""
      : ` over ${Math.round(fields.waitedMs / 1_000).toString()} s`;
  return new StartupStepFailedError({
    step: fields.step,
    reason: fields.reason,
    exhausted,
    attempts,
    cause: fields.cause,
    message: `startup step ${fields.step} failed: reason=${fields.reason}; ${
      exhausted
        ? `a transient failure outlived the step's budget (${attempts.toString()} attempts${waited})`
        : "the failure is not one the step waits out"
    }; last cause=${formatUnknownError(fields.cause, { includeCause: true })}`,
  });
};

/**
 * The `StartupStepFailedError` behind a failed startup: one on its `Cause`,
 * or on the `cause` chain of an error that wraps it (a layer's
 * `ConfigError`). Undefined when there is none.
 */
export const findStartupStepFailure = (
  cause: Cause.Cause<unknown>,
): StartupStepFailedError | undefined => {
  const pending: unknown[] = [
    ...Cause.failures(cause),
    ...Cause.defects(cause),
  ];
  for (let seen = 0; pending.length > 0 && seen < 64; seen += 1) {
    const next = pending.shift();
    if (next instanceof StartupStepFailedError) return next;
    if (typeof next === "object" && next !== null && "cause" in next)
      pending.push((next as { readonly cause?: unknown }).cause);
  }
  return undefined;
};

/** How much retrying a step's transient failures get: attempts in all, or
 * time since the first failure. */
export type StartupStepBudget =
  | Readonly<{ maxAttempts: number }>
  | Readonly<{ maxElapsed: Duration.DurationInput }>;

export type StartupStepRetry<E> = Readonly<{
  /** The step's key among the startup's waits, and its log label. */
  key: string;
  /** Whether a failure is transient. Only those are waited out; an unknown
   * failure is not transient. */
  retryable: (error: E) => boolean;
  /** The named reason a failure waits under (transient) or fails the step
   * under: one name, or one per failure. */
  reason: string | ((error: E) => string);
  /** How long the step waits out transient failures. */
  budget: StartupStepBudget;
  initialMs?: number;
  maxMs?: number;
}>;

/**
 * Runs `step` until it succeeds. A failure `retryable` admits is logged,
 * reported under the step's key, and retried after a wait that doubles from
 * `initialMs` up to `maxMs`, while the budget lasts. A failure it does not
 * admit, or any failure once the budget has run out, fails the step with a
 * `StartupStepFailedError`. Once the step is past its wait its key reports
 * none. Time is read from the effect `Clock`, so a test clock drives it.
 */
export const retryStartupStep = <A, E, R>(
  step: Effect.Effect<A, E, R>,
  options: StartupStepRetry<E>,
): Effect.Effect<A, StartupStepFailedError, R> =>
  Effect.gen(function* () {
    const maxMs = options.maxMs ?? STARTUP_STEP_RETRY_MAX_MS;
    let delayMs = options.initialMs ?? STARTUP_STEP_RETRY_INITIAL_MS;
    const { budget } = options;
    const maxAttempts =
      "maxAttempts" in budget
        ? Math.max(1, Math.floor(budget.maxAttempts))
        : Number.POSITIVE_INFINITY;
    const maxElapsedMs =
      "maxElapsed" in budget
        ? Duration.toMillis(Duration.decode(budget.maxElapsed))
        : Number.POSITIVE_INFINITY;
    let firstFailureAt: number | undefined;
    let waited: string | undefined;
    const clearWait = () =>
      waited === undefined
        ? Effect.void
        : reportStartupWaiting(options.key, []);
    for (let attempt = 1; ; attempt += 1) {
      const result = yield* Effect.either(step);
      if (result._tag === "Right") {
        if (waited !== undefined) {
          yield* clearWait();
          yield* Effect.logInfo(
            `${options.key}: done after ${attempt.toString()} attempts`,
          );
        }
        return result.right;
      }
      const error = result.left;
      const reason =
        typeof options.reason === "string"
          ? options.reason
          : options.reason(error);
      if (!options.retryable(error)) {
        yield* clearWait();
        return yield* Effect.fail(
          startupStepFailed({
            step: options.key,
            reason,
            cause: error,
            attempts: attempt,
          }),
        );
      }
      const now = yield* Clock.currentTimeMillis;
      firstFailureAt ??= now;
      const waitedMs = now - firstFailureAt;
      if (attempt >= maxAttempts || waitedMs + delayMs > maxElapsedMs) {
        yield* clearWait();
        return yield* Effect.fail(
          startupStepFailed({
            step: options.key,
            reason,
            cause: error,
            exhausted: true,
            attempts: attempt,
            waitedMs,
          }),
        );
      }
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
