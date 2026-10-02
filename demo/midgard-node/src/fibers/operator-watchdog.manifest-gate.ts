import { Cause, Effect, Exit, Ref } from "effect";

import { errorMessage } from "../commands/cli-runtime.js";
import type { Globals } from "../services/globals.globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";

/** Readiness reason: the deployment manifest does not match this node's
 * configuration, so the watchdog strikes nobody. */
export const OPERATOR_WATCHDOG_MANIFEST_MISMATCH =
  "operator_watchdog_manifest_mismatch";

/** Readiness reason: the deployment manifest could not be verified
 * `OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED_AFTER` times in a row, so the watchdog
 * strikes nobody yet and keeps retrying. */
export const OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED =
  "operator_watchdog_manifest_unverified";

export const OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED_AFTER = 3;

/** The first retry after a failed verification; each further failure doubles
 * it up to `OPERATOR_WATCHDOG_MANIFEST_RETRY_MAX_MS`. */
export const OPERATOR_WATCHDOG_MANIFEST_RETRY_BASE_MS = 5_000;
export const OPERATOR_WATCHDOG_MANIFEST_RETRY_MAX_MS = 300_000;

/** How long a confirmed mismatch refuses before the manifest is read again,
 * so a corrected manifest file is picked up without a restart. */
export const OPERATOR_WATCHDOG_MANIFEST_MISMATCH_RECHECK_MS = 600_000;

const SOURCE = "operator_watchdog_manifest";

export type ManifestVerification = Readonly<{
  ok: boolean;
  mismatches: readonly string[];
}>;

export type ManifestGateState = Readonly<{
  verdict: "unverified" | "mismatch" | "verified";
  consecutiveErrors: number;
  /** No verification is attempted before this time. */
  retryAtMs: number;
  lastError: string | undefined;
}>;

/** Whether the next strike may be submitted, or until when to defer it. */
export type ManifestGateDecision =
  | Readonly<{ ok: true }>
  | Readonly<{ ok: false; reason: string; untilMs: number }>;

export type ManifestStrikeGate<R> = Readonly<{
  /** Run before every strike. Verifies the manifest when it is not verified
   * yet and no retry is pending; never fails, and catches a defect of the
   * verification too. */
  beforeStrike: (
    nowMs: number,
  ) => Effect.Effect<ManifestGateDecision, never, R>;
  /** Run on every tick on which no strike is due. While a reason is raised it
   * verifies again on the same retry cadence, so the reason clears once the
   * manifest verifies rather than at the next due strike, which may never
   * come. With no reason raised it verifies nothing. Returns the time of its
   * next retry while a reason stays raised, so the caller ticks by then. */
  whileNoStrikeDue: (
    nowMs: number,
  ) => Effect.Effect<number | undefined, never, R>;
  state: Effect.Effect<ManifestGateState>;
}>;

/** Whether `state` has a liveness reason raised. */
const reasonRaised = (state: ManifestGateState): boolean =>
  state.verdict === "mismatch" ||
  (state.verdict === "unverified" &&
    state.consecutiveErrors >= OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED_AFTER);

/**
 * Every lifecycle verb verifies the deployment manifest before it acts; the
 * watchdog verifies it before its first strike rather than once at start, so a
 * manifest that could not be read at start (a transient file or config read
 * failure) no longer disables the watchdog for the process's lifetime. A
 * verification error is retried with bounded backoff. A confirmed mismatch
 * refuses every strike, is surfaced in readiness, and is re-read at a slow
 * cadence. A raised reason is re-verified on ticks with no strike due as
 * well, so it clears without waiting for a strike. Once verified the verdict
 * is kept: the manifest cannot change without a redeploy and a restart.
 */
export const makeManifestStrikeGate = <R>(
  globals: Pick<Globals, "LIVENESS_REASONS">,
  verify: Effect.Effect<ManifestVerification, unknown, R>,
): Effect.Effect<ManifestStrikeGate<R>> =>
  Effect.gen(function* () {
    const state = yield* Ref.make<ManifestGateState>({
      verdict: "unverified",
      consecutiveErrors: 0,
      retryAtMs: 0,
      lastError: undefined,
    });
    const beforeStrike = (nowMs: number) =>
      Effect.gen(function* () {
        const current = yield* Ref.get(state);
        if (current.verdict === "verified") return { ok: true } as const;
        if (nowMs < current.retryAtMs)
          return {
            ok: false,
            reason:
              current.verdict === "mismatch"
                ? "manifest_mismatch"
                : "manifest_unverified",
            untilMs: current.retryAtMs,
          } as const;
        const exit = yield* Effect.exit(verify);
        if (Exit.isFailure(exit)) {
          const failure = Cause.squash(exit.cause);
          const consecutiveErrors = current.consecutiveErrors + 1;
          const retryAtMs =
            nowMs +
            Math.min(
              OPERATOR_WATCHDOG_MANIFEST_RETRY_MAX_MS,
              OPERATOR_WATCHDOG_MANIFEST_RETRY_BASE_MS *
                2 ** Math.min(consecutiveErrors - 1, 30),
            );
          const lastError = errorMessage(failure);
          yield* Ref.set(state, {
            ...current,
            consecutiveErrors,
            retryAtMs,
            lastError,
          });
          yield* Effect.logWarning(
            `🐕 Operator watchdog could not verify the deployment manifest (${consecutiveErrors.toString()} in a row); no strike until it does, retrying at ${new Date(retryAtMs).toISOString()}: ${lastError}`,
          );
          if (
            current.verdict === "unverified" &&
            consecutiveErrors >= OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED_AFTER
          )
            yield* raiseLivenessIncident(
              globals,
              SOURCE,
              OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED,
              `deployment manifest verification failed ${consecutiveErrors.toString()} times in a row: ${lastError}`,
            );
          return {
            ok: false,
            reason:
              current.verdict === "mismatch"
                ? "manifest_mismatch"
                : "manifest_unverified",
            untilMs: retryAtMs,
          } as const;
        }
        if (!exit.value.ok) {
          const retryAtMs =
            nowMs + OPERATOR_WATCHDOG_MANIFEST_MISMATCH_RECHECK_MS;
          const detail = exit.value.mismatches.join("; ");
          yield* Ref.set(state, {
            verdict: "mismatch",
            consecutiveErrors: 0,
            retryAtMs,
            lastError: detail,
          });
          yield* raiseLivenessIncident(
            globals,
            SOURCE,
            OPERATOR_WATCHDOG_MANIFEST_MISMATCH,
            `deployment manifest verification failed (${detail}); the watchdog strikes nobody until the manifest matches, and re-reads it every ${OPERATOR_WATCHDOG_MANIFEST_MISMATCH_RECHECK_MS.toString()} ms`,
          );
          return {
            ok: false,
            reason: "manifest_mismatch",
            untilMs: retryAtMs,
          } as const;
        }
        yield* Ref.set(state, {
          verdict: "verified",
          consecutiveErrors: 0,
          retryAtMs: 0,
          lastError: undefined,
        });
        yield* clearLivenessIncident(globals, SOURCE);
        yield* Effect.logInfo(
          "🐕 Operator watchdog verified the deployment manifest.",
        );
        return { ok: true } as const;
      });
    const whileNoStrikeDue = (nowMs: number) =>
      Effect.gen(function* () {
        if (!reasonRaised(yield* Ref.get(state))) return undefined;
        yield* beforeStrike(nowMs);
        const after = yield* Ref.get(state);
        return reasonRaised(after) ? after.retryAtMs : undefined;
      });
    return { beforeStrike, whileNoStrikeDue, state: Ref.get(state) };
  });
