import { createServer } from "node:http";

import {
  HttpServer,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import type * as HttpApp from "@effect/platform/HttpApp";
import { NodeHttpServer } from "@effect/platform-node";
import {
  Cause,
  Context,
  DefaultServices,
  Effect,
  FiberRef,
  HashSet,
  Logger,
  Ref,
  Scope,
  Tracer,
} from "effect";

import {
  findStartupStepFailure,
  type StartupWaitingReport,
  StartupWaitingReporter,
} from "../services/startup-waiting.js";
import { findTransientBudgetExhausted } from "../services/transient-exhaustion.js";

/** Local startup state: no provider, contract, database or user payload reads. */
export type NodeStartupStage =
  | "runtime_services"
  | "local_preflight"
  | "instance_lock"
  | "database_initialization"
  | "protocol_initialization"
  | "provider_assertions"
  | "l1_follower_catch_up"
  | "follower_view_apply";

type StartupState = {
  readonly stage: NodeStartupStage | "serving" | "fatal";
  /** Named reasons the stage is waiting on, reported by `/readyz`. */
  readonly waitingOn?: readonly string[];
  /** Named reasons startup steps retrying under `retryStartupStep` wait on,
   * by step; reported by `/readyz` after the stage's. */
  readonly stepWaits?: ReadonlyMap<string, readonly string[]>;
  readonly failedStage?: NodeStartupStage | "serving";
  /** The startup step that failed and its named reason, when a
   * `StartupStepFailedError` ended the startup; the source and reason of a
   * `TransientBudgetExhaustedError`. */
  readonly failedStep?: string;
  readonly failedReason?: string;
  readonly application?: HttpApp.Default<unknown, Scope.Scope>;
  readonly runtimeDefaults?: Context.Context<DefaultServices.DefaultServices>;
  readonly runtimeLoggers?: HashSet.HashSet<Logger.Logger<unknown, unknown>>;
};

export type StartupHttp = {
  /** Enters `stage`, waiting on the named reasons `waitingOn` (none by default). */
  readonly setStage: (
    stage: NodeStartupStage,
    waitingOn?: readonly string[],
  ) => Effect.Effect<void>;
  readonly publish: <R>(
    application: HttpApp.Default<unknown, R>,
  ) => Effect.Effect<
    void,
    never,
    Exclude<R, HttpServerRequest.HttpServerRequest | Scope.Scope>
  >;
};

/** The failed stage, step and reason a fatal startup reports. */
const failure = (current: StartupState) => ({
  ...(current.failedStage === undefined
    ? {}
    : { failedStage: current.failedStage }),
  ...(current.failedStep === undefined
    ? {}
    : { failedStep: current.failedStep }),
  ...(current.failedReason === undefined
    ? {}
    : { failedReason: current.failedReason }),
});

/**
 * One listener and one atomic handoff per node instance.
 *
 * When `run` fails, `/readyz` answers `startup_failed` (with the failed
 * stage, step and reason) and the outcome follows the failure's class (owner
 * ruling 2026-10-09; plan §7.5): a transient failure that outlived its
 * budget (`StartupStepFailedError.exhausted`, `TransientBudgetExhaustedError`)
 * fails the effect, so the process exits non-zero and its supervisor's
 * restart is the backoff. Any other failure, deterministic or unknown, holds:
 * the listener keeps serving `startup_failed`, `/healthz` stays live, and
 * the effect never completes until it is interrupted, so a supervisor's
 * restart cannot loop on it.
 */
export const withStartupHttpServer = <A, E, R>(
  port: number,
  run: (startup: StartupHttp) => Effect.Effect<A, E, R>,
) =>
  Effect.gen(function* () {
    const state = yield* Ref.make<StartupState>({ stage: "runtime_services" });
    // The outer middleware and dispatcher share one snapshot per request,
    // including the monitoring defaults used before the HTTP tracing span.
    const requestState = yield* FiberRef.make<StartupState>({
      stage: "runtime_services",
    });
    const startup: StartupHttp = {
      setStage: (stage, waitingOn) =>
        Ref.update(state, (current) => ({
          stage,
          ...(waitingOn === undefined ? {} : { waitingOn }),
          ...(current.stepWaits === undefined
            ? {}
            : { stepWaits: current.stepWaits }),
        })),
      publish: (application) =>
        Effect.gen(function* () {
          // Keep every runtime service (including admission SQL) alive in the
          // enclosing scope. Each request supplies its own request, scope and
          // HTTP parent span; the publisher's startup span is not request context.
          const captured = Context.omit(
            HttpServerRequest.HttpServerRequest,
            Scope.Scope,
            Tracer.ParentSpan,
          )(
            yield* Effect.context<
              Exclude<
                Effect.Effect.Context<typeof application>,
                HttpServerRequest.HttpServerRequest | Scope.Scope
              >
            >(),
          );
          const bound = application.pipe(
            Effect.mapInputContext(
              (
                requestContext: Context.Context<
                  HttpServerRequest.HttpServerRequest | Scope.Scope
                >,
              ) =>
                Context.merge(requestContext, captured) as Context.Context<
                  Effect.Effect.Context<typeof application>
                >,
            ),
          );
          const runtimeDefaults = yield* FiberRef.get(
            DefaultServices.currentServices,
          );
          const runtimeLoggers = yield* FiberRef.get(FiberRef.currentLoggers);
          yield* Ref.set(state, {
            stage: "serving",
            application: bound,
            runtimeDefaults,
            runtimeLoggers,
          });
        }),
    };
    const dispatch = Effect.gen(function* () {
      const current = yield* FiberRef.get(requestState);
      if (current.application !== undefined) return yield* current.application;
      const request = yield* HttpServerRequest.HttpServerRequest;
      const path = request.url.split("?")[0];
      // Live while the process holds: a failed startup that does not exit
      // stays up, so a liveness probe must not restart it.
      if (request.method === "GET" && path === "/healthz")
        return HttpServerResponse.unsafeJson({
          status: current.stage === "fatal" ? "held" : "ok",
          stage: current.stage,
          ...failure(current),
        });
      return HttpServerResponse.unsafeJson(
        {
          ready: false,
          reasons: [
            current.stage === "fatal" ? "startup_failed" : "startup_incomplete",
            ...(current.failedReason === undefined
              ? []
              : [current.failedReason]),
            ...(current.waitingOn ?? []),
            ...[...(current.stepWaits?.values() ?? [])].flat(),
          ],
          stage: current.stage,
          ...failure(current),
        },
        { status: 503 },
      );
    });
    const withRuntimeDefaults = <E, R>(
      application: HttpApp.Default<E, R>,
    ): HttpApp.Default<E, R> =>
      Effect.gen(function* () {
        const snapshot = yield* Ref.get(state);
        let bound = application.pipe(Effect.locally(requestState, snapshot));
        if (snapshot.runtimeDefaults !== undefined)
          bound = bound.pipe(
            Effect.locally(
              DefaultServices.currentServices,
              snapshot.runtimeDefaults,
            ),
          );
        if (snapshot.runtimeLoggers !== undefined)
          bound = bound.pipe(
            Effect.locally(FiberRef.currentLoggers, snapshot.runtimeLoggers),
          );
        return yield* bound;
      });
    yield* HttpServer.serveEffect(dispatch, withRuntimeDefaults);
    const reportStepWaiting: StartupWaitingReport = (key, reasons) =>
      Ref.update(state, (current) => {
        const stepWaits = new Map(current.stepWaits);
        if (reasons.length === 0) stepWaits.delete(key);
        else stepWaits.set(key, reasons);
        return { ...current, stepWaits };
      });
    return yield* run(startup).pipe(
      Effect.locally(StartupWaitingReporter, reportStepWaiting),
      Effect.catchAllCause((cause) =>
        Cause.isInterruptedOnly(cause)
          ? Effect.failCause(cause)
          : Effect.gen(function* () {
              const current = yield* Ref.get(state);
              const step = findStartupStepFailure(cause);
              const spent = findTransientBudgetExhausted(cause);
              const exits = spent !== undefined || step?.exhausted === true;
              if (current.stage !== "fatal") {
                const named =
                  spent !== undefined
                    ? { failedStep: spent.source, failedReason: spent.reason }
                    : step === undefined
                      ? {}
                      : { failedStep: step.step, failedReason: step.reason };
                yield* Ref.set(state, {
                  stage: "fatal",
                  failedStage: current.stage,
                  ...named,
                });
                // Names only (stage, and step and reason when a step failed),
                // never the cause. An exit propagates the original error,
                // cause included, to CLI teardown, which exits non-zero.
                const outcome = exits
                  ? "a transient failure outlived its bound; the node exits non-zero"
                  : "the node stays up, unready, until it is restarted";
                yield* Effect.logError(
                  spent !== undefined
                    ? `node_transient_budget_exhausted stage=${current.stage} source=${spent.source} reason=${spent.reason}; ${outcome}`
                    : step === undefined
                      ? `node_startup_failed stage=${current.stage}; ${outcome}`
                      : `node_startup_failed stage=${current.stage} step=${step.step} reason=${step.reason}; ${outcome}`,
                );
              }
              if (exits) return yield* Effect.failCause(cause);
              return yield* Effect.never;
            }),
      ),
    );
  }).pipe(
    Effect.provide(NodeHttpServer.layer(createServer, { port })),
    Effect.scoped,
  );
