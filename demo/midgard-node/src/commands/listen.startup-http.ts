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

/** Local startup state: no provider, contract, database or user payload reads. */
export type NodeStartupStage =
  | "runtime_services"
  | "local_preflight"
  | "database_initialization"
  | "protocol_initialization"
  | "provider_assertions"
  | "history_initialization"
  | "history_sync"
  | "recovery_preparation";

type StartupState = {
  readonly stage: NodeStartupStage | "serving" | "fatal";
  readonly failedStage?: NodeStartupStage | "serving";
  readonly application?: HttpApp.Default<unknown, Scope.Scope>;
  readonly runtimeDefaults?: Context.Context<DefaultServices.DefaultServices>;
  readonly runtimeLoggers?: HashSet.HashSet<Logger.Logger<unknown, unknown>>;
};

export type StartupHttp = {
  readonly setStage: (stage: NodeStartupStage) => Effect.Effect<void>;
  readonly publish: <R>(
    application: HttpApp.Default<unknown, R>,
  ) => Effect.Effect<
    void,
    never,
    Exclude<R, HttpServerRequest.HttpServerRequest | Scope.Scope>
  >;
};

/** One listener and one atomic handoff per node instance. */
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
      setStage: (stage) => Ref.set(state, { stage }),
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
      if (request.method === "GET" && path === "/healthz")
        return HttpServerResponse.unsafeJson(
          {
            status: current.stage === "fatal" ? "error" : "ok",
            stage: current.stage,
            ...(current.failedStage === undefined
              ? {}
              : { failedStage: current.failedStage }),
          },
          { status: current.stage === "fatal" ? 503 : 200 },
        );
      return HttpServerResponse.unsafeJson(
        {
          ready: false,
          reasons: [
            current.stage === "fatal" ? "startup_failed" : "startup_incomplete",
          ],
          stage: current.stage,
          ...(current.failedStage === undefined
            ? {}
            : { failedStage: current.failedStage }),
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
    return yield* run(startup).pipe(
      Effect.tapErrorCause((cause) =>
        Cause.isInterruptedOnly(cause)
          ? Effect.void
          : Effect.gen(function* () {
              const current = yield* Ref.get(state);
              if (current.stage === "fatal") return;
              yield* Ref.set(state, {
                stage: "fatal",
                failedStage: current.stage,
              });
              // Bound stage only. The original error propagates to CLI teardown.
              yield* Effect.logError(
                `node_startup_failed stage=${current.stage}`,
              );
            }),
      ),
    );
  }).pipe(
    Effect.provide(NodeHttpServer.layer(createServer, { port })),
    Effect.scoped,
  );
