import { request } from "node:http";

import {
  HttpServer,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import {
  Config,
  ConfigProvider,
  Context,
  Deferred,
  Effect,
  Exit,
  Fiber,
  FiberId,
  Layer,
  Logger,
  Ref,
  Scope,
} from "effect";
import { describe, expect, it } from "vitest";

import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { startupStepFailed } from "../src/services/startup-waiting.js";
import {
  findTransientBudgetExhausted,
  signalTransientExhausted,
  type TransientBudgetExhaustedError,
} from "../src/services/transient-exhaustion.js";

class RuntimeDependency extends Context.Tag("StartupHttpTestRuntime")<
  RuntimeDependency,
  string
>() {}

const get = (url: string, method = "GET") => fetch(url, { method });
const silent = Logger.make(() => undefined);
const baseUrl = HttpServer.addressWith((address) =>
  address._tag === "TcpAddress"
    ? Effect.succeed(`http://127.0.0.1:${address.port}`)
    : Effect.die("Expected TCP server"),
);

/** Send headers without completing a declared body: a startup rejection must
 * return immediately without parsing or waiting for the submission body. */
const incompleteSubmission = (url: string) =>
  new Promise<number>((resolve, reject) => {
    const call = request(
      url,
      {
        method: "POST",
        headers: {
          "content-type": "application/json",
          "content-length": "1000000",
        },
      },
      (response) => {
        response.resume();
        resolve(response.statusCode!);
        call.destroy();
      },
    );
    call.on("error", reject);
    call.setTimeout(2000, () =>
      call.destroy(new Error("Startup parsed/waited for request body")),
    );
    call.flushHeaders();
  });

describe("early startup HTTP listener", () => {
  it("serves local probes and rejects all other routes without consuming bodies while startup is pending", async () => {
    await Effect.runPromise(
      withStartupHttpServer(0, () =>
        Effect.gen(function* () {
          const url = yield* baseUrl;
          const health = yield* Effect.promise(() => get(`${url}/healthz`));
          expect(health.status).toBe(200);
          expect(yield* Effect.promise(() => health.json())).toEqual({
            status: "ok",
            stage: "runtime_services",
          });
          const ready = yield* Effect.promise(() => get(`${url}/readyz`));
          expect(ready.status).toBe(503);
          expect(yield* Effect.promise(() => ready.json())).toEqual({
            ready: false,
            reasons: ["startup_incomplete"],
            stage: "runtime_services",
          });
          for (const path of [
            "init",
            "commit",
            "merge",
            "utxos",
            "protocol-info",
          ])
            expect(
              (yield* Effect.promise(() => get(`${url}/${path}`))).status,
            ).toBe(503);
          expect(
            yield* Effect.promise(() => incompleteSubmission(`${url}/submit`)),
          ).toBe(503);
        }),
      ),
    );
  });

  it("publishes one complete handler with its scoped dependency and fresh request context", async () => {
    const released: string[] = [];
    let url = "";
    await Effect.runPromise(
      withStartupHttpServer(0, (startup) =>
        Effect.gen(function* () {
          url = yield* baseUrl;
          const runtimeLayer = Layer.scoped(
            RuntimeDependency,
            Effect.acquireRelease(Effect.succeed("runtime-owned"), (value) =>
              Effect.sync(() => {
                released.push(value);
              }),
            ),
          );
          const full = Effect.gen(function* () {
            const dependency = yield* RuntimeDependency;
            const defaultConfig = yield* Config.string("HANDOFF_CONFIG");
            const req = yield* HttpServerRequest.HttpServerRequest;
            const requestScope = yield* Effect.scope;
            expect(requestScope).not.toBe(yield* Ref.get(startupScope));
            return HttpServerResponse.unsafeJson(
              { dependency, defaultConfig, path: req.url },
              { status: 201 },
            );
          });
          const startupScope = yield* Ref.make<Scope.Scope | undefined>(
            undefined,
          );
          const runtime = Effect.gen(function* () {
            yield* Ref.set(startupScope, yield* Effect.scope);
            yield* startup.publish(full);
            expect(released).toEqual([]);
            for (const path of ["/submit", "/init"]) {
              const response = yield* Effect.promise(() =>
                get(`${url}${path}`, "POST"),
              );
              expect(response.status).toBe(201);
              expect(yield* Effect.promise(() => response.json())).toEqual({
                dependency: "runtime-owned",
                defaultConfig: "runtime-config",
                path,
              });
            }
          }).pipe(
            Effect.provide(runtimeLayer),
            Effect.withConfigProvider(
              ConfigProvider.fromMap(
                new Map([["HANDOFF_CONFIG", "runtime-config"]]),
              ),
            ),
            Effect.scoped,
          );
          yield* runtime;
        }),
      ).pipe(
        Effect.provideService(RuntimeDependency, "early-listener"),
        Effect.withConfigProvider(
          ConfigProvider.fromMap(new Map([["HANDOFF_CONFIG", "early-config"]])),
        ),
      ),
    );
    expect(released).toEqual(["runtime-owned"]);
    await expect(get(`${url}/healthz`)).rejects.toThrow();
  });

  it("holds a startup that failed deterministically up and unready, live, without exposing its cause", async () => {
    const failure = new Error("private-provider-cause");
    const messages: unknown[] = [];
    const logger = Logger.make(({ message }) => {
      messages.push(message);
    });
    const listening = await Effect.runPromise(Deferred.make<string>());
    const running = Effect.runFork(
      withStartupHttpServer(0, (startup) =>
        Effect.gen(function* () {
          const url = yield* baseUrl;
          yield* startup.setStage("provider_assertions");
          const ready = yield* Effect.promise(() => get(`${url}/readyz`));
          expect(yield* Effect.promise(() => ready.json())).toEqual({
            ready: false,
            reasons: ["startup_incomplete"],
            stage: "provider_assertions",
          });
          yield* Deferred.succeed(listening, url);
          return yield* Effect.fail(failure);
        }),
      ).pipe(Effect.provide(Logger.replace(Logger.defaultLogger, logger))),
    );
    const url = await Effect.runPromise(Deferred.await(listening));
    const held = async () => {
      const ready = await get(`${url}/readyz`);
      return { status: ready.status, body: await ready.json() };
    };
    await expect.poll(held).toEqual({
      status: 503,
      body: {
        ready: false,
        reasons: ["startup_failed"],
        stage: "fatal",
        failedStage: "provider_assertions",
      },
    });
    const health = await get(`${url}/healthz`);
    expect(health.status).toBe(200);
    expect(await health.json()).toEqual({
      status: "held",
      stage: "fatal",
      failedStage: "provider_assertions",
    });
    expect(running.unsafePoll()).toBeNull();
    expect(messages.flat()).toEqual([
      "node_startup_failed stage=provider_assertions; the node stays up, unready, until it is restarted",
    ]);
    await Effect.runPromise(Fiber.interrupt(running));
    await expect(get(`${url}/healthz`)).rejects.toThrow();
  });

  it("holds a step that failed deterministically, naming its step and reason", async () => {
    const listening = await Effect.runPromise(Deferred.make<string>());
    const running = Effect.runFork(
      withStartupHttpServer(0, (startup) =>
        Effect.gen(function* () {
          yield* startup.setStage("provider_assertions");
          yield* Deferred.succeed(listening, yield* baseUrl);
          return yield* Effect.fail(
            startupStepFailed({
              step: "lucid",
              reason: "lucid_initialization_pending",
              cause: new Error("private-provider-cause"),
            }),
          );
        }),
      ).pipe(Effect.provide(Logger.replace(Logger.defaultLogger, silent))),
    );
    const url = await Effect.runPromise(Deferred.await(listening));
    await expect
      .poll(async () => (await get(`${url}/readyz`)).json())
      .toEqual({
        ready: false,
        reasons: ["startup_failed", "lucid_initialization_pending"],
        stage: "fatal",
        failedStage: "provider_assertions",
        failedStep: "lucid",
        failedReason: "lucid_initialization_pending",
      });
    expect((await get(`${url}/healthz`)).status).toBe(200);
    expect(running.unsafePoll()).toBeNull();
    await Effect.runPromise(Fiber.interrupt(running));
  });

  it("exits a startup whose step outlived its transient budget, closing the listener", async () => {
    let url = "";
    const messages: unknown[] = [];
    const logger = Logger.make(({ message }) => {
      messages.push(message);
    });
    const program = withStartupHttpServer(0, (startup) =>
      Effect.gen(function* () {
        url = yield* baseUrl;
        yield* startup.setStage("provider_assertions");
        return yield* Effect.fail(
          startupStepFailed({
            step: "lucid",
            reason: "lucid_initialization_pending",
            cause: new Error("private-provider-cause"),
            exhausted: true,
            attempts: 3,
          }),
        );
      }),
    );
    await expect(
      Effect.runPromise(
        program.pipe(
          Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
        ),
      ),
    ).rejects.toThrow("private-provider-cause");
    expect(messages.flat()).toEqual([
      "node_startup_failed stage=provider_assertions step=lucid reason=lucid_initialization_pending; a transient failure outlived its bound; the node exits non-zero",
    ]);
    await expect(get(`${url}/healthz`)).rejects.toThrow();
  }, 20_000);

  it("exits a node whose transient failure outlived its bound after startup", async () => {
    let url = "";
    const messages: unknown[] = [];
    const logger = Logger.make(({ message }) => {
      messages.push(message);
    });
    const exhaustion = Deferred.unsafeMake<
      never,
      TransientBudgetExhaustedError
    >(FiberId.none);
    const program = withStartupHttpServer(0, (startup) =>
      Effect.gen(function* () {
        url = yield* baseUrl;
        yield* startup.publish(
          Effect.succeed(HttpServerResponse.unsafeJson({ ready: true })),
        );
        signalTransientExhausted(exhaustion, {
          source: "driver",
          reason: "l1_events_ingestion_failed",
          detail: "the database did not answer",
        });
        return yield* Effect.raceFirst(
          Effect.never,
          Deferred.await(exhaustion),
        );
      }),
    );
    const exit = await Effect.runPromiseExit(
      program.pipe(
        Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
      ),
    );
    expect(
      Exit.isFailure(exit) && findTransientBudgetExhausted(exit.cause),
    ).toMatchObject({ source: "driver", reason: "l1_events_ingestion_failed" });
    expect(messages.flat()).toEqual([
      "node_transient_budget_exhausted stage=serving source=driver reason=l1_events_ingestion_failed; a transient failure outlived its bound; the node exits non-zero",
    ]);
    await expect(get(`${url}/healthz`)).rejects.toThrow();
  }, 20_000);

  it("does not share published state between node instances and closes on interruption", async () => {
    const entered = await Effect.runPromise(Deferred.make<string>());
    const running = Effect.runFork(
      withStartupHttpServer(0, (startup) =>
        Effect.gen(function* () {
          yield* startup.publish(
            Effect.succeed(HttpServerResponse.unsafeJson({ ready: true })),
          );
          yield* Deferred.succeed(entered, yield* baseUrl);
          yield* Effect.never;
        }),
      ),
    );
    const first = await Effect.runPromise(Deferred.await(entered));
    expect(await (await get(`${first}/readyz`)).json()).toEqual({
      ready: true,
    });
    await Effect.runPromise(
      withStartupHttpServer(0, () =>
        Effect.gen(function* () {
          const second = yield* baseUrl;
          expect(
            (yield* Effect.promise(() => get(`${second}/readyz`))).status,
          ).toBe(503);
        }),
      ),
    );
    await Effect.runPromise(Fiber.interrupt(running));
    await expect(get(`${first}/healthz`)).rejects.toThrow();
  });
});
