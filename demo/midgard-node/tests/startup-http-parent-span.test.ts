import { HttpServer, HttpServerResponse } from "@effect/platform";
import { Effect, Tracer } from "effect";
import { expect, it } from "vitest";

import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";

it.each([false, true])(
  "parents handler spans to their own HTTP request when publisher has span=%s",
  async (withPublisherSpan) => {
    const records: {
      name: string;
      spanId: string;
      parentName: string | undefined;
      parentId: string | undefined;
    }[] = [];
    await Effect.runPromise(
      Effect.gen(function* () {
        const base = yield* Effect.tracer;
        const tracer = Tracer.make({
          span: (...args) => {
            const span = base.span(...args);
            const parent = args[1]._tag === "Some" ? args[1].value : undefined;
            records.push({
              name: args[0],
              spanId: span.spanId,
              parentName: parent?._tag === "Span" ? parent.name : undefined,
              parentId: parent?.spanId,
            });
            return span;
          },
          context: (f, fiber) => base.context(f, fiber),
        });
        yield* withStartupHttpServer(0, (startup) => {
          const run = Effect.gen(function* () {
            const url = yield* HttpServer.addressWith((address) =>
              address._tag === "TcpAddress"
                ? Effect.succeed(`http://127.0.0.1:${address.port}`)
                : Effect.die("TCP expected"),
            );
            yield* startup.publish(
              Effect.gen(function* () {
                yield* Effect.void.pipe(Effect.withSpan("handler-child"));
                return HttpServerResponse.unsafeJson({ ok: true });
              }),
            );
            for (let request = 0; request < 2; request += 1) {
              const response = yield* Effect.promise(() =>
                fetch(`${url}/readyz`),
              );
              expect(response.status).toBe(200);
              expect(yield* Effect.promise(() => response.json())).toEqual({
                ok: true,
              });
            }
            const servers = records.filter(
              ({ name }) => name === "http.server GET",
            );
            const handlers = records.filter(
              ({ name }) => name === "handler-child",
            );
            expect(servers).toHaveLength(2);
            expect(handlers).toHaveLength(2);
            expect(new Set(servers.map(({ spanId }) => spanId)).size).toBe(2);
            expect(handlers.map(({ parentName }) => parentName)).toEqual([
              "http.server GET",
              "http.server GET",
            ]);
            expect(handlers.map(({ parentId }) => parentId)).toEqual(
              servers.map(({ spanId }) => spanId),
            );
          });
          return (
            withPublisherSpan ? run.pipe(Effect.withSpan("node-runtime")) : run
          ).pipe(Effect.withTracer(tracer));
        });
      }),
    );
  },
);
