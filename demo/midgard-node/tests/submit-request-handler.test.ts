/**
 * The raw `POST /submit` handler on a real HTTP server: the ingress answers
 * the router gives every route, the submission answers, the dispatch between
 * it and the Effect application, and a junk flood's log volume.
 */
import { HttpRouter, HttpServer, HttpServerResponse } from "@effect/platform";
import { Effect, Layer } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { SUBMIT_HTTP_BODY_MAX_BYTES } from "../src/commands/http/submission-capacity.js";
import { makeSubmitRequestHandler } from "../src/commands/http/submit.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { withHttpIngress } from "../src/commands/listen-http-ingress.js";
import { DEFAULT_HTTP_RATE_LIMITS } from "../src/services/http-rate-limits.js";
import { Globals } from "../src/services/index.js";
import { acquireSubmitCheckPool } from "../src/services/submit-check-pool.acquire-submit-check-pool.js";
import {
  makeNativeSubmitTx,
  wrapNativeSubmitTx,
} from "./database.test/fixtures.submission.js";
import {
  CLIENT,
  closeSharedPool,
  configLayer,
  lucidLayer,
  MEDIA,
  noLimits,
  refusals,
  runCleanups,
  serveSubmit,
  stubWriter,
} from "./helpers/submit-request-server.js";

afterEach(runCleanups);
afterAll(closeSharedPool);

describe("raw POST /submit handler", () => {
  it("admits a submission and answers with the admission ack", async () => {
    const node = await serveSubmit();
    const tx = makeNativeSubmitTx();
    const reply = await node.call({
      body: wrapNativeSubmitTx(tx.txCanonicalCbor),
    });
    expect(reply.status).toBe(202);
    expect(reply.headers["content-type"]).toBe("application/json");
    expect(reply.headers.connection).not.toBe("close");
    expect(reply.body).toEqual({
      txId: tx.txIdHex,
      status: "queued",
      firstSeenAt: expect.any(String),
      lastSeenAt: expect.any(String),
      duplicate: false,
    });
  });

  it("answers the ingress refusals with the codes and bodies every route gives", async () => {
    const node = await serveSubmit();
    expect(
      await node.call({ chunked: true, body: Buffer.from("8200", "hex") }),
    ).toMatchObject({
      status: 411,
      headers: { connection: "close" },
      body: { error: "Content-Length is required" },
    });
    expect(
      await node.call({
        headers: { "content-length": String(SUBMIT_HTTP_BODY_MAX_BYTES + 1) },
      }),
    ).toMatchObject({
      status: 413,
      headers: { connection: "close" },
      body: { error: "Request body exceeds endpoint limit" },
    });
    expect(await node.call({ client: null })).toMatchObject({
      status: 503,
      headers: { "retry-after": "5" },
      body: {
        error: "Socket peer identity is unavailable",
        error_code: "E_HTTP_CLIENT_IDENTITY_UNAVAILABLE",
      },
    });
    expect(
      await node.call({ path: "/submit?".padEnd(2_100, "a") }),
    ).toMatchObject({
      status: 414,
      body: { error: "Request target is too long" },
    });
  });

  it("answers media-type and check refusals as the router did", async () => {
    const node = await serveSubmit();
    const tx = makeNativeSubmitTx();
    expect(
      await node.call({
        body: wrapNativeSubmitTx(tx.txCanonicalCbor),
        headers: { "content-type": "application/json" },
      }),
    ).toMatchObject({
      status: 415,
      headers: { connection: "close" },
      body: {
        error: `V1 requests must be a canonical proof submission envelope with Content-Type ${MEDIA}`,
      },
    });
    expect(await node.call({ body: Buffer.from("ff00ff", "hex") })).toEqual({
      status: 400,
      headers: expect.objectContaining({ connection: "close" }),
      body: { error: "Invalid canonical V1 submission envelope" },
    });
    // An envelope around bytes that are not a canonical transaction.
    const refused = await node.call({
      body: wrapNativeSubmitTx(Buffer.from("8200", "hex")),
    });
    expect(refused.status).toBe(400);
    expect(refused.body.error).toEqual(expect.any(String));
  });

  it("keeps the connection on a rate refusal, with Retry-After", async () => {
    const node = await serveSubmit({
      enabled: true,
      classes: {
        ...DEFAULT_HTTP_RATE_LIMITS.classes,
        write: {
          peer: { burst: 1, perSecond: 0.001 },
          global: { burst: 1_000, perSecond: 1_000 },
        },
      },
    });
    const body = wrapNativeSubmitTx(makeNativeSubmitTx().txCanonicalCbor);
    expect((await node.call({ body })).status).toBe(202);
    const limited = await node.call({ body });
    expect(limited).toMatchObject({
      status: 429,
      body: {
        error: "Request rate exceeded",
        error_code: "E_HTTP_RATE_LIMITED",
      },
    });
    expect(limited.headers["retry-after"]).toEqual(expect.any(String));
    expect(limited.headers.connection).not.toBe("close");
    expect((await node.call({ body })).status).toBe(429);
    expect(node.connections()).toBe(1);
  });

  it("counts a junk flood by reason and logs a few lines a second", async () => {
    const node = await serveSubmit();
    const junk = Buffer.from("ff00ff", "hex");
    const before = await refusals(node.runtime, "malformed_envelope");
    const startedAt = Date.now();
    // About 1,000 refusals a second for two seconds.
    for (let batch = 0; batch < 40; batch += 1) {
      const due = startedAt + batch * 50;
      const wait = due - Date.now();
      if (wait > 0) await new Promise((resolve) => setTimeout(resolve, wait));
      const replies = await Promise.all(
        Array.from({ length: 50 }, () => node.call({ body: junk })),
      );
      expect(replies.every((reply) => reply.status === 400)).toBe(true);
    }
    const seconds = (Date.now() - startedAt) / 1_000;
    expect(seconds).toBeLessThan(6);
    expect((await refusals(node.runtime, "malformed_envelope")) - before).toBe(
      2_000,
    );
    const logged = node.lines.filter((line) => line.includes("Submit refused"));
    // Every line is the flood's one reason, most of them counting the
    // refusals sampled away since the line before.
    expect(
      logged.every((line) => line.includes("(malformed_envelope)")),
      JSON.stringify(logged),
    ).toBe(true);
    expect(
      logged.some((line) => /\d+ more malformed_envelope/u.test(line)),
    ).toBe(true);
    // The sampler logs one line per reason per wall-clock second: about 3
    // here. The bound allows twice that: one loaded run logged 5 lines over
    // a 2 s flood, which with a single reason means the wall clock stepped.
    expect(logged.length, JSON.stringify(logged)).toBeLessThanOrEqual(
      2 * Math.ceil(seconds) + 1,
    );
  });
});

describe("startup listener dispatch", () => {
  it("sends POST /submit to the raw handler and every other request to the application", async () => {
    let applicationCalls = 0;
    const application = withHttpIngress(
      HttpRouter.empty.pipe(
        HttpRouter.get(
          "/healthz",
          Effect.sync(() => {
            applicationCalls += 1;
            return HttpServerResponse.unsafeJson({ ok: true });
          }),
        ),
      ),
    );
    const services = Layer.mergeAll(
      configLayer(noLimits),
      lucidLayer,
      Globals.Default,
      stubWriter,
    );
    await Effect.runPromise(
      withStartupHttpServer(0, (startup) =>
        Effect.gen(function* () {
          // As the node wires it: the pool lives in the startup scope.
          yield* startup.publish(
            application,
            Effect.flatMap(acquireSubmitCheckPool, (checkPool) =>
              makeSubmitRequestHandler({
                wakeTxQueueProcessor: Effect.void,
                checkPool,
              }),
            ),
          );
          const port = yield* HttpServer.addressWith((address) =>
            address._tag === "TcpAddress"
              ? Effect.succeed(address.port)
              : Effect.die("expected TCP"),
          );
          const url = `http://127.0.0.1:${port.toString()}`;
          const tx = makeNativeSubmitTx();
          const submitted = yield* Effect.promise(() =>
            fetch(`${url}/submit`, {
              method: "POST",
              headers: { "content-type": MEDIA, "x-midgard-client-ip": CLIENT },
              body: new Uint8Array(wrapNativeSubmitTx(tx.txCanonicalCbor)),
            }),
          );
          expect(submitted.status).toBe(202);
          expect(
            (
              (yield* Effect.promise(() => submitted.json())) as {
                txId: string;
              }
            ).txId,
          ).toBe(tx.txIdHex);
          const wrongMethod = yield* Effect.promise(() =>
            fetch(`${url}/submit`, {
              headers: { "x-midgard-client-ip": CLIENT },
            }),
          );
          expect(wrongMethod.status).toBe(405);
          expect(wrongMethod.headers.get("allow")).toBe("POST");
          const health = yield* Effect.promise(() =>
            fetch(`${url}/healthz`, {
              headers: { "x-midgard-client-ip": CLIENT },
            }),
          );
          expect(health.status).toBe(200);
          expect(applicationCalls).toBe(1);
        }),
      ).pipe(Effect.provide(services)),
    );
  });
});
