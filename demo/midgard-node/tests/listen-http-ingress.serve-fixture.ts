import { once } from "node:events";
import {
  Agent,
  type ClientRequest,
  createServer,
  request as httpRequest,
} from "node:http";
import type { AddressInfo } from "node:net";

import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { Effect } from "effect";

import {
  createHttpIngress,
  withHttpIngress,
} from "../src/commands/listen-http-ingress.js";
import { NodeConfig } from "../src/services/config.js";
import {
  DEFAULT_HTTP_RATE_LIMITS,
  type HttpRateLimitPolicy,
} from "../src/services/http-rate-limits.js";

export type Reply = {
  readonly status: number;
  readonly retryAfter: string | undefined;
  readonly body: Record<string, unknown>;
};

export const policyWith = (
  classes: Partial<HttpRateLimitPolicy["classes"]>,
  enabled = true,
): HttpRateLimitPolicy => ({
  enabled,
  classes: { ...DEFAULT_HTTP_RATE_LIMITS.classes, ...classes },
});

const servers: { close: () => Promise<void> }[] = [];
/** Register with `afterEach` in every file that calls `serve`. */
export const closeServers = async () => {
  for (const server of servers.splice(0)) await server.close();
};

/** Real HTTP server whose every route sits behind the router's ingress layer. */
export const serve = async (
  options: {
    readonly policy?: HttpRateLimitPolicy;
    readonly trustedProxyPeers?: readonly string[];
  } = {},
) => {
  let clock = 0;
  let invoked = 0;
  const ingress = createHttpIngress({
    now: () => clock,
    policy: options.policy ?? DEFAULT_HTTP_RATE_LIMITS,
  });
  const handler = Effect.gen(function* () {
    const request = yield* HttpServerRequest.HttpServerRequest;
    if (request.method === "POST") yield* request.arrayBuffer;
    invoked += 1;
    return yield* HttpServerResponse.json({ ok: true });
  });
  const server = createServer(
    Effect.runSync(
      NodeHttpServer.makeHandler(
        withHttpIngress(handler, ingress).pipe(
          Effect.provideService(NodeConfig, {
            HTTP_TRUSTED_PROXY_PEERS: options.trustedProxyPeers ?? [],
          } as never),
        ),
      ),
    ),
  );
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const port = (server.address() as AddressInfo).port;
  const agent = new Agent({ keepAlive: true, maxSockets: 1 });
  const held: ClientRequest[] = [];
  servers.push({
    close: () =>
      new Promise<void>((resolve, reject) => {
        for (const req of held.splice(0)) req.destroy();
        server.closeAllConnections();
        agent.destroy();
        server.close((error) => (error ? reject(error) : resolve()));
      }),
  });
  const call = (
    path: string,
    {
      method = "GET",
      client,
      headers = {},
    }: {
      readonly method?: string;
      readonly client?: string;
      readonly headers?: Record<string, string>;
    } = {},
  ) =>
    new Promise<Reply>((resolve, reject) => {
      const req = httpRequest(
        {
          hostname: "127.0.0.1",
          port,
          path,
          method,
          agent,
          headers: {
            ...(method === "POST"
              ? { "content-type": "application/json", "content-length": "0" }
              : {}),
            ...(client === undefined ? {} : { "x-midgard-client-ip": client }),
            ...headers,
          },
        },
        (res) => {
          const chunks: Buffer[] = [];
          res.on("data", (chunk: Buffer) => chunks.push(chunk));
          res.once("end", () =>
            resolve({
              status: res.statusCode!,
              retryAfter: res.headers["retry-after"] as string | undefined,
              body: JSON.parse(Buffer.concat(chunks).toString("utf8")),
            }),
          );
        },
      );
      req.once("error", reject);
      req.end();
    });
  const statuses = async (
    count: number,
    path: string,
    options: Parameters<typeof call>[1] = {},
  ) => {
    const result: number[] = [];
    for (let index = 0; index < count; index += 1)
      result.push((await call(path, options)).status);
    return result;
  };
  /** Opens a request whose one-byte body never arrives, so it keeps its slot. */
  const hold = (
    path: string,
    { method = "POST", client }: { method?: string; client?: string } = {},
  ) => {
    const req = httpRequest({
      hostname: "127.0.0.1",
      port,
      path,
      method,
      agent: false,
      headers: {
        "content-type": "application/json",
        "content-length": "1",
        ...(client === undefined ? {} : { "x-midgard-client-ip": client }),
      },
    });
    req.on("error", () => undefined);
    req.flushHeaders();
    held.push(req);
  };
  const waitFor = async (predicate: () => boolean) => {
    const deadline = Date.now() + 5_000;
    while (!predicate()) {
      if (Date.now() > deadline) throw new Error("condition not reached");
      await new Promise((resolve) => setTimeout(resolve, 5));
    }
  };
  return {
    call,
    statuses,
    hold,
    waitFor,
    releaseHeld: () => {
      for (const req of held.splice(0)) req.destroy();
    },
    ingress,
    advance: (ms: number) => {
      clock += ms;
    },
    invoked: () => invoked,
  };
};

export const proxied = { trustedProxyPeers: ["127.0.0.1"] } as const;
