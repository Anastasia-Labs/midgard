/**
 * The throughput drill's two clients against the node's own admission
 * (`admitHttpRequest`: rate buckets per client, per prefix and global, and
 * the per-client in-flight cap), run in a local HTTP server. The public
 * ingress route must look like one client that the node never refuses; the
 * node route's synthetic clients must carry the open-loop load without any
 * per-client or per-prefix bucket refusing it.
 */
import { createServer, type IncomingMessage, type Server } from "node:http";
import { type AddressInfo } from "node:net";
import { join } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";

import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";
import { entropyToMnemonic } from "bip39";
import { Option } from "effect";
import {
  admitHttpRequest,
  createHttpIngress,
} from "midgard-node/commands/listen-http-ingress";
import { CLIENT_ACTIVE_MAX } from "midgard-node/commands/listen-http-rate-limit";
import {
  DEFAULT_HTTP_RATE_LIMITS,
  HTTP_RATE_CLASSES,
  type HttpRate,
  type HttpRateLimitPolicy,
} from "midgard-node/services/http-rate-limits";
import { getGlobalDispatcher, setGlobalDispatcher } from "undici";
import { afterEach, describe, expect, it } from "vitest";

import {
  installRouteHttpClient,
  nodeRateLimitsFromEnv,
  nodeRouteClientSpread,
  parseE2EL2StressConfig,
} from "../src/commands/e2e-stress-l2-throughput/index.js";

/**
 * The test's own limits as node variables: every tier of every class at
 * `rate`, in the node's `HTTP_RATE_LIMIT_<CLASS>` format (a burst and a
 * refill per tier the class has). The node's admission and the client both
 * take the policy the node resolves from them, as on a live run, and the
 * tests assert the client keeps inside it whatever its numbers.
 */
const uniformRateEnv = (rate: HttpRate): NodeJS.ProcessEnv =>
  Object.fromEntries(
    HTTP_RATE_CLASSES.map((kind) => [
      `HTTP_RATE_LIMIT_${kind.toUpperCase()}`,
      Object.values(DEFAULT_HTTP_RATE_LIMITS.classes[kind])
        .map(() => `${rate.burst.toString()},${rate.perSecond.toString()}`)
        .join(","),
    ]),
  );

const walletSeedPhrase = entropyToMnemonic("00".repeat(32));

type Observed = {
  readonly statuses: Map<number, number>;
  readonly maxActiveByClient: Map<string, number>;
};

/**
 * A server running the node's admission in front of a handler that holds
 * each admitted request for `holdMs`, recording every status it answers and
 * the most requests each client had admitted at once.
 */
const admissionServer = (
  policy: HttpRateLimitPolicy,
  socketPath: string | undefined,
  holdMs: number,
) => {
  const config = {
    HTTP_TRUSTED_PROXY_PEERS: [],
    HTTP_SOCKET_PATH: socketPath,
    HTTP_RATE_LIMITS: policy,
  };
  const controller = createHttpIngress({ policy });
  const observed: Observed = {
    statuses: new Map(),
    maxActiveByClient: new Map(),
  };
  const active = new Map<string, number>();
  const answer = (status: number) =>
    observed.statuses.set(status, (observed.statuses.get(status) ?? 0) + 1);
  const server = createServer((request: IncomingMessage, response) => {
    request.resume();
    const client =
      socketPath === undefined
        ? (request.socket.remoteAddress ?? "")
        : String(request.headers["x-midgard-client-ip"]);
    const decision = admitHttpRequest(
      {
        method: request.method!,
        url: request.url!,
        remoteAddress: Option.fromNullable(request.socket.remoteAddress),
        headers: Object.fromEntries(
          Object.entries(request.headers).map(([name, value]) => [
            name,
            Array.isArray(value) ? value.join(", ") : value,
          ]),
        ),
      },
      config,
      controller,
    );
    if (!decision.admitted) {
      answer(decision.status);
      response.writeHead(decision.status, decision.headers);
      response.end(JSON.stringify(decision.body));
      return;
    }
    const now = (active.get(client) ?? 0) + 1;
    active.set(client, now);
    observed.maxActiveByClient.set(
      client,
      Math.max(now, observed.maxActiveByClient.get(client) ?? 0),
    );
    void sleep(holdMs).then(() => {
      active.set(client, active.get(client)! - 1);
      decision.release();
      answer(200);
      response.writeHead(200, { "content-type": "application/json" });
      response.end("{}");
    });
  });
  return { server, observed };
};

const listen = async (server: Server, at: string | number) => {
  await new Promise<void>((resolve) =>
    typeof at === "string"
      ? server.listen(at, resolve)
      : server.listen(at, "127.0.0.1", resolve),
  );
  return server;
};

const servers: Server[] = [];
const makeTempDir = createTrackedTempDirFactory("midgard-stress-route-");
afterEach(async () => {
  await Promise.all(
    servers
      .splice(0)
      .map(
        (server) => new Promise((resolve) => server.close(() => resolve(0))),
      ),
  );
});

describe("throughput drill clients against the node's admission", () => {
  // A slow node makes the in-flight cap bind (the node answers a fifth
  // request 503); a fast one makes the rate bind (an unpaced client empties
  // a class's bucket within a second and the node answers 429).
  it.each([
    { binds: "the in-flight cap", holdMs: 500 },
    { binds: "every class's rate", holdMs: 5 },
  ])(
    "keeps the public-ingress route inside one client's limits when $binds would bind",
    async ({ holdMs }) => {
      // Bursts equal to the rate, so a client paced to its share never runs
      // a bucket dry and an unpaced one empties it within a second.
      const env = uniformRateEnv({ burst: 20, perSecond: 20 });
      const policy = nodeRateLimitsFromEnv(env);
      const { server, observed } = admissionServer(policy, undefined, holdMs);
      await listen(server, 0);
      servers.push(server);
      const { port } = server.address() as AddressInfo;
      const endpoint = `http://127.0.0.1:${port.toString()}`;
      const config = parseE2EL2StressConfig({
        route: "public-ingress",
        endpoint: `https://127.0.0.1:${port.toString()}`,
        walletSeedPhrase,
        env,
      });
      expect(config.route?.kind).toBe("public-ingress");
      const previous = getGlobalDispatcher();
      // The ingress terminates TLS; this server stands for the node behind it.
      installRouteHttpClient({ ...config, nodeEndpoint: endpoint }, {});
      try {
        // More of each class than its burst, all at once, as the closed loop's
        // workers and finality observer would issue them.
        const perClass = policy.classes.read.peer.burst + 5;
        const responses = await Promise.all([
          ...Array.from({ length: perClass }, (_, index) =>
            fetch(`${endpoint}/tx-status?tx_hash=${index.toString(16)}`),
          ),
          ...Array.from({ length: perClass }, () =>
            fetch(`${endpoint}/submit`, {
              method: "POST",
              body: new Uint8Array(32),
            }),
          ),
        ]);
        await Promise.all(responses.map((response) => response.text()));
        expect(Object.fromEntries(observed.statuses)).toEqual({
          200: 2 * perClass,
        });
        const [maxActive, ...otherClients] =
          observed.maxActiveByClient.values();
        expect(otherClients).toEqual([]);
        expect(maxActive).toBeLessThanOrEqual(CLIENT_ACTIVE_MAX);
      } finally {
        const installed = getGlobalDispatcher();
        setGlobalDispatcher(previous);
        await installed.close();
      }
    },
    30_000,
  );

  it("spreads the node route's open-loop load over enough synthetic clients that no client or prefix bucket refuses it", async () => {
    const policy = nodeRateLimitsFromEnv(
      uniformRateEnv({ burst: 10, perSecond: 10 }),
    );
    // Ten times one client's rate; the write class has no global bucket.
    const targetRateTps = 100;
    const socketPath = join(await makeTempDir(), "node.sock");
    const { server, observed } = admissionServer(policy, socketPath, 20);
    await listen(server, socketPath);
    servers.push(server);
    const maxInFlight = 16;
    const clientSpread = nodeRouteClientSpread({
      targetRateTps,
      maxInFlight,
      rateLimits: policy,
    });
    const { BenchmarkHttpClient } = (await import(
      new URL(
        "../../midgard-node/scripts/lib/benchmark-http-client.mjs",
        import.meta.url,
      ).href
    )) as {
      BenchmarkHttpClient: new (options: Record<string, unknown>) => {
        request: (
          url: string,
          options: Record<string, unknown>,
        ) => Promise<{ status: number }>;
        close: () => Promise<void>;
      };
    };
    const endpoint = "http://localhost";
    // As the engine builds it for the node route (engine.ts).
    const client = new BenchmarkHttpClient({
      connections: maxInFlight,
      pipelining: 1,
      timeoutMs: 10_000,
      socketPath,
      socketOrigin: endpoint,
      clientSpread,
      clientActiveMax: CLIENT_ACTIVE_MAX,
    });
    try {
      // Open loop: one submission every 1/rate seconds for two seconds,
      // whatever the answers.
      const sent: Promise<{ status: number }>[] = [];
      const intervalMs = 1_000 / targetRateTps;
      const startedAt = performance.now();
      for (let index = 0; index < 2 * targetRateTps; index += 1) {
        const due = startedAt + index * intervalMs;
        const wait = due - performance.now();
        if (wait > 0) await sleep(wait);
        sent.push(
          client.request(`${endpoint}/submit`, {
            method: "POST",
            body: new Uint8Array(32),
            headers: { "content-type": "application/octet-stream" },
          }),
        );
      }
      await Promise.all(sent);
      expect(Object.fromEntries(observed.statuses)).toEqual({
        200: 2 * targetRateTps,
      });
      // One client per /24, and none above the node's in-flight cap.
      const prefixes = new Set(
        [...observed.maxActiveByClient.keys()].map((address) =>
          address.split(".").slice(0, 3).join("."),
        ),
      );
      expect(prefixes.size).toBe(clientSpread);
      expect(
        Math.max(...observed.maxActiveByClient.values()),
      ).toBeLessThanOrEqual(CLIENT_ACTIVE_MAX);
    } finally {
      await client.close();
    }
  }, 30_000);
});
