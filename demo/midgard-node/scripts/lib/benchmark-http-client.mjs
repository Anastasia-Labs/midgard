import { performance } from "node:perf_hooks";

import { Pool } from "undici";

/**
 * The synthetic client addresses of a spread: one per IPv4 /24, taken from
 * the benchmarking range 198.18.0.0/15 (RFC 2544), so no two share the
 * node's per-client or per-prefix bucket and none is a real client's.
 */
export const BENCHMARK_CLIENT_ADDRESS_MAX = 512;

export const benchmarkClientAddresses = (count) => {
  if (
    !Number.isSafeInteger(count) ||
    count < 1 ||
    count > BENCHMARK_CLIENT_ADDRESS_MAX
  )
    throw new Error(
      `a client address spread must be 1 to ${BENCHMARK_CLIENT_ADDRESS_MAX.toString()} addresses`,
    );
  return Array.from(
    { length: count },
    (_, index) =>
      `198.${(18 + (index >> 8)).toString()}.${(index & 255).toString()}.1`,
  );
};

/**
 * The client identities requests to the node's socket carry, in
 * `x-midgard-client-ip`. Each request takes the next address in turn that
 * has fewer than `activeMax` requests in flight, so every address carries an
 * even share of the rate and never more requests at once than the node lets
 * one client hold. A request that finds every address full waits for one,
 * as it would wait in the pool's queue for a connection.
 */
const createClientSpread = ({ count, activeMax }) => {
  const addresses = benchmarkClientAddresses(count);
  const active = new Uint32Array(count);
  const waiting = [];
  let next = 0;
  const take = () => {
    for (let step = 0; step < count; step += 1) {
      const index = (next + step) % count;
      if (active[index] < activeMax) {
        next = (index + 1) % count;
        active[index] += 1;
        let released = false;
        return {
          address: addresses[index],
          release: () => {
            if (released) return;
            released = true;
            active[index] -= 1;
            waiting.shift()?.(take());
          },
        };
      }
    }
    return undefined;
  };
  return {
    capacity: count * activeMax,
    acquire: () => {
      const client = take();
      return client === undefined
        ? new Promise((resolve) => waiting.push(resolve))
        : Promise.resolve(client);
    },
  };
};

/**
 * The stress engine's HTTP client: one undici pool per origin. With
 * `socketPath`, requests to `socketOrigin` (the node's endpoint) go over the
 * node's Unix socket, where the node takes each client's address from
 * `x-midgard-client-ip`; `clientSpread` then names the number of synthetic
 * clients the requests spread over, and `clientActiveMax` the requests each
 * may have in flight (the node's per-client cap). Every other origin, such as
 * the metrics endpoint, stays on TCP.
 */
export class BenchmarkHttpClient {
  constructor({
    connections,
    pipelining,
    timeoutMs,
    socketPath = null,
    socketOrigin = null,
    clientSpread = null,
    clientActiveMax = null,
  }) {
    this.connections = connections;
    this.pipelining = pipelining;
    this.timeoutMs = timeoutMs;
    this.pools = new Map();
    this.socketPath = socketPath;
    this.socketOrigin =
      socketOrigin === null ? null : new URL(socketOrigin).origin;
    if ((socketPath === null) !== (clientSpread === null))
      throw new Error(
        "the node socket and a client address spread go together: the node refuses a socket request that names no client, and reads the name only on its socket",
      );
    if (socketPath !== null && this.socketOrigin === null)
      throw new Error("a node socket needs the origin it stands for");
    if (clientSpread === null) {
      this.spread = null;
      return;
    }
    if (!Number.isSafeInteger(clientActiveMax) || clientActiveMax < 1)
      throw new Error("a client address spread needs a per-client cap");
    this.spread = createClientSpread({
      count: clientSpread,
      activeMax: clientActiveMax,
    });
    // At most `connections * pipelining` requests are in flight, so a spread
    // with room for that many never runs out of addresses.
    if (connections * pipelining > this.spread.capacity)
      throw new Error(
        `${(connections * pipelining).toString()} requests in flight exceed the ${this.spread.capacity.toString()} the client address spread can hold`,
      );
  }

  poolFor(url) {
    const parsed = new URL(url);
    let pool = this.pools.get(parsed.origin);
    if (pool === undefined) {
      pool = new Pool(parsed.origin, {
        connections: this.connections,
        pipelining: this.pipelining,
        headersTimeout: this.timeoutMs,
        bodyTimeout: this.timeoutMs,
        ...(this.socketPath !== null && parsed.origin === this.socketOrigin
          ? { connect: { socketPath: this.socketPath } }
          : {}),
      });
      this.pools.set(parsed.origin, pool);
    }
    return pool;
  }

  async request(url, options = {}) {
    const parsed = new URL(url);
    const pool = this.poolFor(url);
    const startedAt = performance.now();
    const client =
      this.spread !== null && parsed.origin === this.socketOrigin
        ? await this.spread.acquire()
        : null;
    try {
      const response = await pool.request({
        method: options.method ?? "GET",
        path: `${parsed.pathname}${parsed.search}`,
        headers:
          client === null
            ? options.headers
            : { ...options.headers, "x-midgard-client-ip": client.address },
        body: options.body,
        headersTimeout: this.timeoutMs,
        bodyTimeout: this.timeoutMs,
      });
      const text = await response.body.text();
      const latencyMs = performance.now() - startedAt;
      return {
        status: response.statusCode,
        ok: response.statusCode >= 200 && response.statusCode < 300,
        body: text,
        latencyMs,
        json() {
          return text.length === 0 ? {} : JSON.parse(text);
        },
      };
    } finally {
      client?.release();
    }
  }

  async close() {
    await Promise.all(Array.from(this.pools.values(), (pool) => pool.close()));
  }
}
