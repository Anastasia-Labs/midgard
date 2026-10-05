import { createHash } from "node:crypto";
import { createServer } from "node:http";
import { createServer as createTcpServer, type Socket } from "node:net";

import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  committeeBoundReadContext,
  committeeOwnedReadTransports,
  registerCommitteeReadOwner,
} from "../src/availability/committee-owned-read-transports.js";
import {
  committeeScopedFetch,
  committeeScopedOgmiosRpc,
} from "../src/availability/scoped-transports.js";

const limits = {
  requestRefusalMs: 1000,
  httpResponseBytes: 4096,
  webSocketMessageBytes: 4096,
  rawUtxos: 32,
};
const serverFixture = async (webSocket: boolean) => {
  const peers = new Set<Socket>();
  const server = createServer((_request, response) => {
    response.writeHead(200, { "content-type": "application/json" });
    response.write("["); // An incomplete streamed response never finishes.
  });
  server.on("connection", (socket) => {
    peers.add(socket);
    socket.on("close", () => peers.delete(socket));
  });
  if (webSocket)
    server.on("upgrade", (request, socket) => {
      const accept = createHash("sha1")
        .update(
          `${request.headers["sec-websocket-key"]}258EAFA5-E914-47DA-95CA-C5AB0DC85B11`,
        )
        .digest("base64");
      socket.write(
        `HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ${accept}\r\n\r\n`,
      );
      // Consume frames but never answer an RPC or acknowledge a close frame.
      socket.on("data", () => undefined);
    });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address();
  if (!address || typeof address === "string")
    throw new Error("Missing TCP address");
  return {
    url: `http://127.0.0.1:${address.port}`,
    close: async () => {
      for (const socket of peers) socket.destroy();
      await new Promise<void>((resolve) => server.close(() => resolve()));
    },
  };
};
const boundedJoin = async (join: Promise<void>): Promise<void> => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    await Promise.race([
      join,
      new Promise<never>((_resolve, reject) => {
        timer = setTimeout(
          () => reject(new Error("Owned local transport did not join")),
          500,
        );
      }),
    ]);
  } finally {
    if (timer) clearTimeout(timer);
  }
};

const echoFixture = async () => {
  const peers = new Set<Socket>();
  let requests = 0;
  const server = createServer((request, response) => {
    void (async () => {
      requests += 1;
      const chunks: Buffer[] = [];
      for await (const chunk of request) chunks.push(Buffer.from(chunk));
      response.setHeader("content-type", "application/json");
      response.end(
        JSON.stringify({
          method: request.method,
          headers: request.headers,
          body: Buffer.concat(chunks).toString(),
        }),
      );
    })().catch((error: unknown) =>
      response.destroy(
        error instanceof Error ? error : new Error(String(error)),
      ),
    );
  });
  server.on("connection", (socket) => {
    peers.add(socket);
    socket.on("close", () => peers.delete(socket));
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address();
  if (!address || typeof address === "string")
    throw new Error("Missing TCP address");
  const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 5000 });
  const owner = committeeOwnedReadTransports();
  registerCommitteeReadOwner(scope, owner);
  return {
    scope,
    owner,
    requests: () => requests,
    url: `http://127.0.0.1:${address.port}`,
    close: async () => {
      scope.close();
      await owner.drain();
      owner.assertDrained();
      for (const socket of peers) socket.destroy();
      await new Promise<void>((resolve) => server.close(() => resolve()));
    },
  };
};

describe("configured committee owned transport lifetime", () => {
  it("rejects unsupported streaming keepalive bodies before allocating HTTP resources", async () => {
    const f = await echoFixture();
    try {
      await expect(
        f.owner.fetchFor(f.scope)(
          new Request(f.url, { method: "POST", body: "body", keepalive: true }),
        ),
      ).rejects.toThrow("streaming keepalive");
      f.owner.assertDrained();
      expect(f.requests()).toBe(0);
    } finally {
      await f.close();
    }
  });
  it("reuses the parent owner after its first requesting child has closed", async () => {
    const f = await echoFixture();
    const first = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 1000,
      signal: f.scope.signal,
    });
    const second = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 1000,
      signal: f.scope.signal,
    });
    try {
      expect((await f.owner.fetchFor(first)(f.url)).status).toBe(200);
      first.close();
      const result = await f.owner.fetchFor(second)(f.url);
      expect(result.status).toBe(200);
      await result.json();
      expect(f.requests()).toBe(2);
    } finally {
      first.close();
      second.close();
      await f.close();
    }
  });
  it("joins a held WebSocket locally without a peer close acknowledgement", async () => {
    const server = await serverFixture(true);
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 5000 });
    const owner = committeeOwnedReadTransports();
    registerCommitteeReadOwner(scope, owner);
    try {
      const rpc = await committeeScopedOgmiosRpc(server.url, scope, limits);
      const waiting = rpc.request("nextBlock", {});
      const refused = expect(waiting).rejects.toThrow("closed");
      rpc.close();
      await boundedJoin(owner.drain());
      owner.assertDrained();
      await refused;
    } finally {
      scope.close();
      await server.close();
      await owner.drain();
    }
  });

  it("destroys and joins its HTTP pool after expiry during a streamed body", async () => {
    const server = await serverFixture(false);
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 100 });
    const owner = committeeOwnedReadTransports();
    registerCommitteeReadOwner(scope, owner);
    try {
      await expect(
        committeeScopedFetch(scope, limits)(server.url),
      ).rejects.toThrow();
      await boundedJoin(owner.drain());
      owner.assertDrained();
    } finally {
      scope.close();
      await server.close();
      await owner.drain();
    }
  });

  it("carries the parent's transport owner into the SDK child observation", async () => {
    const server = await serverFixture(true);
    const parent = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 5000,
    });
    const child = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 5000,
      signal: parent.signal,
    });
    const owner = committeeOwnedReadTransports();
    registerCommitteeReadOwner(parent, owner);
    try {
      const context = committeeBoundReadContext(
        {
          assertActuationCurrent: async (scope) => {
            if (!scope) throw new Error("Missing child scope");
            const rpc = await committeeScopedOgmiosRpc(
              server.url,
              scope,
              limits,
            );
            rpc.close();
          },
        } as SDK.DaAvailabilityOperationContext,
        parent,
      );
      await context.assertActuationCurrent(child);
      await boundedJoin(owner.drain());
      owner.assertDrained();
    } finally {
      child.close();
      parent.close();
      await server.close();
      await owner.drain();
    }
  });
  it("accepts a native Node Request POST with its headers and exact body", async () => {
    const f = await echoFixture();
    try {
      const input = new Request(f.url, {
        method: "POST",
        headers: { "x-candidate": "bound" },
        body: "exact-candidate",
      });
      const reply = await committeeScopedFetch(f.scope, limits)(input);
      expect(await reply.json()).toMatchObject({
        method: "POST",
        headers: { "x-candidate": "bound" },
        body: "exact-candidate",
      });
      await f.owner.drain();
      f.owner.assertDrained();
    } finally {
      await f.close();
    }
  });

  it("applies native Request init overrides to method, headers and body", async () => {
    const f = await echoFixture();
    try {
      const input = new Request(f.url, {
        method: "POST",
        headers: { "x-original": "old" },
        body: "old",
      });
      const reply = await committeeScopedFetch(f.scope, limits)(input, {
        method: "PUT",
        headers: { "x-replacement": "new" },
        body: "new",
      });
      const value = (await reply.json()) as {
        method: string;
        headers: Record<string, string>;
        body: string;
      };
      expect(value).toMatchObject({
        method: "PUT",
        headers: { "x-replacement": "new" },
        body: "new",
      });
      expect(value.headers).not.toHaveProperty("x-original");
      await f.owner.drain();
      f.owner.assertDrained();
    } finally {
      await f.close();
    }
  });

  it("allows init.signal to replace a Request signal while preserving its scope", async () => {
    const f = await echoFixture();
    try {
      const old = new AbortController();
      old.abort(new Error("Original Request aborted"));
      const input = new Request(f.url, { signal: old.signal });
      const reply = await committeeScopedFetch(f.scope, limits)(input, {
        signal: new AbortController().signal,
      });
      expect(reply.ok).toBe(true);
      await f.owner.drain();
      f.owner.assertDrained();
    } finally {
      await f.close();
    }
  });

  it("refuses a consumed native Request body before opening an owned connection", async () => {
    const f = await echoFixture();
    try {
      const input = new Request(f.url, { method: "POST", body: "consumed" });
      await input.text();
      await expect(
        committeeScopedFetch(f.scope, limits)(input),
      ).rejects.toThrow();
      expect(f.requests()).toBe(0);
      f.owner.assertDrained();
    } finally {
      await f.close();
    }
  });

  it("keeps scope expiry authoritative over Request signal overrides", async () => {
    const f = await echoFixture();
    try {
      f.scope.close();
      await expect(
        committeeScopedFetch(f.scope, limits)(new Request(f.url), {
          signal: new AbortController().signal,
        }),
      ).rejects.toThrow();
      expect(f.requests()).toBe(0);
      f.owner.assertDrained();
    } finally {
      await f.close();
    }
  });

  it.each(["https", "wss"])(
    "joins the provisional %s socket while its peer withholds the TLS handshake",
    async (protocol) => {
      const peers = new Set<Socket>();
      const server = createTcpServer((socket) => {
        peers.add(socket);
        socket.on("data", () => undefined);
        socket.once("close", () => peers.delete(socket));
      });
      await new Promise<void>((resolve) =>
        server.listen(0, "127.0.0.1", resolve),
      );
      const address = server.address();
      if (!address || typeof address === "string")
        throw new Error("Missing TCP address");
      const scope = SDK.createDaAvailabilityReadScope({
        attemptTimeoutMs: 5000,
      });
      const owner = committeeOwnedReadTransports();
      registerCommitteeReadOwner(scope, owner);
      try {
        const endpoint = `${protocol}://127.0.0.1:${address.port}`;
        const capped = { ...limits, requestRefusalMs: 100 };
        await expect(
          protocol === "https"
            ? committeeScopedFetch(scope, capped)(endpoint)
            : committeeScopedOgmiosRpc(endpoint, scope, capped),
        ).rejects.toThrow();
        expect(peers.size).toBe(1);
        expect(scope.remainingMs()).toBeGreaterThan(1000);
        await boundedJoin(owner.drain());
        owner.assertDrained();
        // Remote close may arrive after the local join. It must not stay alive
        // waiting for the peer's TLS response or the parent's later timeout.
        await new Promise((resolve) => setTimeout(resolve, 50));
        expect(peers.size).toBe(0);
      } finally {
        scope.close();
        for (const peer of peers) peer.destroy();
        await new Promise<void>((resolve) => server.close(() => resolve()));
        await owner.drain();
      }
    },
  );
});
