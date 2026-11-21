import { createServer, type RequestListener } from "node:http";
import type { Socket } from "node:net";

import { afterEach, expect, it } from "vitest";

import { openAcceptanceKupoReads } from "../src/devnet-stack/acceptance-payout-sources.js";
import { collectorFixture } from "./devnet-stack-acceptance-payout-collector.fixtures.js";

const cleanups: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of cleanups.splice(0)) await close();
});
const server = async (body: RequestListener) => {
  const http = createServer(body);
  const sockets = new Set<Socket>();
  http.on("connection", (socket) => {
    sockets.add(socket);
    socket.once("close", () => sockets.delete(socket));
  });
  await new Promise<void>((resolve) => http.listen(0, "127.0.0.1", resolve));
  const address = http.address();
  if (address === null || typeof address === "string")
    throw new Error("fixture bind failed");
  let closing: Promise<void> | undefined;
  const close = () =>
    (closing ??= new Promise<void>((resolve, reject) => {
      http.closeAllConnections();
      http.close((error) => (error ? reject(error) : resolve()));
    }));
  cleanups.push(close);
  return {
    url: `http://127.0.0.1:${address.port}`,
    sockets,
    async waitClosed() {
      let timer: ReturnType<typeof setTimeout> | undefined;
      try {
        await Promise.race([
          Promise.all(
            [...sockets].map(
              (socket) =>
                new Promise<void>((resolve) => socket.once("close", resolve)),
            ),
          ),
          new Promise<never>((_resolve, reject) => {
            timer = setTimeout(
              () => reject(new Error("peer socket did not close")),
              1000,
            );
          }),
        ]);
      } finally {
        clearTimeout(timer);
      }
    },
    close,
  };
};

it("reads the actual bounded HTTP body and joins its owned sockets", async () => {
  const endpoint = await server((_request, response) =>
    response.end('[{"datum":null}]'),
  );
  const fixture = collectorFixture();
  const reader = openAcceptanceKupoReads(fixture.scope, endpoint.url, 32);
  try {
    expect(
      await (await reader.fetchImpl(`${endpoint.url}/matches/0@abc`)).json(),
    ).toEqual([{ datum: null }]);
  } finally {
    await reader.close();
  }
  await endpoint.waitClosed();
  expect(endpoint.sockets.size).toBe(0);
  await endpoint.close();
});
it("refuses oversized streamed bytes and drains the physical connection", async () => {
  const endpoint = await server((_request, response) => {
    response.end("x".repeat(1000));
  });
  const fixture = collectorFixture();
  const reader = openAcceptanceKupoReads(fixture.scope, endpoint.url, 16);
  try {
    await expect(reader.fetchImpl(endpoint.url)).rejects.toThrow(/byte bound/);
  } finally {
    await reader.close();
  }
  await endpoint.waitClosed();
  expect(endpoint.sockets.size).toBe(0);
  await endpoint.close();
});
it("revokes a pending real HTTP read, physically closes it and awaits the partner", async () => {
  let accepted!: () => void;
  const entered = new Promise<void>((resolve) => {
    accepted = resolve;
  });
  const endpoint = await server((_request, response) => {
    response.write("[");
    accepted();
  });
  const fixture = collectorFixture();
  const reader = openAcceptanceKupoReads(fixture.scope, endpoint.url, 32);
  const pending = reader.fetchImpl(endpoint.url);
  const refusal = expect(pending).rejects.toThrow();
  await entered;
  fixture.controller.abort();
  await reader.close();
  await refusal;
  await endpoint.waitClosed();
  expect(endpoint.sockets.size).toBe(0);
  await endpoint.close();
});
it("refuses a foreign URL or invalid bound before starting transport", async () => {
  const fixture = collectorFixture();
  expect(() =>
    openAcceptanceKupoReads(fixture.scope, "http://example.invalid:80", 32),
  ).toThrow(/recorded local/);
  expect(() =>
    openAcceptanceKupoReads(fixture.scope, "http://127.0.0.1:12345", 0),
  ).toThrow(/bound/);
  const reader = openAcceptanceKupoReads(
    fixture.scope,
    "http://127.0.0.1:12345",
    32,
  );
  try {
    await expect(reader.fetchImpl("http://127.0.0.1:5433")).rejects.toThrow(
      /escaped/,
    );
  } finally {
    await reader.close();
  }
});
