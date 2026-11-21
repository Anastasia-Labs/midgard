import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";

import { afterEach, expect, it } from "vitest";

import { readJourneyOperations } from "./operations.js";

let server: Server | undefined;

afterEach(async () => {
  await new Promise<void>(
    (resolve) => server?.close(() => resolve()) ?? resolve(),
  );
  server = undefined;
});

/** A stack watcher's operations listener answering every read with `status` and `body`. */
const watcherOperations = async (status: number, body: unknown) => {
  server = createServer((_request, response) => {
    response.writeHead(status, { "content-type": "application/json" });
    response.end(JSON.stringify(body));
  });
  await new Promise<void>((resolve) =>
    server!.listen(0, "127.0.0.1", () => resolve()),
  );
  return `http://127.0.0.1:${(server!.address() as AddressInfo).port.toString()}`;
};

it.each([
  ["/v1/status", "journal_integrity"],
  ["/v1/status", "journal_unavailable"],
  ["/v1/diagnostics?kind=verification&limit=100&cursor=0", "starting"],
])(
  "names the reason a watcher refusing %s with 503 gives (%s)",
  async (path, reason) => {
    const endpoint = await watcherOperations(503, { error: reason });
    await expect(readJourneyOperations(endpoint, path)).rejects.toThrow(
      `Operations HTTP returned 503: {"error":"${reason}"}`,
    );
  },
);

it("reads a watcher's status when it answers", async () => {
  const endpoint = await watcherOperations(200, { liveness: "live" });
  await expect(readJourneyOperations(endpoint, "/v1/status")).resolves.toEqual({
    liveness: "live",
  });
});

it("reads through a watcher that answers 5xx for a moment", async () => {
  let answered = 0;
  server = createServer((_request, response) => {
    answered += 1;
    response.writeHead(answered === 1 ? 503 : 200, {
      "content-type": "application/json",
    });
    response.end(
      JSON.stringify(answered === 1 ? { error: "starting" } : { ok: true }),
    );
  });
  await new Promise<void>((resolve) =>
    server!.listen(0, "127.0.0.1", () => resolve()),
  );
  const endpoint = `http://127.0.0.1:${(server.address() as AddressInfo).port.toString()}`;
  await expect(readJourneyOperations(endpoint, "/v1/status")).resolves.toEqual({
    ok: true,
  });
  expect(answered).toBe(2);
});

it("reads through a refused connection while the watcher's listener comes back", async () => {
  const probe = createServer();
  await new Promise<void>((resolve) =>
    probe.listen(0, "127.0.0.1", () => resolve()),
  );
  const { port } = probe.address() as AddressInfo;
  await new Promise<void>((resolve) => probe.close(() => resolve()));
  const read = readJourneyOperations(
    `http://127.0.0.1:${port.toString()}`,
    "/v1/status",
  );
  // The first try finds nothing listening; the listener is back before the
  // next one.
  await new Promise((resolve) => setTimeout(resolve, 50));
  server = createServer((_request, response) => {
    response.writeHead(200, { "content-type": "application/json" });
    response.end(JSON.stringify({ liveness: "live" }));
  });
  await new Promise<void>((resolve) =>
    server!.listen(port, "127.0.0.1", () => resolve()),
  );
  await expect(read).resolves.toEqual({ liveness: "live" });
});

it("fails at once, by name, on an answer that refuses the read", async () => {
  let answered = 0;
  server = createServer((_request, response) => {
    answered += 1;
    response.writeHead(400, { "content-type": "application/json" });
    response.end(JSON.stringify({ error: "invalid_cursor" }));
  });
  await new Promise<void>((resolve) =>
    server!.listen(0, "127.0.0.1", () => resolve()),
  );
  const endpoint = `http://127.0.0.1:${(server.address() as AddressInfo).port.toString()}`;
  await expect(readJourneyOperations(endpoint, "/v1/status")).rejects.toThrow(
    'Operations HTTP returned 400: {"error":"invalid_cursor"}',
  );
  expect(answered).toBe(1);
});
