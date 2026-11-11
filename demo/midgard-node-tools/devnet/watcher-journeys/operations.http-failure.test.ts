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
