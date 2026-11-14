import { createServer } from "node:http";

import { expect, it } from "vitest";

import { type RunEnv, servicePorts } from "../src/devnet-stack/layout.js";
import { probeServiceReadiness } from "../src/devnet-stack/service-readiness.js";
import type { ServiceSpec } from "../src/devnet-stack/supervisor.js";

const run: RunEnv = {
  runId: "synthetic",
  composeProject: "synthetic",
  networkMagic: 42,
  ogmiosPort: 2337,
  kupoPort: 2442,
  postgresPort: 5432,
  postgresUser: "synthetic",
  postgresPassword: "synthetic",
  postgresDatabase: "synthetic",
  cardanoImage: "synthetic",
  postgresImage: "synthetic",
  portOffset: 0,
};

it.each([200, 503, 401])(
  "the real stack URL probe accepts readiness only on HTTP success: %s",
  async (status) => {
    const targets: string[] = [];
    const server = createServer((request, response) => {
      targets.push(`${request.method} ${request.url}`);
      response.writeHead(status, { "content-type": "application/json" });
      response.end(
        JSON.stringify({
          ready: status === 200,
          reasons: status === 200 ? [] : ["l1_source_stale"],
        }),
      );
    });
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const address = server.address();
    if (address === null || typeof address === "string")
      throw new Error("missing synthetic port");
    try {
      // The real configured service URL contract is in devnet-stack-watcher.
      const operations = `http://127.0.0.1:${servicePorts(run).watcherOperations}`;
      const service: ServiceSpec = {
        name: "watcher",
        command: process.execPath,
        args: [],
        cwd: process.cwd(),
        env: {},
        healthUrl: `${operations}/v1/status`,
        readyUrl: `${operations}/readyz`,
      };
      const target = new URL(service.readyUrl!);
      target.port = String(address.port);
      const result = await probeServiceReadiness(
        { ...service, readyUrl: target.href },
        1000,
      );
      expect(result.status).toBe(status);
      expect(result.ok).toBe(status === 200);
      expect(JSON.parse(result.body)).toEqual({
        ready: status === 200,
        reasons: status === 200 ? [] : ["l1_source_stale"],
      });
      expect(targets).toEqual(["GET /readyz"]);
    } finally {
      await new Promise<void>((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      );
    }
  },
);
