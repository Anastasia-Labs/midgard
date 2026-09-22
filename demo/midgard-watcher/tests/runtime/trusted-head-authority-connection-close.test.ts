import { Agent, request } from "node:http";
import { setImmediate as nextTurn } from "node:timers/promises";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it, vi } from "vitest";

import {
  WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import {
  startWatcherTrustedHeadAuthorityServer,
  type WatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";

const httpSecret = "trusted-head-transport-test-secret-with-entropy";
const head: WatcherRollbackDurableTrustedHead = {
  schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
  policyDigest: "11".repeat(32),
  deploymentMarker: makeDeploymentMarker("22".repeat(32)),
  authenticationKeyId: "33".repeat(32),
  revision: "0",
  snapshotSha256: "44".repeat(32),
  authorityDigest: "55".repeat(32),
  headMac: "66".repeat(32),
};

const call = async (
  endpoint: string,
  agent: Agent,
  path: string,
  options: { body?: string; authenticated?: boolean } = {},
) => {
  // Allow the agent to pool the previous socket; CAS must still connect afresh.
  await nextTurn();
  return await new Promise<{
    status: number | undefined;
    connection: string | undefined;
    reusedSocket: boolean;
    body: unknown;
  }>((resolve, reject) => {
    const outgoing = request(
      `${endpoint}${path}`,
      {
        agent,
        method: options.body === undefined ? "GET" : "POST",
        headers: {
          ...(options.authenticated === false
            ? {}
            : { authorization: `Bearer ${httpSecret}` }),
          ...(options.body === undefined
            ? {}
            : { "content-type": "application/json" }),
        },
      },
      (response) => {
        const chunks: Buffer[] = [];
        response.on("data", (chunk: Buffer) => chunks.push(chunk));
        response.once("error", reject);
        response.once("end", () => {
          resolve({
            status: response.statusCode,
            connection: response.headers.connection,
            reusedSocket: outgoing.reusedSocket,
            body: JSON.parse(Buffer.concat(chunks).toString("utf8")),
          });
        });
      },
    );
    outgoing.once("error", reject);
    outgoing.end(options.body);
  });
};

const withServer = async (
  store: WatcherTrustedHeadAuthorityStore,
  run: (endpoint: string, agent: Agent) => Promise<void>,
) => {
  const server = await startWatcherTrustedHeadAuthorityServer({
    endpoint: "http://127.0.0.1:0",
    httpSecret,
    store,
    unsafeAllowEphemeralPortForTest: true,
  });
  const agent = new Agent({ keepAlive: true, maxSockets: 1 });
  try {
    await run(server.endpoint, agent);
  } finally {
    agent.destroy();
    await server.close();
  }
};

describe("trusted-head authority connection lifetime", () => {
  it("opens a fresh connection for CAS after reads and preserves single CAS outcomes", async () => {
    let current: WatcherRollbackDurableTrustedHead | null = null;
    const compareAndSwap = vi.fn(async () => {
      if (current !== null) return false;
      current = head;
      return true;
    });
    await withServer(
      {
        readRecordAuthenticationKeyId: async () => "77".repeat(32),
        readCurrent: async () => current,
        compareAndSwap,
      },
      async (endpoint, agent) => {
        const identity = await call(endpoint, agent, "/v1/identity");
        const read = await call(endpoint, agent, "/v1/trusted-head");
        const body = JSON.stringify({
          expectedTrustedHead: null,
          nextTrustedHead: head,
        });
        const committed = await call(endpoint, agent, "/v1/trusted-head/cas", {
          body,
        });
        const conflict = await call(endpoint, agent, "/v1/trusted-head/cas", {
          body,
        });
        expect(identity.body).toEqual({
          recordAuthenticationKeyId: "77".repeat(32),
        });
        expect(read.body).toEqual({ head: null });
        expect(committed).toMatchObject({
          status: 200,
          body: { committed: true, head },
        });
        expect(conflict).toMatchObject({
          status: 409,
          body: { committed: false, head },
        });
        expect(compareAndSwap).toHaveBeenCalledTimes(2);
        expect(compareAndSwap).toHaveBeenNthCalledWith(1, {
          expectedTrustedHead: null,
          nextTrustedHead: head,
        });
        for (const response of [identity, read, committed, conflict]) {
          expect(response.connection).toBe("close");
          expect(response.reusedSocket).toBe(false);
        }
      },
    );
  });

  it("also closes rejected and persistence-failure responses before another request", async () => {
    const compareAndSwap = vi.fn(async () => false);
    await withServer(
      {
        readRecordAuthenticationKeyId: async () => "77".repeat(32),
        readCurrent: async () => {
          throw new Error("synthetic storage failure");
        },
        compareAndSwap,
      },
      async (endpoint, agent) => {
        const unauthorized = await call(endpoint, agent, "/v1/trusted-head", {
          authenticated: false,
        });
        const missing = await call(endpoint, agent, "/missing");
        const invalid = await call(endpoint, agent, "/v1/trusted-head/cas", {
          body: "{",
        });
        const failed = await call(endpoint, agent, "/v1/trusted-head");
        const recovered = await call(endpoint, agent, "/v1/identity");
        expect(
          [unauthorized, missing, invalid, failed, recovered].map(
            (response) => response.status,
          ),
        ).toEqual([401, 404, 400, 500, 200]);
        expect(compareAndSwap).not.toHaveBeenCalled();
        for (const response of [
          unauthorized,
          missing,
          invalid,
          failed,
          recovered,
        ]) {
          expect(response.connection).toBe("close");
          expect(response.reusedSocket).toBe(false);
        }
      },
    );
  });
});
