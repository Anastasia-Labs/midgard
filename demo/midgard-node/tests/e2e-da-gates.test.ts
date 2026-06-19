import { createHash } from "node:crypto";
import {
  createServer,
  type IncomingMessage,
  type ServerResponse,
} from "node:http";
import type { AddressInfo } from "node:net";

import { afterEach, describe, expect, it } from "vitest";

import {
  daPayloadCborUrl,
  daPayloadMetadataUrl,
  daWatcherStatusUrl,
  probeDaGate,
  waitForDaGate,
} from "@/e2e/da-gates.js";

const headerHash = "ab".repeat(28);
const deploymentFingerprint = "deployment-test";
const payloadCbor = Buffer.from("d87980", "hex");
const payloadHash = createHash("sha256").update(payloadCbor).digest("hex");

type TestServer = {
  readonly baseUrl: string;
  readonly close: () => Promise<void>;
};

const json = (
  response: ServerResponse,
  statusCode: number,
  body: unknown,
): void => {
  response.writeHead(statusCode, { "content-type": "application/json" });
  response.end(JSON.stringify(body));
};

const startServer = async (
  handler: (request: IncomingMessage, response: ServerResponse) => void,
): Promise<TestServer> => {
  const server = createServer(handler);
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address() as AddressInfo;
  return {
    baseUrl: `http://127.0.0.1:${address.port.toString()}`,
    close: () =>
      new Promise((resolve, reject) =>
        server.close((error) =>
          error === undefined ? resolve() : reject(error),
        ),
      ),
  };
};

const servers: TestServer[] = [];

afterEach(async () => {
  await Promise.all(servers.splice(0).map((server) => server.close()));
});

describe("e2e DA gates", () => {
  it("builds current Midgard and watcher endpoint URLs from base URLs", () => {
    expect(daPayloadMetadataUrl("http://127.0.0.1:3000", headerHash)).toBe(
      `http://127.0.0.1:3000/da/payload/metadata?header_hash=${headerHash}`,
    );
    expect(daPayloadCborUrl("http://127.0.0.1:3000/", headerHash)).toBe(
      `http://127.0.0.1:3000/da/payload?header_hash=${headerHash}`,
    );
    expect(
      daWatcherStatusUrl({
        baseUrl: "http://127.0.0.1:8787",
        deploymentFingerprint,
        headerHash,
      }),
    ).toBe(
      `http://127.0.0.1:8787/v1/deployments/${deploymentFingerprint}/headers/${headerHash}/status`,
    );
  });

  it("classifies available payload and attested watcher status as satisfied", async () => {
    const server = await startServer((request, response) => {
      const url = new URL(request.url ?? "/", "http://test.local");
      if (url.pathname === "/da/payload/metadata") {
        json(response, 200, {
          headerHash,
          payloadHash,
          payloadBytes: payloadCbor.length,
        });
        return;
      }
      if (url.pathname === "/da/payload") {
        response.writeHead(200, { "content-type": "application/cbor" });
        response.end(payloadCbor);
        return;
      }
      if (url.pathname.endsWith(`/headers/${headerHash}/status`)) {
        json(response, 200, {
          headerHash,
          header: { status: "attested" },
          payload: { validationStatus: "verified", payloadSha256: payloadHash },
          signatures: [{ signerIndex: 0 }],
          l1Submissions: [
            {
              txKind: "apply",
              txHash: "cd".repeat(32),
              resultStatus: "confirmed",
            },
          ],
        });
        return;
      }
      json(response, 404, { error: "not found" });
    });
    servers.push(server);

    const result = await probeDaGate({
      headerHash,
      payloadEndpointBaseUrl: server.baseUrl,
      watcherBaseUrl: server.baseUrl,
      deploymentFingerprint,
    });

    expect(result.status).toBe("satisfied");
    expect(result.nextSafeAction).toBe("continue");
    expect(result.payloadBytes).toBe(payloadCbor.length);
    expect(result.payloadHash).toBe(payloadHash);
    expect(result.watcherHeaderStatus).toBe("attested");
    expect(result.watcherPayloadStatus).toBe("verified");
    expect(result.watcherSignatureCount).toBe(1);
    expect(result.watcherL1Submissions).toEqual([
      { txKind: "apply", txHash: "cd".repeat(32), resultStatus: "confirmed" },
    ]);
  });

  it("keeps missing payloads pending instead of allowing merge to proceed", async () => {
    const server = await startServer((_request, response) => {
      json(response, 404, { error: "missing" });
    });
    servers.push(server);

    const result = await probeDaGate({
      headerHash,
      payloadEndpointBaseUrl: server.baseUrl,
    });

    expect(result.status).toBe("pending");
    expect(result.nextSafeAction).toBe("wait_for_da_payload");
    expect(result.reason).toContain("not available");
  });

  it("blocks on watcher payload conflicts", async () => {
    const server = await startServer((request, response) => {
      const url = new URL(request.url ?? "/", "http://test.local");
      if (url.pathname === "/da/payload/metadata") {
        json(response, 200, { headerHash, payloadHash });
        return;
      }
      if (url.pathname === "/da/payload") {
        response.writeHead(200, { "content-type": "application/cbor" });
        response.end(payloadCbor);
        return;
      }
      if (url.pathname.endsWith(`/headers/${headerHash}/status`)) {
        json(response, 200, {
          headerHash,
          header: { status: "attesting" },
          payload: { validationStatus: "root_mismatch" },
          signatures: [],
        });
        return;
      }
      json(response, 404, { error: "not found" });
    });
    servers.push(server);

    const result = await probeDaGate({
      headerHash,
      payloadEndpointBaseUrl: server.baseUrl,
      watcherBaseUrl: server.baseUrl,
      deploymentFingerprint,
    });

    expect(result.status).toBe("blocked");
    expect(result.nextSafeAction).toBe("inspect_da_payload_conflict");
    expect(result.reason).toContain("root_mismatch");
  });

  it("waits through bounded payload lag and returns the first satisfied probe", async () => {
    let payloadRequests = 0;
    const server = await startServer((request, response) => {
      const url = new URL(request.url ?? "/", "http://test.local");
      if (url.pathname === "/da/payload/metadata") {
        json(response, 200, { headerHash, payloadHash });
        return;
      }
      if (url.pathname === "/da/payload") {
        payloadRequests += 1;
        if (payloadRequests === 1) {
          json(response, 404, { error: "not yet" });
          return;
        }
        response.writeHead(200, { "content-type": "application/cbor" });
        response.end(payloadCbor);
        return;
      }
      json(response, 404, { error: "not found" });
    });
    servers.push(server);

    const result = await waitForDaGate({
      headerHash,
      payloadEndpointBaseUrl: server.baseUrl,
      intervalMs: 1,
      sleep: async () => {},
    });

    expect(result.status).toBe("satisfied");
    expect(result.attempts).toBe(2);
    expect(result.timedOut).toBe(false);
  });

  it("rejects payload endpoint paths that are not base URLs", async () => {
    await expect(
      probeDaGate({
        headerHash,
        payloadEndpointBaseUrl: "http://127.0.0.1:3000/da/payload",
      }),
    ).rejects.toThrow("base URL");
  });
});
