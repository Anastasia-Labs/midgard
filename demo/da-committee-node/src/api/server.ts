import {
  createServer,
  type IncomingMessage,
  type ServerResponse,
} from "node:http";
import type { AddressInfo } from "node:net";

import type { CommitteeReadinessSnapshot } from "../committee-service.js";
import { jsonBigIntStringReplacer } from "../json.js";

export type CommitteeApiServer = {
  readonly listen: (port: number, host: string) => Promise<void>;
  readonly address: () => AddressInfo | string | null;
  readonly close: () => Promise<void>;
};

export const createCommitteeApiServer = ({
  readiness,
  manifest = {},
}: {
  readonly deploymentFingerprint: string;
  readonly signerIndex?: number;
  readonly signerValidation?: unknown;
  readonly store: unknown;
  readonly readiness: () =>
    | CommitteeReadinessSnapshot
    | Promise<CommitteeReadinessSnapshot>;
  readonly manifest?: Record<string, unknown>;
  readonly peerReplayWindowMs?: number;
  readonly peerMaxBodyBytes?: number;
  readonly peerRateLimitWindowMs?: number;
  readonly peerRateLimitMaxRequests?: number;
}): CommitteeApiServer => {
  const server = createServer((request, response) => {
    try {
      void routeRequest({
        request,
        response,
        readiness,
        manifest,
      }).catch((error: unknown) => {
        json(response, 500, {
          error: error instanceof Error ? error.message : String(error),
        });
      });
    } catch (error) {
      json(response, 500, {
        error: error instanceof Error ? error.message : String(error),
      });
    }
  });
  return {
    listen: (port, host) =>
      new Promise((resolve) => {
        server.listen(port, host, resolve);
      }),
    address: () => server.address(),
    close: () =>
      new Promise((resolve, reject) => {
        server.close((error) =>
          error === undefined ? resolve() : reject(error),
        );
      }),
  };
};

const routeRequest = ({
  request,
  response,
  readiness,
  manifest,
}: {
  readonly request: IncomingMessage;
  readonly response: ServerResponse;
  readonly readiness: () =>
    | CommitteeReadinessSnapshot
    | Promise<CommitteeReadinessSnapshot>;
  readonly manifest: Record<string, unknown>;
}): Promise<void> => {
  const method = request.method ?? "GET";
  const url = new URL(request.url ?? "/", "http://committee.local");
  if (method === "GET" && url.pathname === "/healthz") {
    json(response, 200, { ok: true });
    return Promise.resolve();
  }
  if (method === "GET" && url.pathname === "/readyz") {
    return Promise.resolve(readiness()).then((snapshot) => {
      json(response, snapshot.ready ? 200 : 503, snapshot);
    });
  }
  if (method === "GET" && url.pathname === "/v1/manifest") {
    json(response, 200, manifest);
    return Promise.resolve();
  }
  json(response, 404, { error: "not found" });
  return Promise.resolve();
};

const json = (
  response: ServerResponse,
  statusCode: number,
  body: unknown,
): void => {
  if (response.writableEnded || response.destroyed) {
    return;
  }
  const payload = `${JSON.stringify(body, jsonBigIntStringReplacer)}\n`;
  if (response.headersSent) {
    response.end();
    return;
  }
  response.writeHead(statusCode, { "content-type": "application/json" });
  response.end(payload);
};
