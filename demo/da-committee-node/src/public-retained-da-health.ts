import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";

import type { PublicRetainedDaListenerStatus } from "./da/libp2p/PublicRetainedDaListener.js";

/** Store reads beyond this are reported as the store being unreachable. */
export const PUBLIC_RETAINED_DA_STORE_PROBE_TIMEOUT_MS = 5_000;

export type PublicRetainedDaHealthDeps = {
  readonly listener: { status(): PublicRetainedDaListenerStatus };
  /** One cheap read-only store query; throws when the store is unreachable. */
  readonly probeStore: () => Promise<void>;
  readonly storeProbeTimeoutMs?: number;
};

export type PublicRetainedDaReadiness = {
  readonly ready: boolean;
  readonly reasons: readonly string[];
  readonly listener: PublicRetainedDaListenerStatus;
};

/**
 * Readiness of the public retained-DA reader: its listener is bound and its
 * read-only store answers now. Every reason clears by itself once the cause
 * does. How served requests ended is reported with the listener status but
 * never makes the reader unready: those requests come from anonymous
 * callers, whose aborted or malformed requests say nothing about the reader.
 */
export const publicRetainedDaReadiness = async (
  deps: PublicRetainedDaHealthDeps,
): Promise<PublicRetainedDaReadiness> => {
  const status = deps.listener.status();
  const reasons: string[] = [];
  if (!status.bound) reasons.push("listener_not_bound");
  const timeoutMs =
    deps.storeProbeTimeoutMs ?? PUBLIC_RETAINED_DA_STORE_PROBE_TIMEOUT_MS;
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    await Promise.race([
      deps.probeStore(),
      new Promise<never>((_resolve, reject) => {
        timer = setTimeout(() => {
          reject(
            new Error(`store did not answer within ${timeoutMs.toString()}ms`),
          );
        }, timeoutMs);
      }),
    ]);
  } catch (error) {
    reasons.push(
      `store_unreachable: ${error instanceof Error ? error.message : String(error)}`,
    );
  } finally {
    clearTimeout(timer);
  }
  return { ready: reasons.length === 0, reasons, listener: status };
};

export type PublicRetainedDaHealthServer = {
  readonly address: () => AddressInfo | string | null;
  readonly close: () => Promise<void>;
};

/**
 * `/healthz` (the process is alive: always 200) and `/readyz` (200 or 503 by
 * {@link publicRetainedDaReadiness}) for the reader's supervisor.
 */
export const listenPublicRetainedDaHealth = async (
  deps: PublicRetainedDaHealthDeps & {
    readonly port: number;
    readonly host: string;
  },
): Promise<PublicRetainedDaHealthServer> => {
  const server: Server = createServer((request, response) => {
    const respond = (status: number, body: unknown): void => {
      if (response.writableEnded) return;
      response.writeHead(status, { "content-type": "application/json" });
      response.end(`${JSON.stringify(body)}\n`);
    };
    const path = new URL(request.url ?? "/", "http://reader.local").pathname;
    if (request.method === "GET" && path === "/healthz") {
      respond(200, { ok: true });
    } else if (request.method === "GET" && path === "/readyz") {
      void publicRetainedDaReadiness(deps).then(
        (readiness) => respond(readiness.ready ? 200 : 503, readiness),
        (error: unknown) =>
          respond(500, {
            error: error instanceof Error ? error.message : String(error),
          }),
      );
    } else {
      respond(404, { error: "not found" });
    }
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(deps.port, deps.host, () => {
      server.off("error", reject);
      resolve();
    });
  });
  return {
    address: () => server.address(),
    close: () =>
      new Promise((resolve) => {
        server.closeAllConnections();
        server.close(() => resolve());
      }),
  };
};
