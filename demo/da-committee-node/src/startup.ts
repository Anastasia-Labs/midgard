import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";

import { L1NetworkMagicUnconfiguredError } from "./l1/provider.ogmios-rpc-session.js";
import { L1SourceIntegrityError } from "./l1/source-integrity.js";
import { JsonStoreLeaseHeldError } from "./store.json-file-lease.js";
import { isInstanceLockHeldElsewhere } from "./store/postgres.instance-lock.js";

/** First wait before re-trying a startup that failed on a dependency. */
export const STARTUP_RETRY_INITIAL_MS = 1_000;
/** Ceiling of that wait as it doubles. */
export const STARTUP_RETRY_MAX_MS = 30_000;

/**
 * A startup failure no retry can repair: the configuration contradicts the
 * deployment or the chain. Everything else a dependency throws while the
 * node starts (Postgres, Kupo or Ogmios not up yet, a peer address that does
 * not resolve yet) is retried.
 *
 * - the store holds state of another deployment
 *   (`stale_deployment_state_requires_fresh_redeploy`), or a persisted L1
 *   source configuration other than the configured one;
 * - the live chain's DA parameters contradict the committee configuration,
 *   or its network identity cannot be or is not proven
 *   (`L1SourceIntegrityError`, `L1NetworkMagicUnconfiguredError`).
 */
export const isFatalStartupError = (error: unknown): boolean =>
  error instanceof L1SourceIntegrityError ||
  error instanceof L1NetworkMagicUnconfiguredError ||
  (error instanceof Error &&
    /stale_deployment_state_requires_fresh_redeploy|persisted L1 source state does not match configured/u.test(
      error.message,
    ));

/** The readiness reason a failed startup attempt reports. */
export const startupReason = (error: unknown): string =>
  isInstanceLockHeldElsewhere(error) || error instanceof JsonStoreLeaseHeldError
    ? "starting:store_instance_lock_held"
    : `starting:${error instanceof Error ? error.message : String(error)}`;

/**
 * Runs `attempt` until it succeeds, with a doubling backoff between tries,
 * reporting each failure as the `starting:<reason>` it leaves readiness on.
 * Rethrows a fatal error at once. A failed attempt must release whatever it
 * opened before it throws.
 */
export const retryStartup = async <T>(args: {
  readonly attempt: () => Promise<T>;
  readonly onFailure: (reason: string) => void;
  readonly write: (line: string) => void;
  readonly isFatal?: (error: unknown) => boolean;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly initialMs?: number;
  readonly maxMs?: number;
}): Promise<T> => {
  const isFatal = args.isFatal ?? isFatalStartupError;
  const sleep =
    args.sleep ??
    ((ms: number) => new Promise<void>((resolve) => setTimeout(resolve, ms)));
  const maxMs = args.maxMs ?? STARTUP_RETRY_MAX_MS;
  let delayMs = args.initialMs ?? STARTUP_RETRY_INITIAL_MS;
  for (let attempt = 1; ; attempt += 1) {
    try {
      return await args.attempt();
    } catch (error) {
      if (isFatal(error)) throw error;
      const reason = startupReason(error);
      args.onFailure(reason);
      args.write(
        `${JSON.stringify({ event: "committee_startup_retry", attempt, retryInMs: delayMs, reason })}\n`,
      );
      await sleep(delayMs);
      delayMs = Math.min(delayMs * 2, maxMs);
    }
  }
};

export type StartingServer = {
  readonly address: () => AddressInfo | string | null;
  readonly setReason: (reason: string) => void;
  readonly close: () => Promise<void>;
};

/**
 * The API port while the node is starting: `/healthz` answers (the process
 * is alive and working on its dependencies) and `/readyz` reports the reason
 * the last attempt failed. Closed before the committee API takes the port.
 */
export const listenStartingServer = async (
  port: number,
  host: string,
): Promise<StartingServer> => {
  let reason = "starting:initializing";
  const server: Server = createServer((request, response) => {
    const path = new URL(request.url ?? "/", "http://committee.local").pathname;
    const [status, body] =
      request.method === "GET" && path === "/healthz"
        ? [200, { ok: true, status: "starting" }]
        : request.method === "GET" && path === "/readyz"
          ? [503, { ready: false, reasons: [reason] }]
          : [404, { error: "not found" }];
    response.writeHead(status, { "content-type": "application/json" });
    response.end(`${JSON.stringify(body)}\n`);
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(port, host, () => {
      server.off("error", reject);
      resolve();
    });
  });
  return {
    address: () => server.address(),
    setReason: (next) => {
      reason = next;
    },
    close: () =>
      new Promise((resolve) => {
        server.closeAllConnections();
        server.close(() => resolve());
      }),
  };
};
