import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";

import { classifyFailure } from "@al-ft/midgard-l1-follower";

import { isInstanceLockHeldElsewhere } from "./store/postgres.instance-lock.js";

/** First wait before re-trying a startup that failed on a dependency. */
export const STARTUP_RETRY_INITIAL_MS = 1_000;
/** Ceiling of that wait as it doubles. */
export const STARTUP_RETRY_MAX_MS = 30_000;
/**
 * How long consecutive transient failures are retried before the startup
 * fails: the node's database budget, which covers a Postgres or Cardano node
 * that is restarting or still starting beside this process.
 */
export const STARTUP_TRANSIENT_BUDGET_MS = 15 * 60_000;

/**
 * How the startup treats a failed attempt:
 * - `waiting`: another live process holds the store's instance lock. That is
 *   another actor's progress, retried without a deadline.
 * - `transient`: a dependency that is down or still starting (a refused,
 *   reset or timed-out connection, a Postgres connection-class error, a
 *   Unix socket not created yet), retried for at most the budget.
 * - `fatal`: anything else, including any failure not recognised, which no
 *   retry is known to repair. The startup fails at once.
 */
export type StartupFailureClass = "waiting" | "transient" | "fatal";

/** The error and every `cause` it wraps, outermost first. */
const causeChain = (error: unknown): unknown[] => {
  const chain: unknown[] = [];
  let current: unknown = error;
  while (current !== undefined && current !== null && chain.length < 8) {
    chain.push(current);
    current = (current as { cause?: unknown }).cause;
  }
  return chain;
};

export const classifyStartupFailure = (error: unknown): StartupFailureClass => {
  if (isInstanceLockHeldElsewhere(error)) return "waiting";
  for (const link of causeChain(error)) {
    if (classifyFailure(link) === "transient") return "transient";
    const { code, syscall } = (link ?? {}) as {
      code?: unknown;
      syscall?: unknown;
    };
    // A Unix socket whose server has not created it yet.
    if (code === "ENOENT" && syscall === "connect") return "transient";
  }
  return "fatal";
};

/** The readiness reason a failed startup attempt reports. */
export const startupReason = (error: unknown): string =>
  isInstanceLockHeldElsewhere(error)
    ? "starting:store_instance_lock_held"
    : `starting:${error instanceof Error ? error.message : String(error)}`;

/**
 * The committee startup failed: a failure that is not transient, or
 * transient failures that outlasted the budget. `cause` is the last
 * attempt's error. `startupFailureOutcome` says what the process does on it.
 */
export class CommitteeStartupFailedError extends Error {
  constructor(
    readonly reason:
      | "committee_startup_dependency_unavailable"
      | "committee_startup_failed",
    readonly attempts: number,
    override readonly cause: unknown,
  ) {
    const detail = cause instanceof Error ? cause.message : String(cause);
    super(
      reason === "committee_startup_dependency_unavailable"
        ? `${reason}: a dependency stayed unavailable past the startup budget after ${attempts.toString()} attempts: ${detail}`
        : `${reason}: a failure no retry is known to repair: ${detail}`,
    );
  }
}

/**
 * Runs `attempt` until it succeeds, with a doubling backoff between tries,
 * reporting each failure as the `starting:<reason>` it leaves readiness on.
 * Each failure is classified (`classify`, default `classifyStartupFailure`):
 * a wait on another live process is retried without a deadline; a transient
 * failure is retried while consecutive transient failures last less than
 * `budgetMs`; anything else, or the budget running out, fails the startup
 * with `CommitteeStartupFailedError`. A one-shot run classifies every failure
 * as fatal. A failed attempt must release whatever it opened before it
 * throws.
 */
export const retryStartup = async <T>(args: {
  readonly attempt: () => Promise<T>;
  readonly onFailure: (reason: string) => void;
  readonly write: (line: string) => void;
  readonly classify?: (error: unknown) => StartupFailureClass;
  readonly budgetMs?: number;
  readonly now?: () => number;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly initialMs?: number;
  readonly maxMs?: number;
}): Promise<T> => {
  const classify = args.classify ?? classifyStartupFailure;
  const budgetMs = args.budgetMs ?? STARTUP_TRANSIENT_BUDGET_MS;
  const now = args.now ?? Date.now;
  const sleep =
    args.sleep ??
    ((ms: number) => new Promise<void>((resolve) => setTimeout(resolve, ms)));
  const maxMs = args.maxMs ?? STARTUP_RETRY_MAX_MS;
  let delayMs = args.initialMs ?? STARTUP_RETRY_INITIAL_MS;
  let failingSince: number | undefined;
  for (let attempt = 1; ; attempt += 1) {
    try {
      return await args.attempt();
    } catch (error) {
      const failure = classify(error);
      if (failure === "fatal")
        throw new CommitteeStartupFailedError(
          "committee_startup_failed",
          attempt,
          error,
        );
      if (failure === "waiting") failingSince = undefined;
      else {
        const at = now();
        failingSince ??= at;
        if (at - failingSince >= budgetMs)
          throw new CommitteeStartupFailedError(
            "committee_startup_dependency_unavailable",
            attempt,
            error,
          );
      }
      const reason = startupReason(error);
      args.onFailure(reason);
      args.write(
        `${JSON.stringify({ event: "committee_startup_retry", attempt, class: failure, retryInMs: delayMs, reason })}\n`,
      );
      await sleep(delayMs);
      delayMs = Math.min(delayMs * 2, maxMs);
    }
  }
};

/** The readiness reason of a startup that failed and holds. */
export const COMMITTEE_STARTUP_FAILED = "committee_startup_failed";

/**
 * What the process does on a failed startup (owner ruling 2026-10-09):
 * - `exit`: transient failures outlasted the startup budget
 *   (`committee_startup_dependency_unavailable`). The process exits non-zero
 *   and its supervisor's restart is the backoff.
 * - `hold`: any other failure, deterministic or unknown (a configuration,
 *   key material or stored-state refusal). No restart is known to repair it,
 *   so the process stays up, unready with `committee_startup_failed`, until
 *   an operator restarts it.
 */
export const startupFailureOutcome = (error: unknown): "exit" | "hold" =>
  error instanceof CommitteeStartupFailedError &&
  error.reason === "committee_startup_dependency_unavailable"
    ? "exit"
    : "hold";

/**
 * Runs the startup `start`. On a failure that exits
 * (`startupFailureOutcome`), or with no starting server (a one-shot run),
 * closes the server and rethrows. On one that holds, `starting` reports
 * `committee_startup_failed` with the failure's message, one named line is
 * written, and the result is undefined: the server keeps the process up.
 */
export const startOrHold = async <T>(
  starting: StartingServer | undefined,
  write: (line: string) => void,
  start: () => Promise<T>,
): Promise<T | undefined> => {
  try {
    return await start();
  } catch (error) {
    if (starting === undefined || startupFailureOutcome(error) === "exit") {
      await starting?.close();
      throw error;
    }
    const detail = error instanceof Error ? error.message : String(error);
    starting.hold(detail);
    write(
      `${JSON.stringify({ event: "committee_startup_held", reason: COMMITTEE_STARTUP_FAILED, detail })}\n`,
    );
    return undefined;
  }
};

export type StartingServer = {
  readonly address: () => AddressInfo | string | null;
  readonly setReason: (reason: string) => void;
  /**
   * The startup failed and holds: `/readyz` reports
   * `committee_startup_failed` with `detail`, and `/healthz` stays live.
   */
  readonly hold: (detail: string) => void;
  readonly close: () => Promise<void>;
};

/**
 * The API port while the node is starting: `/healthz` answers (the process
 * is alive and working on its dependencies) and `/readyz` reports the reason
 * the last attempt failed. Closed before the committee API takes the port;
 * a startup that failed and holds keeps it open (`hold`).
 */
export const listenStartingServer = async (
  port: number,
  host: string,
): Promise<StartingServer> => {
  let reason = "starting:initializing";
  let held: string | undefined;
  const server: Server = createServer((request, response) => {
    const path = new URL(request.url ?? "/", "http://committee.local").pathname;
    const [status, body] =
      request.method === "GET" && path === "/healthz"
        ? [200, { ok: true, status: held === undefined ? "starting" : "held" }]
        : request.method === "GET" && path === "/readyz"
          ? [
              503,
              held === undefined
                ? { ready: false, reasons: [reason] }
                : {
                    ready: false,
                    reasons: [COMMITTEE_STARTUP_FAILED],
                    detail: held,
                  },
            ]
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
    hold: (detail) => {
      held = detail;
    },
    close: () =>
      new Promise((resolve) => {
        server.closeAllConnections();
        server.close(() => resolve());
      }),
  };
};
