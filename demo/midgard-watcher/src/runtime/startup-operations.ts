import { jsonResponse } from "./operations-observability.http-response.js";
import type { WatcherOperationsObservability } from "./operations-observability.js";
import type { WatcherStartupProgress } from "./startup-progress.js";

type HttpObservability = Pick<
  WatcherOperationsObservability,
  "handleHttpRequest"
>;

/** The startup stage most recently begun, and its latest report. */
export type WatcherStartupStatus = Readonly<{
  stage: string;
  outcome: WatcherStartupProgress["outcome"];
  error?: string;
  retryAfterMs?: number;
}>;

export type WatcherStartupOperations = HttpObservability &
  Readonly<{
    /** Records one startup progress report. */
    report: (progress: WatcherStartupProgress) => void;
    /** Serves `operations` for every request from now on. */
    attach: (operations: HttpObservability) => void;
    /** The latest startup report, or null before the first. */
    status: () => WatcherStartupStatus | null;
  }>;

/**
 * The operations surface bound while startup runs, before the runtime's
 * observability exists. `/readyz` answers 503 with the reason
 * `startup:<stage>`, naming the stage most recently begun, and the stage's
 * latest report (a retried L1 transient's error and wait included); the
 * liveness probe `/v1/status` answers 200 with the same reason; every other
 * route answers 503 `starting`. Once `attach` is called, every request goes
 * to the runtime's observability instead.
 */
export const createWatcherStartupOperations = (): WatcherStartupOperations => {
  let attached: HttpObservability | undefined;
  let latest: WatcherStartupStatus | null = null;
  const reasons = () => [`startup:${latest?.stage ?? "pending"}`];
  const startupResponse = (request: Request): Response => {
    if (request.method !== "GET")
      return new Response(null, {
        status: 405,
        headers: Object.freeze({ allow: "GET", "cache-control": "no-store" }),
      });
    const { pathname, search } = new URL(request.url);
    if (pathname === "/readyz" && search === "")
      return jsonResponse(503, {
        ready: false,
        reasons: reasons(),
        l1: [],
        startup: latest,
      });
    if (pathname === "/v1/status" && search === "")
      return jsonResponse(200, {
        observedAtMs: BigInt(Date.now()).toString(),
        liveness: "live",
        readiness: "not_ready",
        readinessReasons: reasons(),
        l1Readiness: [],
        startup: latest,
      });
    return jsonResponse(503, { error: "starting" });
  };
  return Object.freeze({
    report: (progress: WatcherStartupProgress) => {
      latest = Object.freeze({
        stage: progress.stage,
        outcome: progress.outcome,
        ...(progress.error === undefined ? {} : { error: progress.error }),
        ...(progress.retryAfterMs === undefined
          ? {}
          : { retryAfterMs: progress.retryAfterMs }),
      });
    },
    attach: (operations: HttpObservability) => {
      attached = operations;
    },
    status: () => latest,
    handleHttpRequest: async (request: Request): Promise<Response> =>
      attached === undefined
        ? startupResponse(request)
        : await attached.handleHttpRequest(request),
  });
};
