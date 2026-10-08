import {
  isWatcherJournalIntegrityError,
  isWatcherJournalUnavailableError,
} from "../fault-proofs/watcher-journal-database.js";
import { jsonResponse } from "./operations-observability.http-response.js";
import type {
  WatcherOperationsApi,
  WatcherOperationsDiagnosticKind,
} from "./operations-observability.watcher-operations-metrics.js";

export const handleWatcherOperationsHttpRequest = async (
  request: Request,
  api: WatcherOperationsApi,
): Promise<Response> => {
  if (request.method !== "GET") {
    return new Response(null, {
      status: 405,
      headers: Object.freeze({ allow: "GET", "cache-control": "no-store" }),
    });
  }
  let url: URL;
  try {
    url = new URL(request.url);
  } catch {
    return jsonResponse(400, { error: "invalid_request" });
  }
  try {
    if (url.pathname === "/readyz" && url.search === "") {
      const status = api.status();
      const ready = status.readiness === "ready";
      return jsonResponse(ready ? 200 : 503, {
        ready,
        reasons: status.readinessReasons,
        l1: status.l1Readiness,
      });
    }
    if (url.pathname === "/v1/status" && url.search === "") {
      return jsonResponse(200, api.status());
    }
    if (url.pathname === "/v1/metrics" && url.search === "") {
      return jsonResponse(200, api.metrics());
    }
    if (url.pathname === "/v1/diagnostics") {
      const keys = [...url.searchParams.keys()];
      if (
        keys.some(
          (key) => key !== "kind" && key !== "cursor" && key !== "limit",
        ) ||
        new Set(keys).size !== keys.length
      ) {
        throw new Error("invalid diagnostics query");
      }
      const kind = url.searchParams.get("kind");
      const cursor = url.searchParams.get("cursor") ?? undefined;
      const rawLimit = url.searchParams.get("limit");
      const limit = rawLimit === null ? undefined : Number(rawLimit);
      if (kind === null) throw new Error("diagnostic kind is required");
      return jsonResponse(
        200,
        api.diagnostics({
          kind: kind as WatcherOperationsDiagnosticKind,
          ...(cursor === undefined ? {} : { cursor }),
          ...(limit === undefined ? {} : { limit }),
        }),
      );
    }
    return jsonResponse(404, { error: "not_found" });
  } catch (error) {
    // A journal failure is the watcher's state, never the request's fault.
    if (isWatcherJournalIntegrityError(error))
      return jsonResponse(503, { error: "journal_integrity" });
    if (isWatcherJournalUnavailableError(error))
      return jsonResponse(503, { error: "journal_unavailable" });
    return jsonResponse(400, { error: "invalid_request" });
  }
};
