import { normalizeHex } from "@al-ft/midgard-core/hex";

import { transitionTraceError } from "./errors.js";

export type RetainedDaPayloadEndpoint = {
  readonly baseUrl: string;
  readonly deploymentFingerprint?: string;
  readonly allowOperatorDebugPath?: boolean;
};

export type FetchRetainedDaPayloadOptions = {
  readonly headerHash: string;
  readonly endpoints: readonly (string | RetainedDaPayloadEndpoint)[];
  readonly fetchFn?: typeof fetch;
  readonly timeoutMs?: number;
  readonly retries?: number;
};

export type RetainedDaPayloadFetchAttempt = {
  readonly url: string;
  readonly status: "httpError" | "notFound" | "timeout" | "invalidContent";
  readonly detail: string;
};

export type RetainedDaPayloadFetchResult = {
  readonly endpoint: string;
  readonly url: string;
  readonly payloadCbor: Buffer;
  readonly metadata?: unknown;
  readonly attempts: readonly RetainedDaPayloadFetchAttempt[];
};

const normalizeEndpoint = (
  endpoint: string | RetainedDaPayloadEndpoint,
): RetainedDaPayloadEndpoint =>
  typeof endpoint === "string" ? { baseUrl: endpoint } : endpoint;

const endpointUrl = (baseUrl: string, pathAndQuery: string): string => {
  const base = baseUrl.endsWith("/") ? baseUrl : `${baseUrl}/`;
  const path = pathAndQuery.startsWith("/")
    ? pathAndQuery.slice(1)
    : pathAndQuery;
  return new URL(path, base).toString();
};

const payloadPath = ({
  endpoint,
  headerHash,
}: {
  readonly endpoint: RetainedDaPayloadEndpoint;
  readonly headerHash: string;
}): string => {
  if (endpoint.deploymentFingerprint !== undefined) {
    return `/v1/deployments/${encodeURIComponent(
      endpoint.deploymentFingerprint,
    )}/headers/${headerHash}/payload`;
  }
  if (endpoint.allowOperatorDebugPath === true) {
    return `/da/payload?header_hash=${encodeURIComponent(headerHash)}`;
  }
  throw transitionTraceError(
    "fetchFailed",
    "Retained DA payload fetch requires a deploymentFingerprint for the DA committee API; set allowOperatorDebugPath only for local debug endpoints.",
  );
};

const metadataPath = ({
  endpoint,
  headerHash,
}: {
  readonly endpoint: RetainedDaPayloadEndpoint;
  readonly headerHash: string;
}): string => {
  if (endpoint.deploymentFingerprint !== undefined) {
    return `/v1/deployments/${encodeURIComponent(
      endpoint.deploymentFingerprint,
    )}/headers/${headerHash}/payload/metadata`;
  }
  return `/da/payload/metadata?header_hash=${encodeURIComponent(headerHash)}`;
};

const fetchWithTimeout = async (
  fetchFn: typeof fetch,
  url: string,
  timeoutMs: number,
): Promise<Response> => {
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), timeoutMs);
  try {
    return await fetchFn(url, { signal: controller.signal });
  } finally {
    clearTimeout(timeout);
  }
};

const isAbortError = (error: unknown): boolean =>
  error instanceof Error && error.name === "AbortError";

const tryFetchMetadata = async ({
  fetchFn,
  endpoint,
  headerHash,
}: {
  readonly fetchFn: typeof fetch;
  readonly endpoint: RetainedDaPayloadEndpoint;
  readonly headerHash: string;
}): Promise<unknown> => {
  try {
    const response = await fetchFn(
      endpointUrl(endpoint.baseUrl, metadataPath({ endpoint, headerHash })),
    );
    return response.ok ? await response.json() : undefined;
  } catch {
    return undefined;
  }
};

const tryFetchPayload = async ({
  fetchFn,
  endpoint,
  headerHash,
  timeoutMs,
}: {
  readonly fetchFn: typeof fetch;
  readonly endpoint: RetainedDaPayloadEndpoint;
  readonly headerHash: string;
  readonly timeoutMs: number;
}): Promise<
  | {
      readonly ok: true;
      readonly url: string;
      readonly payloadCbor: Buffer;
      readonly metadata?: unknown;
    }
  | { readonly ok: false; readonly attempt: RetainedDaPayloadFetchAttempt }
> => {
  const url = endpointUrl(
    endpoint.baseUrl,
    payloadPath({ endpoint, headerHash }),
  );
  try {
    const response = await fetchWithTimeout(fetchFn, url, timeoutMs);
    if (response.status === 404) {
      return {
        ok: false,
        attempt: { url, status: "notFound", detail: "payload not found" },
      };
    }
    if (!response.ok) {
      return {
        ok: false,
        attempt: {
          url,
          status: "httpError",
          detail: `HTTP ${response.status.toString()}`,
        },
      };
    }
    const contentType = response.headers.get("content-type") ?? "";
    if (
      contentType.length > 0 &&
      !contentType.includes("application/cbor") &&
      !contentType.includes("application/octet-stream")
    ) {
      return {
        ok: false,
        attempt: {
          url,
          status: "invalidContent",
          detail: `unexpected content-type ${contentType}`,
        },
      };
    }
    const payloadCbor = Buffer.from(await response.arrayBuffer());
    if (payloadCbor.length === 0) {
      return {
        ok: false,
        attempt: { url, status: "invalidContent", detail: "empty payload" },
      };
    }
    return {
      ok: true,
      url,
      payloadCbor,
      metadata: await tryFetchMetadata({ fetchFn, endpoint, headerHash }),
    };
  } catch (error) {
    return {
      ok: false,
      attempt: {
        url,
        status: isAbortError(error) ? "timeout" : "httpError",
        detail: error instanceof Error ? error.message : String(error),
      },
    };
  }
};

const sleep = async (ms: number): Promise<void> => {
  await new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
};

export const fetchRetainedDaPayloadByHeaderHash = async ({
  headerHash,
  endpoints,
  fetchFn = fetch,
  timeoutMs = 10_000,
  retries = 1,
}: FetchRetainedDaPayloadOptions): Promise<RetainedDaPayloadFetchResult> => {
  const normalizedHeaderHash = normalizeHex(headerHash, {
    fieldName: "header_hash",
    byteLength: 28,
    trim: true,
  });
  const attempts: RetainedDaPayloadFetchAttempt[] = [];
  for (const rawEndpoint of endpoints) {
    const endpoint = normalizeEndpoint(rawEndpoint);
    for (let attempt = 0; attempt <= retries; attempt += 1) {
      const result = await tryFetchPayload({
        fetchFn,
        endpoint,
        headerHash: normalizedHeaderHash,
        timeoutMs,
      });
      if (result.ok) {
        return {
          endpoint: endpoint.baseUrl,
          url: result.url,
          payloadCbor: result.payloadCbor,
          metadata: result.metadata,
          attempts,
        };
      }
      attempts.push(result.attempt);
      if (attempt < retries) {
        await sleep(50 * 2 ** attempt);
      }
    }
  }
  throw transitionTraceError(
    "fetchFailed",
    `Unable to fetch retained DA payload for header_hash ${normalizedHeaderHash}: ${attempts
      .map((attempt) => `${attempt.url} ${attempt.status} ${attempt.detail}`)
      .join("; ")}`,
  );
};
