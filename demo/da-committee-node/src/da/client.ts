export type DaPayloadFetchSuccess = {
  readonly ok: true;
  readonly endpoint: string;
  readonly payloadCbor: Buffer;
  readonly metadata?: unknown;
};

export type DaPayloadCandidate = Omit<DaPayloadFetchSuccess, "ok">;

export type DaPayloadFetchFailure = {
  readonly ok: false;
  readonly attempts: readonly {
    readonly endpoint: string;
    readonly status: "http_error" | "not_found" | "timeout" | "invalid_content";
    readonly detail: string;
  }[];
};

export type DaPayloadFetchResult =
  | DaPayloadFetchSuccess
  | DaPayloadFetchFailure;

export type DaPayloadCandidatesSuccess = {
  readonly ok: true;
  readonly candidates: readonly DaPayloadCandidate[];
  readonly attempts: DaPayloadFetchFailure["attempts"];
};

export type DaPayloadCandidatesResult =
  | DaPayloadCandidatesSuccess
  | DaPayloadFetchFailure;

export type FetchLike = typeof fetch;

export class DaPayloadClient {
  private readonly endpoints: readonly string[];
  private readonly fetchFn: FetchLike;
  private readonly timeoutMs: number;
  private readonly retries: number;

  constructor({
    endpoints,
    fetchFn = fetch,
    timeoutMs = 10_000,
    retries = 1,
  }: {
    readonly endpoints: readonly string[];
    readonly fetchFn?: FetchLike;
    readonly timeoutMs?: number;
    readonly retries?: number;
  }) {
    this.endpoints = endpoints;
    this.fetchFn = fetchFn;
    this.timeoutMs = timeoutMs;
    this.retries = retries;
  }

  async fetchPayload(headerHash: string): Promise<DaPayloadFetchResult> {
    const result = await this.fetchPayloadCandidates(headerHash);
    if (!result.ok) {
      return result;
    }
    const first = result.candidates[0]!;
    return { ok: true, ...first };
  }

  async fetchPayloadCandidates(
    headerHash: string,
  ): Promise<DaPayloadCandidatesResult> {
    const attempts: {
      readonly endpoint: string;
      readonly status:
        | "http_error"
        | "not_found"
        | "timeout"
        | "invalid_content";
      readonly detail: string;
    }[] = [];
    const candidates: DaPayloadCandidate[] = [];
    for (const endpoint of this.endpoints) {
      for (let attempt = 0; attempt <= this.retries; attempt += 1) {
        const result = await this.tryEndpoint(endpoint, headerHash);
        if (result.ok) {
          candidates.push({
            endpoint: result.endpoint,
            payloadCbor: result.payloadCbor,
            metadata: result.metadata,
          });
          break;
        }
        attempts.push(...result.attempts);
        if (attempt < this.retries) {
          await sleep(50 * 2 ** attempt);
        }
      }
    }
    return candidates.length > 0
      ? { ok: true, candidates, attempts }
      : { ok: false, attempts };
  }

  private async tryEndpoint(
    endpoint: string,
    headerHash: string,
  ): Promise<DaPayloadFetchResult> {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), this.timeoutMs);
    try {
      const payloadUrl = endpointUrl(
        endpoint,
        `/da/payload?header_hash=${encodeURIComponent(headerHash)}`,
      );
      const response = await this.fetchFn(payloadUrl, {
        signal: controller.signal,
      });
      if (response.status === 404) {
        return singleFailure(endpoint, "not_found", "payload not found");
      }
      if (!response.ok) {
        return singleFailure(
          endpoint,
          "http_error",
          `payload endpoint returned ${response.status.toString()}`,
        );
      }
      const contentType = response.headers.get("content-type") ?? "";
      if (
        contentType !== "" &&
        !contentType.includes("application/cbor") &&
        !contentType.includes("application/octet-stream")
      ) {
        return singleFailure(
          endpoint,
          "invalid_content",
          `unexpected content-type ${contentType}`,
        );
      }
      const payloadCbor = Buffer.from(await response.arrayBuffer());
      if (payloadCbor.length === 0) {
        return singleFailure(endpoint, "invalid_content", "empty payload");
      }
      const metadata = await this.tryMetadata(endpoint, headerHash);
      return { ok: true, endpoint, payloadCbor, metadata };
    } catch (error) {
      return singleFailure(
        endpoint,
        isAbortError(error) ? "timeout" : "http_error",
        error instanceof Error ? error.message : String(error),
      );
    } finally {
      clearTimeout(timeout);
    }
  }

  private async tryMetadata(
    endpoint: string,
    headerHash: string,
  ): Promise<unknown> {
    try {
      const metadataUrl = endpointUrl(
        endpoint,
        `/da/payload/metadata?header_hash=${encodeURIComponent(headerHash)}`,
      );
      const response = await this.fetchFn(metadataUrl);
      if (!response.ok) {
        return undefined;
      }
      return await response.json();
    } catch {
      return undefined;
    }
  }
}

const endpointUrl = (endpoint: string, pathAndQuery: string): string => {
  const base = endpoint.endsWith("/") ? endpoint : `${endpoint}/`;
  const path = pathAndQuery.startsWith("/")
    ? pathAndQuery.slice(1)
    : pathAndQuery;
  return new URL(path, base).toString();
};

const singleFailure = (
  endpoint: string,
  status: DaPayloadFetchFailure["attempts"][number]["status"],
  detail: string,
): DaPayloadFetchFailure => ({
  ok: false,
  attempts: [{ endpoint, status, detail }],
});

const sleep = (ms: number): Promise<void> =>
  new Promise((resolve) => {
    setTimeout(resolve, ms);
  });

const isAbortError = (error: unknown): boolean =>
  error instanceof Error && error.name === "AbortError";
