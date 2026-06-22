import { createHash } from "node:crypto";

export const E2E_DA_GATE_SCHEMA_VERSION = "midgard-e2e-da-gate-v1";

export type DaGateStatus = "satisfied" | "pending" | "blocked" | "failed";

export type DaGateHttpEvidence = {
  readonly label: string;
  readonly url: string;
  readonly statusCode: number | null;
  readonly ok: boolean;
  readonly error?: string;
};

export type DaGateProbeResult = {
  readonly schemaVersion: typeof E2E_DA_GATE_SCHEMA_VERSION;
  readonly headerHash: string;
  readonly deploymentFingerprint?: string;
  readonly status: DaGateStatus;
  readonly nextSafeAction:
    | "continue"
    | "wait_for_da_payload"
    | "wait_for_da_attestation"
    | "inspect_da_payload_conflict"
    | "inspect_da_watcher"
    | "fix_da_gate_configuration";
  readonly checkedAt: string;
  readonly payloadEndpointBaseUrl: string;
  readonly watcherBaseUrl?: string;
  readonly payloadBytes: number | null;
  readonly payloadHash: string | null;
  readonly expectedPayloadHash: string | null;
  readonly watcherHeaderStatus: string | null;
  readonly watcherPayloadStatus: string | null;
  readonly watcherSignatureCount: number | null;
  readonly watcherL1Submissions: readonly {
    readonly txKind?: string;
    readonly txHash?: string;
    readonly resultStatus?: string;
  }[];
  readonly http: readonly DaGateHttpEvidence[];
  readonly reason: string;
};

export type WaitForDaGateResult = DaGateProbeResult & {
  readonly attempts: number;
  readonly timedOut: boolean;
};

export type DaGateFetch = (
  input: string,
  init?: RequestInit,
) => Promise<Response>;

export type ProbeDaGateOptions = {
  readonly headerHash: string;
  readonly payloadEndpointBaseUrl: string;
  readonly watcherBaseUrl?: string;
  readonly deploymentFingerprint?: string;
  readonly fetchFn?: DaGateFetch;
  readonly now?: Date;
  readonly requestTimeoutMs?: number;
};

export type WaitForDaGateOptions = ProbeDaGateOptions & {
  readonly timeoutMs?: number;
  readonly intervalMs?: number;
  readonly sleep?: (milliseconds: number) => Promise<void>;
};

const HEADER_HASH_REGEX = /^[0-9a-f]{56}$/i;
const DEFAULT_REQUEST_TIMEOUT_MS = 10_000;
const DEFAULT_WAIT_TIMEOUT_MS = 120_000;
const DEFAULT_WAIT_INTERVAL_MS = 5_000;
const CONFLICTING_PAYLOAD_STATUSES = new Set([
  "malformed_da",
  "root_mismatch",
  "conflicted",
]);

const sha256Hex = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const normalizeBaseUrl = (value: string): string => {
  const parsed = new URL(value);
  const pathname = parsed.pathname.replace(/\/+$/, "");
  if (pathname.endsWith("/da/payload")) {
    throw new Error(
      "DA payload endpoint must be a base URL; do not include /da/payload.",
    );
  }
  parsed.pathname = pathname.length === 0 ? "/" : pathname;
  parsed.search = "";
  parsed.hash = "";
  return parsed.toString().replace(/\/$/, "");
};

const endpointUrl = (
  baseUrl: string,
  path: string,
  params: Readonly<Record<string, string>>,
): string => {
  const url = new URL(`${normalizeBaseUrl(baseUrl)}${path}`);
  for (const [key, value] of Object.entries(params)) {
    url.searchParams.set(key, value);
  }
  return url.toString();
};

export const daPayloadMetadataUrl = (
  baseUrl: string,
  headerHash: string,
): string =>
  endpointUrl(baseUrl, "/da/payload/metadata", { header_hash: headerHash });

export const daPayloadCborUrl = (baseUrl: string, headerHash: string): string =>
  endpointUrl(baseUrl, "/da/payload", { header_hash: headerHash });

export const daWatcherStatusUrl = ({
  baseUrl,
  deploymentFingerprint,
  headerHash,
}: {
  readonly baseUrl: string;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
}): string =>
  endpointUrl(
    baseUrl,
    `/v1/deployments/${encodeURIComponent(
      deploymentFingerprint,
    )}/headers/${headerHash}/status`,
    {},
  );

const fetchWithTimeout = async (
  fetchFn: DaGateFetch,
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

const readJson = async (response: Response): Promise<unknown> => {
  try {
    return await response.json();
  } catch {
    return undefined;
  }
};

const stringField = (value: unknown, field: string): string | null =>
  typeof value === "object" &&
  value !== null &&
  typeof (value as Record<string, unknown>)[field] === "string"
    ? ((value as Record<string, unknown>)[field] as string)
    : null;

const arrayLength = (value: unknown, field: string): number | null =>
  typeof value === "object" &&
  value !== null &&
  Array.isArray((value as Record<string, unknown>)[field])
    ? ((value as Record<string, unknown>)[field] as unknown[]).length
    : null;

const submissionEvidence = (
  value: unknown,
): DaGateProbeResult["watcherL1Submissions"] => {
  if (
    typeof value !== "object" ||
    value === null ||
    !Array.isArray((value as Record<string, unknown>).l1Submissions)
  ) {
    return [];
  }
  return (
    value as { readonly l1Submissions: readonly unknown[] }
  ).l1Submissions.map((entry) =>
    typeof entry === "object" && entry !== null
      ? {
          ...(typeof (entry as Record<string, unknown>).txKind === "string"
            ? { txKind: (entry as Record<string, unknown>).txKind as string }
            : {}),
          ...(typeof (entry as Record<string, unknown>).txHash === "string"
            ? { txHash: (entry as Record<string, unknown>).txHash as string }
            : {}),
          ...(typeof (entry as Record<string, unknown>).resultStatus ===
          "string"
            ? {
                resultStatus: (entry as Record<string, unknown>)
                  .resultStatus as string,
              }
            : {}),
        }
      : {},
  );
};

const classifyProbe = ({
  payloadAvailable,
  payloadHashMatches,
  watcherConfigured,
  watcherHeaderStatus,
  watcherPayloadStatus,
}: {
  readonly payloadAvailable: boolean;
  readonly payloadHashMatches: boolean;
  readonly watcherConfigured: boolean;
  readonly watcherHeaderStatus: string | null;
  readonly watcherPayloadStatus: string | null;
}): Pick<DaGateProbeResult, "status" | "nextSafeAction" | "reason"> => {
  if (!payloadHashMatches) {
    return {
      status: "blocked",
      nextSafeAction: "inspect_da_payload_conflict",
      reason: "payload hash does not match metadata",
    };
  }
  if (!payloadAvailable) {
    return {
      status: "pending",
      nextSafeAction: "wait_for_da_payload",
      reason: "DA payload is not available from the configured endpoint",
    };
  }
  if (!watcherConfigured) {
    return {
      status: "satisfied",
      nextSafeAction: "continue",
      reason: "DA payload is available; watcher status was not requested",
    };
  }
  if (
    watcherPayloadStatus !== null &&
    CONFLICTING_PAYLOAD_STATUSES.has(watcherPayloadStatus)
  ) {
    return {
      status: "blocked",
      nextSafeAction: "inspect_da_payload_conflict",
      reason: `watcher payload status is ${watcherPayloadStatus}`,
    };
  }
  if (watcherPayloadStatus !== "verified") {
    return {
      status: "pending",
      nextSafeAction: "wait_for_da_payload",
      reason: `watcher payload status is ${watcherPayloadStatus ?? "missing"}`,
    };
  }
  if (watcherHeaderStatus !== "attested" && watcherHeaderStatus !== "merged") {
    return {
      status: "pending",
      nextSafeAction: "wait_for_da_attestation",
      reason: `watcher header status is ${watcherHeaderStatus ?? "missing"}`,
    };
  }
  return {
    status: "satisfied",
    nextSafeAction: "continue",
    reason: `watcher header status is ${watcherHeaderStatus}`,
  };
};

export const probeDaGate = async ({
  headerHash,
  payloadEndpointBaseUrl,
  watcherBaseUrl,
  deploymentFingerprint,
  fetchFn = fetch,
  now = new Date(),
  requestTimeoutMs = DEFAULT_REQUEST_TIMEOUT_MS,
}: ProbeDaGateOptions): Promise<DaGateProbeResult> => {
  const normalizedHeaderHash = headerHash.toLowerCase();
  if (!HEADER_HASH_REGEX.test(normalizedHeaderHash)) {
    throw new Error("headerHash must be a 56-character hex string.");
  }
  if (
    (watcherBaseUrl === undefined) !==
    (deploymentFingerprint === undefined)
  ) {
    throw new Error(
      "watcherBaseUrl and deploymentFingerprint must be provided together.",
    );
  }

  const payloadBaseUrl = normalizeBaseUrl(payloadEndpointBaseUrl);
  const http: DaGateHttpEvidence[] = [];
  let metadata: unknown;
  let payloadBytes: Uint8Array | null = null;
  const metadataUrl = daPayloadMetadataUrl(
    payloadBaseUrl,
    normalizedHeaderHash,
  );
  try {
    const response = await fetchWithTimeout(
      fetchFn,
      metadataUrl,
      requestTimeoutMs,
    );
    http.push({
      label: "payload_metadata",
      url: metadataUrl,
      statusCode: response.status,
      ok: response.ok,
    });
    metadata = await readJson(response);
  } catch (error) {
    http.push({
      label: "payload_metadata",
      url: metadataUrl,
      statusCode: null,
      ok: false,
      error: error instanceof Error ? error.message : String(error),
    });
  }

  const payloadUrl = daPayloadCborUrl(payloadBaseUrl, normalizedHeaderHash);
  try {
    const response = await fetchWithTimeout(
      fetchFn,
      payloadUrl,
      requestTimeoutMs,
    );
    http.push({
      label: "payload_cbor",
      url: payloadUrl,
      statusCode: response.status,
      ok: response.ok,
    });
    if (response.ok) {
      payloadBytes = new Uint8Array(await response.arrayBuffer());
    }
  } catch (error) {
    http.push({
      label: "payload_cbor",
      url: payloadUrl,
      statusCode: null,
      ok: false,
      error: error instanceof Error ? error.message : String(error),
    });
  }

  let watcherStatus: unknown;
  const watcherBase =
    watcherBaseUrl === undefined ? undefined : normalizeBaseUrl(watcherBaseUrl);
  if (watcherBase !== undefined && deploymentFingerprint !== undefined) {
    const url = daWatcherStatusUrl({
      baseUrl: watcherBase,
      deploymentFingerprint,
      headerHash: normalizedHeaderHash,
    });
    try {
      const response = await fetchWithTimeout(fetchFn, url, requestTimeoutMs);
      http.push({
        label: "watcher_status",
        url,
        statusCode: response.status,
        ok: response.ok,
      });
      watcherStatus = await readJson(response);
    } catch (error) {
      http.push({
        label: "watcher_status",
        url,
        statusCode: null,
        ok: false,
        error: error instanceof Error ? error.message : String(error),
      });
    }
  }

  const expectedPayloadHash =
    stringField(metadata, "payloadHash") ??
    stringField(metadata, "payloadSha256");
  const payloadHash =
    payloadBytes === null || payloadBytes.length === 0
      ? null
      : sha256Hex(payloadBytes);
  const payloadHashMatches =
    expectedPayloadHash === null ||
    payloadHash === null ||
    expectedPayloadHash === payloadHash;
  const watcherHeader =
    typeof watcherStatus === "object" && watcherStatus !== null
      ? (watcherStatus as Record<string, unknown>).header
      : undefined;
  const watcherPayload =
    typeof watcherStatus === "object" && watcherStatus !== null
      ? (watcherStatus as Record<string, unknown>).payload
      : undefined;
  const watcherHeaderStatus = stringField(watcherHeader, "status");
  const watcherPayloadStatus = stringField(watcherPayload, "validationStatus");
  const classified = classifyProbe({
    payloadAvailable: payloadBytes !== null && payloadBytes.length > 0,
    payloadHashMatches,
    watcherConfigured: watcherBase !== undefined,
    watcherHeaderStatus,
    watcherPayloadStatus,
  });

  return {
    schemaVersion: E2E_DA_GATE_SCHEMA_VERSION,
    headerHash: normalizedHeaderHash,
    ...(deploymentFingerprint === undefined ? {} : { deploymentFingerprint }),
    status: classified.status,
    nextSafeAction: classified.nextSafeAction,
    checkedAt: now.toISOString(),
    payloadEndpointBaseUrl: payloadBaseUrl,
    ...(watcherBase === undefined ? {} : { watcherBaseUrl: watcherBase }),
    payloadBytes: payloadBytes?.length ?? null,
    payloadHash,
    expectedPayloadHash,
    watcherHeaderStatus,
    watcherPayloadStatus,
    watcherSignatureCount: arrayLength(watcherStatus, "signatures"),
    watcherL1Submissions: submissionEvidence(watcherStatus),
    http,
    reason: classified.reason,
  };
};

export const waitForDaGate = async ({
  timeoutMs = DEFAULT_WAIT_TIMEOUT_MS,
  intervalMs = DEFAULT_WAIT_INTERVAL_MS,
  sleep = (milliseconds) =>
    new Promise((resolve) => setTimeout(resolve, milliseconds)),
  ...options
}: WaitForDaGateOptions): Promise<WaitForDaGateResult> => {
  const startedAt = Date.now();
  let attempts = 0;
  let latest: DaGateProbeResult | undefined;
  for (;;) {
    attempts += 1;
    latest = await probeDaGate(options);
    if (latest.status !== "pending") {
      return { ...latest, attempts, timedOut: false };
    }
    if (Date.now() - startedAt >= timeoutMs) {
      return { ...latest, attempts, timedOut: true };
    }
    await sleep(intervalMs);
  }
};
