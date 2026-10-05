/**
 * Parsing and HTTP access for the local Ogmios slot evidence: the `/health`
 * document, the `queryNetwork/tip` slot and the Shelley genesis epoch.
 */
import { createHash } from "node:crypto";

export type SubmitSlotSnapshot = {
  readonly source: "local_ogmios_tip" | "emulator" | "test";
  readonly currentSlot: number;
  /**
   * The slot of the ledger's latest block. `currentSlot` runs ahead of it on
   * wall time between blocks, and the mempool checks a validity lower bound
   * against this tip, not `currentSlot`. Absent where the source has no block
   * gaps (emulator, tests): there `currentSlot` is the ledger tip.
   */
  readonly ledgerTipSlot?: number;
  readonly observedAtMs: number;
  readonly slotLengthMs: number;
  readonly health?: {
    readonly connectionStatus?: string;
    readonly networkSynchronization?: number;
    readonly lastKnownTipSlot?: number;
    readonly lastTipUpdate?: string;
  };
};

export type ShelleyGenesisSlotConfig = {
  readonly startTimeMs: number;
  readonly slotLengthMs: number;
  /**
   * The Praos active-slot coefficient `f`: the expected share of slots that
   * carry a block. Absent only when the genesis document omits it.
   */
  readonly activeSlotsCoefficient?: number;
};

export type ShelleyGenesisSlotEvidence = ShelleyGenesisSlotConfig & {
  readonly configurationSha256: string;
};

export type FetchLike = (
  input: string,
  init?: RequestInit,
) => Promise<Response>;

export type OgmiosHealthEvidence = NonNullable<SubmitSlotSnapshot["health"]>;

/**
 * Why the local Ogmios cannot vouch for the L1 clock right now. Every reason
 * is a state the chain or the local Ogmios leaves on its own (a block gap, a
 * restart, a resync, a genesis not yet started), so callers wait and re-read
 * rather than exit. A malformed answer, a slot length other than the
 * profile's or a wrong network never takes this class.
 */
export type OgmiosSlotEvidenceUnavailableReason =
  | "ogmios_unreachable"
  | "ogmios_query_unavailable"
  | "ogmios_not_connected"
  | "ogmios_not_synchronized"
  | "ogmios_no_tip"
  | "ogmios_tip_stale"
  | "ogmios_clock_disagreement"
  | "genesis_not_started";

export class OgmiosSlotEvidenceUnavailableError extends Error {
  readonly retryable = true;
  readonly reason: OgmiosSlotEvidenceUnavailableReason;

  constructor(
    reason: OgmiosSlotEvidenceUnavailableReason,
    message: string,
    options?: { readonly cause?: unknown },
  ) {
    super(message, options);
    this.name = "OgmiosSlotEvidenceUnavailableError";
    this.reason = reason;
  }
}

/** The transient slot-evidence error anywhere on `error`'s cause chain. */
export const ogmiosSlotEvidenceUnavailableCause = (
  error: unknown,
): OgmiosSlotEvidenceUnavailableError | undefined => {
  let current: unknown = error;
  for (let depth = 0; depth < 16 && current !== undefined; depth += 1) {
    if (current instanceof OgmiosSlotEvidenceUnavailableError) {
      return current;
    }
    current =
      typeof current === "object" && current !== null && "cause" in current
        ? (current as { readonly cause?: unknown }).cause
        : undefined;
  }
  return undefined;
};

// Statuses a healthy Ogmios answers while it, or a proxy in front of it,
// restarts or sheds load. Every other non-2xx status is a refusal.
const TRANSIENT_HTTP_STATUSES: ReadonlySet<number> = new Set([
  408, 425, 429, 500, 502, 503, 504,
]);

// JSON-RPC protocol errors: the request itself is wrong, so asking again
// cannot help. Ogmios query errors (a ledger state not yet acquirable, an era
// not yet reached) are states that pass.
const JSON_RPC_PROTOCOL_ERROR_CODES: ReadonlySet<number> = new Set([
  -32700, -32600, -32601, -32602,
]);

const numberFromUnknown = (value: unknown): number | null => {
  if (typeof value === "number") {
    return Number.isSafeInteger(value) && value >= 0 ? value : null;
  }
  if (typeof value === "bigint") {
    return value >= 0n && value <= BigInt(Number.MAX_SAFE_INTEGER)
      ? Number(value)
      : null;
  }
  if (typeof value === "string" && /^\d+$/.test(value)) {
    const parsed = Number(value);
    return Number.isSafeInteger(parsed) ? parsed : null;
  }
  return null;
};

const synchronizationFromUnknown = (value: unknown): number | undefined => {
  if (typeof value === "number" && Number.isFinite(value)) {
    return value > 1 ? value / 100 : value;
  }
  if (typeof value !== "string") {
    return undefined;
  }
  const trimmed = value.trim();
  if (trimmed.endsWith("%")) {
    const parsed = Number(trimmed.slice(0, -1));
    return Number.isFinite(parsed) ? parsed / 100 : undefined;
  }
  const parsed = Number(trimmed);
  return Number.isFinite(parsed)
    ? parsed > 1
      ? parsed / 100
      : parsed
    : undefined;
};

const record = (value: unknown): Record<string, unknown> | null =>
  typeof value === "object" && value !== null
    ? (value as Record<string, unknown>)
    : null;

const canonicalJsonValue = (value: unknown): unknown => {
  if (Array.isArray(value)) {
    return value.map(canonicalJsonValue);
  }
  const object = record(value);
  if (object !== null) {
    return Object.fromEntries(
      Object.keys(object)
        .sort()
        .map((key) => [key, canonicalJsonValue(object[key])]),
    );
  }
  return value;
};

export const normalizeOgmiosHttpUrl = (url: string): string => {
  const parsed = new URL(url.trim());
  if (parsed.protocol === "ws:") {
    parsed.protocol = "http:";
  } else if (parsed.protocol === "wss:") {
    parsed.protocol = "https:";
  }
  parsed.hash = "";
  return parsed.toString().replace(/\/$/, "");
};

export const joinUrl = (base: string, path: string): string =>
  `${base.replace(/\/+$/, "")}/${path.replace(/^\/+/, "")}`;

export const fetchTextWithTimeout = async (
  fetchImpl: FetchLike,
  url: string,
  init: RequestInit,
  timeoutMs: number,
): Promise<string> => {
  const controller = new AbortController();
  const upstreamSignal = init.signal;
  const abortFromUpstream = () => controller.abort(upstreamSignal?.reason);
  if (upstreamSignal?.aborted === true) {
    abortFromUpstream();
  } else {
    upstreamSignal?.addEventListener("abort", abortFromUpstream, {
      once: true,
    });
  }
  const timeout = setTimeout(() => controller.abort(), timeoutMs);
  const unreachable = (cause: unknown) =>
    // A caller's own cancellation is not an Ogmios outage: pass it through.
    upstreamSignal?.aborted === true
      ? cause
      : new OgmiosSlotEvidenceUnavailableError(
          "ogmios_unreachable",
          `Ogmios at ${url} is unreachable: ${
            cause instanceof Error ? cause.message : String(cause)
          }`,
          { cause },
        );
  try {
    let response: Response;
    let body: string;
    try {
      response = await fetchImpl(url, {
        ...init,
        signal: controller.signal,
      });
      body = await response.text();
    } catch (cause) {
      throw unreachable(cause);
    }
    if (!response.ok) {
      const message = `HTTP ${response.status.toString()} from ${url}: ${body}`;
      throw TRANSIENT_HTTP_STATUSES.has(response.status)
        ? new OgmiosSlotEvidenceUnavailableError("ogmios_unreachable", message)
        : new Error(message);
    }
    return body;
  } finally {
    clearTimeout(timeout);
    upstreamSignal?.removeEventListener("abort", abortFromUpstream);
  }
};

export const parseJson = (body: string, label: string): unknown => {
  try {
    return JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error(`Failed to parse ${label} JSON`, { cause });
  }
};

/**
 * Refuses a JSON-RPC error answer. A protocol error is terminal; an Ogmios
 * query error is the transient `ogmios_query_unavailable`.
 */
export const assertNoOgmiosJsonRpcError = (
  payload: unknown,
  label: string,
): void => {
  const error = record(record(payload)?.error);
  if (error === null) {
    return;
  }
  const code = typeof error.code === "number" ? error.code : undefined;
  const message = `${label} query failed: code=${String(code)},message=${String(
    error.message,
  )}`;
  if (code === undefined || JSON_RPC_PROTOCOL_ERROR_CODES.has(code)) {
    throw new Error(message);
  }
  throw new OgmiosSlotEvidenceUnavailableError(
    "ogmios_query_unavailable",
    message,
  );
};

const activeSlotsCoefficientFromUnknown = (
  value: unknown,
): number | undefined => {
  if (value === undefined || value === null) {
    return undefined;
  }
  const ratio =
    typeof value === "string" ? /^\s*(\d+)\s*\/\s*(\d+)\s*$/.exec(value) : null;
  const coefficient =
    ratio === null
      ? typeof value === "number"
        ? value
        : typeof value === "string"
          ? Number(value)
          : Number.NaN
      : Number(ratio[1]) / Number(ratio[2]);
  if (!Number.isFinite(coefficient) || coefficient <= 0 || coefficient > 1) {
    throw new Error(
      `Ogmios Shelley genesis activeSlotsCoefficient is invalid: ${JSON.stringify(value) ?? typeof value}`,
    );
  }
  return coefficient;
};

const firstSlot = (...values: readonly unknown[]): number | null => {
  for (const value of values) {
    const slot = numberFromUnknown(value);
    if (slot !== null) {
      return slot;
    }
  }
  return null;
};

export const parseOgmiosTipSlot = (payload: unknown): number => {
  const root = record(payload);
  const result = record(root?.result);
  const point = record(result?.point);
  const tip = record(result?.tip);
  const slot = firstSlot(result?.slot, point?.slot, tip?.slot);
  if (slot === null) {
    throw new Error("Ogmios queryNetwork/tip response did not include a slot");
  }
  return slot;
};

export const parseOgmiosShelleyGenesisSlotConfig = (
  payload: unknown,
): ShelleyGenesisSlotEvidence => {
  const root = record(payload);
  const result = record(root?.result);
  const startTime = result?.startTime;
  if (typeof startTime !== "string" || startTime.trim().length === 0) {
    throw new Error(
      "Ogmios Shelley genesis response did not include a startTime",
    );
  }
  const startTimeMs = Date.parse(startTime);
  if (
    !Number.isSafeInteger(startTimeMs) ||
    startTimeMs < 0 ||
    !/(?:Z|[+-]\d{2}:\d{2})$/i.test(startTime)
  ) {
    throw new Error(
      `Ogmios Shelley genesis startTime is invalid: ${startTime}`,
    );
  }

  const slotLength = record(result?.slotLength);
  const slotLengthMs = numberFromUnknown(slotLength?.milliseconds);
  if (slotLengthMs === null || slotLengthMs <= 0) {
    throw new Error(
      "Ogmios Shelley genesis response did not include a positive integer slotLength.milliseconds",
    );
  }
  const activeSlotsCoefficient = activeSlotsCoefficientFromUnknown(
    result?.activeSlotsCoefficient,
  );
  return {
    startTimeMs,
    slotLengthMs,
    ...(activeSlotsCoefficient === undefined ? {} : { activeSlotsCoefficient }),
    configurationSha256: createHash("sha256")
      .update(JSON.stringify(canonicalJsonValue(result)))
      .digest("hex"),
  };
};

/**
 * The `/health` tip fields Ogmios reports as null (or "origin") before it has
 * seen a block: absent evidence to wait for, not a malformed answer.
 */
export const ogmiosHealthTipFieldsNotYetKnown = (
  payload: unknown,
): ReadonlySet<"networkSynchronization" | "lastKnownTip" | "lastTipUpdate"> => {
  const root = record(payload);
  const unknownFields = new Set<
    "networkSynchronization" | "lastKnownTip" | "lastTipUpdate"
  >();
  for (const field of [
    "networkSynchronization",
    "lastKnownTip",
    "lastTipUpdate",
  ] as const) {
    const value = root?.[field];
    if (value === undefined || value === null || value === "origin") {
      unknownFields.add(field);
    }
  }
  return unknownFields;
};

export const parseOgmiosHealthEvidence = (
  payload: unknown,
): OgmiosHealthEvidence => {
  const root = record(payload);
  const lastKnownTip = record(root?.lastKnownTip);
  return {
    ...(typeof root?.connectionStatus === "string"
      ? { connectionStatus: root.connectionStatus }
      : {}),
    ...(synchronizationFromUnknown(root?.networkSynchronization) === undefined
      ? {}
      : {
          networkSynchronization: synchronizationFromUnknown(
            root?.networkSynchronization,
          )!,
        }),
    ...(numberFromUnknown(lastKnownTip?.slot) === null
      ? {}
      : { lastKnownTipSlot: numberFromUnknown(lastKnownTip?.slot)! }),
    ...(typeof root?.lastTipUpdate === "string"
      ? { lastTipUpdate: root.lastTipUpdate }
      : {}),
  };
};
